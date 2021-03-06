%
% thresh: histogram thresholding
%
% input:
%   matrix: the integer matrix to be thresholded
%   nrows, ncols: the number of rows and columns
%   percent: the percentage of cells to retain
%
% output:
%   mask: a boolean matrix filled with true for cells that are kept
%

-module(main).
-export([main/0]).
%% -export([merge_hist/2, thresh/4]).

chunk(Matrix, S, L, Acc) ->
    if
        L > S ->
            {X, Rest} = lists:split(S, Matrix),
            chunk (Rest, S, L - S, [X | Acc]);
        true -> [Matrix | Acc]
    end.

merge_hist (Hist1, Hist2) ->
    dict:merge(fun (_K, V1, V2) ->
                          V1 + V2
                  end, Hist1, Hist2).

empty_hist () ->
    dict:from_list ([]). %%{X,0} || X <- lists:seq (0, 99)]).    

adapt_key(Key) ->
    case get(Key) of
        undefined -> {Key, 0};
        V -> {Key, V}
    end.

chunk_hist(Parent, Chunk) ->
    T1 = now(),

    Max = lists:foldl(
            fun(Row, MaxAcc1) ->
                    M = lists:foldl(fun(X, MaxAcc2) ->
                                            case get(X) of
                                                undefined -> put(X, 1);
                                                V -> put(X, V + 1)
                                            end,
                                            max(MaxAcc2, X)
                                    end,
                                    MaxAcc1,
                                    Row),
                    max(MaxAcc1, M)
            end,
            0,
            Chunk),

    Hist = dict:from_list(
             lists:map(fun(Key) -> adapt_key(Key) end, lists:seq(0, 99))),


    T2 = now(),
    Parent ! {histmax, {Max, Hist}},
    Thresh = receive
                    Result -> Result
             end,

    T3 = now(),
    FilterRow = fun(X) -> X >= Thresh end,
    FilterChunk = fun(X) -> FilterRow(X) end,
    Filtered = lists:map(FilterChunk, Chunk),
    T4 = now(),

    TotalTime = timer:now_diff(T2, T1) + timer:now_diff(T4, T3),

    Parent ! {self(), thresh, TotalTime, Filtered}.

fill_histogram (Cores, N, Percent, Chunks) ->
    Parent = self(),
    
    Pids = [spawn(
              fun() -> chunk_hist (Parent, Chunk) end
             ) || Chunk <- Chunks],

    HistMaxs = [receive
                 {histmax, Result} -> Result
                end || _Pid <- Pids],

    {Max, Hist} =
        lists:foldl (
          fun ({Max1,H1}, {Max2,H2}) ->
                  {max(Max1, Max2), merge_hist(H1, H2)}
          end,
          {0, empty_hist()},
          HistMaxs),

    Histogram = 
        lists:reverse(lists:map (
                        fun ({_Idx, Count}) ->
                                Count
                        end,
                        dict:to_list (Hist))),

    Count = (N * N * Percent) / 100,

    Threshold = get_threshold(Max, Histogram, Count),

    [ Pid ! Threshold || Pid <- Pids],

    FilterParts = [ receive
                        {Pid, thresh, Time, FilterPart} ->
                            {Time, FilterPart}
                    end || Pid <- Pids],
    {Times, Parts} = lists:unzip(FilterParts),
    AllTime = lists:sum(Times),
    io:format(standard_error, "~p~n", [AllTime / 1000000 / Cores]),
    ok.
        
get_threshold(-1, [], _) -> 0;
get_threshold(Index, [Head | _], Count) when Head > Count ->
    Index;
get_threshold(Index, [Head | Tail], Count) ->
    get_threshold (Index - 1, Tail, Count - Head).

thresh(Cores, N, Matrix, Percent) ->
    Chunks = chunk (Matrix, length(Matrix) div Cores, length(Matrix), []),
    Filtered = fill_histogram(Cores, N, Percent, Chunks),
    %% Filtered = fprof:apply(
    %%              fun() -> fill_histogram(Gather, N, Percent, Chunks) end,
    %%              []),
    %% fprof:profile(),
    %% fprof:analyse({dest, []}),
    ok.

read_vector(_, 0) -> [];
read_vector(Nrows, Ncols) -> 
    Val = Nrows * Ncols rem 100,
    [ Val | read_vector(Nrows, Ncols - 1)].

read_matrix(0, _) -> [];
read_matrix(Nrows, Ncols) ->
    [read_vector(Nrows, Ncols) | read_matrix(Nrows - 1, Ncols)].


main() ->
    init:get_argument(gather),

    {ok, [[NStr]]} = init:get_argument(nelts),
    {ok, [[PercentStr]]} = init:get_argument(percent),

    Cores = erlang:system_info(schedulers_online),
    N = list_to_integer(NStr),
    Percent = list_to_integer(PercentStr),

    Matrix = read_matrix(N, N),

    {Time, _Thresh} = timer:tc(fun() -> thresh(Cores, N, Matrix, Percent) end),
    io:format(standard_error, "~p~n", [Time/1000000]).
