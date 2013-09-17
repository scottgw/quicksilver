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
-export([row_hist/1, merge_hist/2, thresh/4]).

% worker, processes and sends the results back
reduce2d_worker(Gather, Parent, Chunk, Function, Agg, Zero) ->
  spawn(fun() ->
                Pid = self(),
                X = lists:map(Function, Chunk),
                Result = case Gather of
                             true -> case Agg of
                                         none -> X;
                                         _ -> lists:foldl(Agg, Zero, X)
                                     end;
                             _ -> ok
                         end,
                Parent ! {Pid, Result}
        end).

reduce2d_join_acc(_Gather, [], _Aggregator, Acc) -> Acc;
reduce2d_join_acc(Gather, [Pid|Pids], Aggregator, Acc) ->
    X = receive
            {Pid, Result} ->
                 Result
        end,
    Acc2 = case Gather of
               true -> case Aggregator of
                           none -> [X | Acc];
                           _ -> Aggregator(Acc, X)
                       end;
               _ -> ok
           end,
    reduce2d_join_acc(Gather, Pids, Aggregator, Acc2).

reduce2d_join(Gather, [Pid|Pids], Aggregator) ->
    X = receive
            {Pid, Result} -> Result
        end,
    reduce2d_join_acc(Gather, Pids, Aggregator, X).

chunk(Matrix, L, Acc) ->
    S = 16,
    if
        L > S ->
            {X, Rest} = lists:split(S, Matrix),
            chunk (Rest, L - S, [X|Acc]);
        true -> [Matrix | Acc]
    end.

reduce2d(Gather, Chunks, Aggr, Zero, Func) ->
    Parent = self(),
    %% parallel for on rows
    Pids = [reduce2d_worker(Gather, Parent, C, Func, Aggr, Zero) || C <- Chunks],
    reduce2d_join(Gather, Pids, Aggr).

max_matrix(Chunks) ->
  reduce2d(true,
           Chunks,
           fun erlang:max/2,
           0,
           fun lists:max/1).

merge_hist (Hist1, Hist2) ->
    dict:merge(fun (_K, V1, V2) ->
                          V1 + V2
                  end, Hist1, Hist2).

empty_hist () ->
    dict:from_list ([]). %%{X,0} || X <- lists:seq (0, 99)]).    

row_hist (Row) ->
    lists:foldl (fun (Elem, HistAcc) ->
                         dict:update_counter (Elem, 1, HistAcc)
                 end,
                 empty_hist(),
                 Row).

adapt_key(Tab, Key) ->
    case ets:member(Tab, Key) of
        true -> [{_,Elem}] = ets:lookup(Tab, Key),
                {Key, Elem};
        _ -> {Key, 0}
    end.

row_hist2(Parent, Chunk) ->
    Tab = ets:new(histogram_table, []),
    lists:foreach(fun(Row) ->
                          lists:foreach(fun(X) ->
                                                case ets:member(Tab, X) of
                                                    true -> ets:update_counter(Tab, X, 1);
                                                    _ -> ets:insert(Tab, {X, 1})
                                                end
                                        end,
                                        Row)
                  end,
                  Chunk),

    Hist = dict:from_list(lists:map(fun(Key) -> adapt_key(Tab, Key) end,
                                    lists:seq(0, 99))),
    ets:delete(Tab),
    Parent ! Hist.


fill_histogram (Chunks) ->
    %% reduce2d(Chunks,
    %%          fun ?MODULE:merge_hist/2,
    %%          empty_hist(),
    %%          fun ?MODULE:row_hist/1).
    Parent = self(),
    Pids = [spawn(fun() -> row_hist2 (Parent, Chunk) end) || Chunk <- Chunks],
    Hists = [receive
                 Result -> Result
             end || _Pid <- Pids],
    Hist = lists:foldl (fun (H1, H2) -> merge_hist(H1, H2) end, empty_hist(), Hists),
    lists:reverse(lists:map (fun ({_Idx, Count}) ->
                                     Count
                             end, dict:to_list (Hist))).
    


get_threshold(-1, [], _) -> 0;
get_threshold(Index, [Head | _], Count) when Head > Count ->
    Index;
get_threshold(Index, [Head | Tail], Count) ->
    get_threshold (Index - 1, Tail, Count - Head).

filter(Gather, Matrix, Threshold) ->
    lists:concat (reduce2d(Gather,
                           Matrix, 
                           none,
                           [],
                           fun(X) ->
                                   lists:map(fun(Y) -> 
                                                     Y >= Threshold 
                                             end,
                                             X) 
                           end)).

thresh(Gather, N, Matrix, Percent) ->
    Chunks = chunk (Matrix, length(Matrix), []),
    io:format("max~n"),
    Nmax = max_matrix(Chunks),
    io:format("histogram~n"),
    Histogram = fill_histogram(Chunks),
    io:format("filtered~n"),
    Count = (N * N * Percent) / 100,
    Threshold = get_threshold(Nmax, Histogram, Count),
    filter(Gather, Chunks, Threshold),
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
    Gather = case gather of
                 error -> false;
                 _ -> true
             end,             
    {ok, [[NStr]]} = init:get_argument(nelts),
    {ok, [[PercentStr]]} = init:get_argument(percent),
    N = list_to_integer(NStr),
    Percent = list_to_integer(PercentStr),

    Matrix = read_matrix(N, N),
    %% fprog:apply(?MODULE, thresh, [N, N, Matrix, Percent]).
    {Time, _Thresh} = timer:tc(fun() -> thresh(Gather, N, Matrix, Percent) end),
    io:format(standard_error, "~p~n", [Time/1000000]).
