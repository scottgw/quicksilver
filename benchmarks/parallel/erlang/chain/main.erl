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
-export([chain_worker/0]).

%%%%%%%%%%%%%%%%%%%%%
%% Randmat
%%%%%%%%%%%%%%%%%%%%%

-define(INT_MAX,4294967296).
-define(RAND_MAX,100).
-define(LCG_A,1664525).
-define(LCG_C,1013904223).

randvec(Ncols, S) ->
    randvec_acc (Ncols, S, []).

randvec_acc (0, _, Acc) ->
    lists:reverse (Acc);
randvec_acc (Ncols, S, Acc) ->
    NewS = (?LCG_A * S + ?LCG_C) rem ?INT_MAX,
    Val = NewS rem ?RAND_MAX,
    randvec_acc (Ncols - 1, NewS, [Val | Acc]).

rand_chunk_acc(_N, _StartSeed, 0, Acc) ->
    Acc;
rand_chunk_acc(N, StartSeed, Height, Acc) ->
    V = randvec(N, StartSeed + Height - 1),
    rand_chunk_acc(N, StartSeed, Height - 1, [V|Acc]).

randmat(N, StartSeed, Height) ->
    timer:tc(fun() -> rand_chunk_acc(N, StartSeed, Height, []) end).

%%%%%%%%%%%%%%
%% Thresh
%%%%%%%%%%%%%

merge_hist (Hist1, Hist2) ->
    dict:merge(fun (_K, V1, V2) ->
                          V1 + V2
                  end, Hist1, Hist2).

empty_hist () ->
    dict:from_list ([]).

adapt_key(Key) ->
    case get(Key) of
        undefined -> {Key, 0};
        V -> {Key, V}
    end.

chunk_hist(Parent, Chunk) ->
    T1 = now(),
    io:foramt("Worker: chunk_hist size ~p~n", [length(Chunk)]),
    {MaxTime, Max} =
        timer:tc(
          fun() -> lists:foldl(
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
                     Chunk) end),
    io:format("chunk hist maxtime: ~p~n", [MaxTime]),
    Hist = dict:from_list(
             lists:map(fun(Key) -> adapt_key(Key) end, lists:seq(0, 99))),

    T2 = now(),
    io:format("chunk hist time1: ~p~n", [timer:now_diff(T2, T1)]),

    Parent ! {histmax, {Max, Hist}},
    Thresh = receive
                    Result -> Result
             end,

    T3 = now(),
    Filtered = lists:map(
                 fun(X) -> lists:map(fun(Y) -> Y >= Thresh end, X) end,
                 Chunk),
    T4 = now(),
    io:format("chunk hist time2: ~p~n", [timer:now_diff(T4, T3)]),

    TotalTime = timer:now_diff(T2, T1) + timer:now_diff(T4, T3),

    {TotalTime, Filtered}.

fill_histogram (Pids, N, Percent) ->
    HistMaxs = [receive {histmax, Result} -> Result end
                || _Pid <- Pids],
    T1 = now(),
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

    Count = (N * N * Percent) div 100,

    Threshold = get_threshold(Max, Histogram, Count),
    T2 = now(),
    [ Pid ! Threshold || Pid <- Pids],
    timer:now_diff(T2, T1).
        
get_threshold(-1, [], _) -> 0;
get_threshold(Index, [Head | _], Count) when Head > Count ->
    Index;
get_threshold(Index, [Head | Tail], Count) ->
    get_threshold (Index - 1, Tail, Count - Head).


%%%%%%%%%%%%%%
%% Winnow
%%%%%%%%%%%%%


winnow_row(_RowNo, _ColNo, [], [], Acc) -> Acc;
winnow_row(RowNo, ColNo, [X|Xs], [M|Ms], Acc) ->
    Y = if
            M -> [{X, {RowNo, ColNo}} | Acc];
            true -> Acc
        end,
    winnow_row(RowNo, ColNo + 1, Xs, Ms, Y).

winnow_acc(_, [], [], Acc) -> Acc;
winnow_acc(RowStart, [MatrixRow|MatChunk], [MaskRow|MaskChunk], Acc) ->
    R = winnow_row(RowStart, 0, MatrixRow, MaskRow, []),
    winnow_acc(RowStart + 1, MatChunk, MaskChunk, [R|Acc]).
    

winnow_process(Parent, MatrixChunk, MaskChunk, RowStart) ->
    T1 = now(),
    %% io:format("~p~n", [{MatrixChunk, MaskChunk, RowStart}]),
    Chunk = winnow_acc(RowStart, MatrixChunk, MaskChunk, []),
    Together = lists:concat(Chunk),
    Sorted = lists:sort(Together),
    %% io:format("winnow worker sorted ~p~n", [Sorted]),
    T2 = now(),
    Time = timer:now_diff(T2, T1),


    Parent ! {self(), Sorted},
    Time.

sort_merge(Xs, [], Acc) ->
    lists:reverse(Acc) ++ Xs;
sort_merge([], Ys, Acc) ->
    lists:reverse(Acc) ++ Ys;
sort_merge([X|Xs], [Y|Ys], Acc) ->
    if
        X =< Y -> sort_merge(Xs, [Y|Ys], [X|Acc]);
        true -> sort_merge([X|Xs], Ys, [Y|Acc])
    end.

sort_merge(Xs, Ys) ->
    sort_merge(Xs, Ys, []).


chunk(Points, RowStart, S, L, Acc) ->
    if
        L > S*2 - 1->
            {X, Rest} = lists:split(S, Points),
            chunk (Rest, RowStart + S, S, L - S, [{RowStart, X}|Acc]);
        true -> [{RowStart, Points} | Acc]
    end.

core_chunk(Matrix, Cores) ->
    chunk(Matrix, 0, length(Matrix) div Cores, length(Matrix), []).

winnow(Cores, Pids, Nelts) ->
    %% Merging sorted lists from workers, collecting times.
    {Sorted, TotalTime} =
        lists:foldl(
          fun(Pid, {Sorted, TotalTime}) ->
                  receive
                      {Pid, List} -> List
                  end,
                  {SortTime, Merged} =
                      timer:tc(fun() -> sort_merge(Sorted, List) end),
                  {Merged, SortTime + TotalTime}
          end,
          {[], 0},
          Pids),

    T1 = now(),
    Stride = length(Sorted) div Nelts,
    Final = drop_nth(Stride, Sorted, Nelts, []),
    io:format("winnow dropnth: ~p~n", [length(Final)]),
    T2 = now(),

    {TotalTime + timer:now_diff(T2, T1), Final}.


drop_nth(_Stride, _List, 0, Acc) -> lists:reverse(Acc);
drop_nth(Stride, List, Nelts, Acc) ->
    {[{_, Pos} | _], Rest} = lists:split(Stride, List),
    drop_nth(Stride, Rest, Nelts - 1, [Pos | Acc] ).

%%%%%%%%%%%
%% Outer %%
%%%%%%%%%%%
sqr(X) ->
    X * X.

distance({Ax, Ay}, {Bx, By}) ->
    math:sqrt(sqr(Ax - Bx) + sqr(Ay - By)).


calc_row (_Nelts, _RowN, [], _Points, Acc) -> Acc;
calc_row (Nelts, RowN, [Other|Others], Points, Acc) ->
    Dists = lists:map(fun (X) -> distance (X, Other) end, Points),
    Max = lists:max(Dists),
    {Pre, [_|Post]} = lists:split(RowN, Dists),
    Final = Pre ++ [Max*Nelts|Post],
    calc_row(Nelts, RowN + 1, Others, Points, [Final | Acc]).

outer_worker ({RowStart, Rows}, Points) ->
    Nelts = length(Points),
    timer:tc(
      fun() ->
              MatrixPart = calc_row(Nelts, RowStart, Rows, Points, []),
              Vector = [ distance({0.0, 0.0}, P) || P <- Points ],
              {MatrixPart, Vector}
      end).

outer(Pids, Points) ->
    PointChunks = core_chunk(Points, length(Pids)),
    io:format("-------------------------- ~p ~p~n", [length(PointChunks), length(Pids)]),
    PointPids = lists:zip(Pids, PointChunks),
    [Pid ! {outer_start, Chunk, Points} || {Pid, Chunk} <- PointPids ].

%%%%%%%%%%%%%
%% Product %%
%%%%%%%%%%%%%

prod_row ([], [], Acc) ->
    Acc;
prod_row ([R|Row], [V|Vector], Acc) ->
    prod_row (Row, Vector, Acc + (R*V)).

prod_chunk([], _Vector, Acc) ->
    Acc;
prod_chunk([C|Chunk], Vector, Acc) ->
    X = prod_row(C, Vector, 0),
    prod_chunk(Chunk, Vector, [X|Acc]).

prod_chunk(Chunk, Vector) ->
    T1 = now(),
    Result = prod_chunk(Chunk, Vector, []),
    T2 = now(),
    {timer:now_diff(T2, T1), Result}.

%%%%%%%%%%%
%% Chain %%
%%%%%%%%%%%

chain_worker() ->
    %% Randmat and start
    receive
        {randmat_start, Parent, Seed, StartRow, MaxHeight, Nelts} ->
            {Parent, Seed, StartRow, MaxHeight, Nelts}
    end,
    io:format("worker received start message~n"),
    {RandmatTime, RandmatPart} = randmat(Nelts, Seed, MaxHeight),
    io:format("worker finished randmat~n"),

    %% Thresh
    {ThreshTime, MaskPart} = chunk_hist(Parent, RandmatPart),
    io:format("worker finished thresh~n"),

    %% Winnow
    WinnowTime = winnow_process (Parent, RandmatPart, MaskPart, StartRow),
    io:format("worker finished winnow~n"),

    %% Outer
    receive
        {outer_start, Rows, Points} -> {Rows, Points}
    end,
    io:format("worker starting outer~n"),
    {OuterTime, {MatrixPart, Vector}} = outer_worker(Rows, Points),
    io:format("worker finished outer: ~p~n", [length(MatrixPart)]),

    %% Product
    {ProductTime, Result} = prod_chunk(MatrixPart, Vector),
    io:format("worker finished product~n"),

    io:format("Worker times: ~p~n",
              [{RandmatTime, ThreshTime, WinnowTime, OuterTime, ProductTime}]),
    TotalTime = RandmatTime + ThreshTime + WinnowTime + OuterTime + ProductTime,

    Parent ! {self(), final_result, Result, TotalTime},
    io:format("worker sent product~n"),
    ok.

chain_master(Cores, Nelts, Seed, Percent, WinnowNelts) ->
    Self = self(),

    CoreList = lists:seq(0, Cores - 1),
    MaxHeight = Nelts div Cores,

    PidCores = [ {spawn(fun() -> chain_worker() end), Core} ||
                   Core <- CoreList],
    {Pids, CoreNums} = lists:unzip (PidCores),

    %% Randmat
    [ Pid ! {randmat_start, Self, Seed + CoreNum * MaxHeight,
             CoreNum * MaxHeight, MaxHeight, Nelts}
      || {Pid, CoreNum} <- PidCores],

    %% Thresh
    ThreshTime = fill_histogram(Pids, Nelts, Percent),

    %% Winnow
    {WinnowTime, WinnowResult} = winnow(Cores, Pids, WinnowNelts),

    %% Outer
    outer(Pids, WinnowResult),
    io:format("master finished outer~n"),

    %% Product
    ResultsTimes = [receive
                   {Pid, final_result, Result, Time} -> {Result, Time}
               end || Pid <- Pids],
    {Results, Times} = lists:unzip(ResultsTimes),

    TotalTime = lists:sum(Times)/Cores + WinnowTime + ThreshTime,
    FlatResults = lists:concat(Results),
    io:format("num results: ~p~n", [length (FlatResults)]),
    io:format("master time: ~p~n", [(WinnowTime + ThreshTime) / 1000000]),
    io:format("compute time: ~p~n", [TotalTime / 1000000]),
    io:format(standard_error, "~p~n", [TotalTime / 1000000]),
    ok.


main() ->
    init:get_argument(gather),

    {ok, [[NeltStr]]} = init:get_argument(nelts),
    {ok, [[SeedStr]]} = init:get_argument(seed),
    {ok, [[PercentStr]]} = init:get_argument(percent),
    {ok, [[WinnowNeltStr]]} = init:get_argument(winnow_nelts),

    Cores = erlang:system_info(schedulers_online),
    Nelts = list_to_integer(NeltStr),
    Seed = list_to_integer(SeedStr),
    Percent = list_to_integer(PercentStr),
    WinnowNelts = list_to_integer(WinnowNeltStr),

    io:format("cores: ~p~n", [Cores]),

    {Time, _Result} =
        timer:tc(
          fun() -> chain_master(Cores, Nelts, Seed, Percent, WinnowNelts) end),
    io:format(standard_error, "~p~n", [Time/1000000]).
