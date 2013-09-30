%
% winnow: weighted point selection
%
% input:
%   matrix: an integer matrix, whose values are used as masses
%   mask: a boolean matrix showing which points are eligible for
%     consideration
%   nrows, ncols: the number of rows and columns
%   nelts: the number of points to select
%
% output:
%   points: a vector of (x, y) points
%
-module(main).
-export([main/0]).

row_process(_RowNo, _ColNo, [], [], Acc) -> Acc;
row_process(RowNo, ColNo, [X|Xs], [M|Ms], Acc) ->
    Y = if
            M =:= 1 -> [{X, {RowNo, ColNo}} | Acc];
            true -> Acc
        end,
    row_process(RowNo, ColNo + 1, Xs, Ms, Y).

chunk_process_acc(_, [], [], Acc) -> Acc;
chunk_process_acc(RowStart, [MatrixRow|MatChunk], [MaskRow|MaskChunk], Acc) ->
    R = row_process(RowStart, 0, MatrixRow, MaskRow, Acc),
    chunk_process_acc(RowStart + 1, MatChunk, MaskChunk, R).
    
chunk_process(Parent, MatrixChunk, MaskChunk, RowStart) ->
    receive start -> ok end,
    T1 = now(),
    Chunk = chunk_process_acc(RowStart, MatrixChunk, MaskChunk, []),
    T11 = now(),
    io:format("Chunk time: ~p ~p ~p ~p ~p~n",
              [RowStart,
               length(MatrixChunk),
               length(MaskChunk),
               length(Chunk),
               timer:now_diff(T11, T1)]),
    Sorted = lists:sort(Chunk),
    T2 = now(),
    Time = timer:now_diff(T2, T1)/1000000,

    Parent ! {Sorted, Time}.

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


chunk(Matrix, S, L, Acc) ->
    if
        L > S ->
            {X, Rest} = lists:split(S, Matrix),
            chunk (Rest, S, L - S, [X | Acc]);
        true -> [Matrix | Acc]
    end.

core_chunk(Matrix, Cores) ->
    chunk(Matrix, length(Matrix) div Cores, length(Matrix), []).

winnow(Cores, Matrix, Mask, Nelts) ->
    ChunkMatrix = core_chunk(Matrix, Cores),
    ChunkMask = core_chunk(Mask, Cores),
    Parent = self(),
    {Starts, _} = lists:mapfoldl(fun(Chunk, Acc) -> {Acc, Acc + length(Chunk)} end,
                            0, ChunkMatrix),

    ChunkZips = lists:zip3(Starts, ChunkMatrix, ChunkMask),

    io:format("row process~n"),
    Pids = [spawn(
              fun() -> chunk_process (Parent,
                                      MatrixChunk,
                                      MaskChunk,
                                      RowStart)
              end) || {RowStart, MatrixChunk, MaskChunk} <- ChunkZips],
              
    %% {Pids, _RowStart} 
    %%     = lists:foldl(
    %%         fun({MatrixChunk, MaskChunk}, {Pids, RowStart}) ->
    %%                 %% io:format("~p~n",[now()]),
    %%                 Pid = spawn(
    %%                         fun() -> chunk_process (Parent,
    %%                                                 MatrixChunk,
    %%                                                 MaskChunk,
    %%                                                 RowStart)
    %%                         end),
    %%                 {[Pid|Pids], RowStart + length(MatrixChunk)}
    %%         end,
    %%         {[],0},
    %%         ChunkZips),


    [Pid ! start || Pid <- Pids],

    %% Merging sorted lists from workers, collecting times.
    {Sorted, TotalTime} =
        lists:foldl(
          fun(_Pid, {Sorted, TotalTime}) ->
                  receive
                      {List, Time} -> {List, Time}
                  end,
                  {SortTime, Merged} =
                      timer:tc(fun() -> sort_merge(Sorted, List) end),
                  {Merged, SortTime/1000000 + Time/Cores + TotalTime}
          end,
          {[], 0},
          Pids),

    T1 = now(),
    Stride = length(Sorted) div Nelts,
    Final = drop_nth(Stride, Sorted),
    T2 = now(),
    io:format(standard_error, "~p~n", [TotalTime + 
                                           timer:now_diff(T2, T1)/1000000]),
    Final.

drop_nth(Stride, List) ->
    if
        length(List) < Stride -> boo;
        true -> {[{_, Pos} | _], Rest} = lists:split(Stride, List),
                [Pos | drop_nth(Stride, Rest)]
    end.

read_vector(_, 0) -> [];
read_vector(Nrows, Ncols) -> 
    Val = if 
              (Nrows * Ncols) rem (Ncols + 1) =:= 1 -> 1;
              true -> 0
          end,
    [ Val | read_vector(Nrows, Ncols - 1)].

read_mask(0, _) -> [];
read_mask(Nrows, Ncols) -> 
    [read_vector(Nrows, Ncols) | read_mask(Nrows - 1, Ncols)].


read_matrix(0, _) -> [];
read_matrix(Nrows, Ncols) -> 
    [lists:duplicate(Ncols, 0) | read_matrix(Nrows - 1, Ncols)].

main() ->          
    {ok, [[OrigNeltsStr]]} = init:get_argument(orignelts),
    {ok, [[NeltsStr]]} = init:get_argument(nelts),

    Cores = erlang:system_info(schedulers_online),
    N = list_to_integer(OrigNeltsStr),
    Nelts = list_to_integer(NeltsStr),

    Matrix = read_matrix(N, N),
    Mask = read_mask(N, N),
    %% io:format("~p~n", [Matrix]),
    %% io:format("~p~n", [Mask]),
    {Time, _Val} = timer:tc(fun() -> winnow(Cores, Matrix, Mask, Nelts) end),
    io:format(standard_error, "~p~n", [Time/1000000]),
    ok.
