%
% outer: outer product
%
% input:
%   points: a vector of (x, y) points
%   nelts: the number of points
%
% output:
%   matrix: a real matrix, whose values are filled with inter-point
%     distances
%   vector: a real vector, whose values are filled with origin-to-point
%     distances
%

-module(main).
-export([main/0]).

sqr(X) ->
    X * X.

distance({Ax, Ay}, {Bx, By}) ->
    X = Ax - Bx,
    Y = Ay - By,
    math:sqrt((X * X) + (Y * Y)).


calc_row (_Nelts, _RowN, [], _Points, Acc) -> Acc;
calc_row (Nelts, RowN, [Other|Others], Points, {Time, Acc}) ->
    T1 = now(),
    Dists = lists:map(fun (X) -> distance (X, Other) end, Points),
    T2 = now(),
    Max = lists:max(Dists),
    {Pre, [_|Post]} = lists:split(RowN, Dists),
    Final = Pre ++ [Max*Nelts|Post],
    calc_row(Nelts, RowN + 1, Others, Points,
             {Time + timer:now_diff(T2,T1) , [Final | Acc]}).

calc_rows (Parent, Nelts, {RowStart, Rows}, Points) ->
    receive start -> ok end,
    {Time, {InnerTime, Result}} =
       timer:tc(fun() -> calc_row(Nelts, RowStart, Rows, Points, {0, []}) end),
    io:format("row-start: ~p chunk-size: ~p point-size: ~p whole time: ~p inner time: ~p~n", [RowStart, length(Rows), length(Points), Time/1000000, InnerTime/1000000]),
    Parent ! {self(), {Time, Result}}.

chunk(Points, RowStart, S, L, Acc) ->
    if
        L > S ->
            {X, Rest} = lists:split(S, Points),
            chunk (Rest, RowStart + S, S, L - S, [{RowStart, X}|Acc]);
        true -> [{RowStart, Points} | Acc]
    end.

core_chunk(Matrix, Cores) ->
    chunk(Matrix, 0, length(Matrix) div Cores, length(Matrix), []).

outer(Nelts, Points) ->
    Parent = self(),
    Cores = erlang:system_info(schedulers_online),

    io:format("generating chunks~n"),
    PointChunks = core_chunk(Points, Cores),

    io:format("calculating rows~n"),
    Pids = [spawn(fun() -> calc_rows(Parent, Nelts, PointChunk, Points) end)
            || PointChunk <- PointChunks ],

    lists:map(fun(Pid) -> Pid ! start end, Pids),

    {Times, Matrix} = 
        lists:unzip([receive {Pid, Result} -> Result end || Pid <- Pids]),

    TotalTime = lists:sum(Times),

    io:format("calculating vector~n"),
    {Time, Vector} =
        timer:tc(fun() -> [distance ({0,0}, A) || A <- Points] end),
    io:format("vector time ~p~n", [Time/1000000]),
    io:format(standard_error, "~p~n", [(Time + TotalTime/Cores)/1000000]),
    {Matrix, Vector}.

read_vector_of_points(Nelts) -> read_vector_of_points(Nelts, []).

read_vector_of_points(0, Acc) -> Acc;
read_vector_of_points(Nelts, Acc) ->
    read_vector_of_points(Nelts - 1, [{0, 0} | Acc]).

main() ->
    {ok, [[NeltsStr]]} = init:get_argument(nelts),
    Nelts = list_to_integer(NeltsStr),

    Points = read_vector_of_points(Nelts),

%%    percept:start("percept.log"),
    {Time, _Result} = timer:tc(fun () ->  outer(Nelts, Points) end),
%%    percept:stop(),
    io:format(standard_error, "~p~n", [Time/1000000]),
    ok.

