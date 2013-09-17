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

join(Pids) ->
    Results = [receive 
                   {Pid, Result} -> 
                       Result 
               end 
               || Pid <- Pids],
    Results.
    %% lists:concat(Results).

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

calc_rows (Gather, Parent, Nelts, {RowStart, Rows}, Points) ->
    X = calc_row(Nelts, RowStart, Rows, Points, []),
    case Gather of
        true -> Parent ! {self(), X};
        _ -> Parent ! {self(), ok}
    end.


chunk(Points, RowStart, L, Acc) ->
    S = 1,
    if
        L >= S ->
            {X, Rest} = lists:split(S, Points),
            chunk (Rest, RowStart + S, L - S, [{RowStart, X}|Acc]);
        true -> [{RowStart, Points} | Acc]
    end.

chunk(Points) ->
    chunk(Points, 0, length(Points), []).

outer(Gather, Nelts, Points) ->
    Parent = self(),
    PointChunks = chunk(Points),
    io:format("generating indices~n"),
    io:format("calculating rows~n"),
    Pids = [spawn(fun() -> calc_rows(Gather, Parent, Nelts, PointChunk, Points) end)
            || PointChunk <- PointChunks ],
    Matrix = join (Pids),
    io:format("calculating vector~n"),
    Vector = [distance ({0,0}, A) || A <- Points],
    {Matrix, Vector}.

read_vector_of_points(Nelts) -> lists:duplicate(Nelts, {0, 0}).

main() ->
    Gather = case gather of
                 error -> false;
                 _ -> true
             end,
    {ok, [[NeltsStr]]} = init:get_argument(nelts),
    Nelts = list_to_integer(NeltsStr),

    Points = read_vector_of_points(Nelts),
    {Time, _Result} = timer:tc(fun () ->  outer(Gather, Nelts, Points) end),
    io:format(standard_error, "~p~n", [Time/1000000]),
    ok.

