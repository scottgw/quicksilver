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
            M == 1 -> [{X, RowNo, ColNo} | Acc];
            true -> Acc
        end,
    row_process(RowNo, ColNo + 1, Xs, Ms, Y).

row_process(Parent, {MatrixRow, MaskRow, RowNo}) ->
    Parent ! row_process(RowNo, 0, MatrixRow, MaskRow, []).

% bounded sort from: http://jinnipark.tumblr.com/post/156214523/erlang-parallel
p_qsort(L) ->
    Self = self(),
    Pid = spawn(fun() ->
                        split(L, 1000, Self)
                end),
    merge(Pid).

split([], _Limit, Parent) ->
    Parent ! {self(), []};
split([Pivot | T], Limit, Parent) when Limit > 2 ->
    Self = self(),
    Limit2 = Limit div 2,
    Pid1 = spawn(fun() ->
                         split([X || X <- T, X < Pivot], Limit2, Self)
                 end),
    Pid2 = spawn(fun() ->
                         split([X || X <- T, not (X < Pivot)], Limit2, Self)
                 end),
    Parent ! {Self, merge(Pid1) ++ [Pivot] ++ merge(Pid2)};
split(L, _Limit, Parent) ->
    Parent ! {self(), lists:sort(L)}.

merge(Ref) ->
    receive
        {Ref, Value} ->
      Value
    end.

winnow(Gather, N, Matrix, Mask, Nelts) ->
    RowZips = lists:zip3(Matrix, Mask, lists:seq(0, N - 1)),
    Parent = self(),
    io:format("row process~n"),
    [ spawn(fun () ->
                    row_process (Parent, RowZip)
            end)
      || RowZip <- RowZips],
    Results = [ receive Res -> Res end || _ <- RowZips],
    io:format("sorting~n"),
    Sorted = p_qsort(lists:flatten(Results)),
    Masked = lists:sum( lists:flatten(Mask) ),
    io:format("pruning~n"),
    ToDrop = max(0, Masked - Nelts),
    {_, FinalN} = lists:split (ToDrop, Sorted),
    [[ Coord || {_, Coord} <- FinalN ]].

read_vector(_, 0) -> [];
read_vector(Nrows, Ncols) -> 
    Val = if 
              (Nrows * Ncols) rem (Ncols + 1) == 1 -> 1;
              true -> 0
          end,
    [ Val | read_vector(Nrows, Ncols - 1)].

read_matrix(0, _) -> [];
read_matrix(Nrows, Ncols) -> 
    [read_vector(Nrows, Ncols) | read_matrix(Nrows - 1, Ncols)].

main() ->
    Gather = case gather of
                 error -> false;
                 _ -> true
             end,             
    {ok, [[OrigNeltsStr]]} = init:get_argument(orignelts),
    {ok, [[NeltsStr]]} = init:get_argument(nelts),
    N = list_to_integer(OrigNeltsStr),
    Nelts = list_to_integer(NeltsStr),

    Matrix = read_matrix(N, N),
    Mask = read_matrix(N, N),
    {Time, _Val} = timer:tc(fun() -> winnow(Gather, N, Matrix, Mask, Nelts) end),
    io:format(standard_error, "~p~n", [Time/1000000]),
    ok.
