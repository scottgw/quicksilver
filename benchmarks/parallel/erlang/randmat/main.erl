%
% randmat: random number generation
%
% input:
%   nrows, ncols: the number of rows and columns
%   s: the seed
%
% output:
%   matrix: a nrows x ncols integer matrix
%

-module(main).
-export([main/0]).
-export([row_proc/4]).
-define(INT_MAX,4294967296).
-define(RAND_MAX,100).
-define(LCG_A,1664525).
-define(LCG_C,1013904223).

randvet(Ncols, S) ->
    randvet_acc (Ncols, S, []).

randvet_acc (0, _, Acc) ->
    lists:reverse (Acc);
randvet_acc (Ncols, S, Acc) ->
    NewS = (?LCG_A * S + ?LCG_C) rem ?INT_MAX,
    Val = NewS rem ?RAND_MAX,
    randvet_acc (Ncols - 1, NewS, [Val | Acc]).

row_proc(Gather, Parent, Ncols, S) ->
    V = randvet(Ncols, S),
    case Gather of
        true -> Parent ! V;
        _ -> Parent ! ok
    end.

randmat_acc (_, 0, _, _, Acc) -> lists:reverse (Acc);
randmat_acc (Gather, I, N, S, Acc) ->
    randmat_acc (Gather, I - 1, N, S + 1,
                 [spawn(?MODULE, row_proc, [Gather, self(), N, S]) | Acc]).

join_acc ([], Acc) -> lists:reverse (Acc);
join_acc ([_P|Pids], Acc) -> 
    join_acc (Pids, [receive Result -> Result end | Acc]).

randmat(Gather, N, S) -> join_acc (randmat_acc (Gather, N, N, S, []), []).

main() ->
    init:get_argument(gather),
    Gather = case gather of
                 error -> false;
                 _ -> true
             end,             
    {ok, [[NStr]]} = init:get_argument(nelts),
    {ok, [[SStr]]} = init:get_argument(seed),
    N = list_to_integer(NStr),
    S = list_to_integer(SStr),

    {Time, _Val} = timer:tc(fun() -> randmat(Gather, N, S) end),
    io:format(standard_error, "~p~n", [Time/1000000]).
