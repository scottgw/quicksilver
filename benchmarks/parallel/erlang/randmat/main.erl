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

rand_chunk_acc(N, StartSeed, 0, Acc) ->
    Acc;
rand_chunk_acc(N, StartSeed, Height, Acc) ->
    %% io:format("~p~n", [{N, StartSeed, Height}]),
    V = randvec(N, StartSeed + Height - 1),
    rand_chunk_acc(N, StartSeed, Height - 1, [V|Acc]).

rand_chunk(Parent, N, StartSeed, Height) ->
    T1 = now(),
    C = rand_chunk_acc(N, StartSeed, Height, []),
    T2 = now(),

    Parent ! {self(), {C, timer:now_diff(T2, T1)/1000000}}.

randmat_acc (0, _, _, _, _, Acc) -> Acc;
randmat_acc (Workers, Remaining, Start, N, S, Acc) ->
    Parent = self(),
    Height = Remaining div Workers,

    Pid = spawn(fun() -> rand_chunk(Parent, N, S + Start, Height) end),

    NewStart = Start + Height,
    NewRemaining = Remaining - Height,
    randmat_acc (Workers - 1, NewRemaining, NewStart, N, S, [Pid | Acc]).


randmat(W, N, S) -> randmat_acc (W, N, 0, N, S, []).

join_acc ([], Acc) -> Acc;
join_acc ([P|Pids], Acc) ->
    Result = receive
                 {P, X} -> X
             end,
    join_acc (Pids, [Result | Acc]).


join (Pids) ->
    {Results, Times} = lists:unzip(join_acc(Pids, [])),
    TotalTime = lists:sum(Times),
    io:format(standard_error, "~p~n", [TotalTime/4]).

randmat(N, S) -> join (randmat (4, N, S)).

main() ->
    init:get_argument(gather),

    {ok, [[NStr]]} = init:get_argument(nelts),
    {ok, [[SStr]]} = init:get_argument(seed),


    Cores = erlang:system_info(schedulers_online),
    N = list_to_integer(NStr),
    S = list_to_integer(SStr),

    {Time, _Val} = timer:tc(fun() -> randmat(N, S) end),
    io:format(standard_error, "~p~n", [Time/1000000]).
