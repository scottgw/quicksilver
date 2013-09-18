%
% product: a matrix-vector product
%
% input:
%   nelts: the number of elements
%   matrix: a real matrix
%   vector: a real vector
%
% output:
%   result: a real vector, whose values are the result of the product
%

-module(main).
-export([main/0]).
-export([product/2]).

join(Pids) ->
    Parts = [receive {Pid, Result, Time} -> {Result, Time} end || Pid <- Pids],
    {Results, Times} = lists:unzip(Parts),
    TotalTime = lists:sum(Times),
    Result = lists:reverse(lists:append(Results)),
    {TotalTime, Result}.

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
    {Result, timer:now_diff(T2, T1)}.

chunk(Matrix, S, L, Acc) ->
    if
        L > S ->
            {X, Rest} = lists:split(S, Matrix),
            chunk (Rest, S, L - S, [X | Acc]);
        true -> [Matrix | Acc]
    end.

core_chunk(Matrix, Cores) ->
    chunk(Matrix, length(Matrix) div Cores, length(Matrix), []).

product(Matrix, Vector) ->
    Parent = self(),
    Cores = erlang:system_info(schedulers_online),
    Chunked = core_chunk(Matrix, Cores),
    
    {Time, Answer} = 
        join([spawn(
                fun() -> 
                        {X, Time} = prod_chunk (C, Vector),
                        Parent ! {self(), X, Time}
                end)
              || C <- Chunked]),
    io:format(standard_error, "~p~n", [Time/(Cores*1000000)]),
    Answer.

read_vector(Nelts) ->
    lists:duplicate(Nelts, 0).

read_matrix(Nelts) ->
    lists:duplicate(Nelts, lists:duplicate(Nelts, 0)).

run(Nelts) ->
    Matrix = read_matrix(Nelts),
    Vector = read_vector(Nelts),
    %% fprof:trace(start),
    {Time, _Answer} =
        timer:tc(fun() -> product(Matrix, Vector) end),
    io:format(standard_error, "~p~n", [Time/1000000]).
    %% fprof:trace(stop),
    %% fprof:profile(),
    %% fprof:analyse(),

main() ->
    {ok, [[NeltsStr]]} = init:get_argument(nelts),
    Nelts = list_to_integer(NeltsStr),
    run(Nelts).

