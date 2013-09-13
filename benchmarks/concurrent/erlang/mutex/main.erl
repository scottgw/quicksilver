-module(main).
-compile(export_all).
-export([main/1, main/2]).

shared(Parent, 0) ->
    Parent ! {done, 0};
shared(Parent, I) ->
    receive
        ping -> shared (Parent, I-1)
    end.

worker(_Shared, 0) -> ok; 
worker(Shared, I) ->
    Shared ! ping,
    worker(Shared, I-1).

main(Iters, Workers) ->
    Parent = self(),
    Shared = spawn(fun() -> shared(Parent, Iters * Workers) end),
    lists:foreach (fun(_X) -> spawn(fun() -> worker(Shared, Iters) end) end,
                   lists:seq(1, Workers)),

    receive
        {done, I} -> io:format("~p~n", [Workers*Iters - I])
    end.

main([ItersStr, WorkersStr, _Rest]) ->
    {Iters, _} = string:to_integer(atom_to_list(ItersStr)),
    {Workers, _} = string:to_integer(atom_to_list(WorkersStr)),

    {Time, _Val} = timer:tc( fun() -> main(Iters, Workers) end),

    io:format(standard_error, "~p~n",[Time / 1000000]).
