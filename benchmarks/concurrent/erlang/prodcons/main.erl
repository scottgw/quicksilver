-module(main).
-compile(export_all).
-export([main/1, main/2]).

shared(Parent, _Consumers, _Vals, 0) ->
    Parent ! {done, 0};
shared(Parent, Consumers, Vals, I) ->
    {Consumers2, Vals2} = 
        receive
            {fetch, P} -> {[P|Consumers], Vals};
            {put, Val} -> {Consumers, [Val|Vals]}
        end,

    if
        I > 0 ->
            case {Consumers2, Vals2} of
                {[C|Cs], [V|Vs]} ->
                    C ! V,
                    shared(Parent, Cs, Vs, I-1);
                _ ->
                    shared(Parent, Consumers2, Vals2, I-1)
            end;
        true -> shared(Parent, Consumers, Vals, I)
    end.

consumer(_Shared, 0) ->
    ok;
consumer(Shared, I) ->
    Shared ! {fetch, self()},
    receive
        _V -> consumer(Shared, I-1)
    end.

producer(_Shared, 0) ->
    ok;
producer(Shared, I) ->
    Shared ! {put, I},
    producer(Shared, I-1).

main(Iters, Workers) ->
    Parent = self(),
    Shared = 
        spawn(fun() -> shared(Parent, [], [], 2*Iters * Workers) end),
    lists:foreach (fun(_X) -> 
                           spawn(fun() -> 
                                         producer(Shared, Iters)
                                 end)
                   end,
                   lists:seq(1, Workers)),
    lists:foreach (fun(_X) -> 
                           spawn(fun() -> 
                                         consumer(Shared, Iters)
                                 end)
                   end,
                   lists:seq(1, Workers)),

    receive
        {done, I} -> io:format("~p~n", [2*Workers*Iters - I])
    end.

main([ItersStr, WorkersStr, _Rest]) ->
    {Iters, _} = string:to_integer(atom_to_list(ItersStr)),
    {Workers, _} = string:to_integer(atom_to_list(WorkersStr)),

    {Time, _Val} = timer:tc( fun() -> main(Iters, Workers) end),

    io:format(standard_error, "~p~n",[Time / 1000000]).
