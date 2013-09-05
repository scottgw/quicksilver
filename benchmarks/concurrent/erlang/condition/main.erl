-module(main).
-compile(export_all).
-export([main/1, main/2]).

shared(Parent, _Odds, _Evens, _Status, 0) ->
    Parent ! {done, 0};
shared(Parent, Odds, Evens, Status, I) ->
    {Odds2, Evens2, Status2, I2} = 
        receive
            {odd, Requester} -> {[Requester|Odds], Evens, Status, I};
            {even, Requester} -> {Odds, [Requester|Evens], Status, I};
            finished -> {Odds, Evens, free, I-1}
        end,

    if
        I2 > 0 ->
            {Odds4, Evens4, Status4} =
                case Status2 of
                    free ->
                        if
                            I2 rem 2 =:= 0 -> 
                                case Evens2 of
                            [P|Evens3] -> P ! {resume, I2},
                                          {Odds2, Evens3, busy}
                                end;
                            true          -> 
                                case Odds2 of
                                    [P|Odds3] -> P ! {resume, I2},
                                                 {Odds3, Evens2, busy}
                                end
                        end;
                    busy -> {Odds2, Evens2, busy}
                end,
            shared (Parent, Odds4, Evens4, Status4, I2);
        true -> shared (Parent, Odds, Evens, Status, I2)
    end. 
worker(_Shared, _Flag, 0) -> ok; 
worker(Shared, Flag, I) ->
    Shared ! {Flag, self()},
    receive
        {resume, _Val} -> Shared ! finished
    end,
    worker(Shared, Flag, I-1).

main(Iters, Workers) ->
    Parent = self(),
    Shared = 
        spawn(fun() -> shared(Parent, [], [], free, 2*Iters * Workers) end),
    lists:foreach (fun(_X) -> 
                           spawn(fun() -> 
                                         worker(Shared, even, Iters)
                                 end)
                   end,
                   lists:seq(1, Workers)),
    lists:foreach (fun(_X) -> 
                           spawn(fun() -> 
                                         worker(Shared, odd, Iters)
                                 end)
                   end,
                   lists:seq(1, Workers)),

    receive
        {done, I} -> io:format("~p~n", [2*Workers*Iters - I])
    end.

main([ItersStr, WorkersStr]) ->
    {Iters, _} = string:to_integer(atom_to_list(ItersStr)),
    {Workers, _} = string:to_integer(atom_to_list(WorkersStr)),

    {Time, _Val} = timer:tc( fun() -> main(Iters, Workers) end),

    io:format(standard_error, "~p~n",[Time / 1000000]).
