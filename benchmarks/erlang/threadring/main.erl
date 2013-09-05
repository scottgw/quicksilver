% The Computer Language Benchmarks Game
% http://benchmarksgame.alioth.debian.org/
%%% Contributed by Jiri Isa
%%% optimized run time options by shun shino

-module(main).
-export([main/1, roundtrip/3]).

-define(RING, 503).

start(Parent, Token) ->
    H = lists:foldl(
          fun(Id, Pid) -> spawn(main, roundtrip, [Parent, Id, Pid]) end, 
          self(), 
          lists:seq(?RING, 2, -1)),
    H ! Token,
    roundtrip(Parent, 1, H).

roundtrip(Parent, Id, Pid) ->
   receive
      1 ->
         io:fwrite("~b~n", [Id]),
         Parent ! done;
      Token ->
           Pid ! Token - 1,
           roundtrip(Parent, Id, Pid)
   end.

run(Token) ->
    Parent = self(),
    spawn(fun() -> start(Parent, Token) end),
    receive
        done -> ok
    end. 

main([Arg]) ->
    {Token, _} = string:to_integer(atom_to_list(Arg)),
    {Time, _} = timer:tc(fun() -> run(Token) end),
    io:format(standard_error, "~p~n", [Time/1000000]).
