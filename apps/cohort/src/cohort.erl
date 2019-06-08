-module(cohort).
-behaviour(application).

-export([start/2,
         stop/1]).

start(StartType, StartArgs) ->
    logger:info("~w start(~w, ~w)~n", [node(), StartType, StartArgs]),
    {ok, spawn(fun loop/0)}.

stop(_State) ->
    ok.

loop() ->
    logger:debug("~w in loop~n", [node()]),
    receive _ -> ok end,
    loop().
