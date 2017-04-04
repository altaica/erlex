-module(conf_srv01).
-export([start/0]).

start() ->
    spawn(fun loop/0).

loop() ->
    receive
        {From, join} ->
            From ! {self(), joined}
    end,
    loop().
