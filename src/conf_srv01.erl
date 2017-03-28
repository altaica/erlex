-module(conf_srv01).
-export([start/0]).

start() ->
    spawn(fun loop/0).

loop() ->
    receive
        {join, From} ->
            From ! {joined, ok}
    end,
    loop().
