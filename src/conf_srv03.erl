-module(conf_srv03).
-export([start/0, stop/0, join/0]).

start() ->
    Pid = spawn(fun loop/0),
    register(?MODULE, Pid).

stop() ->
    ?MODULE ! stop.

join() ->
    ?MODULE ! {join, self()}.

loop() ->
    receive
        {join, From} ->
            From ! {joined, ok},
            loop();
        stop ->
            unregister(?MODULE)
    end.
