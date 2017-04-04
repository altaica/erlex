-module(conf_srv03).
-export([start/0, stop/0, join/0]).

start() ->
    Pid = spawn(fun loop/0),
    register(?MODULE, Pid).

stop() ->
    ?MODULE ! stop.

join() ->
    ?MODULE ! {join, self()},
    receive
        {joined, ?MODULE, ok} -> ok
        after 5000 -> {error, timeout}
    end.

loop() ->
    receive
        {join, From} ->
            From ! {joined, ?MODULE, ok},
            loop();
        stop ->
            unregister(?MODULE)
    end.
