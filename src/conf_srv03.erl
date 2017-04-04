-module(conf_srv03).
-export([start/0, stop/0, join/0]).

start() ->
    Pid = spawn(fun loop/0),
    register(?MODULE, Pid).

stop() ->
    ?MODULE ! stop.

join() ->
    ?MODULE ! {self(), join},
    receive
        {?MODULE, joined} -> ok
        after 5000 -> {error, timeout}
    end.

loop() ->
    receive
        {From, join} ->
            From ! {?MODULE, joined},
            loop();
        stop ->
            unregister(?MODULE)
    end.
