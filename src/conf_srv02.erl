-module(conf_srv02).
-export([start/0, stop/0]).

start() ->
    Pid = spawn(fun loop/0),
    register(?MODULE, Pid).

stop() ->
    ?MODULE ! stop.

loop() ->
    receive
        {From, join} ->
            From ! {?MODULE, joined},
            loop();
        stop ->
            unregister(?MODULE)
    end.
