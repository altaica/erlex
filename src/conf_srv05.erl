-module(conf_srv05).
-export([start/0, stop/0, join/1]).

start() ->
    Pid = spawn(fun() -> loop([]) end),
    register(?MODULE, Pid).

stop() ->
    ?MODULE ! stop.

join(Caller) ->
    ?MODULE ! {join, Caller, self()},
    receive
        {joined, Callers} -> Callers
    end.

loop(Conference) ->
    receive
        {join, Caller, From} ->
            From ! {joined, Conference},
            loop([Caller | Conference]);
        stop ->
            unregister(?MODULE)
    end.

