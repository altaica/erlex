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
        {joined, ?MODULE, Callers} -> {ok, Callers}
        after 5000 -> {error, timeout}
    end.

loop(Conference) ->
    receive
        {join, Caller, From} ->
            From ! {joined, ?MODULE, Conference},
            loop([Caller | Conference]);
        stop ->
            unregister(?MODULE)
    end.

