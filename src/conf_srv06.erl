-module(conf_srv06).
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
            {Callers, Pids} = lists:unzip(Conference),
            lists:foreach(fun(To) -> To ! {joined, Caller} end, Pids),
            From ! {joined, Callers},
            loop([{Caller, From} | Conference]);
        stop ->
            unregister(?MODULE)
    end.
