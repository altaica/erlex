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
        {joined, ?MODULE, Callers} -> {ok, Callers}
        after 5000 -> {error, timeout}
    end.

loop(Conference) ->
    receive
        {join, Caller, From} ->
            {Callers, Pids} = lists:unzip(Conference),
            lists:foreach(fun(To) -> To ! {joined, ?MODULE, Caller} end, Pids),
            From ! {joined, ?MODULE, Callers},
            loop([{Caller, From} | Conference]);
        stop ->
            unregister(?MODULE)
    end.
