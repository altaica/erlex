-module(conf_srv06).
-export([start/0, stop/0, join/1]).

start() ->
    Pid = spawn(fun() -> loop([]) end),
    register(?MODULE, Pid).

stop() ->
    ?MODULE ! stop.

join(Caller) ->
    ?MODULE ! {self(), {join, Caller}},
    receive
        {?MODULE, {joined, Callers}} -> {ok, Callers}
        after 5000 -> {error, timeout}
    end.

loop(Conference) ->
    receive
        {From, {join, Caller}} ->
            {Pids, Callers} = lists:unzip(Conference),
            lists:foreach(fun(To) -> To ! {?MODULE, {joined, Caller}} end, Pids),
            From ! {?MODULE, {joined, Callers}},
            loop([{From, Caller} | Conference]);
        stop ->
            unregister(?MODULE)
    end.
