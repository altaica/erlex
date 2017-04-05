-module(conf_srv05).
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
            From ! {?MODULE, {joined, Conference}},
            loop([Caller | Conference]);
        stop ->
            ok
    end.

