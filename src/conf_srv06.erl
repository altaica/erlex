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
        {joined, Conference} -> Conference
    end.

loop(Conference) ->
    receive
        {join, Caller, From} ->
            From ! {join, Conference},
            announce(Caller, Conference),
            loop([{Caller, From} | Conference]);
        stop ->
            unregister(?MODULE)
    end.

announce(_Caller, []) -> ok;
announce(Caller, [{Id, To} | Conference]) ->
    To ! {joined, Id, Caller},
    announce(Caller, Conference).

