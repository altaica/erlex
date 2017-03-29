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
            From ! {joined, callers(Conference)},
            announce(Caller, Conference),
            loop([{Caller, From} | Conference]);
        stop ->
            unregister(?MODULE)
    end.

callers(Conference) ->
    [Caller || {Caller, _From} <- Conference].

announce(Caller, Conference) ->
    [To ! {joined, Id, Caller} || {Id, To} <- Conference].
