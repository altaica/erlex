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
            From ! {joined, callers(Conference)},
            announce(Caller, Conference),
            loop([{Caller, From} | Conference]);
        stop ->
            unregister(?MODULE)
    end.

callers(Conference) -> callers(Conference, []).
callers([{Caller, _From} | Conference], Callers) ->
    callers(Conference, [Caller | Callers]);
callers([], Callers) -> Callers.

announce(Caller, [{Id, To} | Conference]) ->
    To ! {joined, Id, Caller},
    announce(Caller, Conference);
announce(_Caller, []) -> ok.

