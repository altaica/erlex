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
            From ! {joined, handle_join(Caller, Conference)},
            loop([{Caller, From} | Conference]);
        stop ->
            unregister(?MODULE)
    end.

handle_join(Caller, Conference) ->
    [handle_join(Caller, Id, To) || {Id, To} <- Conference].

handle_join(Caller, Id, To) ->
    To ! {joined, Id, Caller},
    Id.
