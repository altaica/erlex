-module(conf_srv04).
-export([start/0, stop/0, join/1]).

start() ->
    Pid = spawn(fun() -> loop([]) end),
    register(?MODULE, Pid).

stop() ->
    ?MODULE ! stop.

join(Caller) ->
    ?MODULE ! {join, Caller, self()}.

loop(Conference) ->
    receive
        {join, Caller, From} ->
            From ! {joined, Conference},
            loop([Caller | Conference]);
        stop ->
            unregister(?MODULE)
    end.

