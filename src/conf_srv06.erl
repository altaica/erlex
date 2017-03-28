-module(conf_srv06).
-export([start/0, stop/0, join/1, send/1]).

start() ->
    Pid = spawn(fun() -> loop([]) end),
    register(?MODULE, Pid).

stop() ->
    ?MODULE ! stop.

join(Caller) ->
    ?MODULE ! {join, Caller, self()}.

send(Message) ->
    ?MODULE ! {send, Message, self()}.

loop(Conference) ->
    receive
        {join, Caller, From} ->
            From ! {join, Conference},
            announce(Caller, Conference),
            loop([{Caller, From} | Conference]);
        {send, Message, From} ->
            [To ! {data, Id, Message} || {Id, To} <- Conference, To =/= From],
            loop(Conference);
        stop ->
            unregister(?MODULE)
    end.

announce(_Caller, []) -> ok;
announce(Caller, [{Id, To} | Conference]) ->
    To ! {joined, Id, Caller},
    announce(Caller, Conference).

