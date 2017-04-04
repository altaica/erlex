-module(conf_srv07).
-export([start/0, stop/0, join/1, send/1]).

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

send(Message) ->
    ?MODULE ! {send, Message, self()}.

loop(Conference) ->
    receive
        {join, Caller, From} ->
            From ! {joined, callers(Conference)},
            announce(Caller, Conference),
            loop([{Caller, From} | Conference]);
        {send, Message, _From} ->
            broadcast(Message, Conference),
            loop(Conference);
        stop ->
            unregister(?MODULE)
    end.

callers(Conference) ->
    [Caller || {Caller, _From} <- Conference].

announce(Caller, Conference) ->
    [To ! {joined, Id, Caller} || {Id, To} <- Conference].

broadcast(Message, Conference) ->
    [To ! {data, Message} || {_Id, To} <- Conference].
