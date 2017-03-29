-module(conf_srv08).
-export([start/0, stop/0, join/1, send/1]).

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

send(Message) ->
    ?MODULE ! {send, Message, self()}.

loop(Conference) ->
    receive
        {join, Caller, From} ->
            From ! {joined, callers(Conference)},
            announce(Caller, Conference),
            loop([{Caller, From} | Conference]);
        {send, Message, From} ->
            broadcast(Message, caller_id(From, Conference), Conference),
            loop(Conference);
        stop ->
            unregister(?MODULE)
    end.

callers(Conference) ->
    [Caller || {Caller, _From} <- Conference].

announce(Caller, Conference) ->
    [To ! {joined, Id, Caller} || {Id, To} <- Conference].

broadcast(Message, Caller, Conference) ->
    [To ! {data, Id, Caller, Message} || {Id, To} <- Conference, Id =/= Caller].

caller_id(From, [{Caller, From} | _Conference]) -> Caller;
caller_id(From, [_Participant | Conference]) ->
    caller_id(From, Conference).
