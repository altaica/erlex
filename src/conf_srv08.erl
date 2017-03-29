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
            From ! {joined, handle_join(Caller, Conference)},
            loop([{Caller, From} | Conference]);
        {send, Message, From} ->
            handle_send(Message, From, Conference),
            loop(Conference);
        stop ->
            unregister(?MODULE)
    end.

handle_join(Caller, Conference) ->
    [announce(Caller, Id, To) || {Id, To} <- Conference].

announce(Caller, Id, To) ->
    To ! {joined, Id, Caller},
    Id.

handle_send(Message, From, Conference) ->
    Caller = caller_id(From, Conference),
    [To ! {data, Caller, Message} || {_Id, To} <- Conference, To =/= From].

caller_id(From, [{Caller, From} | _Conference]) -> Caller;
caller_id(From, [_Participant | Conference]) ->
    caller_id(From, Conference).
