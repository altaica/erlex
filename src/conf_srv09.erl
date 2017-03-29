-module(conf_srv09).
-export([start/0, stop/0, join/1, send/1]).

start() ->
    Pid = spawn(fun() -> loop([]) end),
    register(?MODULE, Pid).

stop() ->
    ?MODULE ! stop.

join(Caller) ->
    %TODO link...
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
            Response = {data, caller_id(From, Conference), Message},
            [To ! Response || {_Id, To} <- Conference, To =/= From],
            loop(Conference);
        %TODO handle link drop
        stop ->
            unregister(?MODULE)
    end.

handle_join(Caller, Conference) ->
    [handle_join(Caller, Id, To) || {Id, To} <- Conference].

handle_join(Caller, Id, To) ->
    To ! {joined, Id, Caller},
    Id.

caller_id(From, [{Id, Pid} | _Conference]) when From =:= Pid -> Id;
caller_id(From, [_Participant | Conference]) ->
    caller_id(From, Conference).
