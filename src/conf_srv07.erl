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
        {joined, Conference} -> Conference
    end.

send(Message) ->
    ?MODULE ! {send, Message, self()}.

loop(Conference) ->
    receive
        {join, Caller, From} ->
            From ! {join, Conference},
            announce(Caller, Conference),
            loop([{Caller, From} | Conference]);
        {send, Message, From} ->
            Response = {data, caller_id(From, Conference), Message},
            [To ! Response || {_Id, To} <- Conference, To =/= From],
            loop(Conference);
        stop ->
            unregister(?MODULE)
    end.

announce(_Caller, []) -> ok;
announce(Caller, [{Id, To} | Conference]) ->
    To ! {joined, Id, Caller},
    announce(Caller, Conference).

caller_id(From, [{Id, Pid} | _Conference]) when From =:= Pid -> Id;
caller_id(From, [_Participant | Conference]) ->
    caller_id(From, Conference).
