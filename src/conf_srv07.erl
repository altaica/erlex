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
            [To ! {data, Id, Message} || {Id, To} <- Conference, To =/= From],
            loop(Conference);
        stop ->
            unregister(?MODULE)
    end.

handle_join(Caller, Conference) ->
    [handle_join(Caller, Id, To) || {Id, To} <- Conference].

handle_join(Caller, Id, To) ->
    To ! {joined, Id, Caller},
    Id.
