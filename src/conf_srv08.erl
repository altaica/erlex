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
            handle_join(Caller, From, Conference),
            loop([{Caller, From} | Conference]);
        {send, Message, From} ->
            handle_send(Message, From, Conference),
            loop(Conference);
        stop ->
            unregister(?MODULE)
    end.

handle_join(Caller, From, Conference) ->
    {Callers, Pids} = lists:unzip(Conference),
    From ! {joined, Callers},
    lists:foreach(fun(To) -> To ! {joined, ?MODULE, Caller} end, Pids).

handle_send(Message, From, Conference) ->
    {Caller, From} = lists:keyfind(From, 2, Conference),
    [To ! {data, ?MODULE, Caller, Message} || {Id, To} <- Conference, Id =/= Caller].
