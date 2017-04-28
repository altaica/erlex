-module(conf_srv01).
-export([start/0, join/0, send/1, stop/0]).
-include("conf_srv.hrl").

%%% API

start() ->
    Pid = spawn(fun() -> loop([]) end),
    true = register(?MODULE, Pid),
    {ok, Pid}.

join() ->
    ?MODULE ! {call, self(), join},
    receive
        {joined, _Conference} = Reply -> Reply
    end.

send(Message) ->
    ?MODULE ! {call, self(), {send, Message}},
    receive
        ok -> ok
    end.

stop() ->
    ?MODULE ! stop,
    ok.

%%% Implementation

loop(Conference) ->
    receive
        {call, Pid, join} ->
            monitor(process, Pid),
            broadcast({connected, Pid}, Conference),
            Pid ! {joined, Conference},
            loop([Pid | Conference]);
        {call, Pid, {send, Message}} ->
            broadcast({message, Pid, Message}, Conference),
            Pid ! ok,
            loop([Pid | Conference]);
        {'DOWN', _Ref, process, Pid, _Reason} ->
            Remaining = lists:delete(Pid, Conference),
            broadcast({connected, Pid}, Remaining),
            loop(Remaining);
        stop ->
            ok
    end.

broadcast(Event, Conference) ->
    lists:foreach(fun(P) -> P ! Event end, Conference).
