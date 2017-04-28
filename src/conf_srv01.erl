%%% @doc Direct implementation of conference server.
%%% @copyright 2017 Phil Dempster

-module(conf_srv01).
-export([start/0, join/0, send/1, stop/0]).

%%% API

%% @doc Start the conference server.
-spec start() -> {ok, pid()}.
start() ->
    Pid = spawn(fun() -> loop([]) end),
    true = register(?MODULE, Pid),
    {ok, Pid}.

%% @doc Join the conference; returns list of existing participants.
-spec join() -> {joined, [pid()]}.
join() ->
    ?MODULE ! {call, self(), join},
    receive
        {joined, _Conference} = Reply -> Reply
    end.

%% @doc Send a broadcast message to all participants.
-spec send(term()) -> ok.
send(Message) ->
    ?MODULE ! {call, self(), {send, Message}},
    receive
        ok -> ok
    end.

%% @doc Stop the conference server.
-spec stop() -> ok.
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
