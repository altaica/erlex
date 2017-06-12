%%% @doc OTP gen_server implementation of conference server.
%%% @copyright 2017 Phil Dempster

-module(conf_srv03).
-behaviour(gen_server).
-export([start/0, stop/0, join/0, send/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

%%% API

%% @doc Start the conference server.
-spec start() -> {ok, pid()}.
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% @doc Join the conference.
%% @returns List of existing participants.
-spec join() -> {joined, [pid()]}.
join() ->
    gen_server:call(?MODULE, join).

%% @doc Send a broadcast message to all participants.
-spec send(term()) -> ok.
send(Message) ->
    gen_server:call(?MODULE, {send, Message}).

%% @doc Stop the conference server.
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).


%%% Implementation

init([]) ->
    {ok, []}.

handle_call(join, {Pid, _Tag}, Conference) ->
    monitor(process, Pid),
    broadcast({connected, Pid}, Conference),
    {reply, {joined, Conference}, [Pid | Conference]};
handle_call({send, Message}, {Pid, _Tag}, Conference) ->
    broadcast({message, Pid, Message}, Conference),
    {reply, ok, Conference}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, Conference) ->
    Remaining = lists:delete(Pid, Conference),
    broadcast({disconnected, Pid}, Remaining),
    {noreply, Remaining}.

code_change(_OldVsn, Conference, _Extra) ->
    {ok, Conference}.

terminate(_Reason, _Conference) ->
    ok.

broadcast(Event, Conference) ->
    lists:foreach(fun(P) -> P ! Event end, Conference).
