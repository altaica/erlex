-module(gen_conf_srv).
-behaviour(gen_server).

-export([start/0, stop/0, join/1, send/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

join(Caller) ->
    gen_server:call(?MODULE, {join, Caller}).

send(Message) ->
    gen_server:call(?MODULE, {send, Message}).


init([]) ->
    process_flag(trap_exit, true),
    {ok, #{}}.

handle_call({join, Caller}, {From, _Tag}, State) ->
    [To ! {joined, Caller} || To <- maps:keys(State)],
    {reply, {joined, maps:values(State)}, State#{From => Caller}};
handle_call({send, Message}, {From, _Tag}, State) ->
    [To ! {data, maps:get(From, State), Message} || To <- maps:keys(State)],
    {reply, ok, State};
handle_call(stop, _From, State) ->
    {stop, normal, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', From, _Reason}, State) ->
    {{Caller, From}, Updated} = maps:take(From, State),
    [To ! {left, Caller} || To <- maps:keys(Updated)],
    {noreply, Updated}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

