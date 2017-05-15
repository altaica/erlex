%%% @doc Implementation of conference server with generic code separated.
%%% @copyright 2017 Phil Dempster

-module(conf_srv02).
-export([start/0, join/0, send/1, stop/0]).

%%% API

%% @doc Start the conference server.
-spec start() -> {ok, pid()}.
start() ->
    gen_start(?MODULE, []).

%% @doc Join the conference.
%% @returns List of existing participants.
-spec join() -> {joined, [pid()]}.
join() ->
    gen_call(?MODULE, join).

%% @doc Send a broadcast message to all participants.
-spec send(term()) -> ok.
send(Message) ->
    gen_call(?MODULE, {send, Message}).

%% @doc Stop the conference server.
-spec stop() -> ok.
stop() ->
    gen_stop(?MODULE).


%%% Generic

gen_start(Server, InitState) ->
    Pid = spawn(fun() -> gen_loop(InitState) end),
    true = register(Server, Pid),
    {ok, Pid}.

gen_call(Server, Request) -> gen_call(Server, Request, 5000).
gen_call(Server, Request, Timeout) ->
    Tag = make_ref(),
    Server ! {call, {self(), Tag}, Request},
    receive
        {Tag, Reply} ->
            Reply
    after Timeout ->
        error(timeout)
    end.

gen_stop(Server) ->
    Mref = erlang:monitor(process, Server),
    true = exit(whereis(Server), stop),
    receive
        {'DOWN', Mref, process, _Pid, stop} ->
            ok
    end.

gen_loop(State) ->
    receive
        {call, From, Request} ->
            {reply, Reply, NewState} = handle_call(Request, From, State),
            gen_reply(From, Reply),
            gen_loop(NewState);
        InfoMsg ->
            {noreply, NewState} = handle_info(InfoMsg, State),
            gen_loop(NewState)
    end.

gen_reply({Pid, Tag}, Reply) ->
    Pid ! {Tag, Reply}.


%%% Implementation

handle_call(join, {Pid, _Tag}, Conference) ->
    monitor(process, Pid),
    broadcast({connected, Pid}, Conference),
    {reply, {joined, Conference}, [Pid | Conference]};
handle_call({send, Message}, {Pid, _Tag}, Conference) ->
    broadcast({message, Pid, Message}, Conference),
    {reply, ok, Conference}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, Conference) ->
    Remaining = lists:delete(Pid, Conference),
    broadcast({disconnected, Pid}, Remaining),
    {noreply, Remaining}.

broadcast(Event, Conference) ->
    lists:foreach(fun(P) -> P ! Event end, Conference).
