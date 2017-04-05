-module(conf_srv08).
-export([start/0, stop/0, join/1, send/1]).

start() ->
    Pid = spawn(fun() -> loop([]) end),
    register(?MODULE, Pid).

stop() ->
    ?MODULE ! stop.

join(Caller) ->
    ?MODULE ! {self(), {join, Caller}},
    receive
        {?MODULE, {joined, Callers}} -> {ok, Callers}
        after 5000 -> {error, timeout}
    end.

send(Message) ->
    ?MODULE ! {self(), {send, Message}}.

loop(Conference) ->
    receive
        {From, {join, Caller}} ->
            handle_join(From, Caller, Conference),
            loop([{From, Caller} | Conference]);
        {From, {send, Message}} ->
            handle_send(From, Message, Conference),
            loop(Conference);
        stop ->
            ok
    end.

handle_join(From, Caller, Conference) ->
    {Pids, Callers} = lists:unzip(Conference),
    From ! {?MODULE, {joined, Callers}},
    lists:foreach(fun(To) -> To ! {?MODULE, {joined, Caller}} end, Pids).

handle_send(From, Message, Conference) ->
    {From, Caller} = lists:keyfind(From, 2, Conference),
    [To ! {?MODULE, {data, Caller, Message}} || {To, Id} <- Conference,
                                                     Id =/= Caller].
