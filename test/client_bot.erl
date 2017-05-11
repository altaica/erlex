-module(client_bot).
-export([start/1, stop/1]).

start(Server) ->
    Tester = self(),
    Pid = spawn(
        fun() ->
            {joined, Conference} = Server:join(),
            Tester ! Conference,
            loop(Server, Conference)
        end),
    receive Reply -> {Pid, Reply}
    after 10 -> timeout
    end.

stop(Call) ->
    Call ! {hup, self()},
    receive Reply -> Reply
    after 10 -> timeout
    end.

loop(Server, ExpectedGreetings) ->
    receive
        {connected, Pid} ->
            % Send greeting to new caller.
            ok = Server:send({hello, Pid}),
            loop(Server, ExpectedGreetings);
        {diconnected, _Pid} ->
            % Pid should not be in expected list.
            loop(Server, ExpectedGreetings);
        {message, From, {hello, Pid}} when From =/= Pid, Pid =:= self() ->
            % Somebody else has greeted us. Remove from the expected list.
            loop(Server, lists:delete(From, ExpectedGreetings));
        {hup, Pid} when ExpectedGreetings =:= [] ->
            Pid ! ok;
        {hup, Pid} ->
            Pid ! {not_heard, ExpectedGreetings}
    end.
