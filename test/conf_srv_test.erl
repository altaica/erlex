-module(conf_srv_test).
-include_lib("eunit/include/eunit.hrl").

conf_srv01_test() ->
    Pid = conf_srv01:start(),
    Pid ! {join, self()},
    receive
        Response -> ?assertMatch({joined, ok}, Response)
    end.

conf_srv02_test() ->
    ?assertMatch(true, conf_srv02:start()),
    conf_srv02 ! {join, self()},
    ?assertMatch({joined, ok}, wait_response()),
    ?assertMatch(stop, conf_srv02:stop()).

conf_srv03_test() ->
    ?assertMatch(true, conf_srv03:start()),
    ?assertMatch(ok,   conf_srv03:join()),
    ?assertMatch(stop, conf_srv03:stop()).

conf_srv05_test() ->
    ?assertMatch(true, conf_srv05:start()),
    lists:foldl(fun sync_join/2, [], [joe, mike, robert, elephant]),
    ?assertMatch(stop, conf_srv05:stop()).

sync_join(Caller, State) ->
    ?assertMatch(State, conf_srv05:join(Caller)),
    [Caller | State].

conf_srv06_test() ->
    ?assertMatch(true, conf_srv06:start()),
    Callers = [joe, mike, robert, elephant],
    lists:foldl(fun conf_srv06_client/2, {[], Callers}, Callers),
    % TODO this would be better as a common test, with 1 test per caller
    % and the above in the suite init
    [?assertMatch({ok, P}, wait_response()) || P <- Callers],
    ?assertMatch(stop, conf_srv06:stop()).

conf_srv06_client(Caller, {Participants, [Caller | Callers]}) ->
    Tester = self(),
    spawn(fun() ->
            conf_srv06:join(Caller),
            [{joined, P} = wait_response() || P <-Callers],
            Tester ! {ok, Caller}
        end),
    {[Caller | Participants], Callers}.

wait_response() ->
    receive
        Response -> Response
        after 1000 -> timeout
    end.
