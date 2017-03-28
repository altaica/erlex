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
    receive
        Response -> ?assertMatch({joined, ok}, Response)
    end,
    ?assertMatch(stop, conf_srv02:stop()).

conf_srv03_test() ->
    ?assertMatch(true, conf_srv03:start()),
    conf_srv03:join(),
    receive
        Response -> ?assertMatch({joined, ok}, Response)
    end,
    ?assertMatch(stop, conf_srv03:stop()).

conf_srv04_test() ->
    ?assertMatch(true, conf_srv04:start()),
    lists:foldl(fun async_join/2, [], [joe, mike, robert, elephant]),
    ?assertMatch(stop, conf_srv04:stop()).

async_join(Caller, State) ->
    conf_srv04:join(Caller),
    receive
        Response ->
            ?assertMatch({joined, State}, Response),
            [Caller | State]
    end.

conf_srv04a_test() ->
    ?assertMatch(true, conf_srv04a:start()),
    lists:foldl(fun sync_join/2, [], [joe, mike, robert, elephant]),
    ?assertMatch(stop, conf_srv04a:stop()).

sync_join(Caller, State) ->
    ?assertMatch(State, conf_srv04a:join(Caller)),
    [Caller | State].

%conf_srv_test05+ => spawn receiver processes for each caller.
%May want to do this under CT.
