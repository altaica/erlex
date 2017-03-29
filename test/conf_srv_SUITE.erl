-module(conf_srv_SUITE).
-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

all() -> [conf_srv01, conf_srv02, conf_srv03, conf_srv05].

init_per_testcase(conf_srv01, Config) -> Config;
init_per_testcase(TC, Config) ->
    true = TC:start(),
    Config.

end_per_testcase(conf_srv01, _Config) -> ok;
end_per_testcase(TC, _Config) ->
    stop = TC:stop().

conf_srv01(_Config) ->
    Pid = conf_srv01:start(),
    Pid ! {join, self()},
    {joined, ok} = response().

conf_srv02(_Config) ->
    conf_srv02 ! {join, self()},
    {joined, ok} = response().

conf_srv03(_Config) ->
    ok = conf_srv03:join().

conf_srv05(_Config) ->
    Participants = [joe, mike, robert, elephant_in_the_room],
    lists:foldl(fun expect_joined/2, [], Participants).

expect_joined(Caller, Callers) ->
    Callers = conf_srv05:join(Caller),
    [Caller | Callers].

response() ->
    receive
        Response -> Response
    end.
