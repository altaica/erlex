-module(conf_srv01_SUITE).
-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

all() -> [joe_joins, mike_joins, robert_joins].

init_per_suite(Config) ->
    {ok, _Pid} = conf_srv01:start(),
    Config.

end_per_suite(_Config) ->
    ok = conf_srv01:stop().

joe_joins(Config) ->    common:join(conf_srv01, Config).
mike_joins(Config) ->   common:join(conf_srv01, Config).
robert_joins(Config) -> common:join(conf_srv01, Config).
