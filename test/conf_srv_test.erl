%%% @doc Ensure coverage for those hard-to-reach bits.
%%% @copyright 2017 Phil Dempster

-module(conf_srv_test).
-include_lib("eunit/include/eunit.hrl").

conf_srv02_timeout_coverage_test() ->
    ?assertMatch({ok, _Pid}, conf_srv02:start()),
    ?assertError(timeout, conf_srv02:gen_call(conf_srv02, join, 0)),
    ?assertMatch(ok, conf_srv02:stop()).

conf_srv03_cast_coverage_test() ->
    ?assertMatch({ok, _Pid}, conf_srv03:start()),
    ?assertMatch(ok, gen_server:cast(conf_srv03, dummy)),
    ?assertMatch(ok, conf_srv03:stop()).

conf_srv03_code_change_coverage_test() ->
    ?assertMatch({ok, []}, conf_srv03:code_change(1, [], none)).
