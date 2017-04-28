%%% API type specifications common to all implementations.

-spec conf_srv01:start() -> {ok, pid()}.
-spec conf_srv01:join() -> {joined, [pid()]}.
-spec conf_srv01:send(term()) -> ok.
-spec conf_srv01:stop() -> ok.
