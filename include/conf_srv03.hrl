%%% API type specifications common to all implementations.

-spec conf_srv03:start() -> {ok, pid()}.
-spec conf_srv03:join() -> {joined, [pid()]}.
-spec conf_srv03:send(term()) -> ok.
-spec conf_srv03:stop() -> ok.
