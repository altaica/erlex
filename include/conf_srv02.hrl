%%% API type specifications common to all implementations.

-spec conf_srv02:start() -> {ok, pid()}.
-spec conf_srv02:join() -> {joined, [pid()]}.
-spec conf_srv02:send(term()) -> ok.
-spec conf_srv02:stop() -> ok.
