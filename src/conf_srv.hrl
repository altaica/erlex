%%% API type specifications common to all implementations.

-spec start() -> {ok, pid()}.
-spec join() -> {joined, [pid()]}.
-spec send(term()) -> ok.
-spec stop() -> ok.
