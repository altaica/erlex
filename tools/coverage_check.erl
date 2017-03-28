#!/usr/bin/env escript

main(_) ->
    {ok, IoDevice} = file:open("_build/test/cover/index.html", [read]),
    read_lines(IoDevice).

read_lines(IoDevice) ->
    {ok, Grep} = re:compile("<td><strong>Total</strong></td><td>([0-9]{1,3})%</td>"),
    case file:read_line(IoDevice) of
        {ok, Line} ->
            case grep_line(Grep, Line) of
                {coverage, 100} -> 0;
                _ ->
                    read_lines(IoDevice)
            end;
        eof ->
            io:format("Coverage incomplete~n"),
            1
    end.

grep_line(Grep, Line) ->
    case re:run(Line, Grep) of
        {match, [_Match1, {Start, Stop}]} ->
            S = string:sub_string(Line, Start + 1, Start + Stop),
            {Pct, _} = string:to_integer(S),
            {coverage, Pct};
        _Matches ->
            nomatch
    end.

