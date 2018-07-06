#!/bin/sh

set -u
set -e

rm -rf _build

escript scripts/rebar3 do xref, dialyzer, eunit, ct, cover
escript scripts/rebar3 as lint lint

erl -noshell -eval "erl_tidy:dir("apps", [{test, true}])." -s init stop \
    | grep -v "tidying directory" \
    | grep -v "apps/[a-z0-9_]*/test"

sh scripts/coverage_check.sh
