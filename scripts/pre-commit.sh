#!/bin/sh

set -u
set -e

rm -rf _build

escript scripts/rebar3 do xref, dialyzer, eunit, ct, cover
escript scripts/rebar3 as lint lint
sh scripts/tidy_check.sh
sh scripts/coverage_check.sh
escript scripts/rebar3 as prod, magnumopus do release
escript scripts/rebar3 as prod, obsequilis do release
sh scripts/cohort_test.sh
