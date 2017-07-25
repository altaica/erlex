#!/bin/sh

set -u
set -e

rm -rf _build

rebar3 do version, tree, xref
time rebar3 dialyzer
time rebar3 do eunit, ct, cover
escript scripts/elvis -Vv --config scripts/elvis.config rock
sh scripts/coverage_check.sh

rebar3 as prod do version, tree, tar

