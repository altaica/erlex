#!/bin/sh

set -u
set -e

rm -rf _build

rebar3 do dialyzer, xref, eunit, ct, cover
rebar3 as lint lint
sh scripts/coverage_check.sh

rebar3 as prod do version, tree, tar

