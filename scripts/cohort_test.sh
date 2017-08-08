#!/bin/sh

set -e

scripts/start_cohort.sh &
escript scripts/cohort_test.erl
