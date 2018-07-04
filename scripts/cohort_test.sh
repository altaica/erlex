#!/bin/sh

set -e

sh scripts/start_cohort.sh &
sh scripts/start_cohort.sh &
escript scripts/cohort_test.erl
