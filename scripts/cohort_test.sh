#!/bin/sh

set -e

sh scripts/start_cohort.sh &
sh scripts/start_cohort.sh &
sleep 1
escript scripts/cohort_test.erl
