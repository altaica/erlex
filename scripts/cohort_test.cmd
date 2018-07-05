@echo off
setlocal

call scripts\start_cohort.cmd
sleep 1
call escript scripts\cohort_test.erl
