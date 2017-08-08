@echo off
setlocal

call scripts\start_cohort.cmd
call escript scripts\cohort_test.erl
