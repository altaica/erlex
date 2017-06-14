@echo off
setlocal

CFGDIR=apps/cohort/config

:: Start active and standby nodes.
call :start_node magnumopus
call :start_node obsequilis

exit /B %ERRORLEVEL%

:: Function to start a node and run the `cohort' application on it.
:start_node
start "Node %1" werl.exe -sname %1@localhost -config %CFGDIR%/%1 -args_file %CFGDIR%/vm.args
