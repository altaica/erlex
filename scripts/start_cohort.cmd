@echo off
setlocal

:: Start active and standby nodes.
call :start_node magnumopus
call :start_node obsequilis

exit /B %ERRORLEVEL%

:: Function to start a node and run the `cohort' application on it.
:start_node
start werl.exe -sname %1@localhost -args_file apps/cohort/config/vm.args
