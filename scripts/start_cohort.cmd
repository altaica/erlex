@echo off
setlocal

:: Start active and standby nodes.
call :start_node magnumopus
call :start_node obsequilis

exit /B %ERRORLEVEL%

:: Function to start a node and run the `cohort' application on it.
:start_node
start werl.exe  -pa         _build/default/lib/cohort/ebin ebin ^
                -args_file  apps/cohort/config/%1.args ^
                -config     apps/cohort/config/sys.config ^
                -eval       "application:start(cohort)"
