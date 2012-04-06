@echo off
set Eval= -eval "application:start(quickstart_inets),application:start(svecc),application:start(crypto),ct_run:script_start()"
set Comm= +P 500000 -env ERL_MAX_ETS_TABLES 20000 -setcookie 3ren -pa %cd%\additionmod\erlsoap\ebin -pa %cd%\additionmod\erlsom-1.2.1\ebin -pa %cd%\iconv -pa %cd%\web\mochiweb -pa %cd%\web\ebin -pa %cd%\core\ebin -pa %cd%\nitrogen\ebin %cd%\nitrogen\include 
:PARSE
IF "%1" == "" 			goto COMMAND
IF "%1" == "-config"		goto CONFIG
IF "%1" == "-vts"		goto VTS
IF "%1" == "-browser"		goto BROWSER
IF "%1" == "-shell"		goto SHELL
IF "%1" == "-ctname"		goto CTNAME
IF "%1" == "-ctmaster"		goto CTMASTER
goto OTHER

:CONFIG
set Comm=%Comm% -ct_config
shift
goto PARSE

:VTS
set Vts=1
set Comm=%Comm% %1
shift
goto PARSE

:BROWSER
set Browser=%2
set Comm=%Comm% %1
shift
goto PARSE

:SHELL
set Shell=1
set Comm=%Comm% %1
shift
goto PARSE

:CTNAME
set Ctname=%2
set Comm=%Comm%
shift
goto PARSE

:CTMASTER
set Ctmaster=1
set Comm=%Comm%
shift
goto PARSE

:OTHER
set Comm=%Comm% %1
shift
goto PARSE

:COMMAND
IF "%Vts%" == "1"	goto H_VTS
IF "%Shell%" == "1"	goto H_SHELL
IF "%Ctname%" == "1"	goto H_CTNAME
IF "%Ctmaster%" == "1"	goto H_CTMASTER
goto H_OTHER


:H_VTS
erl -sname ct -s webtool script_start vts %Browser% %Comm% %Eval%
goto END

:H_SHELL
erl -sname ct %Comm% %Eval%
goto END

:H_CTNAME
erl -sname %Ctname% %Comm% %Eval%
goto END

:H_CTMASTER
erl -sname ct_master %Comm%
goto END

:H_OTHER
erl -sname ct %Comm% %Eval% -s erlang halt
goto END

:END
set Eval=
set Comm=
