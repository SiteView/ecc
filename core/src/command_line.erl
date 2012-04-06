%% ---
%% command_line
%%
%%---
-module(command_line,[]).
-compile(export_all).

exec(Cmd)->
	%io:format("exec:~p~n",[Cmd]),
	os:cmd(Cmd).

exec(Cmd,Monitor)->
	%%io:format("exec:~p~n",[Cmd]),
	THIS:exec(Cmd).
