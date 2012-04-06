-module(wmiproxy).

-export([init/0, execute/1]).

init() ->
	case os:type() of
		{win32, _} ->
			erlang:load_nif("./ebin/wmiproxy", 0);
		_ ->
			ok
	end.

execute([OS, Host, User, Password, Parameters])->
	case os:type() of
		{unix, _}->
			wmic:execute(OS, Host, User, Password, Parameters);
		_ ->
			io:format("Parameters:~p~n", [Parameters]),
			{error, "wmiproxy is not loaded!"}
	end.
			