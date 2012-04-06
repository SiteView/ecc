-module(netbios_command).

-export([exec/5]).

exec(_Host, _Port, _User, _Passwd, Cmd) ->
	{ok, os:cmd(Cmd)}.

