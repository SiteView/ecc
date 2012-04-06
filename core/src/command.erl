-module(command).

-export([get_command/1]).

get_command(Conn) ->
	if
		Conn =:= "SSH" ->
			{ok, ssh_command};
		Conn =:= "rlogin" ->
			{ok, rlogin_command};
		Conn =:= "Telnet" ->
			{ok, telnet_command};
		Conn =:= "NetBIOS" ->
			{ok, netbios_command};
		Conn =:= "WMI" ->
			{ok, wmi_command};
		true ->
			{error, no_command}
	end.
	