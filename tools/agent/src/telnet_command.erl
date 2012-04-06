-module(telnet_command).

-export([exec/5]).
-export([test/0]).

-define(NIDS_TELNET_IAC, 255).
-define(NIDS_TELNET_DO, 253).
-define(NIDS_TELNET_DONT, 254).
-define(NIDS_TELNET_WILL, 251).
-define(NIDS_TELNET_WONT, 252).
-define(NIDS_TELNET_SB, 250).
-define(NIDS_TELNET_SE, 240).

-record(status, {host, port, user, passwd, login_prompt, passwd_prompt, prompt, status}).

test() ->
  exec("192.168.0.118", 22, "root", "siteview123","mpstat -P ALL 60  | while read ; do [[ $REPLY = ??:??:* ]] && echo \"${REPLY#???????????}\"; done").  

exec(Host, Port, User, Passwd, Cmd) ->
	Status = #status{host=Host, port=Port, user=User, passwd=Passwd, login_prompt="login:"++" *$", passwd_prompt="Password:"++" *$", prompt="\\$"++" *$"},
	case connect_to_server(Status) of
		{ok, Status1, Sock} ->
			Result = execute_command(Status1, Sock, Cmd),
			gen_tcp:close(Sock),
			Result;
		{error, Reason} ->
			{error, Reason}
	end.
	
connect_to_server(Status) ->
	case gen_tcp:connect(Status#status.host, Status#status.port, [list, {packet, raw}, {active, false}]) of
		{ok, Sock} ->
			Status1 = Status#status{status=init},
			login_to_server(Status1, Sock, "");
		{error, Reason} ->
			{error, Reason}
	end.

login_to_server(Status, Sock, Data) ->
	case gen_tcp:recv(Sock, 0) of
		{ok, List} ->
			Data1 = parse_iac(Data ++ List, "", Sock),
			case match_prompt(Status, Data1) of
				{ok, init} ->
					gen_tcp:send(Sock, Status#status.user),
					gen_tcp:send(Sock, "\n"),
					Status1 = Status#status{status=login},
					login_to_server(Status1, Sock, "");					
				{ok, login} ->
					gen_tcp:send(Sock, Status#status.passwd),
					gen_tcp:send(Sock, "\n"),
					Status1 = Status#status{status=passwd},
					login_to_server(Status1, Sock, "");					
				{ok, prompt} ->
					Status1 = Status#status{status=prompt},
					{ok, Status1, Sock};
				{error, Reason} ->
					gen_tcp:close(Sock),
					{error, Reason};
				_ ->
					login_to_server(Status, Sock, Data1)
			end;
		{error, Reason} ->
			gen_tcp:close(Sock),
			{error, Reason}
	end.
	
match_prompt(Status, Data) ->
	case regexp:match(Data, Status#status.login_prompt) of
		{match, _, _} ->
			{ok, init};
		nomatch ->
			case regexp:match(Data, Status#status.passwd_prompt) of
				{match, _, _} ->
					{ok, login};
				nomatch ->
					case regexp:match(Data, "ogin[ |\.]*incorrect[\\r|\\n]*$") of
						{match, _, _} ->
							{error, login_error};
						nomatch ->
							case regexp:match(Data, Status#status.prompt) of
								{match, _, _} ->
									{ok, prompt};
								nomatch ->
									ok;
								{error, Reason} ->
									{error, Reason}
							end;						
						{error, Reason} ->
							{error, Reason}
					end;
				{error, Reason} ->
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.
	
execute_command(Status, Sock, Cmd) ->
	gen_tcp:send(Sock, Cmd),
	gen_tcp:send(Sock, "\r\n"),
	get_cmd_result(Status, Sock, "").
	
get_cmd_result(Status, Sock, Data) ->
	case gen_tcp:recv(Sock, 0) of
		{ok, List} ->
			Data1 = parse_iac(Data ++ List, "", Sock),
			case match_prompt(Status, Data1) of
				{ok, prompt} ->
					extract_result(Status#status.prompt, Data1);
				{error, Reason} ->
					{error, Reason};
				_ ->
					get_cmd_result(Status, Sock, Data1)	
			end;
		{error, Reason} ->
			{error, Reason}
	end.	
	
extract_result(_Prompt, Data) ->
	Start = string:str(Data, "\n"),
	if
		Start =:= 0 ->
			{error, parse_result_error};
		true ->
			End = string:rstr(Data, "\n"),
			if
				End =:= 0 ->
					{error, parse_result_error};
				true ->
					{ok, string:substr(Data, Start+1, End-Start)}
			end
	end.
		
parse_iac([], Result, _Sock) ->
	Result;
parse_iac([T|Data], Result, Sock) when T=:= ?NIDS_TELNET_IAC ->
	{Data1, Result1} = delete_iac(Data, Result, Sock),
	parse_iac(Data1, Result1, Sock);
parse_iac([T|Data], Result, Sock) ->
	Result1 = Result ++ [T],
	parse_iac(Data, Result1, Sock).

delete_iac([], Result, _Sock) ->
	{[], Result};
delete_iac([?NIDS_TELNET_IAC|Data], Result, _Sock)->
	Result1 = Result ++ [?NIDS_TELNET_IAC],
	{Data, Result1};
delete_iac([?NIDS_TELNET_DO|Data], Result, _Sock)->
	Len = length(Data) +1,
	if
		Len < 2 ->
			Data1 = [],
			Result1 = Result ++ [?NIDS_TELNET_IAC, ?NIDS_TELNET_DO] ++ Data;
		true ->
			Data1 = string:substr(Data, 2, length(Data)-1),
			gen_tcp:send(_Sock, [255, 252, lists:nth(1, Data)]),
			Result1 = Result
	end,
	{Data1, Result1};
delete_iac([?NIDS_TELNET_DONT|Data], Result, _Sock)->
	Len = length(Data) +1,
	if
		Len < 2 ->
			Data1 = [],
			Result1 = Result ++ [?NIDS_TELNET_IAC, ?NIDS_TELNET_DONT] ++ Data;
		true ->
			Data1 = string:substr(Data, 2, length(Data)-1),
			gen_tcp:send(_Sock, [255, 252, lists:nth(1, Data)]),
			Result1 = Result
	end,
	{Data1, Result1};
delete_iac([?NIDS_TELNET_WILL|Data], Result, _Sock)->
	Len = length(Data) +1,
	if
		Len < 2 ->
			Data1 = [],
			Result1 = Result ++ [?NIDS_TELNET_IAC, ?NIDS_TELNET_WILL] ++ Data;
		true ->
			Data1 = string:substr(Data, 2, length(Data)-1),
			gen_tcp:send(_Sock, [255, 254, lists:nth(1, Data)]),			
			Result1 = Result
	end,
	{Data1, Result1};
delete_iac([?NIDS_TELNET_WONT|Data], Result, _Sock)->
	Len = length(Data) +1,
	if
		Len < 2 ->
			Data1 = [],
			Result1 = Result ++ [?NIDS_TELNET_IAC, ?NIDS_TELNET_WONT] ++ Data;
		true ->
			Data1 = string:substr(Data, 2, length(Data)-1),
			gen_tcp:send(_Sock, [255, 252, lists:nth(1, Data)]),			
			Result1 = Result
	end,
	{Data1, Result1};
delete_iac([?NIDS_TELNET_SB|Data], Result, _Sock)->
	End = string:str(Data, [?NIDS_TELNET_SE]),
	if
		End =:= 0 ->
			Data1 = [],
			Result1 = Result ++ [?NIDS_TELNET_IAC, ?NIDS_TELNET_SB] ++ Data;
		true ->
			Data1 = string:substr(Data, End+1, length(Data)-End),
			Result1 = Result
	end,
	{Data1, Result1};
delete_iac([T|Data], Result, _Sock)->
	Len = length(Data) +1,
	if
		Len < 1 ->
			Data1 = [],
			Result1 = Result ++ [?NIDS_TELNET_IAC, T] ++ Data;
		true ->
			Data1 = string:substr(Data, 1, length(Data)),
			Result1 = Result
	end,
	{Data1, Result1}.
	
	
