-module(rlogin_command).

-export([exec/5]).

-record(status, {host, port, user, passwd, local_user, passwd_prompt, prompt, status}).

exec(Host, Port, User, Passwd, Cmd) ->
	Status = #status{host=Host, port=Port, user=User, passwd=Passwd, local_user="root", passwd_prompt="Password:"++" *$", prompt="\\$"++" *$"},
	case connect_to_server(Status) of
		{ok, Status1, Sock} ->
			Result = execute_command(Status1, Sock, Cmd),
			gen_tcp:close(Sock),
			Result;
		{error, Reason} ->
			{error, Reason}
	end.
	
connect_to_server(Status) ->
	case gen_tcp:connect(Status#status.host, Status#status.port, [list, {port, 887}, {reuseaddr, true}, {packet, raw}, {active, false}]) of
		{ok, Sock} ->
			gen_tcp:send(Sock, [0]),
			gen_tcp:send(Sock, Status#status.local_user ++ [0] ++ Status#status.user ++ [0] ++ "xterm/38400" ++ [0]),
			Status1 = Status#status{status = init},
			login_to_server(Status1, Sock, "");
		{error, Reason} ->
			{error, Reason}
	end.

login_to_server(Status, Sock, Data) ->
	case gen_tcp:recv(Sock, 0) of
		{ok, List} ->
			Data1 = Data ++ List,
			case match_prompt(Status, Data1) of
				{ok, init} ->
					login_to_server(Status, Sock, string:substr(Data1, 2, length(Data1)-1));					
				{ok, login} ->
					gen_tcp:send(Sock, Status#status.passwd),
					gen_tcp:send(Sock, "\r"),
					login_to_server(Status, Sock, "");	
				{ok, window} ->	
					gen_tcp:send(Sock, [255, 255, 115, 115, 0, 24, 0, 80, 4, 0, 4, 0]),
					login_to_server(Status, Sock, string:substr(Data1, 13, length(Data1)-12));	
				{ok, prompt} ->
					{ok, Status, Sock};
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
							match_window(Data);
						{error, Reason} ->
							{error, Reason}
					end;
				{error, Reason} ->
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

match_window(Data) ->
	Len = length(Data),
	if
		Len < 12 ->
			ok;
		true ->
			C1 = lists:nth(1, Data),
			C2 = lists:nth(2, Data),
			C3 = lists:nth(3, Data),
			if
				C1 =:= 0 andalso C2 =:= 255 andalso C3=:= 255 ->
					{ok, window};
				C1 =:= 255 andalso C2 =:= 255 ->
					{ok, window};
				true ->
					ok
			end
	end.

execute_command(Status, Sock, Cmd) ->
	gen_tcp:send(Sock, Cmd),
	gen_tcp:send(Sock, "\r"),
	get_cmd_result(Status, Sock, "").
	
get_cmd_result(Status, Sock, Data) ->
	case gen_tcp:recv(Sock, 0) of
		{ok, List} ->
			Data1 = Data ++ List,
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
		
	