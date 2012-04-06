-module(telnet_help).
-compile(export_all).

-define(NIDS_TELNET_IAC,255).
-define(NIDS_TELNET_DO,253).
-define(NIDS_TELNET_DONT,254).
-define(NIDS_TELNET_WILL,251).
-define(NIDS_TELNET_WONT,252).
-define(NIDS_TELNET_SB,250).
-define(NIDS_TELNET_SE,240).

-define(KURLok,"ok").
-define(URLNoConnectionError,"unable to connect to server").
-define(URLBadHostNameError,"unknown host name").
-define(URLTimeoutError,"timed out reading").
-define(URLUnKnownError,"unknown error").

-define(TIMEOUT,20000).

-record(status,{host,port,user,passwd,login_prompt,passwd_prompt,prompt,status,os}).

remote_ping(Server,Port,User,Password,Prompt,To) ->
	case connect(Server,Port,User,Password,Prompt) of
		{ok,Sock,Status} ->
			%~ io:format("Status:~p~n",[Status]),
			case Status#status.os of
				linux ->
					{ok,Result} = command(Status,Sock,"ping "++To++" -c 4"),
					Result_list = string:tokens(Result,"\r\n"),
					[Last] = lists:sublist(Result_list,length(Result_list)-1,1),
					%~ io:format("Last:~p~n",[Last]),
					case string:tokens(Last,"=") of
						[_|[Resutl_info]] ->
							Info = string:tokens(Resutl_info,"/"),
							%~ io:format("Info:~p~n",[Info]),
							[Min_rtt] = lists:sublist(Info,1,1),
							[Max_rtt] = lists:sublist(Info,2,1),
							[Avg_rtt] = lists:sublist(Info,3,1),
							{ok,[{min_rtt,Min_rtt},{max_rtt,Max_rtt},{avg_rtt,Avg_rtt}]};
						_ ->
							{error,pingerror}
					end;
				win32 ->
					{ok,Result} = command(Status,Sock,"ping "++To++" -n 4"),
					Result_list = string:tokens(Result,"\r\r\n"),
					[Last] = lists:sublist(Result_list,length(Result_list)-1,1),
					%~ io:format("Last:~p~n",[Last]),
					case string:tokens(Last,",") of
						Resutl_info ->
							F = fun(Element) ->
									Index = string:str(Element,"=")+1,
									string:substr(Element,Index)
								end,
							Info = lists:map(F,Resutl_info),
							%~ io:format("Info:~p~n",[Info]),
							[Min_rtt] = lists:sublist(Info,1,1),
							[Max_rtt] = lists:sublist(Info,2,1),
							[Avg_rtt] = lists:sublist(Info,3,1),
							{ok,[{min_rtt,Min_rtt},{max_rtt,Max_rtt},{avg_rtt,Avg_rtt}]};
						[Last] ->
							{error,pingerror}
					end;
				unknow ->
					{error,unknowos}
			end;
		{error,Reason} -> {error,Reason}
	end.

connect(Server,Port,User,Password,Prompt) ->
	case gen_tcp:connect(Server,Port,[list,{packet,raw},{active,false}]) of
		{ok,Sock} ->
			Status = #status{host=Server,port=Port,user=User,passwd=Password,login_prompt="ogin: $",passwd_prompt="assword: $",prompt=Prompt},
			case login_to_server(Status,Sock,"") of
				{ok, _Status, Sock} -> 
					os(Status,Sock);
				{error, _Reason} -> {error,loginerror}
			end;
		{error,_} ->
			{error,connecterror}
	end.

os(Status,Sock) ->
	{ok,Result} = command(Status,Sock,"cmd"),
	%~ io:format("Result:~p~n",[Result]),
	case regexp:match(Result,"command not found") of
		{match,_Start,_Length} ->
			{ok,Sock,Status#status{os=linux}};
		nomatch ->
			{ok,Sock,Status#status{os=win32}};
		{error,_Error} ->
			{ok,Sock,Status#status{os=unknow}}
	end.
	
command(Status,Sock,Command) ->
	gen_tcp:send(Sock,Command++"\r\n"),
	loop(Status,Sock,"").

loop(Status,Sock,Data) ->
	case gen_tcp:recv(Sock, 0, 20*1000) of
		{ok, List} ->
			Concat_data = parse_iac(Data ++ List, "", Sock),
			%~ io:format("match_prompt:~p~n",[match_prompt(Status, Concat_data)]),
			case match_prompt(Status, Concat_data) of
				{ok, prompt} ->
					%~ io:format("Receive:~p~n",[Concat_data]),
					{ok,Concat_data};
				_->	loop(Status,Sock, Concat_data)
			end
	end.
	
login_to_server(Status, Sock, Data) ->
	case gen_tcp:recv(Sock, 0, ?TIMEOUT) of
		{ok, List} ->
			Data1 = parse_iac(Data ++ List, "", Sock),
			%~ io:format("match_prompt:~p~n",[match_prompt(Status, Data1)]),
			case match_prompt(Status, Data1) of
				{ok, init} ->
					gen_tcp:send(Sock, Status#status.user),
					gen_tcp:send(Sock, "\r\n"),
					Status1 = Status#status{status=login},
					login_to_server(Status1, Sock, "");					
				{ok, login} ->
					gen_tcp:send(Sock, Status#status.passwd),
					gen_tcp:send(Sock, "\r\n"),
					Status1 = Status#status{status=passwd},
					login_to_server(Status1, Sock, "");					
				{ok, prompt} ->
					Status1 = Status#status{status=prompt},
					{ok, Status1, Sock};
				{error, Reason} ->
					gen_tcp:close(Sock),
					{error, Reason};
				_ ->
					%~ io:format("nomatch~n"),
					login_to_server(Status, Sock, Data1)
			end;
		{error, Reason} ->
			gen_tcp:close(Sock),
			{error, Reason}
	end.

match_prompt(Status, Data) ->
	%%is there login prompt
	case regexp:match(Data, Status#status.login_prompt) of
		{match, _, _} ->
			{ok, init};
		nomatch ->
			%%is there password prompt
			case regexp:match(Data, Status#status.passwd_prompt) of
				{match, _, _} ->
					{ok, login};
				nomatch ->
					%%is there login error
					case regexp:match(Data, "ogin[ |\.]*incorrect[\\r|\\n]*$") of
						{match, _, _} ->
							{error, login_error};
						nomatch ->
							%%is there command prompt
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
	
%%excute the command in telnet server	
execute_command(Status, Sock, Cmd) ->
	%%send the command
	gen_tcp:send(Sock, Cmd),
	gen_tcp:send(Sock, "\r\n"),
	%%process result
	get_cmd_result(Status, Sock, "").

%%parse the command reult
get_cmd_result(Status, Sock, Data) ->
	%%receive result
	case gen_tcp:recv(Sock, 0) of
		{ok, List} ->
			%%concat the reault
			Data1 = parse_iac(Data ++ List, "", Sock),
			%%is there a new command prompt which means the end of command result
			case match_prompt(Status, Data1) of
				{ok, prompt} ->
					%%extract the result string from the data
					extract_result(Status#status.prompt, Data1);
				{error, Reason} ->
					{error, Reason};
				_ ->
					%%if no command prompt, there must have further data
					get_cmd_result(Status, Sock, Data1)	
			end;
		{error, Reason} ->
			{error, Reason}
	end.	
	
%%extract the command result	
extract_result(_Prompt, Data) ->
	%%find the first \n, which is the start of result
	Start = string:str(Data, "\n"),
	if
		%%no \n? something error
		Start =:= 0 ->
			{error, parse_result_error};
		%%find the last \n, which is the end og result
		true ->
			End = string:rstr(Data, "\n"),
			if
				%%no \n? something error
				End =:= 0 ->
					{error, parse_result_error};
				%%extract result
				true ->
					{ok, string:substr(Data, Start+1, End-Start)}
			end
	end.
		
%%IAC is the control data of the telnet, which 	mixed in the telnet data.
%%It is not what we want to see. Also some IAC must response to continue telnet session. 
%%Therefor we must delete it from data, sometime response some setting data to telnet server

%%find the IAC code from result string and delete it
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
%%double IAC, last IAC is not a IAC code, add it to result string	
delete_iac([?NIDS_TELNET_IAC|Data], Result, _Sock)->
	Result1 = Result ++ [?NIDS_TELNET_IAC],
	{Data, Result1};
%%do IAC, it's length is 3	
delete_iac([?NIDS_TELNET_DO|Data], Result, _Sock)->
	Len = length(Data) +1,
	if
		%%if length is less than 3, continue to get result
		Len < 2 ->
			Data1 = [],
			Result1 = Result ++ [?NIDS_TELNET_IAC, ?NIDS_TELNET_DO] ++ Data;
		true ->
		%%do IAC is deleted, and send back a response to telnet server
			Data1 = string:substr(Data, 2, length(Data)-1),
			gen_tcp:send(_Sock, [255, 252, lists:nth(1, Data)]),
			Result1 = Result
	end,
	{Data1, Result1};
%%dont IAC, it's length is 3	
delete_iac([?NIDS_TELNET_DONT|Data], Result, _Sock)->
	Len = length(Data) +1,
	if
		%%if length is less than 3, continue to get result
		Len < 2 ->
			Data1 = [],
			Result1 = Result ++ [?NIDS_TELNET_IAC, ?NIDS_TELNET_DONT] ++ Data;
		%%dont IAC is deleted, and send back a response to telnet server
		true ->
			Data1 = string:substr(Data, 2, length(Data)-1),
			gen_tcp:send(_Sock, [255, 252, lists:nth(1, Data)]),
			Result1 = Result
	end,
	{Data1, Result1};
%%will IAC, it's length is 3	
delete_iac([?NIDS_TELNET_WILL|Data], Result, _Sock)->
	Len = length(Data) +1,
	if
		%%if length is less than 3, continue to get result
		Len < 2 ->
			Data1 = [],
			Result1 = Result ++ [?NIDS_TELNET_IAC, ?NIDS_TELNET_WILL] ++ Data;
		%%will IAC is deleted, and send back a response to telnet server
		true ->
			Data1 = string:substr(Data, 2, length(Data)-1),
			gen_tcp:send(_Sock, [255, 254, lists:nth(1, Data)]),			
			Result1 = Result
	end,
	{Data1, Result1};
%%wont IAC, it's length is 3	
delete_iac([?NIDS_TELNET_WONT|Data], Result, _Sock)->
	Len = length(Data) +1,
	if
		%%if length is less than 3, continue to get result
		Len < 2 ->
			Data1 = [],
			Result1 = Result ++ [?NIDS_TELNET_IAC, ?NIDS_TELNET_WONT] ++ Data;
		%%do IAC is deleted
		true ->
			Data1 = string:substr(Data, 2, length(Data)-1),
			Result1 = Result
	end,
	{Data1, Result1};
%%SB IAC, it's length is variable	
delete_iac([?NIDS_TELNET_SB|Data], Result, _Sock)->
	%%find the SE char, which is the end of SB 
	End = string:str(Data, [?NIDS_TELNET_SE]),
	if
		%%not found, continue to receive data
		End =:= 0 ->
			Data1 = [],
			Result1 = Result ++ [?NIDS_TELNET_IAC, ?NIDS_TELNET_SB] ++ Data;
		%%delete the SB IAC
		true ->
			Data1 = string:substr(Data, End+1, length(Data)-End),
			Result1 = Result
	end,
	{Data1, Result1};
%%other IAC, their length is 2	
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
	
	
