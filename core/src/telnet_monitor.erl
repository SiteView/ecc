%% ---
%% Telnet Monitor
%%
%%---

%% @copyright 2008-2009 Dragonflow
%% @author Jian Huang <jian.huang@dragonflow.com>
%% @version 1.0

%% @doc telnet monitor
%%
%%This module is to test telnet service:
%%1. test the connection of telnet service
%%2. execute one command in remote machine
%%3. match the result of the command

-module(telnet_monitor, [BASE]).
-extends(atomic_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

-define(NIDS_TELNET_IAC, 255).
-define(NIDS_TELNET_DO, 253).
-define(NIDS_TELNET_DONT, 254).
-define(NIDS_TELNET_WILL, 251).
-define(NIDS_TELNET_WONT, 252).
-define(NIDS_TELNET_SB, 250).
-define(NIDS_TELNET_SE, 240).

-define(KURLok, "ok").
-define(URLNoConnectionError, "unable to connect to server").
-define(URLBadHostNameError, "unknown host name").
-define(URLTimeoutError, "timed out reading").
-define(URLUnKnownError, "unknown error").

-record(status, {host, port, user, passwd, login_prompt, passwd_prompt, prompt, status}).

%% @spec new() -> Obj
%% @type Obj = term()
%% @doc create a new instance for telnet monitor
new()->
	Obj = atomic_monitor:new(),
	Obj:set_attribute(round_trip_time, 0), %"n/a"
	Obj:set_attribute(status, 200),		%"ok"
	{?MODULE,Obj}.

%% @spec defaultTitle(Params) -> List
%% @type Params = [term()]
%% @type List = string()
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	BASE:defaultTitle(Params) ++ ": " ++ case proplists:get_value(telnetserver,Params) of undefined->"";V->V end.

%% @spec update() -> Result
%% @type Result = term()
%% @doc update is the run function called by schedule to test the  telnet service
update() ->
	{ok, {_, ServerString}} = BASE:get_property(telnetserver),	
	case ServerString of
		"" ->
			%%return false when host is null
			THIS:set_attribute(?CATEGORY, nodata),
			THIS:set_attribute(?NO_DATA, true),
			THIS:set_attribute(?STATE_STRING, "UnKonwn Telnet Server");
		_ ->
			%%get hostname and port from server string
			case string:tokens(ServerString, ":") of
				[A, B] ->
					Server = A,
					Port = B;
				[A] ->
					Server = A,
					Port = 23;
				_ ->
					Server = ServerString,
					Port = 23
			end,		
		
			case inet:getaddr(Server, inet) of 
				{ok, Server1} ->
					ok;
				{error, _} ->
					Server1 = {0,0,0,0}
			end,
		
			%%get timeout number
			_Timeout = case BASE:get_property(timeout) of
						{ok, {_, T}} ->
							T*1000;
						_ ->
							60000	%60s
					end,
			
			%%rember the start time		
			StartT = now(),	
			%%timer:start(),
			%%timer:send_after(Timeout, "Timeout"),
			
			%%call function to test
			[Code, Desc, _R] = checkTelnet(Server1, Port),
			%%handle the result and store it to database
			case Code of
				200 ->
					Cost = timer:now_diff(now(), StartT),%microseconds	
					StrC = sv_datetime:microSecondsToStrSeconds(Cost),
					%io:format(" Cost:~p~n", [StrC]),
					THIS:set_attribute(round_trip_time, Cost/1000), 
					THIS:set_attribute(status, Code),
					THIS:set_attribute(?STATE_STRING, StrC ++ " sec");					
				_ ->
					%THIS:set_attribute(round_trip_time, "n/a"),  
					THIS:set_attribute(?STATE_STRING, Desc),
					THIS:set_attribute(status, Code)
			end		
	end.


%% @spec checkTelnet(Server, Port) -> [I, S, L]
%% @type Servet = string() | atom() | ip_address()
%% @type Port = 0..65535
%% @doc check the telnet 
checkTelnet(Server, Port)->	
	%%connect to telnet server
	case gen_tcp:connect(Server, Port, [list, {packet, raw}, {active, false}]) of
		{ok, Sock} ->
			%%get the property value of the monitor
			{ok, {_, User}} = BASE:get_property(username),
			{ok, {_, Pass}} = BASE:get_property(password),
			{ok, {_, Match}} = BASE:get_property(matchcontent),
			{ok, {_, LPrompt}} = BASE:get_property(loginprom),
			{ok, {_, PPrompt}} = BASE:get_property(passwordprom),
			{ok, {_, Prompt}} = BASE:get_property(prompt),			
			{ok, {_, Cmd}} = BASE:get_property(command),
			%%$ is the match key, just add "\\" ahead of it
			case Prompt of
				"$" ->
					Prompt1 = "\\$";
				_ ->	
					Prompt1 = Prompt
			end,	
			
			Status = #status{host=Server, port=Port, user=User, passwd=Pass, login_prompt=LPrompt ++ " *$", passwd_prompt=PPrompt++" *$", prompt=Prompt1++" *$"},
			
			%%login to telnet
			case login_to_server(Status, Sock, "") of
				{ok, Status1, Sock} ->
					if
						%%if command is not set, return success
						Cmd =:= "" ->
							I = 200, %KURLok
							S = ?KURLok,
							L = 0;
						true ->
							%%execute the command
							case execute_command(Status1, Sock, Cmd) of
								{ok, Res} ->
									%%match the result of command
									Content = string:str(Res, Match),
									if
										%%no match content, return success
										Match =:= "" ->
											I = 200, %KURLok
											S = ?KURLok,
											L = length(Res);
										%%match error
										Content =:= 0 ->
											I = -994,
											S = "match error",
											L = 0;
										%%match the content
										true ->
											I = 200, %KURLok
											S = ?KURLok,
											L = length(Res)
									end;
								{error, Reason} ->
									%%command execute error
									I = -995,
									%%change result from atom to list
									if
 										is_atom(Reason) =:= true ->
											S = atom_to_list(Reason);
										true ->
											S = Reason
									end,
									L = 0
							end
					end;	
				{error, Reason} ->
					%%login error
					I = -995,
					if
						is_atom(Reason) =:= true ->
							S = atom_to_list(Reason);
						true ->
							S = Reason
					end,
					L = 0
			end;
		%%connect error	
		{error,econnrefused} ->
			I = -998, %URLNoConnectionError
			S = ?URLNoConnectionError,
			L = 0;
		{error, nxdomain} ->
			I = -997, %URLBadHostNameError
			S = ?URLBadHostNameError,
			L = 0;
		{error, timeout} ->
			I = -996, %URLTimeoutError
			S = ?URLTimeoutError,
			L = 0;		
		{error, Reason} ->
			I = -1000, %URLUnKnownError
			S = atom_to_list(Reason),
			L = 0
	end,	
	[I, S, L].

%%login to telnet server
login_to_server(Status, Sock, Data) ->
	{ok, {_, Timeout}} = BASE:get_property(timeout),
	%%receive the data from socket
	case gen_tcp:recv(Sock, 0, Timeout*1000) of
		{ok, List} ->
			%%concat the data
			Data1 = parse_iac(Data ++ List, "", Sock),
			%%match the login, password and command prompt
			case match_prompt(Status, Data1) of
				{ok, init} ->
					%%find login prompt, send the user name
					gen_tcp:send(Sock, Status#status.user),
					gen_tcp:send(Sock, "\r\n"),
					Status1 = Status#status{status=login},
					login_to_server(Status1, Sock, "");					
				{ok, login} ->
					%%find the password prompt, send the password
					gen_tcp:send(Sock, Status#status.passwd),
					gen_tcp:send(Sock, "\r\n"),
					Status1 = Status#status{status=passwd},
					login_to_server(Status1, Sock, "");					
				{ok, prompt} ->
					%%find the command prompt, login success.
					Status1 = Status#status{status=prompt},
					{ok, Status1, Sock};
				{error, Reason} ->
					%%something error
					gen_tcp:close(Sock),
					{error, Reason};
				_ ->
					%%not thing match, continue to receive data 
					login_to_server(Status, Sock, Data1)
			end;
		{error, Reason} ->
			%%connection with telnet server error
			gen_tcp:close(Sock),
			{error, Reason}
	end.

%%match the login, password and command prompt
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
	
	
%% @spec verify(Params) -> {ok, []} | {error, Reason}
%% @type Params = [term()]
%% @type Reason = string()
%% @doc verify is the function called by schedule to verify the parameter of monitor inputed by user
%%	telnet monitor verify timeout and hostname
verify(Params)->
    Errs = 
	case proplists:get_value(timeout,Params) of
    ""->
	    [{timeout,"timeout missing."}];
    Time->
		if
			not is_number(Time) ->
				[{timeout,"timeout must be a number."}];
			true->
				[]
		end
	end ++
	case proplists:get_value(telnetserver,Params) of
    ""->
		[{telnetserver,"Server Name missing"}];
    Host->
	    case string:rstr(Host," ") of
		    0->
			    [];
			_->
			    [{telnetserver, "no spaces are allowed"}]
	    end
	end ++
	case proplists:get_value(telnetserver,Params) of
    ""->
		[{telnetserver,"Server Name missing"}];
    Host->
		case string:tokens(Host, ":") of
			[_A, B] ->
				if
					not is_number(B) ->
						[{telnetserver, "port is not a number"}];
					true ->
						[]
				end;
			_ ->
				[]
		end
	end ++	
	case BASE:verify(Params) of
		{error,E}->
			E;
		_->
			[]
	end,	
	
	if length(Errs)>0 ->
	    {error,Errs};
    true ->
	    {ok,""}
	end.
		

%% @spec get_classifier(error) -> List
%% @type List = [Tuple]
%% @type Tule = {status, Logic, Value}
%% @type Logic = '!=' | '==' | '>' | '<' | 'contain
%% @type Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{status, '!=', 200}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{status, '>',  200}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{status, '==', 200}]
	end.

	
%% @spec get_template_property() -> List
%% @type List = [term()]
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
%% 1.elements show on gui interface
%% 2.elements to evaluate the return value of telnet server 
get_template_property()->
	BASE:get_template_property() ++
	[
	#property{name=telnetserver, title="Telnet Server", type=text, editable=true, order=1, description="the IP address or host name of the Telnet server (examples: 206.168.191.21 or demo.siteview.com)"},
	#property{name=username, title="User Name", type=text, editable=true, order=2, description="user name used to log in to the server via Telnet"},
	#property{name=password, title="Password", type=password, editable=true, order=3, description="password  used to log in to the server via Telnet"},
	#property{name=command, title="Command", type=text, editable=true, order=4, description="optional command to run on the server"},
	#property{name=loginprom, title="Login Prompt", default="login:", advance=true, order=8, description="the label printed out when the remote system prompts for a login"},
	#property{name=timeout, title="Timeout", type=numeric, default=60, advance=true, order=9, description="the time out, seconds, to wait for the telnet connection to made and the command to complete",baselinable=true},
	#property{name=passwordprom, title="Password Prompt", default="password:", advance=true, order=10, description="the label printed out when the remote system prompts for a password"},
	#property{name=prompt, title="Prompt", default="#", advance=true, order=11, description="the prompt printed out when the remote system is waiting for input"},
	#property{name=matchcontent, title="Match Content", advance=true, order=12, description="optional text to match against content of the file"},
	#property{name=status, title="status", type=numeric, order=13, configurable=false, state=true},
	#property{name=round_trip_time, title="round trip time(milliseconds)", type=numeric, order=14, configurable=false, state=true,baselinable=true}	
	].
