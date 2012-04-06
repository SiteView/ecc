-module(siteview_command,[Host,Cmd,Machine]).
-include("monitor.hrl").
-include("config.hrl").
-compile(export_all).

-define(runScriptsDLL,"unixTelnetCmd").  %%-- yi.duan unix telnet

exec() -> 
    case Host of
    "" ->
        case platform:getOs()  of
        1 ->
            Ret = os:cmd("tools\\perfex.exe"++Cmd),
            {ok,string:tokens(Ret,"\r\n")}; 
        _ ->
            dealSshDate(os:cmd(Cmd)) 
        end;    
    _ ->
        case Machine#machine.method of
        "WMI" ->
            io:format("WMIYeah~n"),
	    io:format("WMICmd: ~p~n", [Cmd]),
	    %~ io:format("wmi wmi wmi wmi wmi wmi ~n",[]),
            wmiScript:wmiExeBat(Host, Machine#machine.login, Machine#machine.passwd, Cmd);
        "SSH" ->
	      %~ io:format("SSH SSH SSH SSH SSH SSH ~n",[]),
            dealSshDate(ssh_command:exec(Host, Machine#machine.sshport, Machine#machine.login,Machine#machine.passwd, Cmd));        
        "NetBIOS" ->            
            NetBiosCmd = "tools\\perfex.exe -connect " ++ Host ++ " -u " ++Machine#machine.login ++ " -p " ++ Machine#machine.passwd ++" " ++ Cmd,
            Ret = os:cmd(NetBiosCmd),
	    %~ io:format("NetBIOS NetBIOS NetBIOS NetBIOS NetBIOS NetBIOS ~n",[]),
            {ok,string:tokens(Ret,"\r\n")};            
        "Telnet" ->
	   		?LOG("telnet ~p~n",[{Host,Cmd}]), 

			Port = case lists:keysearch(port, 1, Machine#machine.other) of
						{value, {port, Value}} ->
							Value;
						_ ->
							23
				   end,
%% port -------------------------------------begin	 
			% Result22 = runWinScripts({Host, Machine#machine.login,Machine#machine.passwd, Cmd, integer_to_list(Port)}),
			% io:format("Pid Pid Pid Pid Pid ~p~n",[Result22]),
%% port -------------------------------------end	
			
%% nif -------------------------------------begin	
			 Paras="_MachineName="++Host++"$_UserAccount="++Machine#machine.login++"$_PassWord="
		         ++Machine#machine.passwd++"$_Port="++integer_to_list(Port)++"$_Cmd="++Cmd,
			 io:format("Paras = ~p~n", [Paras]),
			 Result = monitorc2erlang:getrefreshedmonitor(Paras, "ecc8monitor/monitor.dll", "get_telnet_info"),
%% nif -------------------------------------end	
			
%% erlang inline tcp --------------------------------begin	
	    %	{ok,Pid} = st_telnet:connect(Host,Port,Machine#machine.login,Machine#machine.passwd),
		%	io:format("Pid Pid Pid Pid Pid ~p,~p~n",[Pid,Cmd]),
	    %	Result = st_telnet:cmd(Pid,Cmd),
	   	%	st_telnet_client:close(Pid),
%% erlang inline tcp --------------------------------end	
			
%% invoke plink	--------------------------------
		    % PlinkCmd = "tools\\plink.exe" ++ Machine#machine.login ++ "@" ++  Machine#machine.host ++ " -telnet " ++" -P " 
	    	%		++ Port ++" -pw " ++ Machine#machine.passwd ++ Cmd,
			% Result = os:cmd("plink.exe 192.168.0.223 -P 22 -ssh -l root -pw siteview123!@# vmstat").
			% Result = os:cmd(PlinkCmd),
%% invoke plink	----------------------------	  
	
			% io:format("Pid Pid Pid Pid Pid ~p~n",[Result]),
			% Len = length(Result),
			% Newdata = lists:sublist(Result,Len-4), 
			% Result2 = {ok,lists:flatten(Newdata)}, 
			% io:format("Pid Pid Pid Pid Pid ~p~n",[Result2]),
			
			 Result2 = {ok,Result},        % nif
			{ok,ResultData} = Result2,     % nif
			
 
			%{ok,ResultData} = Result,    
			if ResultData =:= [] ->
			  	 null;
			   ResultData =/= [] ->
				 %  dealSshDate(Result)
			 	   dealSshDate(Result2)   % nif
		    end;
		
			% end;
            % telnet_command:exec(Host,23,Machine#machine.login,Machine#machine.passwd,Cmd);  
        "rlogin" ->
            rlogin_command:exec(Host,513,Machine#machine.login,Machine#machine.passwd,Cmd);  
        _ ->
            null
       end
    end.       

dealSshDate(Date) ->
    case Date of 
    {ok,String} ->
        List = textutils:split2(String),  
		io:format("Command String:~p~n",[List]), 
        {ok,List};
    {error, Reason} ->
        if is_atom(Reason)  -> 
            %?LOG("Command String error:~p~n",[atom_to_list(Reason)]),          
            {error, atom_to_list(Reason)};
        true ->      
            {error,Reason}  
        end   
    end.

init() ->
    {_, Path} = file:get_cwd(),  
    case erl_ddll:load_driver(Path++"\\tools", ?runScriptsDLL) of
	ok -> ok;
	{error, already_loaded}  -> ok;
	{error,ErrorDesc} ->
		io:format("===>~p~n", [erl_ddll:format_error(ErrorDesc)]),
		exit({error, could_not_load_driver});
	_ ->  exit({error, could_not_load_driver})
    end. 
 
runWinScripts(Msg) ->
	case init() of
	  ok ->
		 Port = open_port({spawn, ?runScriptsDLL}, [binary]),
		 Bin = term_to_binary(Msg),
		 port_command(Port,Bin),
		 Result = receive_port_data(Port),
		 case Result of
			{error,timeout} -> 
			  {error,timeout};
			_ ->	    
			  port_close(Port),
			  Result,
			  binary_to_term(Result)
		 end;
	  Error -> Error
	end.

receive_port_data(Port) ->
    receive
        {Port, {data, Data}} ->  Data
	after 80000 ->  {error,timeout}
    end.

