-module(sysmon).
-compile(export_all). 


sleep(Time)->
    receive 
	after Time ->   ok
    end.

start() -> spawn(fun() -> loop() end).

loop() ->
     sleep(5000),
     io:format("\rtelnet_client:~p",[{length(dump_proc())}]),
     loop().
     
     
dump_proc()->
	Proc = erlang:processes(),
	dump_proc(Proc,[]).
	
dump_proc([L|R],Acc) ->
     Process_info = process_info(L),
     case lists:keysearch('dictionary',1,Process_info) of
          {value,{'dictionary',Dictionary}} ->
			case  lists:keysearch('$initial_call',1,Dictionary) of			     
			     {value,{'$initial_call',{telnet_client,init,_}}} -> dump_proc(R,[Process_info|Acc]);
			     _ -> dump_proc(R,Acc)
			end;
	  _ -> dump_proc(R,Acc)
     end;
dump_proc([],Acc) -> lists:reverse(Acc).

dump_ports()->
	Ports = lists:map(fun(X)->  erlang:port_info(X) end,erlang:ports()),
	Ports.
	
getstatus(Pid,Host,Count) ->        
	ProcessInfo = process_info(Pid),
	case lists:keysearch(dictionary,1,ProcessInfo) of
		{value,{dictionary,Dictionary}} ->
			 %~ io:format("Result:~p~n",[{Host,Pid,lists:keysearch(status,1,Dictionary)}]),
			case  lists:keysearch(status,1,Dictionary) of
			     false -> ok;
			     {value,{status,stoped}} -> ok;
			     _ when Count > 120 -> error;
			     _ -> 
				  sleep(500),
				  getstatus(Pid,Host,Count+1)
			end;
		_ -> error
	end.	