-module(df_snmp_info).
-compile(export_all).
-include_lib("snmp/include/snmp_types.hrl").

get_object_next(IPList,CommunityList,Timeout,Port,Retry,Oid) ->
	create_agn(IPList,CommunityList,Timeout,Port,Retry,Oid).

create_agn([],_,_,_,_,_) -> [];
create_agn([IP|IPList],CommunityList,Timeout,Port,Retry,Oid) ->
	Result = getAgn(IP,CommunityList,Timeout,Port,Retry,Oid),
	[Result|create_agn(IPList,CommunityList,Timeout,Port,Retry,Oid)].

getAgn(IP,[],Timeout,Port,Retry,Oids) -> 
	[Oid] = Oids,
	{{IP,"",Timeout,Port,Retry},[{Oid,"Error Comnunity"}]}; 
getAgn(IP,[Community|CommunityList],Timeout,Port,Retry,Oids) ->
	[Oid] = Oids,
	snmp_ex2_manager:unagent(IP),
	snmp_ex2_manager:agent(IP,[{community,Community}]),
	Result = snmp_ex2_manager:sync_get_next(IP,Port,Oids),
	io:format("Result:~p~n",[Result]),
	case Result of
		{ok,{_,_,[{_,Oid2,_,Value,_}|_]},_} ->
            case lists:prefix(Oid,Oid2) of
                true ->
                    {{IP,Community,Timeout,Port,Retry},[{Oid2,Value}]};  
                _    ->
                    io:format("test for not match ~p~n", [Oid2]),
                    {{IP,Community,Timeout,Port,Retry},[{Oid,"Not match"}]}
            end;
        {error,timeout} -> 
            getAgn(IP,CommunityList,Timeout,Port,Retry,Oids);
        R -> 
            io:format("test for receiveR ~p~n", [R]),
            {{IP,"",Timeout,Port,Retry},[{Oid,R}]}
    end.



get_object_next_async(IPList,CommunityList,Timeout,Port,Retry,Oid) ->
	%%Pid_main = spawn(fun() -> recV_async([],Timeout,{length(IPList),0}) end),
	create_agn_async(self(),IPList,CommunityList,Timeout,Port,Retry,Oid),
	recV_async([],Timeout,{length(IPList),0}).

create_agn_async(Pid_main,[],_,_,_,_,_) -> Pid_main!"Traversal IP over";
create_agn_async(Pid_main,[IP|IPList],CommunityList,Timeout,Port,Retry,Oid) ->
	spawn_link(fun() -> getAgn_async(Pid_main,IP,CommunityList,Timeout,Port,Retry,Oid) end),
	create_agn_async(Pid_main,IPList,CommunityList,Timeout,Port,Retry,Oid).

getAgn_async(Pid_main,IP,[],Timeout,Port,Retry,Oids) -> 
	[Oid] = Oids,
	Pid_main!{error,{{IP,"",Timeout,Port,Retry},[{Oid,"Error Comnunity"}]}}; 
getAgn_async(Pid_main,IP,[Community|CommunityList],Timeout,Port,Retry,Oids) ->
	[Oid] = Oids,
	snmp_ex2_manager:unagent(IP),
	snmp_ex2_manager:agent(IP,[{community,Community}]),
	Result = snmp_ex2_manager:sync_get_next(IP,Port,Oids),
	io:format("Result:~p~n",[Result]),
	case Result of
		{ok,{_,_,[{_,Oid2,_,Value,_}|_]},_} ->
            case lists:prefix(Oid,Oid2) of
                true ->
                    Pid_main!{ok,{{IP,Community,Timeout,Port,Retry},[{Oid2,Value}]}};  
                _    ->
                    io:format("test for not match ~p~n", [Oid2]),
                    Pid_main!{error,{{IP,Community,Timeout,Port,Retry},[{Oid,"Not match"}]}}
            end;
        {error,timeout} -> 
            getAgn_async(Pid_main,IP,CommunityList,Timeout,Port,Retry,Oids);
        R -> 
            io:format("test for receiveR ~p~n", [R]),
            Pid_main!{error,{{IP,"",Timeout,Port,Retry},[{Oid,R}]}}
    end.
	
recV_async(Result_List,_,{All,Len}) when Len =:= All -> Result_List;
recV_async(Result_List,Timeout,{All,Len}) ->
    receive
        {ok,Result} -> 
			io:format("ResultList:~p~n",[[Result|Result_List]]),
            recV_async([Result|Result_List],Timeout,{All,Len+1});
        {error,Result} ->
            recV_async([Result|Result_List],Timeout,{All,Len+1});
        R ->
			io:format("R:~p~n",[R]),
            recV_async(Result_List,Timeout,{All,Len})
    after Timeout ->
        Result_List
    end.

test() ->
	snmp_ex2_manager:start_link(),
	get_object_next_async([{192,168,0,254},{192,168,0,251}],["public","public1"],5000,161,2,[[1,3,6,1,2,1,1,1]]),
	get_object_next_async([{192,168,0,254},{192,168,0,251}],["public","public1"],5000,161,2,[[1,3,6,1,2,1,1,2]]).