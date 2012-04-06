-module(network).
-compile(export_all).

remote_ping({snmp_remote_ping,Counters,Config})->
    %%io:format("Config:~p~n",[Config]),
    Instance = random:uniform(1000),
    Ver = proplists:get_value(version,Config,"2c"),
    SetComm = proplists:get_value(setCommunity,Config,"private"),
    GetComm = proplists:get_value(getCommunity,Config,"public"),
    FromIp = proplists:get_value(from,Config),
    Protocol = proplists:get_value(protocol,Config,1),
    ToIpHex = ip_to_hex(proplists:get_value(to,Config)),
    PacketTimeOut = proplists:get_value(packetTimeOut,Config,1000),
    PacketCount = proplists:get_value(packetCount,Config,10),
    PacketSize = proplists:get_value(packetSize,Config,100),
    InstansStr = integer_to_list(Instance),
    %io:format("~p~n",[GetComm]),
    snmp_remote_ping_set([Ver,SetComm,FromIp,InstansStr,Protocol,ToIpHex,PacketTimeOut,PacketCount,PacketSize]),
    [{list_to_atom(Counter),g_snmp_get([Ver,GetComm,FromIp,g_snmp_ping_oid(list_to_atom(Counter),InstansStr)])} || Counter <- Counters].

g_telnet_ping(Counters,Config,Template)->
    %io:format("Config:~p~n",[Config]),
    Params = [{Key,Value}||{Key,Value}<-Config,Value=/=[]],
    Value = case api_nnm:telnet_connect(Params) of
		{ok,C} ->	
		    %io:format("Ping:~p~n",["ping " ++ proplists:get_value(to,Config)]),
		    case api_nnm:telnet_send(C,"ping " ++ proplists:get_value(to,Config)) of
			{ok,Data}->
			    api_nnm:telnet_close(C),
			    case re:run(Data,Template,[global,{capture,all_but_first}]) of
				{match,[DataMatches]} when length(Counters) == length(DataMatches) ->
				    Values = extract(Data,Template);
				_ ->
				    []
			    end;
			_ ->[]
		    end;
		_ ->
		    %io:format("Failed"),
		    []			
	    end,
    %io:format("Ping Result:~p~n",[lists:zip(Counters,Value)]),
    lists:zip(Counters,Value).

telnet_ping(Counters,Config,Template) ->
    Value = try g_telnet_ping(Counters,Config,Template)
	    catch
		_:_ -> []
	    end,
    R = get_counter(Counters,Value),
    R.

extract(Data,Template)->
   case re:run(Data,Template,[global,{capture,all_but_first}]) of
       {match,[Matches]} ->
	   [g_match_value(Data,Match) || Match <- Matches];
       _ ->
	   []
   end.

get_counter(Counters,Values)->
    Counter = [{list_to_atom(Counter),proplists:get_value(Counter,Values,[])} || Counter <- Counters],
    %io:format("~p~n",[Counter]),
    Counter.

g_match_value(String,{StartIndex,Length}) ->
    string:substr(String,StartIndex + 1, Length).

get_remote_ping_desc([])->
    [];
get_remote_ping_desc([{Counter,Value}|T] = Data) ->
    string:join([g_desc(_Counter,_Value) || {_Counter,_Value} <- Data ],",").

g_desc(Counter,Value) ->
    R = atom_to_list(Counter) ++ "=" ++ g_value(nnm_monitor_util:to_list(Value)),
    %%io:format("~p~n",[R]),
    R.

g_value([])->
    "unknow";
g_value(V) ->
    V.

ip_to_hex(Ip)->
    %%string:join([to_hex(list_to_integer(Part)) || Part <- string:tokens(Ip,".")]," ").
    [list_to_integer(Part) || Part <- string:tokens(Ip,".")].
to_hex(Num)->
    string:join(io_lib:fwrite("~.16B",[Num]),"").

snmp_remote_ping_set([Ver,SetComm,FromIp,InstansStr,Protocol,ToIpHex,PacketTimeOut,PacketCount,PacketSize])->
    OidStr16 = "1.3.6.1.4.1.9.9.16.1.1.1.16." ++ InstansStr,
    OidStr15 = "1.3.6.1.4.1.9.9.16.1.1.1.15." ++ InstansStr,
    OidStr2 = "1.3.6.1.4.1.9.9.16.1.1.1.2." ++ InstansStr,
    OidStr3 = "1.3.6.1.4.1.9.9.16.1.1.1.3." ++ InstansStr,
    OidStr4 = "1.3.6.1.4.1.9.9.16.1.1.1.4." ++ InstansStr,
    OidStr5 = "1.3.6.1.4.1.9.9.16.1.1.1.5." ++ InstansStr,
    OidStr6 = "1.3.6.1.4.1.9.9.16.1.1.1.6." ++ InstansStr,
    OidStr12 = "1.3.6.1.4.1.9.9.16.1.1.1.12." ++ InstansStr,

    s_do(
      {
[
 {[{ip,FromIp},{setCommunity,SetComm},{snmpVer,Ver}],[{OidStr16,i,6}]}
 ,{[{ip,FromIp},{setCommunity,SetComm},{snmpVer,Ver}],[{OidStr16,i,5}]}
 ,{[{ip,FromIp},{setCommunity,SetComm},{snmpVer,Ver}],[{OidStr15,s,"siteview"}]}
 ,{[{ip,FromIp},{setCommunity,SetComm},{snmpVer,Ver}],[{OidStr2,i,Protocol}]}
 ,{[{ip,FromIp},{setCommunity,SetComm},{snmpVer,Ver}],[{OidStr3,s,ToIpHex}]}
 ,{[{ip,FromIp},{setCommunity,SetComm},{snmpVer,Ver}],[{OidStr6,i,PacketTimeOut}]}
 ,{[{ip,FromIp},{setCommunity,SetComm},{snmpVer,Ver}],[{OidStr4,i,PacketCount}]}
 ,{[{ip,FromIp},{setCommunity,SetComm},{snmpVer,Ver}],[{OidStr5,i,PacketSize}]}
 ,{[{ip,FromIp},{setCommunity,SetComm},{snmpVer,Ver}],[{OidStr16,i,1}]}
],
	  {[{ip,FromIp},{setCommunity,SetComm},{snmpVer,Ver}],[{OidStr16,i,6}]}
}
     ).

s_do({[],_})->
    [];
s_do({[ {Param,Setting} = Command  | T ] , {_ResetParam,_ResetSetting}=Reset })->
    %%io:format("~p~n~p~n",[Param,Setting]),
    case api_nnm:snmp_set(Param,Setting) of
	ok ->
	    s_do({T,Reset});
	_ ->
	    api_nnm:snmp_set(_ResetParam,_ResetSetting)
    end.

g_snmp_get([Ver,GetComm,FromIp,OidStr]=Param)->
    %io:format("~p~n",[Param]),
    case api_nnm:snmp_get([{ip,FromIp},{snmpVer,"v1"},{getCommunity,GetComm}],OidStr) of 
	{ok,{_Oid,Value}}->
	    binary_to_term(Value);
       	_ ->
	    []
    end.

g_snmp_ping_oid(max_rtt,InstanceStr)->
    "1.3.6.1.4.1.9.9.16.1.1.1.13." ++ InstanceStr;

g_snmp_ping_oid(avg_rtt,InstanceStr) ->
    "1.3.6.1.4.1.9.9.16.1.1.1.12." ++ InstanceStr;

g_snmp_ping_oid(min_rtt,InstanceStr) ->
    "1.3.6.1.4.1.9.9.16.1.1.1.11." ++ InstanceStr;

g_snmp_ping_oid(received_packets,InstanceStr) ->
    "1.3.6.1.4.1.9.9.16.1.1.1.10." ++ InstanceStr;

g_snmp_ping_oid(sent_packets,InstanceStr) ->
    "1.3.6.1.4.1.9.9.16.1.1.1.9." ++ InstanceStr.






