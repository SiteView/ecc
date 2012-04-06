%% @author kaiyang.cheng@dragonflow.com
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc COM+ Monitor Connector.
%%
%%Description: For COM+ monitor,using http protocal send command to probe.exe
-module(complusconnector,[Host,Port,Username,Password,Proxy,ProxyUsername,ProxyPassword]).
-compile(export_all).
-export([new/7,delete/0,resetProbeMetricList/1,getMetricValuesFromProbe/2,sendCommandToProbe/8,pingProbeToCheckResetState/1,refreshProbeMetricsList/2,getMetricListFromProbe/4,probeAppearsReStarted/1]).
-export([syncProbeWithSSMetricsMap/3,addMissCounters/4,removeAdditionalCounters/6,addProbeMetric/4,removeProbeMetric/3,parseProbeReturnValuesIntoHashMap/2,getNewMetricValuesFromProbe/3]).

-define(Params,[stateString,hostName,hostPort,authUserName,authPassword,proxyURL,proxyUserName,proxyPassword,timeout,probeReset,dataStore,myMetricsMap]).

-include("monitor.hrl").

-define(HTTP_GET_METRICLIST_CMD, "/ComPlus?op=list").
-define(HTTP_GET_METRICVALUES_CMD, "/ComPlus?op=values").
-define(HTTP_REMOVE_PROBEMETRIC_CMD, "/ComPlus?op=remove").
-define(HTTP_ADD_PROBEMETRIC_CMD, "/ComPlus?op=add").
-define(HTTP_RESET_PROBEMETRICLIST_CMD, "/ComPlus?op=reset").
-define(HTTP_GET_PROBEMETRICLISTSIZE_CMD, "/ComPlus?op=count").

%% @spec new(Host,Port,Username,Password,Proxy,ProxyUsername,ProxyPassword) -> ok
%% where
%% Host = string()
%% Port = string()
%% Username = string()
%% Password = string()
%% Proxy = string()
%% ProxyUsername = string()
%% ProxyPassword = string()
%% @doc create a new ets.
new(Host,Port,Username,Password,Proxy,ProxyUsername,ProxyPassword) ->
    put(stateString,""),
	put(hostName, Host),
	put(hostPort, Port),
	put(authUserName, Username),
	put(authPassword, Password),
	put(proxyURL, Proxy),
	put(proxyUserName, ProxyUsername),
	put(proxyPassword, ProxyPassword),
	put(timeout, 60),
	put(probeReset, false),
	put(dataStore, ""),
	put(myMetricsMap, []),
	{?MODULE,Host,Port,Username,Password,Proxy,ProxyUsername,ProxyPassword}.

%% @spec delete() -> ok
%% @doc delete ets.
delete() ->ok.

cleanup() ->
    [erase(K) || K <- ?Params],
    ok.
	
getstatestring() ->
	get(statestring).
	
setProbeUpdateInterval(I) ->
	put(probeUpdateInterval, I).

getMetricsMap() ->
	get(myMetricsMap).
	
setMetricsMap(Map) ->
	put(myMetricsMap, Map).
	
settimeout(T) ->
	put(timeout, T).
	
getprobeReset() ->
	get(probeReset).
	
setprobeReset(F) ->
	put(probeReset, F).
	
%% @spec resetProbeMetricList(Monitor) -> Flag
%% where
%% Monitor = instance()
%% Flag = bool()
%% @doc reset the probe value.
resetProbeMetricList(Monitor) ->
	Flag1 = true,
	try sendCommandToProbe("/ComPlus?op=reset",[],"","",false,Monitor,null,0) of
		{Flag,_,_} ->
			Flag
	catch
	throw:X->io:format("error in resetProbeMetricList:~p~n",[X]),
	Flag1
	end.
	
%% @spec getMetricValuesFromProbe(StringBuffer,Monitor) -> {Date,StateString}
%% where
%% StringBuffer = string()
%% Monitor = instance()
%% Date = string()
%% StateString = string()
%% @doc get value from probe,return statestring and the date.
getMetricValuesFromProbe(StringBuffer,Monitor) ->
	
    put(dataStore, ""),
    %%callback is:
    {_,Date,StateString} = getNewMetricValuesFromProbe("",StringBuffer,Monitor),
    put(dataStore, Date),
    {Date,StateString}.
    

%% @spec sendCommandToProbe(S,Array,Date,StringBuffer,Flag,Monitor,S1,I) -> {Flag1,Responsebody,StateString}
%% where
%% S = string()
%% Array = list()
%% Date = string()
%% StringBuffer = string()
%% Flag = bool()
%% Monitor = instance()
%% S1 = string()
%% I = integer()
%% Flag1 = bool()
%% Responsebody = string()
%% StateString = string()
%% @doc send command to probe.exe,return value,if error occur return exception.
%% responsebody: return the message body
%% statestring: in the error message generated in the process, passing between functions and as a basis to judge
%% Every time when entering this function has been modified to pass parameters statestring
%% Try catch each time after the call was needed to remove the current statestring
%% Function itself does not return error during the call information
sendCommandToProbe(S,Array,Date,StringBuffer,Flag,Monitor,S1,I) ->
    if
        S=:=?HTTP_GET_METRICVALUES_CMD ->
           %% Because the target machine's configuration, then send a message to the target process can be difficult to wait to get all the values
            platform:sleep(2000);
        true ->
            ok
    end,
    put(stateString,StringBuffer),
	Timeout = get(timeout),
	Host = get(hostName),
	Port = get(hostPort),
    ProxyURL = get(proxyURL),
    ProxyUserName = get(proxyUserName),
    ProxyPassword = get(proxyPassword),
    AuthUserName = get(authUserName),
    AuthPassword = get(authPassword),
    {ok,{_,Profile}} = Monitor:get_attribute(profile),
	Flag1=true,
    SocketSession = socketsession:new(Monitor),
    SocketSession:initialize(Monitor),
	S3 = if
		S1=/=null ->
			S1;
		true ->
			"http://" ++ Host ++ ":" ++ Port ++ S
	end,
    %%The url tag as a lock, the whole com + monitor only do the lock request
    LockIndex = Host ++ ":" ++ Port,
    httputils:is_lock(LockIndex),
	if
		(Flag and ((S=:="/ComPlus?op=remove") orelse (S=:="/ComPlus?op=add"))) ->
			try pingProbeToCheckResetState(Monitor)
			catch
			throw:X->X,
			refreshProbeMetricsList(StringBuffer,Monitor),
			throw(X)
			end;
		true ->
			ok
	end,
	J = if
		I > 0 ->
			I;
		true ->
			Timeout
	end,
    SocketSession:setEncodePostData("forceNoEncode"),
    M = url_monitor:new(),
    M:set_attribute(profile,Profile),
    URLResults = M:checkURL(browsable_urlcontent,SocketSession,S3,null,null,ProxyURL,ProxyUserName,ProxyPassword,Array,AuthUserName,AuthPassword,null,Date,16#fffffffffffffff,null,0,J*1000,null),
    M:delete(),
    %%SocketSession:delete(),
    Status = URLResults#urlresults.status,
    Body = URLResults#urlresults.body,
    Head = URLResults#urlresults.head,
    FR = if
        ((Status=/=200) orelse (Status=:=-996) and (Body=:="")) ->
            Exception = StringBuffer++integer_to_list(Status) ++ "\n Explanation: " ++ httputils:lookupStatus(Status),
            put(stateString,Exception),
            {false,Body,Exception};
        true ->
            Flag2 = probeAppearsReStarted(Head),
            if
                ((S=:="/ComPlus?op=values") and Flag2) ->
                    {_,StateString} = refreshProbeMetricsList(StringBuffer,Monitor),
                    throw(StateString);
                true ->
                    {Flag1,Body,StringBuffer}
            end
    end,
    %%unlock
    httputils:release_lock(LockIndex),
    FR.
    
%% @spec pingProbeToCheckResetState(Monitor) -> Flag
%% where
%% Monitor = instance()
%% Flag = bool()
%% @doc send count operation to probe,checkout if active metrics is 0.
pingProbeToCheckResetState(Monitor) ->
	{Flag,Body,_} = sendCommandToProbe("/ComPlus?op=count",[],"","",false,Monitor,null,0),
	Flag1 = if
		Flag ->
			J = string:str(Body,"active metrics"),
			if
				J=/=0 ->
					S1 = string:sub_string(Body,1,J-1),
					case string:to_integer(string:strip(S1)) of
                        {error,_}->
                            true;
                        {Count,_} when Count>0 ->
                            false;
                        _->
                            true
                    end;
                true ->
                    true
            end;
        true ->
            true
    end,
    if
        Flag1 ->
            throw("Probe has zero active monitors.");
        true ->
            Flag1
    end.

%% @spec refreshProbeMetricsList(StringBuffer,Monitor) -> {Flag,StateString}
%% where
%% StringBuffer = string()
%% Monitor = instance()
%% Flag = bool()
%% StateString = string()
%% @doc Add the counters in map to probe.
refreshProbeMetricsList(StringBuffer,Monitor) ->
	Map = get(myMetricsMap),
	Key = [K||{K,_}<-Map],
	Len = length(Key),
	if
		Len>0 ->
			try addProbeMetric(Key,StringBuffer,false,Monitor) of
			Result ->
				Result
			catch
			throw:X->X,
            StateString = get(stateString),
            {true,StateString}
			end;
		true->
			{true,StringBuffer}
	end.
	
%% @spec getMetricListFromProbe(Date,StateString,Monitor,I) -> {Flag,Date1,StateString1}
%% where
%% Date = string()
%% StringBuffer = string()
%% Monitor = instance()
%% I = integer()
%% Flag = bool()
%% Date1 = bool()
%% StateString1 = string()
%% @doc Find counters from probe.exe.
getMetricListFromProbe(Date,StateString,Monitor,I) ->
	try sendCommandToProbe("/ComPlus?op=list",[],Date,StateString,false,Monitor,null,I) of
		Result->
			Result
	catch
	throw:X->X,
    StateString = get(stateString),
	{true,Date,StateString}
	end.

%% @spec probeAppearsReStarted(Head) -> Flag
%% where
%% Head = list()
%% Flag = bool()
%% @doc Checkout ResponseHead with "content-length",if length isn't 0,return false.
probeAppearsReStarted(Head) ->
    case proplists:get_value("content-length",Head) of
        undefined ->
            true;
        Count ->
            case string:to_integer(Count) of
                {error,_}->
                    true;
                {N,_} when N>0 ->
                    false;
                _->
                    true
            end
    end.
                
	
endTrimCRLF(S) ->
	I1 = string:str(S,"\r\n"),
	I = if
		I1=:= 0 ->
			string:str(S,"\r");
		true ->
			I1
	end,
	if
		I>0 ->
			string:sub_string(S,1,I-1);
		true ->
			S
	end.

%% @spec syncProbeWithSSMetricsMap(Map,StringBuffer,Monitor) -> {Flag, Date, StateString}
%% where
%% Map = list()
%% StringBuffer = string()
%% Flag = bool()
%% Date = list()
%% StateString = string()
%% @doc Sync to probe get the counters value.
syncProbeWithSSMetricsMap(Map,StringBuffer,Monitor) ->
	{Date1,StateString1} = getMetricValuesFromProbe(StringBuffer,Monitor),
	if
		StateString1=/="" ->
			{false,Map,StateString1};
		true ->
			{N,NewMap} = parseProbeReturnValuesIntoHashMap([],Date1),
			if
				N=:=0 ->
					{true,NewMap,StateString1};
				true ->
					MetricsMap = get(myMetricsMap),
					Keyset = [K||{K,_}<-MetricsMap],
                    
                   %% keyset-oriented times of all required monitoring counter, NewMap removed from the probe all the counters and their values ​​by the number of parameters to determine whether the jump, 2 to, 3 jump
                   
                    Tuple = addMissCounters(NewMap,Keyset,StateString1,Monitor),
                    %%
                    case size(Tuple) of
                        3->
                            Tuple;
                        _->
                            {Flag,StateString2} = Tuple,
                            Keyset1 = [K1||{K1,_}<-NewMap],
                            
                          %% keyset1 removed from the probe in the counters of the name, keyset is the time required to monitor all counters name
                          %% To find those who are not in newmap this required monitoring counters, use the remove method removed from the probe
                            Tuple1 = removeAdditionalCounters(Keyset1,Keyset,StateString2,Monitor,Flag,NewMap),
                            %%
                            case size(Tuple1) of
                                3 ->
                                    Tuple1;
                                _->
                                    {Flag1,StateString3} = Tuple1,
                                    {Flag1,[],StateString3}
                            end
                    end
            end
    end.

%% @spec addMissCounters(NewMap,Keyset,StateString,Monitor) -> {Flag, Date, StateString}|{Flag,StateString}
%% where
%% NewMap = list()
%% Keyset = list()
%% StateString = string()
%% Monitor = instance()
%% Flag = bool()
%% Date = list()
%% StateString = string()
%% @doc Check miss counters,if there are miss counters,add them.
addMissCounters(NewMap,Keyset,StateString,Monitor) ->
    Counters = findMissCounters(NewMap,Keyset),
    if
        Counters=/=[] ->
            try addProbeMetric(Counters,StateString,true,Monitor) of
                Result->
                    Result
            catch
            throw:X->X,
            StateString1 = get(stateString),
            {true,NewMap,StateString1}
            end;
        true ->
            {true,StateString}
    end.

%% @spec removeAdditionalCounters(Name,Keyset,StateString,Monitor,Flag,NewMap) -> {Flag, Date, StateString}|{Flag,StateString}
%% where
%% Name = list()
%% NewMap = list()
%% Keyset = list()
%% StateString = string()
%% Monitor = instance()
%% Flag = bool()
%% Date = list()
%% StateString = string()
%% @doc Check additional counters,if there are,remove them.
removeAdditionalCounters(Name,Keyset,StateString,Monitor,Flag,NewMap) ->
    Counters = findAdditionalCounters(Name,Keyset),
    if
        Counters=/=[] ->
            try removeProbeMetric(Counters,StateString,Monitor) of
                {Flag1,_,StateString1} ->
                    {Flag1,StateString1}
            catch
            throw:X->X,
            StateString2 = get(stateString),
            {true,NewMap,StateString2}
            end;
        true ->
            {Flag,StateString}
    end.

findMissCounters(_,[])->[];
findMissCounters(NewMap,[F|R]) ->
    case proplists:get_value(F,NewMap) of
        undefined ->
            [F]++findMissCounters(NewMap,R);
        _->
            findMissCounters(NewMap,R)
    end.
    
findAdditionalCounters([],_)->[];
findAdditionalCounters([F|R],Keyset) ->
    case lists:member(F,Keyset) of
        false ->
            [F]++findAdditionalCounters(R,Keyset);
        _->
            findAdditionalCounters(R,Keyset)
    end.


%% @spec addProbeMetric(Arraylist,StringBuffer,Flag,Monitor) -> {Flag,StateString}
%% where
%% Arraylist = list()
%% StringBuffer = string()
%% Monitor = instance()
%% Flag = bool()
%% StateString = string()
%% @doc Send add operation to probe.
%%arraylist Has not been modified
addProbeMetric(Arraylist,StringBuffer,Flag,Monitor) ->
	Flag1=true,
	if
		Arraylist=:=[] ->
			{Flag1,StringBuffer};
		true ->
			Array = ["Custom-Content: "++X||X<-Arraylist],
			try sendCommandToProbe("/ComPlus?op=add",Array,"",StringBuffer,Flag,Monitor,null,0) of
				{F,_,NewStateString} ->
					{F,NewStateString}
			catch
			throw:X->X,
            StateString = get(stateString),
			{Flag1,StateString}
			end
	end.
	
%% @spec removeProbeMetric(Arraylist,StringBuffer,Monitor) -> {Flag,Body,StateString}
%% where
%% Arraylist = list()
%% StringBuffer = string()
%% Monitor = instance()
%% Flag = bool()
%% StateString = string()
%% Body = string()
%% @doc Send remove operation to probe.
removeProbeMetric(Arraylist,StringBuffer,Monitor) ->
    Array = ["Custom-Content: "++X||X<-Arraylist],
    sendCommandToProbe("/ComPlus?op=remove",Array,"",StringBuffer,true,Monitor,null,0).

%% @spec parseProbeReturnValuesIntoHashMap(Map,Body) -> {N,Map1}
%% where
%% Map = list()
%% Body = string()
%% N = integer()
%% Map1 = list()
%% @doc Parse the value get from probe.
parseProbeReturnValuesIntoHashMap(Map,Body) ->
	Len = string:len(Body),
    if
        Len=:=0 ->
            {0,Map};
        true ->
            As = string:tokens(Body,"\n"),
            for2(As,Map,0)
    end.

for2([],Map,N) ->{N,Map};
for2([F|R],Map,N) ->

    NewF = case F of
        [0|Rest] ->
            Rest;
        _->
            F
    end,
	I = string:str(NewF,"="),
	if
		I>0 ->
			S2 = string:strip(string:sub_string(NewF,1,I-1)),
			S3 = endTrimCRLF(string:sub_string(NewF,I+1,string:len(F))),
			for2(R,Map++[{S2,S3}],N+1);
		true ->
			for2(R,Map,N)
	end.

%% @spec getNewMetricValuesFromProbe(Date,StringBuffer,Monitor) -> {Flag,Date1,StateString}
%% where
%% Date = list()
%% StringBuffer = string()
%% Monitor = instance()
%% Flag = bool()
%% Date1 = list()
%% StateString = string()
%% @doc Parse the value get from probe.
getNewMetricValuesFromProbe(Date,StringBuffer,Monitor) ->
	try sendCommandToProbe("/ComPlus?op=values",[],Date,StringBuffer,true,Monitor,null,0) of
		Result ->
			Result
	catch
	throw:X->X,
	StateString = get(stateString),
	{false,Date,StateString}
	end.
	


	