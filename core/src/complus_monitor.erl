%% @author kaiyang.cheng@dragonflow.com
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc COM+ Monitor.
%% 
%% Description: COM+ Server monitor to monitor the performance of COM+ software components registered
%% %% Versions supported: COM+ applications
%% Platform: Windows 2000 and later
%% Requirement:
%% a: This monitor requires a COM+ probe that must be installed on each production system to be monitored.
%%
-module(complus_monitor,[BASE]).
-extends(browsable_urlcontent_base).
-compile(export_all).
-include("monitor_template.hrl").
-include("monitor.hrl").

%% @spec new() -> ok
%% @doc Start the monitor instance.
new()->
	Obj = browsable_urlcontent_base:new(),
	Obj:set_attribute(myComPlusConn,null),
	Obj:set_attribute(gettingData,false),
	Obj:set_attribute(dataStore,[]),
	Obj:set_attribute(siteViewMetricsMap,[]),
    Obj:set_attribute(countersName,[]),
    Obj:set_attribute(lastValue,[]),
	{?MODULE,Obj}.

%% @spec verify(Params) -> ErrorList
%% where
%% Params = [{atom(),integer()|string()}]
%% ErrorList = [string()]
%% @doc page check.
%%
%% Description:Check timeout property.
verify(Params)->
	T = proplists:get_value(timeout,Params),
	Errs = case is_number(T) of
		true ->
			if
				T=<0 ->
					[{timeout,"Timeout value must be greater than zero."}];
				true ->
					[]
			end;
		_->
			[{timeout,"Timeout value must be numeric."}]
	end ++
	case BASE:verify(Params) of
		{error,E}->
			E;
		_->
			[]
	end,
	if
		length(Errs)>0 ->
			{error,Errs};
		true ->
			{ok,""}
	end.

%% @spec update() -> ok
%% @doc Run the monitor.
update() ->
    {ok,{_,Class}} = THIS:get_property(?CLASS),
	{ok,{_,Id}} = THIS:get_property(?ID),
    Profile = list_to_atom(atom_to_list(Class) ++ "-" ++ atom_to_list(Id)),
    THIS:set_attribute(profile,Profile),
    THIS:set_attribute(countersInError,0),
	%%MyComPlusConn = case THIS:get_attribute(myComPlusConn) of
	%%	{ok,{_,null}} ->
	%%		getMyComPlusConnection();
	%%	{ok,{_,V}}->
	%%		V
	%%end,
    MyComPlusConn = getMyComPlusConnection(),
	THIS:set_attribute(myComPlusConn,MyComPlusConn),
    {ok,{_,Browse}} = THIS:get_property(browse),
    CountersName = httputils:changeCountersName(Browse),
    THIS:set_attribute(countersName,CountersName),
  %% Here completed this counters the map with MyComPlusConn synchronization, then the map is only the second to be monitored counters, and these counters have been marked this monitor id
     %% In the course of future changes should be made MyComPlusConn be common to all com ets, and it implements the function lock
	Flag = synchronizeWithSSMetricsMap(),
    %% Here to complete the process from the probe values
	if
		Flag ->
			{Flag1,HashMap,StringBuffer} = MyComPlusConn:syncProbeWithSSMetricsMap("","",THIS);
		true ->
			HashMap = [],
			Flag1 = Flag,
			StringBuffer=""
	end,
    %%
	if
		Flag1 ->
			if
				HashMap=/=[] ->
                    StateString = StringBuffer,
					AS = parseForCounterValues1(HashMap,[]);
				true ->
					{Date,StateString} = MyComPlusConn:getMetricValuesFromProbe(StringBuffer,THIS),
					AS = parseForCounterValues2(Date,[])
			end;
		true ->
			StateString = StringBuffer,
            AS = []
	end,
    NewAS = case lists:member("",AS) of
        true ->
            {ok,{_,LastValue}} = THIS:get_attribute(lastValue),
            if
                LastValue=/=[] ->
                    LastValue;
                true ->
                    AS
            end;
        _ ->
            AS
    end,
    FinalState = if
        StateString=:="" ->
            setCountersValue(NewAS,CountersName,"");
        true ->
            setCountersError(CountersName),
            StateString
    end,
    THIS:set_attribute(lastValue,NewAS),
    THIS:set_attribute(?STATE_STRING,FinalState).
    
    
setCountersValue([],_,State)->State;
setCountersValue([Value|R],[{ID,Name}|R1],State)->
    case Value of
        [] ->
            THIS:inc_attribute(countersInError),
            THIS:set_attribute(ID,"n/a"),
            Error = "not found",
            setCountersValue(R,R1,State++Name++" = "++Error++"<br>");
        _ ->
            {Int,_} = string:to_integer(Value),
			{Float_value,_} = string:to_float(Value),
            THIS:set_attribute(ID,Int),
			Format_value = io_lib:format("~.2f",[Float_value]),
            setCountersValue(R,R1,State++Name++" = "++Format_value++"<br>")
    end.
    
setCountersError([])->ok;
setCountersError([F|R])->
    case F of
        {ID,_}->
            THIS:inc_attribute(countersInError),
            THIS:set_attribute(ID,"n/a");
        _->
            ok
    end,
    setCountersError(R).
    
parseForCounterValues1(HashMap,AS)->
    {ok,{_,CountersName}} = THIS:get_attribute(countersName),
    searchdate(CountersName,HashMap,AS).
        
searchdate([],_,AS)->AS;
searchdate([{_,V}|R],HashMap,AS)->
	case proplists:get_value(V,HashMap) of
        undefined ->
            searchdate(R,HashMap,AS++[""]);
        Date ->
            searchdate(R,HashMap,AS++[Date])
    end;
searchdate([_|R],HashMap,AS)->
    searchdate(R,HashMap,AS).
    
parseForCounterValues2(Date,AS)->
    {ok,{_,MyComPlusConn}} = THIS:get_attribute(myComPlusConn),
    {_,NewMap} = MyComPlusConn:parseProbeReturnValuesIntoHashMap([],Date),
    {ok,{_,CountersName}} = THIS:get_attribute(countersName),
    searchdate(CountersName,NewMap,AS).
    
%% @spec synchronizeWithSSMetricsMap() -> bool
%% @doc synchronize the monitor with metrics map.
synchronizeWithSSMetricsMap() ->
	Flag = true,
    {ok,{_,ID}} = THIS:get_property(?ID),
	{ok,{_,CountersName}} = THIS:get_attribute(countersName),
    
    %%Will be marked on the monitor countername this monitor ID,​​hashset is the monitoring of all selected countersname
	Hashset = for1(CountersName,ID),
    
   %% From the map will have this ID countername all out, Iterator is all the monitors are marked off countersname,
     %% If the monitor is modified when editing a counter may be removed countername more than hashset
	Iterator = getSSMetricsListForMonitor(ID),
    
    %% Because of these reasons may make the Iterator contains more than counters hashset, but not this much counter to monitor the counters, so remove them
	for2(Iterator,Hashset,ID),
    
	Flag.

for1([],_) ->[];
for1([{_,Name}|R],ID) ->
	recordSSMetricUse(Name,ID),
	[Name]++for1(R,ID);
for1([_|R],ID)->
    for1(R,ID).
	
for2([],_,_)->ok;
for2([F|R],Hashset,ID) ->
	Flag = lists:any(fun(X) ->X=:=F end,Hashset),
	if
		(not Flag) ->
			removeSSMetricUse(F,ID);
		true ->
			ok
	end,
    for2(R,Hashset,ID).

%% @spec removeSSMetricUse(string,monitorinstance) -> bool
%% @doc To remove this from the map monitor the use of information.
removeSSMetricUse(S,ID) ->
	Flag = true,
	MyComPlusConn = case THIS:get_attribute(myComPlusConn) of
		{ok,{_,null}} ->
			getMyComPlusConnection();
		{ok,{_,V}} ->
			V
	end,
	Map = MyComPlusConn:getMetricsMap(),
    Iterator = if
		S=/="" ->
			[S];
		true ->
			[K||{K,_}<-Map]
	end,
	Array = for3(Iterator,Map,MyComPlusConn,ID,[]),
    for4(Array,MyComPlusConn),
	Flag.
    
for3([],_,_,_,Array)->Array;
for3([F|R],Map,MyComPlusConn,ID,Array) ->
	case proplists:get_value(F,Map) of
        undefined ->
            NewArray = Array;
		Hashset1 ->
            NewHash = lists:delete(ID,Hashset1),
			MyComPlusConn:setMetricsMap(lists:keyreplace(F,1,Map,{F,NewHash})),
            NewArray = if
                NewHash =:= [] ->
                    Array++[F];
                true ->
                    Array
            end
    end,
	for3(R,Map,MyComPlusConn,ID,NewArray).

for4([],_)->ok;
for4([F|R],MyComPlusConn)->
    Map = MyComPlusConn:getMetricsMap(),
    MyComPlusConn:setMetricsMap(lists:keydelete(F,1,Map)),
    for4(R,MyComPlusConn).
	
%%Add to a countername all been operated on his monitor set the id
recordSSMetricUse(S,ID) ->
	Flag = true,
	MyComPlusConn = case THIS:get_attribute(myComPlusConn) of
		{ok,{_,null}} ->
			getMyComPlusConnection();
		{ok,{_,V}} ->
			V
	end,
	Map = MyComPlusConn:getMetricsMap(),
	case proplists:get_value(S,Map) of
		undefined ->
			MyComPlusConn:setMetricsMap(Map++[{S,[ID]}]);
        V1 ->
            MyComPlusConn:setMetricsMap(Map++[{S,V1++[ID]}])
	end,
	Flag.
	
%% @spec getSSMetricsListForMonitor(monitorinstance) ->list
%% @doc To have this user information countername removed from the map.
getSSMetricsListForMonitor(ID) ->
	{ok,{_,MyComPlusConn}} = THIS:get_attribute(myComPlusConn),
	Map = MyComPlusConn:getMetricsMap(),
	[K||{K,V}<-Map,lists:any(fun(X)->X=:=ID end,V)].
	
%% @spec getMyComPlusConnection() ->ets
%% @doc create a ets contain information from page,if ets has already exist,use it.
getMyComPlusConnection() ->
	{ok,{_,Host}} = THIS:get_property(host),
	{ok,{_,Port}} = THIS:get_property(port),
	{ok,{_,Username}} = THIS:get_property(username),
	{ok,{_,Password}} = THIS:get_property(password),
	{ok,{_,Proxy}} = THIS:get_property(proxy),
	{ok,{_,ProxyUsername}} = THIS:get_property(proxyUserName),
	{ok,{_,ProxyPassword}} = THIS:get_property(proxyPassword),
	Timeout = case THIS:get_property(timeout) of
		{ok,{_,T}} when T>0 ->
			T;
		_->
			60
	end,
	getComPlusConnection(Host,Port,Username,Password,Proxy,ProxyUsername,ProxyPassword,THIS,Timeout).
	
getComPlusConnection(Host,Port,Username,Password,Proxy,ProxyUsername,ProxyPassword,Monitor,Timeout) ->
	%%URL = Host++":"++Port,
	%%{ok,{_,SiteViewMetricsMap}} = THIS:get_attribute(siteViewMetricsMap),
    %%ComPlusConnector = case proplists:get_value(URL,SiteViewMetricsMap) of
     %%   undefined ->
     %%       Connector = complusconnector:new(Host,Port,Username,Password,Proxy,ProxyUsername,ProxyPassword),
     %%       THIS:set_attribute(siteViewMetricsMap,SiteViewMetricsMap++[{URL, Connector}]),
     %%       Connector;
     %%   V->
     %%       V
    %%end,
    ComPlusConnector = complusconnector:new(Host,Port,Username,Password,Proxy,ProxyUsername,ProxyPassword),
	ComPlusConnector:settimeout(Timeout),
	ProbeReset = ComPlusConnector:getprobeReset(),
	Flag = ComPlusConnector:resetProbeMetricList(Monitor),
	if
		((not ProbeReset) and Flag) ->
			ComPlusConnector:setprobeReset(true);
		true ->
			ok
	end,
	ComPlusConnector.
	
%% @spec createXMLFromTemplate(string) -> string
%% @doc get content from string.
createXMLFromTemplate(S) ->
	I = string:str(S,"<browse_data>"),
	XML = if
		I>0 ->
			string:sub_string(S,I-1,string:len(S));
		true ->
			S
	end,
    THIS:getCounterByXml(XML).
	
%% @spec getBrowseData(list) -> string
%% @doc api function for web page get counters.
getBrowseData(Params) ->
    Timeout = proplists:get_value(timeout,Params),
    Host = proplists:get_value(host,Params),
    if
        length(Host)>0 ->
            Port = proplists:get_value(port,Params),
            Username = proplists:get_value(username,Params),
            Password = proplists:get_value(password,Params),
            Proxy = proplists:get_value(proxy,Params),
            ProxyUsername = proplists:get_value(proxyUserName,Params),
            ProxyPassword = proplists:get_value(proxyPassword,Params),
            MyComPlusConn = complusconnector:new(Host,Port,Username,Password,Proxy,ProxyUsername,ProxyPassword),
            MyComPlusConn:settimeout(Timeout),
            Profile = httputils:createProfile(complus_monitor),
            M=complus_monitor:new(),
            M:set_attribute(profile,Profile),
            {Flag,Date,StateString} = MyComPlusConn:getMetricListFromProbe("","",M,120),
            MyComPlusConn:delete(),
            M:delete(),
            I = string:str(Date,getStatString()),
            if
                (not Flag) ->
                    {error,StateString};
                I==0 ->
                    {error,"File does not contain system statistics."};
                true ->
                    createXMLFromTemplate(Date)
            end;
        true ->
            {error,"host is empty"}
    end.

%% @spec getStatString() -> string
%% @doc set the start string.
getStatString() ->
	"<browse_data>".
	
%% @spec extractProbeActiveMonitorCountFromResponse(string) -> integer
%% @doc get active monitor counter number from string.
extractProbeActiveMonitorCountFromResponse(S)->
	I = 10,
	J = string:str(S,"ActiveMonitors:"),
	if
		J>0 ->
			K = string:str(S,"\r\n"),
			if
				K=:=0 ->
					0;
				true ->
					S1 = string:sub_string(S,J+15,K-1),
					list_to_integer(S1)
			end;
		true ->
			I
	end.
	
getHostname()->
	case THIS:get_property(host) of
		{ok,{_,V}}->
			V;
		_->
			BASE:getHostname()
	end.
	
%% @spec get_template_property() -> list
%% @doc Put the property to template.
get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=host,title="COM+ Probe Host Name",description="Host name of the COM+ server-side probe (example: www.mysap.com)",type=text,editable=true,order=2},
	#property{name=port,title="COM+ Probe Port Number",description="Port number of the COM+ server-side probe (example: 8008)",type=text,editable=true,order=3,default="8008"},
	#property{name=username,title="Authorization User Name",description="optional user name if the COM+ server requires authorization",type=text,editable=true,order=4},
	#property{name=password,title="Authorization Password",description="optional password if the COM+ server requires authorization",type=password,editable=true,order=5}
	].
