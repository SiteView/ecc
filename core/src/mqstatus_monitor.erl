%% Author: Administrator
%% Created: 2010-4-27
%% Description: TODO: Add description to mqstatus_monitor
-module(mqstatus_monitor,[BASE]).
-extends(perfmon_monitorbase).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").
-define(REG_NAME, java_mail_box).
-define(DEBUG_INFO, debug_info).
-define(RECEIVE_TIME_OUT, 10*1000).

-define(LATESTEVENTTIME,latestEventTime).
-define(LASTMSGDATE,lastMsgDate).
-define(LASTMSGTIME, lastMsgTime).
-define(TIMEOUT,60*1000).
-define(MaxCounters,15).
%% @spec new() -> Obj
%% @type Obj = term()
%% @doc create a new instance for db2 database monitor
new() ->
	Base = perfmon_monitorbase:new(),
	Base:set_attribute(?LATESTEVENTTIME , 0),
	Base:set_attribute(?LASTMSGDATE , []),
	Base:set_attribute(?LASTMSGTIME , []),
	{?MODULE,Base}.
%% @spec defaultTitle(Params) ->string()
%%  Params = [term()]
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	Host = proplists:get_value(hostname,Params),
	if
		length(Host)>0->
			BASE:defaultTitle(Params) ++":" ++ Host;
		true ->
			BASE:defaultTitle(Params)
	end.
getAvailableObjects()->
	["Channels","Queues"].

getScalarValues(Prop,Params)->
	case Prop of
		useMqStatCodes ->
			[{"ibmCodes","ibmCodes"},{"siteviewCodes","siteviewCodes"}];
		_ ->
			BASE:getScalarValues(Prop, Params)
	end.
getAvailableInstances(Object) ->
 	Request=get_args1(),
	case Object of
		"Channels" ->
%% 			TestData=[
%% 			          {hostName,"192.168.6.35"},{portNumber,"1414"},{channelName,"SSS"},{queueMgr,"QM_siteview_5fs8e9"},
%% 			          {cipherSuite,""},{username,""},{password,""},{obj,"Channels"}
%% 			        ],
			Java_Node = siteview:get_java_node(),
			Response = rpc(?REG_NAME, Java_Node, {"com.dragonflow.erlangecc.monitor.MQStatusMonitor", "getInstances", Request++[{obj,"Channels"}]}, ?TIMEOUT);
		"Queues" ->
%% 			TestData=[
%% 			          {hostName,"192.168.6.35"},{portNumber,"1414"},{channelName,"SSS"},{queueMgr,"QM_siteview_5fs8e9"},
%% 			          {cipherSuite,""},{username,""},{password,""},{obj,"Queues"}
%% 			        ],
			Java_Node = siteview:get_java_node(),
			Response = rpc(?REG_NAME, Java_Node, {"com.dragonflow.erlangecc.monitor.MQStatusMonitor", "getInstances", Request++[{obj,"Queues"}]}, ?TIMEOUT);
		_ ->
			[]
     end.
outgetAvailableInstances(Params) ->
 	 HostName = proplists:get_value(serverHostName, Params),
	 PortNumber = proplists:get_value(serverPortNumber, Params),
	 ChannelName = proplists:get_value(channel, Params),
	 QueueMgr = proplists:get_value(queueMgr, Params),
	 CipherSuite = proplists:get_value(cipherSuite, Params),
	 Username = proplists:get_value(username, Params),
	 Password = proplists:get_value(password, Params),
	 Object= proplists:get_value(obj, Params),
	Request= [
	 {hostName, HostName},
 	 {portNumber, PortNumber},
	 {channelName, ChannelName},
	 {queueMgr, QueueMgr},
	 {cipherSuite, CipherSuite},
	 {username, Username},
	 {password, Password},
	 {obj,Object}
	],
	 
	Java_Node = siteview:get_java_node(),
	Response = rpc(?REG_NAME, Java_Node, {"com.dragonflow.erlangecc.monitor.MQStatusMonitor", "getInstances", Request}, ?TIMEOUT)
.
getAvailableCounters(Object) ->
	case Object of
		"Channels" ->
			Ret=["Channel Bytes Received", "Channel Bytes Sent", "Channel Status", "Channel Time Between Sends", "No. of Channel Buffers Sent", "No. of Channel Buffers Received", "No. of Channel Messages Transferred", "Event: Channel Activated", "Event: Channel Not Activated", "Event: Channel Started", "Event: Channel Stopped", "Event: Channel Stopped by User"];
		"Queues" ->
			Ret=["Current Queue Depth", "Queue Open Input Count", "Queue Open Output Count", "Event: Queue Depth High", "Event: Queue Depth Low", "Event: Queue Full", "Event: Queue Service Interval High", "Event: Queue Service Interval Ok"];
		_ ->
			Ret=[]
     end,
	  Ret.


%% @spec get_args() -> List
%% @type List = [term()]
%% @doc get_args is the function to get arguments to be sent to java node needed.
get_args() ->
	THIS:set_attribute(?DEBUG_INFO, "get args..."),
	{ok, {_, HostName}} = THIS:get_property(serverHostName),
	{ok, {_, PortNumber}} = THIS:get_property(serverPortNumber),
	{ok, {_, ChannelName}} = THIS:get_property(channel),
	{ok, {_, QueueMgr}} = THIS:get_property(queueMgr),
	{ok, {_, AltQueueMgr}} = THIS:get_property(altqueueMgr),
	{ok, {_, ReturnMqStatCodes}} = THIS:get_property(useMqStatCodes),
	{ok, {_, CipherSuite}} = THIS:get_property(cipherSuite),
	{ok, {_, Username}} = THIS:get_property(username),
	{ok, {_, Password}} = THIS:get_property(password),
	{ok, {_, LatestEventTime}} = THIS:get_attribute(?LATESTEVENTTIME),
	{ok, {_, LastMsgDate}} = THIS:get_attribute(?LASTMSGDATE),
	{ok, {_, LastMsgTime}} = THIS:get_attribute(?LASTMSGTIME),
	Measurements = getPerfmonMeasurements(),
	[
	 {hostName, HostName},
 	 {portNumber, PortNumber},
	 {channelName, ChannelName},
	 {queueMgr, QueueMgr},
	 {altQueueMgr, AltQueueMgr},
 	 {returnMqStatCodes, ReturnMqStatCodes},
	 {cipherSuite, CipherSuite},
	 {username, Username},
	 {password, Password},
	 {latestEventTime, LatestEventTime},
	 {lastMsgDate, LastMsgDate},
	 {lastMsgTime, LastMsgTime},
	 {measurements, Measurements}
	].

%% @spec get_args() -> List
%% @type List = [term()]
%% @doc get_args is the function to get arguments to be sent to java node needed.
get_args1() ->
	{ok, {_, HostName}} = THIS:get_property(serverHostName),
	{ok, {_, PortNumber}} = THIS:get_property(serverPortNumber),
	{ok, {_, ChannelName}} = THIS:get_property(channel),
	{ok, {_, QueueMgr}} = THIS:get_property(queueMgr),
	{ok, {_, CipherSuite}} = THIS:get_property(cipherSuite),
	{ok, {_, Username}} = THIS:get_property(username),
	{ok, {_, Password}} = THIS:get_property(password),
	[
	 {hostName, HostName},
 	 {portNumber, PortNumber},
	 {channelName, ChannelName},
	 {queueMgr, QueueMgr},
	 {cipherSuite, CipherSuite},
	 {username, Username},
	 {password, Password}
	].
%% @spec get_counters() -> List
%% @type List = [term()]
%% @doc get_counters is the function that return counters selected by user.
getPerfmonMeasurements() ->
	THIS:set_attribute(?DEBUG_INFO, "get counters..."),
	{ok, {_, Perfmon}} = THIS:get_property(perfmon),
	Len = length(Perfmon),
	if
		(Len > 0) ->
			Perfmon;
		true ->
			[]
	end.
%% @spec getCounterSize() -> int
%% @type List = [term()]
%% @doc getCounterSize is the function that return counters size selected by user.
getCounterSize() ->
	{ok, {_, Perfmon}} = THIS:get_property(perfmon),
	Len = length(Perfmon).
%%@spec rpc(RegName, Node, Msg) -> Response
%%@type RegName = atom()
%%@type Node = atom()
%%@type Msg = [tuple()]
%%@doc remote process calling for the java node with messages.
rpc(RegName, Node, Msg, Timeout) ->
	THIS:set_attribute(?DEBUG_INFO, "remote process call java node ..."),
	Ping = net_adm:ping(Node),
    if
        Ping==pang ->
            [{"error","Connect Java Node Error! "}];
        true ->
            {RegName, Node} ! Msg,	
            receive
                {ok, _, Ret} ->	
                    Ret;
                {error, _From, [Ret]} ->
                    [{error,Ret}]		
            after Timeout ->
                [{error, "time is out. "}]
            end
    end.	
update()->	
	  case getCounterSize() > ?MaxCounters of
		   true->
		    THIS:set_attribute(?NO_DATA,true),
		    THIS:set_attribute(?CATEGORY,?NO_DATA),
		    THIS:set_attribute(countersInError,?MaxCounters),
		    THIS:set_attribute(?STATE_STRING,lists:flatten(io_lib:format("the counters > MAX_COUNTER:~p",[?MaxCounters])));
		  _ ->
			  Request = get_args(),
			  Java_Node = siteview:get_java_node(),
%% 	TestData=[
%% 			  {hostName,"192.168.6.35"},{portNumber,"1414"},{channelName,"SSS"},{queueMgr,"QM_siteview_5fs8e9"},
%% 			  {altQueueMgr,""},{returnMqStatCodes,"ibmCodes"},{cipherSuite,""},{username,""},{password,""},
%% 			  {measurements,["Channels/tSSS/tNo. of Channel Messages Transferred"]}
%% 			 ],
			  Response = rpc(?REG_NAME, Java_Node, {"com.dragonflow.erlangecc.monitor.MQStatusMonitor", "update", Request}, ?TIMEOUT),
			  updateValues(Response)
	  end,
	ok.
updateValues([])->
	ok;
updateValues([H|T])->
	case H of
		{"error", Error} ->
			THIS:set_attribute(countersInError, THIS:getCounterSize()),
			THIS:set_attribute(?STATE_STRING, Error),
			stop;
		{"stateString", State_String} ->
			THIS:set_attribute(?STATE_STRING,  util:replace(State_String,",","<br>")),
			updateValues(T);
		{"countersInError", Counter_Errors} ->
			THIS:set_attribute(countersInError, Counter_Errors),
			updateValues(T);
%% 		{counters_value, Counters_Value} ->
%%   	set_counter_value(Counters_Value),
%% 			updateValues(T);
		{_,_}->
			THIS:setCounterValue(H),
			updateValues(T);
		_->
			updateValues(T)
	end.
setCounterValue({Key,Value})->
	THIS:set_attribute(Key,Value).
  

get_classifier(error)->
	
	Cls = case THIS:get_property(error_classifier) of
				{ok,{error_classifier,Classifier}}->
					Classifier;
				_->
					[{countersInError,'>',0}]
			end,
	if 
		length(Cls) < 10 ->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end;
get_classifier(warning)->
	Cls =case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'>',0}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end;
get_classifier(good)->
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'==',0}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end.


get_template_property() ->
    BASE:get_template_property() ++ 
	[
	  #property{name=serverHostName,title="MQ Server Name",type=text,configurable=true,editable=true,state=false,description="the name of the server",order=1},
	  #property{name=serverPortNumber,title="MQ Server Port",default="1414",type=text,configurable=true,editable=true,state=false,description="Port number of the MQ Server (install default is 1414)",order=2},
	  #property{name=channel,title="Server Connection Channel",type=text,configurable=true,editable=true,state=false,description="Channel for connecting to the MQ Server",order=3},
	  #property{name=queueMgr,title="Queue Manager",type=text,configurable=true,editable=true,state=false,description="Target queue manager in the MQ Server",order=4},
	  #property{name=altqueueMgr,title="Alternate Queue Manager",type=text,configurable=true,editable=true,state=false,description="(Optional) QMgr forwarding events to the Primary QMgr",order=7},
      #property{name=useMqStatCodes,title="Channel Status Code Scheme",type=scalar,configurable=true,editable=true,state=false,description="Select reporting scheme for MQ channel status codes",order=9},
	  #property{name=username,title="User Name",type=text,configurable=true,editable=true,state=false,description="the username for login to MQ server",order=5},
	  #property{name=password,title="Password",type=password,configurable=true,editable=true,state=false,description="the password for login to MQ server",order=6},
	  #property{name=cipherSuite,title="cipherSuite",type=text,configurable=true,editable=true,state=false,description="the password for cipherSuite to MQ server",order=8},
 	  #property{name=perfmon,title="counter",type=panel2,configurable=true,editable=true,state=false,order=10,description="Current selection of counters."}
	 ].
