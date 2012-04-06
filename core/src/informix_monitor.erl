%% Author: Administrator
%% Created: 2010-6-23
%% Description: TODO: Add description to informix_monitor
-module(informix_monitor,[BASE]).
-extends(atomic_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").
-define(MAX_COUNTER,10).
-define(REG_NAME, java_mail_box).
-define(DEBUG_INFO, debug_info).
-define(TIMEOUT,60*1000).
-define(RECEIVE_TIME_OUT, 10*1000).
-define(MaxCounters,15).

new()->
	Base = atomic_monitor:new(),
	Base:set_attribute(countersInError,0),
	{?MODULE,Base}.

getMaxCounter()->
	?MAX_COUNTER.

%% @spec defaultTitle(Params) ->string()
%%  Params = [term()]
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	Host = proplists:get_value(host,Params),
	if
		length(Host)>0->
			BASE:defaultTitle(Params) ++":" ++ Host;
		true ->
			BASE:defaultTitle(Params)
	end.
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
            [{error,"Connect Java Node Error! "}];
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
%%@spec rpc(RegName, Node, Msg) -> Response
%%@type RegName = atom()
%%@type Node = atom()
%%@type Msg = [tuple()]
%%@doc remote process calling for the java node with messages.
rpc1(RegName, Node, Msg, Timeout) ->
	THIS:set_attribute(?DEBUG_INFO, "remote process call java node ..."),
	Ping = net_adm:ping(Node),
    if
        Ping==pang ->
            [{error,"Connect Java Node Error! "}];
        true ->
            {RegName, Node} ! Msg,	
            receive
                {ok, _, Ret1} ->	
					Ret= utftogbk(Ret1),
			         qsort(Ret);
                {error, _From, [Ret]} ->
                    [{error,Ret}]		
            after Timeout ->
                [{error, "time is out. "}]
            end
    end.
%%get the result, parse it now
process_result([],_)->"";
process_result([C|T],Result)->
	%%look for counters information
	case lists:keysearch(element(1,C),1,Result) of
		{value,{K,V}}->
			%%some special thing must to do for float to show correctly
			VV = 
			if
				is_float(V) ->
					erlang:round(V*100)/100;
				true ->
					V
			end,
			%%set the counter atrribute
			THIS:set_attribute(K,VV),
			%%set the show string
			lists:flatten(io_lib:format("~p=~p<br>",[element(2,C),VV])) ++ 
				process_result(T,Result);
		_->
			THIS:set_attribute(element(1,C),"n/a"),
			THIS:inc_attribute(countersInError),
			lists:flatten(io_lib:format("~p=n/a<br>",[element(2,C)])) ++ 
				process_result(T,Result)
	end.
qsort([]) -> [];
qsort([Pivot|T]) ->
	qsort([X || X <- T, element(2,X) < element(2, Pivot)])
	++ [Pivot] ++
	qsort([X || X <- T, element(2,X) >= element(2,Pivot)]).	
%% @spec get_args() -> List
%% @type List = [term()]
%% @doc get_args is the function to get arguments to be sent to java node needed.
get_args1(Params) ->
	 Server = proplists:get_value(dnamicserver, Params),
	 Host = proplists:get_value(host, Params),
	 Port = proplists:get_value(port, Params),
	 Username = proplists:get_value(username, Params),
	 Password = proplists:get_value(password, Params),
	[
	 {dnamicserver, Server},
 	 {host, Host},
	 {port, Port},
	 {username, Username},
	 {password, Password}
	].
%% @spec get_args() -> List
%% @type List = [term()]
%% @doc get_args is the function to get arguments to be sent to java node needed.
get_args() ->
	{ok, {_, Server}} = THIS:get_property(dnamicserver),
	{ok, {_, Host}} = THIS:get_property(host),
	{ok, {_, Port}} = THIS:get_property(port),
	{ok, {_, Username}} = THIS:get_property(username),
	{ok, {_, Password}} = THIS:get_property(password),
	Counters=buildcounters(get_counters()),
	[
	 {dnamicserver, Server},
 	 {host, Host},
	 {port, Port},
	 {username, Username},
	 {password, Password},
	  {counters,Counters}
	].
%% @spec get_counters() -> CounterList
%% @type CounterList = [Counter]
%% @type Counter = [string(), string()]
%% @doc get counters that user has selected.
get_counters() ->
	{ok, {_, Browse}} = THIS:get_property(browse),
	Len = length(Browse),
	if
		(Len > 0) ->
			Browse;
		true ->
			[]
	end.	
buildcounters([])->
	[];
buildcounters([H|E])->
	{K,V}=H,
	[{K,iconv:convert("utf-8","gbk",V)}]++buildcounters(E).
getCounterSize()->
	{ok, {_, Browse}} = THIS:get_property(browse),
	Len = length(Browse).
%% @spec getBrowseData(Params) -> Result
%% @type Param  = [term()]
%% @type Result = [{CounterValue, CounterName}]
%% @doc getBrowseData(Params) is the function called by schedule to get counters of Weblogic6x server
%%	CounterValue is the Counter's OID, which will be used to get value
%%	CounterName is the show name of the counter
getBrowseData(Params)->
	%%get argument	
   Request=get_args1(Params),
   Java_Node = siteview:get_java_node(),
  Response = rpc1(?REG_NAME, Java_Node, {"com.dragonflow.erlangecc.monitor.InformixMonitor", "getBrowseData", Request}, ?TIMEOUT).
update()->	
	  case getCounterSize() > ?MaxCounters of
		   true->
		    THIS:set_attribute(?NO_DATA,true),
		    THIS:set_attribute(?CATEGORY,?NO_DATA),
		    THIS:set_attribute(countersInError,?MaxCounters),
		    THIS:set_attribute(?STATE_STRING,lists:flatten(io_lib:format("the counters > MAX_COUNTER:~p",[?MaxCounters])));
		  _ ->
			  initCounters(),
			  Request = get_args(),
			  Java_Node = siteview:get_java_node(),
%% 	TestData=[
%% 			  {hostName,"192.168.6.35"},{portNumber,"1414"},{channelName,"SSS"},{queueMgr,"QM_siteview_5fs8e9"},
%% 			  {altQueueMgr,""},{returnMqStatCodes,"ibmCodes"},{cipherSuite,""},{username,""},{password,""},
%% 			  {measurements,["Channels/tSSS/tNo. of Channel Messages Transferred"]}
%% 			 ],
			  Response = rpc(?REG_NAME, Java_Node, {"com.dragonflow.erlangecc.monitor.InformixMonitor", "update", Request}, ?TIMEOUT),
			  updateValues(Response)
	  end,
	ok.
initCounters()->
	THIS:set_attribute(countersInError, 0),
	lists:foreach(fun(X)->THIS:set_attribute(element(1,X),"n/a"),THIS:set_attribute(convert1(element(2,X)),"n/a") end, THIS:get_counters()).
updateValues([])->
	ok;
updateValues([H|T])->
	case H of
		{error, Error} ->
			THIS:set_attribute(countersInError, THIS:getCounterSize()),
			THIS:set_attribute(pStatus, "error"),
			THIS:set_attribute(?CATEGORY,?ERROR_CATEGORY),
			THIS:set_attribute(?STATE_STRING, Error),
			stop;
		{"stateString", State_String} ->
			ConvertString=convert1(State_String),
			THIS:set_attribute(?STATE_STRING, util:replace(ConvertString,",","<br>") ),
			updateValues(T);
		{"countersInError", Counter_Errors} ->
			THIS:set_attribute(countersInError, Counter_Errors),
			updateValues(T);
		{"pStatus",Status} ->
			THIS:set_attribute(pStatus, Status),
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
	THIS:set_attribute(convert1(Key),Value).
utftogbk([])->
	[];
utftogbk([H|E])->
	{K,V}=H,
	[{K, convert(V)}]++utftogbk(E).
convert([H|R], Acc)->
	case H > 255 of
		true->
			<<H1:8, H2:8>> = <<H:16>>,
			List = iconv:convert("utf-32", "utf-8", [0, 0, H1, H2]),
			convert(R, lists:reverse(List) ++ Acc);
		_ ->
			convert(R, [H|Acc])
	end;
convert([], Acc)->
	lists:reverse(Acc).
%% 	iconv:convert("utf-8", "gbk", lists:reverse(Acc)).

convert(Msg)->
	convert(Msg, []).
convert1([H|R], Acc)->
	case H > 255 of
		true->
			<<H1:8, H2:8>> = <<H:16>>,
			List = iconv:convert("utf-32", "utf-8", [0, 0, H1, H2]),
			convert1(R, lists:reverse(List) ++ Acc);
		_ ->
			convert1(R, [H|Acc])
	end;
convert1([], Acc)->
lists:reverse(Acc).
%%  io:format("convert1:~p ~n", [L]),
%%  L.
%% %%   io:format("convert1:~p ~n", [L]),
%%  	iconv:convert("utf-8", "gbk", lists:reverse(Acc)).

convert1(Msg)->
%% 	io:format("oldstatus:~p ~n", [Msg]),
	convert1(Msg, []).
verify(Params)->
	Errs =
	case proplists:get_value(browse,Params) of
		undefined->
			[{browse,"must select at least one  counter"}];
		[]->
			[{browse,"must select at least one  counter"}];
		_->
			[]
	end ++
	case proplists:get_value(dnamicserver,Params) of
		undefined->
			[{dnamicserver,"dnamicserver is null"}];
		[]->
			[{dnamicserver,"dnamicserver is null"}];
		_->
			[]
	end ++
		case proplists:get_value(port,Params) of
		undefined->
			[{port,"port is null"}];
		[]->
			[{port,"port is null"}];
		_->
			[]
	end ++
		case proplists:get_value(host,Params) of
		undefined->
			[{host,"host is null"}];
		[]->
			[{host,"host is null"}];
		_->
			[]
	end ++
	case BASE:verify(Params) of
		{error,Be}->
			Be;
		_->
			[]
	end,
	if
		length(Errs)>0->
			{error,Errs};
		true ->
			{ok,""}
	end.
get_classifier(error)->
	
	Cls = case THIS:get_property(error_classifier) of
				{ok,{error_classifier,Classifier}}->
					Classifier;
				_->
					[{pStatus,'!=',"ok"}]
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
			[{pStatus,'!=',"ok"}]
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
			[{pStatus,'==',"ok"}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end.
getCostInLicensePoints()->
	{ok,{_,Counters}} = THIS:get_property(browse),
	length(Counters).

getLogProperties(This)->
	{ok,{_,Counters}} = THIS:get_property(browse),
	Temp = This:get_template_property(),
	[X#property.name || X<-Temp,X#property.state=:=true] ++ [convert1(X)||{_,X}<- Counters].

getStatePropertyObjects()->
	[#property{name=pStatus,title="status",type=text,order=100,configurable=false,state=true},
	 #property{name=countersInError,title="counters in error",type=numeric,editable=true,configurable=false,state=true,order=100}  
	].

get_template_property()->
	BASE:get_template_property() ++ 
	[ #property{name=dnamicserver,title="Dynamic Server Name",type=text,configurable=true,editable=true,state=false,description="the server Instances of informix database", order=1},
	  #property{name=host,title="Host Name",type=text,configurable=true,editable=true,state=false,description="the name of the server",order=2},
	  #property{name=port,title="Port",default="1526",type=text,configurable=true,editable=true,state=false,description="the port of the server",order=3},
	  #property{name=username,title="User Name",type=text,configurable=true,editable=true,state=false,description="the user name of server",order=4},
	  #property{name=password,title="PassWord",type=password,configurable=true,editable=true,state=false,description="the password of server",order=5},
	  #property{name=browse,title="Counters", description="Current selection of counters.",type=browsable,editable=true,order=100},
	  #property{name=countersInError,title="counters in error",type=numeric,editable=true,configurable=false,state=true,order=100},
	  #property{name=pStatus,title="status",type=text,order=100,configurable=false,state=true}
	].
