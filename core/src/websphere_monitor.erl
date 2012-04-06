%% Author: Administrator
%% Created: 2010-6-1
%% Description: TODO: Add description to websphere_monitor
-module(websphere_monitor,[BASE]).
-extends(browsable_base).
-compile(export_all).
-include("monitor_template.hrl").
-include("monitor.hrl").
-define(MAX_COUNTER,10).
-define(TIMEOUT,120*1000).
-define(REG_NAME, java_mail_box).
%% @spec new() -> Obj
%% @type Obj = term()
%% @doc create a new instance for weblogic6x monitor
new()->
	Base = browsable_base:new(),
	{?MODULE,Base}.

%% @spec defaultTitle(Params) -> List
%% @type Params = [term()]
%% @type List = string()
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	BASE:defaultTitle(Params) ++ ":" ++ proplists:get_value(server,Params).

getScalarValues(Prop,Params)->
%% 	{"4.x","4.x"},{"5.x","5.x"},
	case Prop of
		version ->
			[{"6.x","6.x"},{"6.1x","6.1x"},{"7.0x","7.0x"}];
		_ ->
			BASE:getScalarValues(Prop, Params)
	end.
%% @spec get_args() -> List
%% @type List = [term()]
%% @doc get_args is the function to get arguments to be sent to java node needed.
get_args() ->
	{ok, {_, HostName}} = THIS:get_property(server),
	{ok, {_, PortNumber}} = THIS:get_property(port),
	{ok, {_, Username}} = THIS:get_property(username),
	{ok, {_, Password}} = THIS:get_property(password),
	{ok, {_, Version}} = THIS:get_property(version),
	{ok, {_, WebSphereDir}} = THIS:get_property(webSphereDir),
	{ok, {_, ClientProps}} = THIS:get_property(clientProps),
	{ok, {_, Timeout}} = THIS:get_property(timeout),
	{ok, {_, TrustStore}} = THIS:get_property(trustStore),
	{ok, {_, TrustStorePassword}} = THIS:get_property(trustStorePassword),
	{ok, {_, KeyStore}} = THIS:get_property(keyStore),
	{ok, {_, KeyStorePassword}} = THIS:get_property(keyStorePassword),
	Counters=buildcounters(get_counters()),
	[
	 {server, HostName},
 	 {port, PortNumber},
	 {username, Username},
	 {password, Password},
	 {version, Version},
	 {webSphereDir, WebSphereDir},
	 {clientProps, ClientProps},
	 {timeout, Timeout},
	 {trustStore, TrustStore},
	 {trustStorePassword, TrustStorePassword},
	 {keyStore, KeyStore},
	 {keyStorePassword, KeyStorePassword},
	 {counters,Counters}
	
	].

buildcounters([])->
	[];
buildcounters([H|E])->
	{K,V}=H,
	[{K,iconv:convert("utf-8","gbk",V)}]++buildcounters(E).
getCounterSize()->
	{ok, {_, Browse}} = THIS:get_property(browse),
	Len = length(Browse).
update()->	
	  case getCounterSize() > ?MAX_COUNTER of
		   true->
		    THIS:set_attribute(?NO_DATA,true),
		    THIS:set_attribute(?CATEGORY,?NO_DATA),
		    THIS:set_attribute(countersInError,?MAX_COUNTER),
		    THIS:set_attribute(?STATE_STRING,lists:flatten(io_lib:format("the counters > MAX_COUNTER:~p",[?MAX_COUNTER])));
		  _ ->
			  Request = get_args(),
			  Java_Node = siteview:get_java_node(),
%% 	TestData=[
%% 			  {hostName,"192.168.6.35"},{portNumber,"1414"},{channelName,"SSS"},{queueMgr,"QM_siteview_5fs8e9"},
%% 			  {altQueueMgr,""},{returnMqStatCodes,"ibmCodes"},{cipherSuite,""},{username,""},{password,""},
%% 			  {measurements,["Channels/tSSS/tNo. of Channel Messages Transferred"]}
%% 			 ],
%% 			  
%% 			  Response = rpc(?REG_NAME, Java_Node, {"com.dragonflow.erlangecc.monitor.WebSphereMonitor", "update", Request}, ?TIMEOUT),
%% 			  updateValues(Response)
   Response= try			  
      java_node:send("com.dragonflow.erlangecc.monitor.WebSphereMonitor","update",Request,?TIMEOUT) of
		{ok,_,Ret}->
			%%get the counter value
			Ret;
		{error,_,[Err]}->
			%%some internal error hanppen in java node
			[{"error",Err}];
		{Ret, _, _}->
			%%get counter value error
			[{"error", atom_to_list(Ret)}];	
		_->
			[{"error","java node return a unknow error."}]
	catch _:X ->		 
			 [{"error","Connect Java Node Error! "}]
	      end,
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
			ConvertString=convert1(State_String),
			THIS:set_attribute(?STATE_STRING, util:replace(ConvertString,",","<br>") ),
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
	THIS:set_attribute(convert1(Key),Value).
%% rpc(RegName, Node, Msg, Timeout) ->
%% 	Ping = net_adm:ping(Node),
%%     if
%%         Ping==pang ->
%%             {error,"Connect Java Node Error! "};
%%         true ->
%%             {RegName, Node} ! Msg,	
%%             receive
%%                 {ok, _, Ret} ->	
%%                     Ret;
%%                 {error, _From, Ret} ->
%%                     {error,Ret}		
%%             after Timeout ->
%%                 {error, "time is out. "}
%%             end
%%     end.
%% @spec getBrowseData(Params) -> Result
%% @type Param  = [term()]
%% @type Result = [{CounterValue, CounterName}]
%% @doc getBrowseData(Params) is the function called by schedule to get counters of Weblogic6x server
%%	CounterValue is the Counter's OID, which will be used to get value
%%	CounterName is the show name of the counter
getBrowseData(Params)->
	%%get argument	
     HostName = proplists:get_value(server, Params),
	 PortNumber = proplists:get_value(port, Params),
	 Username = proplists:get_value(username, Params),
	 Password = proplists:get_value(password, Params),
	 Version = proplists:get_value(version, Params),
 	 WebSphereDir = proplists:get_value(webSphereDir, Params),
	 ClientProps = proplists:get_value(clientProps, Params),
	 Timeout= proplists:get_value(timeout, Params),
	 TrustStore = proplists:get_value(trustStore, Params),
	 TrustStorePassword = proplists:get_value(trustStorePassword, Params),
	 KeyStore= proplists:get_value(keyStore, Params),
    KeyStorePassword = proplists:get_value(keyStorePassword, Params),
%% 	Password = proplists:get_value(password, Params),
	Request = [
	 {server, HostName},
 	 {port, PortNumber},
	 {username, Username},
	 {password, Password},
	 {version, Version},
	 {webSphereDir, WebSphereDir},
	 {clientProps, ClientProps},
	 {timeout, Timeout},
	 {trustStore, TrustStore},
	 {trustStorePassword, TrustStorePassword},
	 {keyStore, KeyStore},
	 {keyStorePassword, KeyStorePassword}
	
	],	
	%%send the request to java node
	case java_node:send("com.dragonflow.erlangecc.monitor.WebSphereMonitor","getBrowseData",Request,?TIMEOUT) of
		{ok,_,Ret1}->
			%%get the counter value
			Ret= utftogbk(Ret1),
			qsort(Ret);
		{error,Err}->
			%%some internal error hanppen in java node
			{error,Err};
		{Ret, _, _}->
			%%get counter value error
			{error, atom_to_list(Ret)};	
		_->
			{error,"java node return a unknow error."}
	end.

utftogbk([])->
	[];
utftogbk([H|E])->
	{K,V}=H,
	[{K, convert(V)}]++utftogbk(E).



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


%% @spec verify(Params) -> {ok, []} | {error, Reason}
%% @type Params = [term()]
%% @type Reason = string()
%% @doc verify is the function called by schedule to verify the parameter of monitor inputed by user
%%	ipmi monitor verify timeout, hostname and port
verify(Params)->
    Errs = 
	case proplists:get_value(port,Params) of
    ""->
	    [{port,"port missing."}];
    Port->
		if
			not is_number(Port) ->
				[{port,"Port must be a number."}];
			true->
				[]
		end
	end ++
	case proplists:get_value(server,Params) of
    ""->
		[{server,"Host Name missing"}];
    Host->
	    case string:rstr(Host," ") of
		    0->
			    [];
			_->
			    [{server, "no spaces are allowed"}]
	    end
	end ++
	case proplists:get_value(webSphereDir,Params) of
    ""->
		[{webSphereDir, "the path of the weblogic jar file should not be empty"}];
    _->
		[]
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
	
getMaxCounter()->
	?MAX_COUNTER.

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
			
%% @spec get_classifier(error) -> List
%% @type List = [Tuple]
%% @type Tule = {status, Logic, Value}
%% @type Logic = '!=' | '==' | '>' | '<' | 'contain
%% @type Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
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
%%  	iconv:convert("utf-8", "gbk", lists:reverse(Acc)).

convert1(Msg)->
	convert1(Msg, []).
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
getHostname()->
	case THIS:get_property(server) of
		{ok,{_,V}}->
			V;
		_->
			BASE:getHostname()
	end.
qsort([]) -> [];
qsort([Pivot|T]) ->
	qsort([X || X <- T, element(2,X) < element(2, Pivot)])
	++ [Pivot] ++
	qsort([X || X <- T, element(2,X) >= element(2,Pivot)]).	
%% @spec get_template_property() -> List
%% @type List = [term()]
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
%% 1.elements show on gui interface
%%
get_template_property()->
	BASE:get_template_property() ++ 
	[
    %% 	 #property{name=status,title="status",configurable=false,state=true},
	 %%#property{name=target,title="Target",type=text,description="the logical name of the server. If empty, the hostname will be used.",order=1},
	 #property{name=server,title="Server",type=text,description="the name of the server",order=1},
	 #property{name=port,title="Port Number",type=numeric,default=8880,description="the port number of WebSphere server",order=2},
	 #property{name=username,title="User Name",type=text,description="the username for login to WebSphere server",order=3},
	 #property{name=password,title="Password",type=password,description="the password for login to WebSphere server",order=4},
	 #property{name=version,title="Version",type=scalar,description="WebSphere Version",order=5},
	 #property{name=webSphereDir,title="webSphereDir",type=text,description="Enter path to WebSphere Directory. This directory should contain at least an Admin Console installation.",order=6},
	 #property{name=clientProps,title="ClientProps",type=text,default="soap.client.props",description="custom client properties file. For WebSphere 5.x+, you should select an appropriate soap.client.props file. <WebSphereDir>/properties/soap.client.props will be used by default.",order=7},
	 #property{name=timeout,title="Timeout(s)",type=numeric,default=60,description="The timeout (in seconds) to wait for the response",order=8},
	 #property{name=trustStore,title="Trust Store",type=text,default="C:\\WebSphere\\AppServer\\profiles\\default\\etc\\DummyClientTrustFile.jks",description="Enter the path of the SSL trust file.",order=9},
	 #property{name=trustStorePassword,title="Trust Store Password",default="WebAS",type=password,description="Enter the password for the SSL trust store file (For WebSphere 6.x with global security enabled)",order=10},
	 #property{name=keyStore,title="Key Store",type=text,default="C:\\WebSphere\\AppServer\\profiles\\default\\etc\\DummyClientKeyFile.jks",description="Enter the path of the SSL key store file.",order=11},
	 #property{name=keyStorePassword,title="Key Store Password",default="WebAS",type=password,description="Enter the password for the SSL key store file (For WebSphere 6.x with global security enabled)",order=12}
	].

