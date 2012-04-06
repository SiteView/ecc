%% ---
%% WebLogic6x Monitor
%%
%%---

%% @copyright 2008-2009 Dragonflow
%% @author Xianfang Shi <xianfang.shi@dragonflow.com>, Jian Huang <jian.huang@dragonflow.com>
%% @version 1.0

%% @doc Weblogic6x monitor
%%
%%This module is to test Weblogic6x and later version service:
%%1. get weblogic performance counter from WebLogic server
%%2. get performance data from WebLogic server 

-module(weblogic6x_monitor,[BASE]).
-extends(browsable_base).
-compile(export_all).

-include("monitor_template.hrl").
-include("monitor.hrl").

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

%% @spec update() -> Result
%% @type Result = term()
%% @doc update is the run function called by schedule to test the  weblogic service
update()->
	%%get property
	{ok,{_,Server}} = THIS:get_property(server),
	{ok,{_,Port}} = THIS:get_property(port),
	{ok,{_,Usr}} = THIS:get_property(usr),
	{ok,{_,Pwd}} = THIS:get_property(pwd),
	{ok,{_,Secure}} = THIS:get_property(secure),
	{ok,{_,WeblogicJar}} = THIS:get_property(weblogicJar),
	{ok,{_,Counters}} = THIS:get_property(browse),
	
	%%format the host:port string
	Host = Server ++":" ++ integer_to_list(Port),

	THIS:set_attribute(countersInError,0),
	
	case ip_utils:check_ip(Server) of
		{ok,_} ->
			%%send argument to java node
			case java_node:send("com.dragonflow.erlangecc.monitor.Weblogic6xMonitor","getValues",
						[{host,Host},{usr,Usr},{pwd,Pwd},{secure,Secure},{weblogicJar,WeblogicJar},{counter,Counters}],600000) of
				{ok,_,Ret}->
					%%get the result, parse it now
					 Str = process_result(Counters,Ret),
					 THIS:set_attribute(status,"ok"),
					 THIS:set_attribute(?STATE_STRING,Str);

				{Error,_,_}->
					%%some thing error
					THIS:set_attribute(status,"error"),
					THIS:set_attribute(countersInError,length(Counters)),
					THIS:set_attribute(?STATE_STRING,atom_to_list(Error));
				{error,Err}->
					THIS:set_attribute(status,"error"),
					THIS:set_attribute(countersInError,length(Counters)),
					THIS:set_attribute(?STATE_STRING,Err);
				_->
					%%unkown error
					THIS:set_attribute(status,"error"),
					THIS:set_attribute(?NO_DATA,true),
					THIS:set_attribute(?CATEGORY,?NO_DATA),
					THIS:set_attribute(?STATE_STRING,"error")
			end;
		_->
			THIS:set_attribute(?NO_DATA,true),
			THIS:set_attribute(?CATEGORY,?NO_DATA),
			THIS:set_attribute(?STATE_STRING,"error-WebLogic Server invalid.")
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

%% @spec getBrowseData(Params) -> Result
%% @type Param  = [term()]
%% @type Result = [{CounterValue, CounterName}]
%% @doc getBrowseData(Params) is the function called by schedule to get counters of Weblogic6x server
%%	CounterValue is the Counter's OID, which will be used to get value
%%	CounterName is the show name of the counter
getBrowseData(Params)->
	%%get argument
	Server = proplists:get_value(server,Params),
	Usr = proplists:get_value(usr,Params),
	Pwd = proplists:get_value(pwd,Params),
	Secure = case proplists:get_value(secure,Params) of
				"on"->
					true;
				_->
					false
			end,
	WeblogicJar = proplists:get_value(weblogicJar,Params),
	Port = proplists:get_value(port,Params),

	%%format the host name
	Host = Server ++":" ++ Port,

	%%send the request to java node
	case java_node:send("com.dragonflow.erlangecc.monitor.Weblogic6xMonitor","getBrowseData",[{host,Host},{usr,Usr},{pwd,Pwd},{secure,Secure},{weblogicJar,WeblogicJar}]) of
		{ok,_,Ret}->
			%%get the counter value
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
	case proplists:get_value(weblogicJar,Params) of
    ""->
		[{weblogicJar, "the path of the weblogic jar file should not be empty"}];
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
			[{status,'!=',"ok"}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'!=',"ok"}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'==',"ok"}]
	end.

getHostname()->
	case THIS:get_property(server) of
		{ok,{_,V}}->
			V;
		_->
			BASE:getHostname()
	end.
	
%% @spec get_template_property() -> List
%% @type List = [term()]
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
%% 1.elements show on gui interface
%% 2.elements to evaluate the return value of Weblogic6x server 
get_template_property()->
	BASE:get_template_property() ++ 
	[
	 #property{name=status,title="status",configurable=false,state=true},
	 %%#property{name=target,title="Target",type=text,description="the logical name of the server. If empty, the hostname will be used.",order=1},
	 #property{name=server,title="Server",type=text,description="the name of the server",order=1},
	 #property{name=port,title="Port Number",type=numeric,default=7001,description="the port number of WebLogic server",order=1},
	 #property{name=secure,title="Secure Server",type=bool,description="is this a secure server?",order=1},
	 #property{name=usr,title="User Name",type=text,description="the username for login to WebLogic server",order=1},
	 #property{name=pwd,title="Password",type=password,description="the password for login to WebLogic server",order=1},
	 #property{name=weblogicJar,title="WebLogic Jar File",type=text,description="enter weblogic jar file in java/plugin directory. For example: <tt>weblogic.jar</tt>.",order=1}
	].
	
qsort([]) -> [];
qsort([Pivot|T]) ->
	qsort([X || X <- T, element(2,X) < element(2, Pivot)])
	++ [Pivot] ++
	qsort([X || X <- T, element(2,X) >= element(2,Pivot)]).
	
