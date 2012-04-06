%% ---
%% IPMI Monitor
%%
%%---

%% @copyright 2008-2009 Dragonflow
%% @author Jian Huang <jian.huang@dragonflow.com>
%% @version 1.0

%% @doc IPMI monitor
%%
%%This module is to test IPMI service:
%%1. get sensor counter from IPMI server
%%2. get chassis and sensor data from IPMI server 

-module(ipmi_monitor,[BASE]).
-extends(browsable_base).
-compile(export_all).

-include("monitor_template.hrl").
-include("monitor.hrl").


%% @spec new() -> Obj
%% @type Obj = term()
%% @doc create a new instance for ipmi monitor
new()->
	Base = browsable_base:new(),
	{?MODULE,Base}.

%% @spec defaultTitle(Params) -> List
%% @type Params = [term()]
%% @type List = string()
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	BASE:defaultTitle(Params) ++ ": " ++ case proplists:get_value(server,Params) of undefined->"";V->V end.

%% @spec update() -> Result
%% @type Result = term()
%% @doc update is the run function called by schedule to test the  ipmi service
update()->
	%%get property
	{ok,{_,Server}} = THIS:get_property(server),
	{ok,{_,Port}} = THIS:get_property(port),
	{ok,{_,Usr}} = THIS:get_property(usr),
	{ok,{_,Pwd}} = THIS:get_property(pwd),
	THIS:set_attribute(countersInError,0),
	{ok,{_,Counters}} = THIS:get_property(browse),

	%%is ip ok?
	case ip_utils:check_ip(Server) of
		{ok,_} ->
			%%send message to java node
			Temp = java_node:send("com.dragonflow.erlangecc.monitor.IpmiMonitor","getValues",[{host,Server},{port, Port}, {usr,Usr},{pwd,Pwd},{counters, Counters}]),
			case Temp of
				%%get the result, parse it now				
				{ok,_,Ret}->
					 Str = process_result(Counters, Ret),
					 THIS:set_attribute(?STATE_STRING,Str);
				%%there is something error when get result
				{Error,_,_}->
					THIS:set_attribute(countersInError,length(Counters)),
					THIS:set_attribute(?STATE_STRING,atom_to_list(Error));
				{error,Error}->
					THIS:set_attribute(countersInError,length(Counters)),
					if
						is_atom(Error) == true ->
							THIS:set_attribute(?STATE_STRING,atom_to_list(Error));
						true ->
							THIS:set_attribute(?STATE_STRING,Error)
					end;	
				_->
					 Str = process_result(Counters, []),
					 THIS:set_attribute(?STATE_STRING,Str)
			end;
		_->
			THIS:set_attribute(?NO_DATA,true),
			THIS:set_attribute(?CATEGORY,?NO_DATA),
			THIS:set_attribute(?STATE_STRING,"error-IPMI Server invalid.")
	end.	

%%process the result return by java, 
process_result([],_)->
	[];
process_result([C|T],Result)->
		case C of
		{"countersInError", Count} ->
			THIS:set_attribute(countersInError,list_to_integer(Count));
		_ ->
			
			case lists:keysearch(element(1,C),1,Result) of
				{value,{K,VVV}}->
					V = number_util:get_value(VVV),	
					VV = 
					if
						is_float(V) ->
							erlang:round(V*100)/100;
						true ->
							V
					end,
					THIS:set_attribute(K,VV),
					lists:flatten(io_lib:format("~s = ~s<br>",[element(2,C),VV])) ++ 
						process_result(T,Result);
				_->
					THIS:set_attribute(element(1,C),"n/a"),
					THIS:inc_attribute(countersInError),
					lists:flatten(io_lib:format("~s = n/a<br>",[element(2,C)])) ++ 
						process_result(T,Result)
			end
	end.
	

%% @spec getBrowseData(Params) -> Result
%% @type Param  = [term()]
%% @type Result = [{CounterValue, CounterName}]
%% @doc getBrowseData(Params) is the function called by schedule to get counters of IPMI server
%%	CounterValue is the Counter's OID, which will be used to get value
%%	CounterName is the show name of the counter
getBrowseData(Params)->
	Server = proplists:get_value(server,Params),
	Usr = proplists:get_value(usr,Params),
	Port = proplists:get_value(port,Params),
	Pwd = proplists:get_value(pwd,Params),
	CountersList = 	[{"Chassis", "Chassis"}] ++
					[{"ChassisPowerOn", "Chassis/Power On"}] ++
					[{"ChassisPowerOverload", "Chassis/Power Overload"}] ++
					[{"ChassisPowerInterlock", "Chassis/Power Interlock"}] ++
					[{"ChassisPowerFault", "Chassis/Power Fault"}] ++
					[{"ChassisPowerControlFault", "Chassis/Power Control Fault"}] ++
					[{"ChassisDriveFault", "Chassis/Drive Fault"}] ++
					[{"ChassisCoolingFanFault", "Chassis/Cooling Fan Fault"}] ++
					[{"ChassisIntrusion", "Chassis/Intrusion"}] ++
					[{"ChassisFrontPanelLockout", "Chassis/Front Panel Lockout"}] ++
					[{"Senser", "Senser"}],
				
	case java_node:send("com.dragonflow.erlangecc.monitor.IpmiMonitor","getCounters",[{host,Server},{port,Port},{usr,Usr},{pwd,Pwd}]) of
		{ok,_,Ret}->
			qsort(adjust_counters(Ret, CountersList));
		{error,Err}->
			{error,Err};
		{Ret, _, _}->
			{error, atom_to_list(Ret)};
		_ ->
			{error,"java node return a unknow error."}
	end.
	
%%add prefix to list in a tree and change the counters name from string to atom
adjust_counters([], Result) ->
	lists:sort(fun(X,Y)->element(1,X) > element(1,Y) end,[{V,K}||{K,V}<-Result]);
adjust_counters([T|List], Result) ->
	{V, K} = T,
	K1 = "Senser/" ++ K,
	adjust_counters(List, Result ++ [{V, K1}]).

%% @spec get_classifier(error) -> List
%% @type List = [Tuple]
%% @type Tule = {status, Logic, Value}
%% @type Logic = '!=' | '==' | '>' | '<' | 'contain
%% @type Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error)->
	%%get max counters the system support
	Count = THIS:getMaxCounter(),
	Cls = case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'>',0}]
	end,
	%%show max counter	  
	if 
		length(Cls) < Count ->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,Count - length(Cls)));
		true ->
			Cls
	end;		
get_classifier(warning)->
	%%get max counters the system support
	Count = THIS:getMaxCounter(),
	Cls = case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'>',0}]
	end,
	%%show max counter	  
	if 
		length(Cls) < Count ->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,Count - length(Cls)));
		true ->
			Cls
	end;		
get_classifier(good)->
	%%get max counters the system support
	Count = THIS:getMaxCounter(),
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'==',0}]
	end,
	%%show max counter	  
	if 
		length(Cls) < Count ->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,Count - length(Cls)));
		true ->
			Cls
	end.	

%% @spec verify(Params) -> {ok, []} | {error, Reason}
%% @type Params = [term()]
%% @type Reason = string()
%% @doc verify is the function called by schedule to verify the parameter of monitor inputed by user
%%	ipmi monitor verify timeout, hostname and port
verify(Params)->
    Errs = 
	case proplists:get_value(timeout,Params) of
    ""->
	    [{timeout,"timeout missing."}];
    Time->
		if
			not is_number(Time) ->
				[{timeout,"timeout must be a number."}];
			true->
				[]
		end
	end ++
	case proplists:get_value(port,Params) of
    ""->
	    [{timeout,"port missing."}];
    Port->
		if
			not is_number(Port) ->
				[{timeout,"Port must be a number."}];
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
%% 2.elements to evaluate the return value of IPMI server 
get_template_property()->
	BASE:get_template_property() ++ 
	[#property{name=server,title="Server",type=text,description="Ipmi sensor server IP address",order=1},
	 #property{name=port,title="Port",type=numeric,default=623,description="the port number of the IPMI server (default is 623)",order=1},
	 #property{name=usr,title="Username",type=text,description="user name to connect the IPMI server",order=1},
	 #property{name=pwd,title="Password",type=password,description="password to connect the IPMI server",order=1},
	 #property{name=timeout, title="Timeout", type=numeric, default=60, advance=true, order=9, description="the time out, seconds, to wait for the IPMI connection to made and the command to complete",baselinable=true}
	].

qsort([]) -> [];
qsort([Pivot|T]) ->
	qsort([X || X <- T, element(2,X) < element(2, Pivot)])
	++ [Pivot] ++
	qsort([X || X <- T, element(2,X) >= element(2,Pivot)]).
