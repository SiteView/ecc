%% ---
%% VMWarePerformance Monitor
%%
%%---

%% @copyright 2008-2009 Dragonflow
%% @author Jian Huang <jian.huang@dragonflow.com>
%% @version 1.0

%% @doc VMWarePerformance monitor
%%
%%This module is to test VMWare Performance:
%%1. get host and virtual host counters from the VMWare server
%%2. get counter's value from the server

-module(vmwareperformance_monitor,[BASE]).
-extends(browsable_base).
-compile(export_all).

-include("monitor_template.hrl").
-include("monitor.hrl").


%% @spec new() -> Obj
%% @type Obj = term()
%% @doc create a new instance for vmware performance monitor
new()->
	Base = browsable_base:new(),
	{?MODULE,Base}.

%% @spec defaultTitle(Params) -> List
%% @type Params = [term()]
%% @type List = string()
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	BASE:defaultTitle(Params) ++ ": " ++ case proplists:get_value(url,Params) of undefined->"";V->V end.

%% @spec update() -> Result
%% @type Result = term()
%% @doc update is the run function called by schedule to test the  vmware performance service
update()->
	%%get property
	{ok,{_,Server}} = THIS:get_property(url),
	{ok,{_,Timeout}} = THIS:get_property(timeout),
	{ok,{_,Usr}} = THIS:get_property(usr),
	{ok,{_,Pwd}} = THIS:get_property(pwd),
	
	THIS:set_attribute(countersInError,0),
	{ok,{_,Counters}} = THIS:get_property(browse),

	%%send message to java node
	case java_node:send("com.dragonflow.erlangecc.monitor.VMWarePerformanceMonitor","getValues",[{url, Server},{timeout, Timeout}, {usr,Usr},{pwd,Pwd},{counters, Counters}]) of
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
	end.	

%%process the result return by java, 
process_result([], _)->
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
					lists:flatten(io_lib:format("~p=~p<br>",[element(2,C),VV])) ++ 
						process_result(T,Result);
				_->
					THIS:set_attribute(element(1,C),"n/a"),
					THIS:inc_attribute(countersInError),
					lists:flatten(io_lib:format("~p=n/a<br>",[element(2,C)])) ++ 
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
	Server = proplists:get_value(url,Params),
	Usr = proplists:get_value(usr,Params),
	Timeout = proplists:get_value(timeout,Params),
	Pwd = proplists:get_value(pwd,Params),
				
	case java_node:send("com.dragonflow.erlangecc.monitor.VMWarePerformanceMonitor","getCounters",[{url, Server},{timeout, Timeout},{usr,Usr},{pwd,Pwd}]) of
		{ok,_,Ret}->
			qsort(Ret);
		{error,Err}->
			{error,Err};
		{Ret, _, _}->
			{error, atom_to_list(Ret)};
		_ ->
			{error,"java node return a unknow error."}
	end.

	
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
	case proplists:get_value(url, Params) of
    ""->
		[{url,"URL missing"}];
    Host->
	    case string:rstr(Host," ") of
		    0->
			    [];
			_->
			    [{url, "no spaces are allowed"}]
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
	case THIS:get_property(url) of
		{ok,{_,V}}->
			V;
		_->
			BASE:getHostname()
	end.
	
%% @spec get_template_property() -> List
%% @type List = [term()]
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
%% 1.elements show on gui interface
%% 2.elements to evaluate the return value of telnet server 
get_template_property()->
	BASE:get_template_property() ++ 
	[#property{name=url,title="Server URL",type=text,description="VMWare server Url, example: http://www.xxx.com/sdk",order=1},
	 #property{name=usr,title="Username",type=text,description="user name to connect the VMWare server",order=1},
	 #property{name=pwd,title="Password",type=password,description="password to connect the VMWare server",order=1},
	 #property{name=timeout, title="Timeout", type=numeric, default=60, advance=true, order=9, description="the time out, seconds, to wait for the telnet connection to made and the command to complete",baselinable=true}
	].

trim_empty_counter([], New) ->
	New;
trim_empty_counter([T|Old], New) ->
	{K, _V} = T,
	if
		K =:= " " orelse K =:= "" ->
			trim_empty_counter(Old, New);
		true ->
			New1 = T  ++ New,
			trim_empty_counter(Old, New1)
	end.
	
qsort([]) -> [];
qsort([Pivot|T]) ->
	qsort([X || X <- T, element(2,X) < element(2, Pivot)])
	++ [Pivot] ++
	qsort([X || X <- T, element(2,X) >= element(2,Pivot)]).
	

	