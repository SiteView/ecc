%% ---
%% Sybase Monitor
%%
%%---

%% @copyright 2008-2009 Dragonflow
%% @author XIanfang.shi, Jian Huang
%% @version 1.0

%% @doc IPMI monitor
%%
%%This module is to test Sybase service:
%%1. get performance counter from Sybase server
%%2. get selected performance value from Sybase server 

-module(sybase_monitor,[BASE]).
-extends(browsable_mdr_base).
-compile(export_all).

-include("monitor_template.hrl").
-include("monitor.hrl").

%% @spec new() -> Obj
%% @type Obj = term()
%% @doc create a new instance for sybase monitor
new()->
	Base = browsable_mdr_base:new(),
	Base:set_attribute('Perform$cpu_busy_total', "n/a"),
	Base:set_attribute('Perform$io_busy_total', "n/a"),
	Base:set_attribute('Perform$pack_received_total', "n/a"),
	Base:set_attribute('Perform$pack_sent_total', "n/a"),
	Base:set_attribute('Perform$pack_errors_total', "n/a"),
	Base:set_attribute('Perform$total_read_total', "n/a"),
	Base:set_attribute('Perform$total_write_total', "n/a"),
	Base:set_attribute('Perform$connect_total', "n/a"),
	{?MODULE,Base}.

%% @spec defaultTitle(Params) -> List
%% @type Params = [term()]
%% @type List = string()
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	BASE:defaultTitle(Params) ++ ": " ++ case proplists:get_value(server,Params) of undefined->"";V->V end.

%% @spec update() -> Result
%% @type Result = term()
%% @doc update is the run function called by schedule to test the  sybase service
update()->
	%%get argument to get value
	{ok,{_,Server}} = THIS:get_property(server),
	{ok,{_,Usr}} = THIS:get_property(usr),
	{ok,{_,Pwd}} = THIS:get_property(pwd),
	Counter = get_counters(),
	THIS:set_attribute(countersInError,0),
	
	case ip_utils:check_ip(Server) of
		{ok,_} ->
			%%send the request to java node
			case java_node:send("com.dragonflow.erlangecc.monitor.SybaseMonitor","getValues",[{host,Server},{usr,Usr},{pwd,Pwd}, {counters, Counter}]) of
				{ok,_,Ret}->
					%%response contains the validate value
					%%get the counters
					{ok,{_,Counters}} = THIS:get_property(browse),
					%%set the counter's values
					Str = process_result(Counters,Ret),
					%%set remember attribute
					process_attribute(Ret),
					%%set the show string
					 THIS:set_attribute(?STATE_STRING,Str);
				{Error,_,_}->
					%%java node fail to get counter's values
					{ok,{_,Counters}} = THIS:get_property(browse),
					%%set all counters to error status
					THIS:set_attribute(countersInError,length(Counters)),
					%%set error show string
					THIS:set_attribute(?STATE_STRING,atom_to_list(Error));
				{error,Error}->
					%%java node has some internal error
					{ok,{_,Counters}} = THIS:get_property(browse),
					THIS:set_attribute(countersInError,length(Counters)),
					THIS:set_attribute(?STATE_STRING,Error);
				_->
					%%this condition should not happen
					{ok,{_,Counters}} = THIS:get_property(browse),
					Str = process_result(Counters,[]),
					THIS:set_attribute(?STATE_STRING,Str)
			end;
		_->
			THIS:set_attribute(?NO_DATA,true),
			THIS:set_attribute(?CATEGORY,?NO_DATA),
			THIS:set_attribute(?STATE_STRING,"error-Sybase Server invalid.")
	end.

%%process the result return by java, 
process_result([],_)->"";
process_result([C|T],Result)->
	%%find the countr information 
	case lists:keysearch(element(1,C),1,Result) of
		{value,{K,V}}->
			%%validate counter
			VV = 
			if
				%%erlang float must do something specail to show incorrectly
				is_float(V) ->
					erlang:round(V*100)/100;
				true ->
					V
			end,
			%%set attribute
			THIS:set_attribute(K,VV),
			%%set show string
			lists:flatten(io_lib:format("~p=~p<br>",[element(2,C),VV])) ++ 
				process_result(T,Result);
		_->
			%%information are not found
			%%set attribute to "n/a"
			THIS:set_attribute(element(1,C),"n/a"),
			%%increment the counter error number
			THIS:inc_attribute(countersInError),
			lists:flatten(io_lib:format("~p=n/a<br>",[element(2,C)])) ++ 
				process_result(T,Result)
	end.

%% @spec verify(Params) -> {ok, []} | {error, Reason}
%% @type Params = [term()]
%% @type Reason = string()
%% @doc verify is the function called by schedule to verify the parameter of monitor inputed by user
%%	ipmi monitor verify timeout and host ip
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

%% @spec getBrowseData(Params) -> Result
%% @type Param  = [term()]
%% @type Result = [{CounterValue, CounterName}]
%% @doc getBrowseData(Params) is the function called by schedule to get counters of IPMI server
%%	CounterValue is the Counter's OID, which will be used to get value
%%	CounterName is the show name of the counter
getBrowseData(Params)->
	%%get the argument to get counters
	Server = proplists:get_value(server,Params),
	Usr = proplists:get_value(usr,Params),
	Pwd = proplists:get_value(pwd,Params),
	
	%%send get counters request to java node
	case java_node:send("com.dragonflow.erlangecc.monitor.SybaseMonitor","getCounters",[{host,Server},{usr,Usr},{pwd,Pwd}]) of
		{ok,_,Ret}->
			%%sort the counters
			lists:sort(fun(X,Y)->element(1,X) > element(1,Y) end,[{V,K}||{K,V}<-Ret]);
		{error,Err}->
			%%some error happen
			{error,Err};
		{Error,_,_}->
			{error, Error};
		_->
			{error,"java node return a unknow error."}
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
			[{countersInError,'>',0}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'>',0}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'==',0}]
	end.

%% @spec get_template_property() -> List
%% @type List = [term()]
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
%% 1.elements show on gui interface
get_template_property()->
	BASE:get_template_property() ++ 
	[#property{name=server,title="Server",type=text,description="Sybase server name.For Sybase 11.9 or later",order=1},
	 #property{name=usr,title="Username",type=text,description="Enter the Username for Sybase",order=1},
	 #property{name=pwd,title="Password",type=password,description="Enter the Password for Sybase",order=1},
	 #property{name=timeout, title="Timeout", type=numeric, default=60, advance=true, order=1, description="the time out, seconds, to wait for the database connection to made and the command to complete",baselinable=true}
	].

%update_test()->
%	THIS:add_properties([{server,"192.168.2.21"},{usr,"sa"},{pwd,""}]),
%	R = THIS:getBrowseData([{server,"192.168.2.21"},{usr,"sa"},{pwd,""}]),
%	?assert(length(R)>0).

	
get_counters() ->
	{ok,{_,Cpu}} = THIS:get_attribute('Perform$cpu_busy_total'),
	{ok,{_,IO}} = THIS:get_attribute('Perform$io_busy_total'),
	{ok,{_,Rec}} = THIS:get_attribute('Perform$pack_received_total'),
	{ok,{_,Send}} = THIS:get_attribute('Perform$pack_sent_total'),
	{ok,{_,Error}} = THIS:get_attribute('Perform$pack_errors_total'),
	{ok,{_,Read}} = THIS:get_attribute('Perform$total_read_total'),
	{ok,{_,Write}} = THIS:get_attribute('Perform$total_write_total'),
	{ok,{_,Conn}} = THIS:get_attribute('Perform$connect_total'),
	[{'Perform$cpu_busy_total', Cpu}, {'Perform$io_busy_total', IO}, {'Perform$pack_received_total', Rec},
	 {'Perform$pack_sent_total', Send}, {'Perform$pack_errors_total', Error}, {'Perform$total_read_total', Read},
	 {'Perform$total_write_total', Write}, {'Perform$connect_total', Conn}].
	
set_attributes(Key, Value) ->
	VV = 
		if
			is_float(Value) ->
				erlang:round(Value*100)/100;
			true ->
				Value
		end,
	THIS:set_attribute(list_to_atom(Key),VV).
	
process_attribute([]) ->
	ok;
process_attribute([C|Counters]) ->
	case element(1, C) of
		"Perform$cpu_busy_total" ->
			set_attributes(element(1, C), element(2, C));
		"Perform$io_busy_total" ->
			set_attributes(element(1, C), element(2, C));
		"Perform$pack_received_total" ->
			set_attributes(element(1, C), element(2, C));
		"Perform$pack_sent_total" ->
			set_attributes(element(1, C), element(2, C));
		"Perform$pack_errors_total" ->
			set_attributes(element(1, C), element(2, C));
		"Perform$total_read_total" ->
			set_attributes(element(1, C), element(2, C));
		"Perform$total_write_total" ->
			set_attributes(element(1, C), element(2, C));
		"Perform$connect_total" ->	
			set_attributes(element(1, C), element(2, C));
		_ ->
			ok
	end,
	process_attribute(Counters).
	
	