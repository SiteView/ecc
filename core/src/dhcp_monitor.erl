%% ---
%% DHCP Monitor
%%
%%---

%% @copyright 2008-2009 Dragonflow
%% @author Jian Huang <jian.huang@dragonflow.com>
%% @version 1.0

%% @doc dhcp monitor
%%
%%This module is to test dhcp service:
%%1. test the work status of dhcp server
%%2. get one ip address from the dhcp server
%%3. if define an ip, verify the available of ip
%%4. calculate the speed of getting an ip


-module(dhcp_monitor, [BASE]).
-extends(atomic_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

%% @spec new() -> Obj
%% @type Obj = term()
%% @doc create a new instance for dhcp monitor
new()->	
	Base = atomic_monitor:new(),
	Base:set_attribute(round_trip_time, 0), 
	{?MODULE,Base}.
	
%% @spec update() -> Result
%% @type Result = term()
%% @doc update is the run function called by schedule to test the dhcp service
update() ->	
	Timeout = case BASE:get_property(timeout) of
		{ok, {_, T}} ->
			T*1000;
		_ ->
			10000
	end,		
	
	%%acquire a ip address fron dhcp server
	StartTime = now(),
	[A, _B, C, D, E] = acquireAddress(Timeout),	

	case A == "n/a" of
		true ->
			THIS:set_attribute(?CATEGORY, nodata),
			THIS:set_attribute(?NO_DATA, true),
			THIS:set_attribute(status, C),
			THIS:set_attribute(?STATE_STRING, E);
		false ->	
			THIS:set_attribute(?MEASUREMENT, A),
			THIS:set_attribute(round_trip_time, timer:now_diff(now(), StartTime)/1000),
			THIS:set_attribute(status, C),
			THIS:set_attribute(pStatusText, D),
			THIS:set_attribute(?STATE_STRING, E)		
	end.

%% @spec get_classifier(error) -> List
%% @type List = [Tuple]
%% @type Tule = {status, Logic, Value}
%% @type Logic = '!=' | '==' | '>' | '<' | 'contain
%% @type Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok, {error_classifier, Classifier}}->
			Classifier;
		_->
			[{status, '!=', "ok"}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok, {warning_classifier, Classifier}}->
			Classifier;
		_->
			[{status, '!=', "ok"}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok, {good_classifier, Classifier}}->
			Classifier;
		_->
			[{status, '==', "ok"}]
	end.

%% @spec verify(Params) -> {ok, []} | {error, Reason}
%% @type Params = [term()]
%% @type Reason = string()
%% @doc verify is the function called by schedule to verify the parameter of monitor inputed by user
%%	dhcp monitor verify timeout and request client address
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
	case proplists:get_value(clientaddress,Params) of
    ""->
		[];
    Host->
	    case string:rstr(Host," ") of
		    0->
			    [];
			_->
			    [{clientaddress, "no spaces are allowed"}]
	    end
	end ++
	case BASE:verify(Params) of
		{error,E}->
			E;
		_->
			[]
	end,	
	
	if length(Errs)>0 ->
	    {error,Errs};
    true ->
	    {ok,""}
	end.
		
%% @spec get_template_property() -> List
%% @type List = [term()]
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
%% 1.elements show on gui interface
%% 2.elements to evaluate the return value of telnet server 
get_template_property()->
	BASE:get_template_property() ++
	[	    
		#property{name=timeout, title="Timeout", default=10, type=numeric, advance=false, order=1, description="The amount of time, in seconds, to wait for a response from a DHCP server",baselinable=true},
		#property{name=clientaddress, title="Requested Client Address", advance=true, order=2, description="(Optional) The IP Address to request from the DHCP Server"},
        #property{name=status, title="status", type=text, order=3, configurable=false, state=true},
        #property{name=round_trip_time, title="round trip time(milliseconds)", type=numeric, order=4, configurable=false, state=true,baselinable=true}	
	].


%%the real function to get dhcp ip address
acquireAddress(Timeout) ->
	{ok, {_, ReqIp}} = THIS:get_property(clientaddress),
	
	case java_node:send("com.dragonflow.erlangecc.monitor.DHCPMonitor","getValues",[{ip, ReqIp},{timeout, Timeout}]) of
		%%get the result, parse it now
		{ok,_,Ret}->
			process_result(Ret, []);
		%%there is something error when get result
		{Error,_,_}->
			["n/a", 0, "error", atom_to_list(Error), atom_to_list(Error)];
		{error,Error}->
			if
				is_atom(Error) == true ->
					["n/a", 0, "error", atom_to_list(Error), atom_to_list(Error)];
				true ->	
					["n/a", 0, "error", Error, Error]
			end;
		_->
			["n/a", 0, "error", "Unknow Error", "Unknow Error"]
	end.
	
process_result([], Result)->
	["ok", 0, "ok", Result, Result];
process_result([C|T],Result)->
	{K, V} = C,
	process_result(T,Result ++ lists:flatten(io_lib:format("~p=~p<br>",[K,V]))).
	