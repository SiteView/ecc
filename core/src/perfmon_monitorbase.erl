%% Author: Administrator
%% Created: 2010-4-26
%% Description: TODO: Add description to perfmon_monitorbase
-module(perfmon_monitorbase,[BASE]).
-extends(atomic_monitor).
-compile(export_all).
-include("monitor_template.hrl").
-define(MAX_COUNTER,10).

new()->
	Base = atomic_monitor:new(),
	Base:set_attribute(countersInError,0),
	{?MODULE,Base}.

getMaxCounter()->
	?MAX_COUNTER.

getAvailableObjects()->
	[].
getAvailableInstances() ->
	[].
getAvailableCounters() ->
	[].
getPerfmonMeasurements()->
	[].
updateValues() ->
	[].
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
verify(Params)->
	Errs =
	case proplists:get_value(perfmon,Params) of
		undefined->
			[{perfmon,"must select at least one  counter"}];
		[]->
			[{perfmon,"must select at least one  counter"}];
		_->
			[]
	end ++
	case proplists:get_value(serverHostName,Params) of
		undefined->
			[{serverHostName,"HostName is null"}];
		[]->
			[{serverHostName,"HostName is null"}];
		_->
			[]
	end ++
		case proplists:get_value(serverPortNumber,Params) of
		undefined->
			[{serverPortNumber,"port is null"}];
		[]->
			[{serverPortNumber,"port is null"}];
		_->
			[]
	end ++
		case proplists:get_value(channel,Params) of
		undefined->
			[{channel,"channel is null"}];
		[]->
			[{channel,"channel is null"}];
		_->
			[]
	end ++
		case proplists:get_value(queueMgr,Params) of
		undefined->
			[{queueMgr,"Queue Manager is null"}];
		[]->
			[{queueMgr,"Queue Manager is null"}];
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


getCostInLicensePoints()->
	{ok,{_,Counters}} = THIS:get_property(perfmon),
	length(Counters).

getLogProperties(This)->
	{ok,{_,Counters}} = THIS:get_property(perfmon),
	Temp = This:get_template_property(),
	[X#property.name || X<-Temp,X#property.state=:=true] ++Counters.

getStatePropertyObjects()->
	[ #property{name=countersInError,title="counters in error",type=numeric,editable=true,configurable=false,state=true,order=100}].

get_template_property() ->
    BASE:get_template_property() ++ 
	[
	 #property{name=countersInError,title="counters in error",type=numeric,configurable=false,editable=true,state=true,order=100}
	].
