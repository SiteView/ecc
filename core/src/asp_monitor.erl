%% @author Cheng kaiyang <kaiyang.cheng@dragonflow.com>
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc Active Server Pages Monitor.
%% 
%% Description: For Microsoft ASP server on Windows NT systems
%% Versions supported: Windows NT Terminal Server 4.0, Microsoft Windows 2000 Server
%% Platform: Windows
-module(asp_monitor,[BASE]).
-extends(ntcounter_base).
-compile(export_all).
-include("monitor_template.hrl").
-include("monitor.hrl").

-export([verify/1,update/0,get_classifier/1,getCountersContent/0,setCountersContent/1,getCounterObjects/0,getDefaultInstance/0,get_template_property/0]).

-define(MAX_COUNTER,20).

new()->
	{?MODULE,ntcounter_base:new()}.

%% @spec verify(Params) -> ErrorList
%% where
%% Params = [{atom(),integer()|string()}]
%% ErrorList = [string()]
%% @doc page check.
%% Description: check counters
verify(Params)->
	Errs =
	case proplists:get_value(counters,Params) of
		[]->
			[{counters,"Counters is missing"}];
		_->
			[]
	end++
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
%%
%% Description: Get counters value with perfex.exe
%% extend ntcounter_base.erl
update()->
	{ok,{_,Machine}}=THIS:get_property(machine),
	case THIS:getCounters(THIS) of
		[] ->
			THIS:set_attribute(lastmeasurementtime,0),
			THIS:set_attribute(lastMeasurement,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
			THIS:set_attribute(measurement,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
			THIS:set_attribute(values,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
			THIS:set_attribute(?NO_DATA,true),
			THIS:set_attribute(?CATEGORY,?NO_DATA),
			THIS:set_attribute(?STATE_STRING,"No counters selected");
		L when length(L) >?MAX_COUNTER ->
			THIS:set_attribute(contersInError,100),
			THIS:set_attribute(lastmeasurementtime,0),
			THIS:set_attribute(lastMeasurement,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
			THIS:set_attribute(measurement,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
			THIS:set_attribute(values,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
			THIS:set_attribute(?NO_DATA,true),
			THIS:set_attribute(?CATEGORY,?NO_DATA),
			THIS:set_attribute(?STATE_STRING,"You have exceeded the maximum number of counters available");
		L->
			THIS:getPerformanceData(Machine,L)
	end.
	
%% @spec get_classifier(Flag) -> Threshold
%% where
%% Flag = (error|warning|good)
%% Threshold = [{Attribute,Opera,Value}]
%% Attribute = atom()
%% Opera = atom()
%% Value = (integer()|atom()|string())
%% @doc Set default threshold value.
get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{contersInError,'>',0}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{contersInError,'>',0}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier; 
		_->
			[{contersInError,'==',0}]
	end.	

%% This confusing relationship between the call
%% As getCostInLicensePoints not pass parameters, if placed in the base class, then the parent class out into the subclass method, this parameter in erlang is not allowed if the module
%% But getCounters (This) is different, since it was introduced to the caller method, although getCountersContent not defined in the parent class, but in the end is still sub-class monitor call it, is equivalent to indirect
%% Sub-class call their own way, this is different from the java part of the structure
getCostInLicensePoints() ->
    I = THIS:getActiveCounters(THIS),
    I*1.

%% @spec getCountersContent() -> Counters
%% where
%% Counters = [{Counternumber,Countervalue}]
%% Counternumber = integer()
%% Countervalue = string()
%% @doc Get counters content.
getCountersContent()->
	R = case THIS:get_property(counters) of
		{ok,{_,V}} ->
			V;
		_->
			[]
	end,
	counters(R).
	
counters([])->[];
counters([F|R])->
	case F of
		{K,V}->
			[K++"@"++V]++counters(R);
		_->
			counters(R)
	end.
	
%% @spec setCountersContent(Counters) -> ok
%% where
%% Counters = [{Counternumber,Countervalue}]
%% Counternumber = integer()
%% Countervalue = string()
%% @doc Set counters content.
setCountersContent(S)->
	THIS:set_property(counters,S).
	
%% @spec getCounterObjects() -> string()
%% @doc define asp server objects.
getCounterObjects()->
	"Active Server Pages".
	%%"Server".
	
%% @spec getDefaultInstance() -> string()
%% @doc define asp server instance.
getDefaultInstance()->
	"SINGLE".

%% @spec getDefaultCounters() -> string()
%% @doc define asp server default counters.
getDefaultCounters()->
	"Active Server Pages -- Errors/Sec -- SINGLE,Active Server Pages -- Requests/Sec -- SINGLE,Active Server Pages -- Bytes Total/sec -- SINGLE,Active Server Pages -- Transactions/Sec -- SINGLE,Active Server Pages -- Errors During Script Runtime -- SINGLE,Active Server Pages -- Request Wait Time -- SINGLE,Active Server Pages -- Requests Executing -- SINGLE,Active Server Pages -- Requests Not Found -- SINGLE,Active Server Pages -- Requests Queued -- SINGLE,Active Server Pages -- Requests Rejected -- SINGLE".

%% @spec get_template_property() -> PropertyList
%% where
%% PropertyList = [PropertyRecord]
%% PropertyRecord = record()
%% @doc Add properties to template property list.
get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=counters,title="NTCounters",description="Current selection of counters,counters numbers should less than 20.",type=counters,editable=true,order=1},
	#property{name=contersInError,title="counters in error",type=numeric,state=true,configurable=false}
	].