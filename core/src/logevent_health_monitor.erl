%% 
%% @doc logevent_health_monitor
%%
%%
-module(logevent_health_monitor,[BASE]).
-extends(multiContent_base).
-compile(export_all).
-include("monitor_template.hrl").
-include("monitor.hrl").

-define(MAX_COUNTERS,30).

new()->
	Obj = multiContent_base:new(),
	logevent_health:start_link(),
	{?MODULE,Obj}.

init(This,Data)->
	BASE:init(This,Data),
	case proplists:get_value(counters,Data) of
		undefined->
			pass;
		Counters->
			[logevent_health:add_searchs(X)||{X,_}<-Counters]
	end,
	{ok,""}.

update()->
	%logevent_health:log("akkakskipped #1akksdf"),
	{ok,{_,Counters}} = THIS:get_property(counters),
	R = parse_counters(Counters,""),
	THIS:set_attribute(status,"ok"),
	THIS:set_attribute(?STATE_STRING,R).

parse_counters([],R)->R;
parse_counters([{C,_}|T],R)->
	Size = 
	case logevent_health:get_matchs(C) of
		error->
			0;
		{ok,Matchs}->
			length(Matchs)
	end,
	THIS:set_attribute(C,Size),
	St = lists:flatten(io_lib:format("~s:~w<br>",[C,Size])),
	parse_counters(T,R++St).


getCountersContent()->
	case THIS:get_property(counters) of
		{ok,{_,V}}->
			V;
		_->
			[]
	end.

getCostInLicensePoints()->0.

getTemplateFile()->"counters.logEvents".


getDefaultCounters()->
	THIS:getCounters(THIS,"").


get_classifier(error)->
	Count = THIS:getMaxCounters(),
	Cls = case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'!=',"ok"}]
	end,
	if 
		length(Cls) < Count ->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,Count - length(Cls)));
		true ->
			Cls
	end;
get_classifier(warning)->
	Count = THIS:getMaxCounters(),
	Cls = case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'!=',"ok"}]
	end,

	if 
		length(Cls) < Count ->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,Count - length(Cls)));
		true ->
			Cls
	end;
get_classifier(good)->
	Count = THIS:getMaxCounters(),
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'==',"ok"}]
	end,
	if 
		length(Cls) < Count ->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,Count - length(Cls)));
		true ->
			Cls
	end.

get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=status,title="status",configurable=false,state=true},
	#property{name=counters,title="Counters", description="Current selection of counters.",type=counters,editable=true,order=1,default=THIS:getDefaultCounters()}
	].
