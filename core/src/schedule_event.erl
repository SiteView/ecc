%% ---
%%schedule_event
%%
%%---
-module(schedule_event,[Time,Repeat,LastTime,Action]).
-compile(export_all).
%%-extends(propertied_object).

%%new(Time,Repeat,LastTime,Action)->
%%	Obj = siteview_object:new(null),
%%	Obj:set_attribute(time,Time),
%%	Obj:set_attribute(repeat,Repeat),
%%	Obj:set_attribute(lasttime,LastTime),
%%	Obj:set_attribute(action,Action),
%%	{?MODULE,Obj,Time,Repeat,LastTime,Action}.

get_action()->Action.
%%	case THIS:get_attribute(action) of
%%		{ok,{action,Act}}->
%%			Act;
%%		_->
%%			{error,not_found_action}
%%	end.

get_lasttime()->LastTime.

get_time()->Time.
%%	case THIS:get_attribute(time) of
%%		{ok,{time,Tm}}->
%%			Tm;
%%		_->
%%			{error,not_found_time}
%%	end.

is_repeated()->Repeat.
%%	case THIS:get_attribute(repeat) of
%%		{ok,{repeat,Rpt}}->
%%			Rpt;
%%		_->
%%			{error,not_found_repeat}
%%	end.

%%set_repeated(Val)->
%%	THIS:set_attribute(repeat,Val).

do_action()->Action:trigger(Action).
%%	{ok,{action,Act}} = THIS:get_attribute(action),
	%%io:format("schedule_event do_action~p,~p~n",[THIS:get_time(),Act]),
%%	Act:trigger(Act).


calculateNextTime(Val)->
	Val.