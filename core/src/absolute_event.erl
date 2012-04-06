%% 
%% @doc schedule event at absolute time,this event will trigger at once a weekday
%% @version{1.0}
%% @copyright 2009 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>
%%
-module(absolute_event,[BASE,Action,Sche]).
-extends(schedule_event).
-compile(export_all).

-include("monitor.hrl").

-export([new/2,calculateNextTime/1,get_time/0]).

%% @spec new(Action,Sche) -> Object
%% where
%%	Action = term()
%%	Sche = #schedule{}
%%	Object = term()
%% @doc create a absolute_event instance,Action is instance of action module or it's extends module,Sche is a record of #schedule
new(Action,Sche) ->
	%io:format("absolute_event_new:~p~n",[Sche]),
	% M = Action:get_monitor(),
	% Sche = M:get_schedule(),
	Day = sv_datetime:get_day(),
	ScheDay = lists:sort(fun(X,Y)->X<Y end,proplists:get_value(Day,Sche#schedule.days)),
	Obj = case ScheDay of
		[]->
			schedule_event:new(0,true,sv_datetime:now(),Action);
		[D|_] ->	
			schedule_event:new(sv_datetime:time(D),true,0,Action)
		end,
	%io:format("absolute_event:~p~n",[Obj]),
	{absolute_event,Obj,Action,Sche}.

%% @spec calculateNextTime(L) -> Object
%% where
%%	L = integer()
%%	Object = term()
%% @doc calcalate the next trigger time of this event,return a new instance contain the new time.
calculateNextTime(L)->
	% M = Action:get_monitor(),
	% Sche = M:get_schedule(),
	Day = sv_datetime:get_day(),
	ScheDay = lists:sort(fun(X,Y)->X<Y end,proplists:get_value(Day,Sche#schedule.days)),
	Tm = find_next_time(L,ScheDay,THIS:get_lasttime()),
	case Tm of
		0->
			Obj = schedule_event:new(0,true,L,Action),
			{absolute_event,Obj,Action,Sche};
		_->
			Obj = schedule_event:new(Tm,true,L,Action),
			{absolute_event,Obj,Action,Sche}
	end.
	
%% @spec get_time()->integer()
%% @doc get this event's trigger time,return elecc format time(see sv_datetime)
%%
get_time()->
	% M = Action:get_monitor(),
	% Sche = M:get_schedule(),
	case BASE:get_time() of
		0->
			Day = sv_datetime:get_day(),
			ScheDay = lists:sort(fun(X,Y)->X<Y end,proplists:get_value(Day,Sche#schedule.days)),
			L = THIS:get_lasttime(),
			find_next_time(sv_datetime:now(),ScheDay,L);
		Else->
			Else
	end.

find_next_time(_,[],_)->0;
find_next_time(Lasttime,[Tm|T],Last)->
	Time = sv_datetime:time(Tm),
	if
		Time < Lasttime andalso Time >Last ->
			Time;
		true ->
			THIS:find_next_time(Lasttime,T,Last)
	end.