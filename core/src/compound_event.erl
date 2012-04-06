%% Author: chengxingyu
%% Created: 2009-10-21
%% Description: TODO: Add description to compound_event
-module(compound_event,[BASE,Action,ScheduleEvents]).
-compile(export_all).
-extends(schedule_event).

%%
%%
%%
new(Action,ScheduleEvents)->
	Obj=schedule_event:new(0,true,sv_datetime:now(),Action),
	{?MODULE,Obj,Action,ScheduleEvents}.

%%
%%
%%
addEvent(Sche)->
	ScheTimes= [SE:calculateNextTime(sv_datetime:now()) ||SE<-Sche],
	Times=[SE1:get_time() ||SE1<-ScheTimes],
    MinTime=THIS:getMinTime(2051193600000,Times),
	Obj=schedule_event:new(MinTime,true,sv_datetime:now(),Action),
	{?MODULE,Obj,Action,ScheTimes}.
getScheduleEvents()->
	ScheduleEvents.
%%
%%
%%
calculateNextTime(L)->
   Sches=THIS:getScheduleEvents(),
   ScheTimes= [SE:calculateNextTime(L) ||SE<-Sches],
   Times=[SE1:get_time() ||SE1<-ScheTimes],
   MinTime=getMinTime(2051193600000,Times),
   if MinTime=:=2051193600000 ->
		 Mt=L-1;
	   true ->Mt=MinTime
   end,
   Obj=schedule_event:new(Mt,true,sv_datetime:now(),Action),
   io:format("~p~n report scheduler",[Mt]),
   {?MODULE,Obj,Action,ScheTimes}.
%%
%%
%%
get_time()->
	Time=BASE:get_time(),
	Time.
get_action()->
	Action.
getMinTime(Min,[])->
	Min;
%%2051193600000
%%get min value
%%
getMinTime(Min,[H|E])->
	if Min>H ->
		   getMinTime(H,E);
	   Min<H ->
		    getMinTime(Min,E);
	   true ->ok
	end.
do_action()->
	Action:do_action().
	
