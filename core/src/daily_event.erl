%% Author: chengxingyu
%% Created: 2009-10-21
%% Description: TODO: Add description to daily_event
-module(daily_event,[BASE,Action1,Day,Seconds,Lasttime]).
-compile(export_all).
-extends(schedule_event).

%%
%%no Seconds<0
%%
new(Action1,Day,Seconds,Lasttime)->
	Obj=schedule_event:new(0,true,Lasttime,Action1),
	{?MODULE,Obj,Action1,Day,Seconds,Lasttime}.

%%
%% Local Functions
%%
getday()->
	Day.

getSeconds()->
	Seconds.


calculateNextTime(L)->
	{Y,M,D}=erlang:date(),
	D1=sv_datetime:time({{Y,M,D},{0,0,0}})+Seconds*1000,
    Newtime=THIS:find_time(L,D1),
	Time=THIS:get_time(),
	{YY,MM,DD}=sv_datetime:getDate(L),
	TmpDay=sv_datetime:get_day({YY,MM,DD}),
%% 	{LYY,LMY,LDD}=sv_datetime:getDate(Lasttime),
	if Lasttime>0,Time=:=0,TmpDay=:=Day ->
		   if D1<L ->
				 Ret=L-1;
			  true ->Ret=Newtime
				end;
	   true ->Ret=Newtime
	 end,	   	   
	Obj=schedule_event:new(Ret,true,Lasttime,Action1),
	{?MODULE,Obj,Action1,Day,Seconds,Lasttime}.
get_time()->
	Time=BASE:get_time(),
	Time.

find_time([],Newtime) ->
	Newtime;
find_time(Last,Newtime)->
	{Y,M,D}=sv_datetime:getDate(Newtime),
	TmpDay=sv_datetime:get_day({Y,M,D}),
	if Newtime=<Last;TmpDay=/=Day ->
		   T=Newtime+86400000,
		   find_time(Last,T);
	    true->
		   find_time([],Newtime)
	end.


	
