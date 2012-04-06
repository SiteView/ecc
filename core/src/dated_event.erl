%% Author: chengxingyu
%% Created: 2009-10-21
%% Description: TODO: Add description to dated_event
-module(dated_event,[BASE,Action,Day,Seconds,Lasttime]).
-compile(export_all).
-extends(schedule_event).

%%
%%no Seconds<0
%%
new(Action,Day,Seconds,Lasttime)->
	Obj=schedule_event:new(0,true,Lasttime,Action),
	{?MODULE,Obj,Action,Day,Seconds,Lasttime}.

%%
%% Local Functions
%%

calculateNextTime(L)->
	{Y,M,D}=erlang:date(),
	D1=sv_datetime:time({{Y,M,D},{0,0,0}})+Seconds*1000,
    Newtime=THIS:find_time(L,D1),
	Time=THIS:get_time(),
	{YY,MM,DD}=sv_datetime:getDate(L),
	TmpDay=DD,
%% 	{LYY,LMY,LDD}=sv_datetime:getDate(Lasttime),
	if Lasttime>0,Time=:=0,TmpDay=:=Day ->
		   if D1<L ->
				 Ret=L-1;
			  true ->Ret=Newtime
				end;
	   true ->Ret=Newtime
	 end,	   	   
	Obj=schedule_event:new(Ret,true,Lasttime,Action),
	{?MODULE,Obj,Action,Day,Seconds,Lasttime}.
get_time()->
	Time=BASE:get_time(),
	Time.

find_time([],Newtime) ->
	Newtime;
find_time(Last,Newtime)->
	{Y,M,D}=sv_datetime:getDate(Newtime),
	TmpDay=D,
	if Newtime=<Last;TmpDay=/=Day ->
		   T=Newtime+86400000,
		   find_time(Last,T);
	    true->
		   find_time([],Newtime)
	end.