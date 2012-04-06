%% ---
%%periodic_event
%%
%%---
-module(reportrealtime_event,[BASE,Frequency,LastUpdate,Action,Repeat]).
-compile(export_all).
-extends(schedule_event).

new(Frequency,LastUpdate,Action,Repeat)->
	Now =  sv_datetime:now(),
	Tm = if 
			LastUpdate + 1 > Now ->
				LastUpdate + 1;
			true->
				Now
		end,
	Obj = schedule_event:new(Tm,Repeat,0,Action),
	{?MODULE,Obj,Frequency,LastUpdate,Action,Repeat}.

calculateNextTime(L)->
	Tm = THIS:get_time(),
%%	{ok,{time,Tm}} = THIS:get_attribute(time),
	NewTm = if
				L > Tm ->
					if 
						Tm =:= 0 ->
							L;
						true->
							L + Frequency
					end;
				true->
					Tm
			end,
%%	THIS:set_attribute(time,NewTm),
%%	NewTm.
	Obj = schedule_event:new(NewTm,Repeat,0,Action),
	{?MODULE,Obj,Frequency,LastUpdate,Action,Repeat}.
do_action()->
	Action:do_action().					
