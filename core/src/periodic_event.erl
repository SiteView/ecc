%% ---
%%periodic_event
%%
%%---
-module(periodic_event,[BASE,Frequency,LastUpdate,Action,Repeat]).
-compile(export_all).
-extends(schedule_event).

new(Frequency,LastUpdate,Action,Repeat)->
	Now =  sv_datetime:now(),
	Tm = if 	
			LastUpdate + Frequency > Now ->
				LastUpdate + Frequency;
			true->
				Now
		end,
	Obj = schedule_event:new(Tm,Repeat,0,Action),
	{?MODULE,Obj,Frequency,LastUpdate,Action,Repeat}.

calculateNextTime(L)->
	Tm = THIS:get_time(),
%%	{ok,{time,Tm}} = THIS:get_attribute(time),
	NewTm = if
				L > Tm + Frequency ->
					if 
						Tm =:= 0 ->
							L;
						true->
							L + Frequency
					end;
				true->
					Tm + Frequency
			end,
%%	THIS:set_attribute(time,NewTm),
%%	NewTm.
	Obj = schedule_event:new(NewTm,Repeat,0,Action),
	{?MODULE,Obj,Frequency,L,Action,Repeat}.
					
