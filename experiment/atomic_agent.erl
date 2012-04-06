%% Author: John
%% Created: 2012-1-24
%% Description: TODO: Add description to atomic_agent
-module(atomic_agent).

-compile(export_all).

-include("../../include/monitor_template.hrl").
-include("../../include/monitor.hrl").
-include("../../include/object.hrl").

extends() -> base_agent.

atomic_agent(Self,Mind) ->
	io:format("atomic agent:~w,~w~n", [Self,Mind]),
	object:set(Self,?MEASUREMENTTIME,#property_table{name=?MEASUREMENTTIME,title="Measurement time",value=10,type=numeric,advance=true,default= 100,order=1,description="amount of time to get the measurement",baselinable=true}),
	object:set(Self,'schedule',#property_table{name=schedule,title="Schedule",type=schedule,editable=true,advance=true, default="all",
						  description="schedule for the monitor to be enabled - for example, \"weekdays, 9-6\" enables the monitor to run from 9am to 6pm, Monday - Friday"}),
	
	object:super(Self,[Mind]).

?ACTION(start) -> {wakeup_event,init}.

init(Self,EventType,Pattern,State) ->
	io:format ( "Action: init, [State]:~w, [Event type]:~w, [Pattern]: ~w '\n",	[State,EventType,Pattern]),
	object:do(Self,waiting).


on_starting(Self) ->
	io:format("This ping_agent behaviour is starting \n").

on_stopping(Self) ->
	io:format("This ping_agent behaviour is going to be terminated \n").

start(Mind) ->
	case object:check_name(Mind) of
		available -> X = object:new(?MODULE,[Mind]),
				object:start(X),
				eresye:assert(Mind,{wakeup}),
				X;
		_ -> name_not_available
	end.

