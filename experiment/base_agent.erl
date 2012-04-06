%% Author: John
%% Created: 2012-1-24
%% Description: TODO: Add description to base_agent
-module(base_agent).
-compile(export_all).

-include("../../include/monitor_template.hrl").
-include("../../include/monitor.hrl").
-include("../../include/classifierstring.hrl"). 

-include("../../include/object.hrl").

extends () -> nil.

base_agent(Self,Mind) ->
	io:format("base agent:~w, Mind:~w~n", [Self,Mind]), 
	object:set(Self,?MIND,#property_table{name=?MIND,value=Mind,title="The Mind of the monitor",type=text,editable=true,order=1,optional = false,default="",
										  description="The inference rule engine."}),
	object:set(Self,?NAME,#property_table{name=?NAME,title="Title",type=text,editable=true,order=102,optional = true,default="",
										  description="title that should appear in the Monitor table (optional)"}),
	object:set(Self,?FREQUENCY,#property_table{name=?FREQUENCY,title="Update every",type=frequency,value=300,editable=true,default=300,
											   description="last update time."}),
	object:set(Self,?LASTUPDATE,#property_table{name=?LASTUPDATE,title="Last Update time",type=datetime,value=0,editable=false,default=0,
												description="amount of time between checks of a monitor"}),
	object:set(Self,?DISABLETIME,#property_table{name=?DISABLETIME,title="Disable time",type=numeric,value=0,editable=true,default=0,
												 description="amount of time to disable monitoring, 0 will be disable permanently."}),
	object:set(Self,?DISABLED,#property_table{name=?DISABLED,title="Disable",type=bool,editable=true,advance=true,order =1,default=false,
											  description="temporarily disable monitor sampling and alerting"}),
	object:set(Self,?VERFIY_ERROR,#property_table{name=?VERFIY_ERROR,title="Verify Error",type=bool,editable=true,advance=true,order=50,default=false,
												  description="if the monitor detects an error, immediately perform the check again to verify the error."}),
	object:set(Self,?ERROR_FREQUENCY,#property_table{name=?ERROR_FREQUENCY,title="Update every (on errors)",type=frequency,editable=true,advance=true,default=0,order=60,
													 description="the amount of time between checks whenever the status of the monitor is not ok; the Update value from above is used."}),
	object:set(Self,?DEPENDS_ON,#property_table{name=?DEPENDS_ON,title="Depends On",type=scalar,editable=true,advance=true,default="none",
												description="Choose the monitor that this monitor depends on"}),
	object:set(Self,?DEPENDS_CONDITION,#property_table{name=?DEPENDS_CONDITION,title="Depends Condition",type=scalar,editable=true,default="good",advance=true,
													   description="If OK, this monitor is only enabled if the Depends On monitor is OK."}),
	
	eresye:start(Mind).

?PATTERN(?FREQUENCY ) -> ?VALUE(?FREQUENCY)*1000;
?PATTERN(?DISABLETIME) -> ?VALUE(?DISABLETIME)*1000.


start(Mind) ->
	case object:check_name(Mind) of
		available -> X = object:new(?MODULE,[Mind]),
				object:start(X),
				eresye:assert(Mind,{wakeup}),
				X;
		_ -> name_not_available
	end.

