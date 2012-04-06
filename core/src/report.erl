%% ---
%%monitor group
%%
%%---
-module(report,[BASE]).
-extends(monitor).
-compile(export_all).
-include("monitor.hrl").
-include("report.hrl").
-include("monitor_template.hrl").

new()->
	Obj = monitor:new(),
%% 	Obj:set_attribute(?LAST_UPDATE,0),
%% 	Obj:set_attribute(?LAST_CATEGORY,nodata),
%% 	Obj:set_attribute(?CATEGORY,nodata),
%% 	Obj:set_attribute(?STATE_STRING,""),	
	{?MODULE,Obj}.

getScalarValues(Prop,_)->
	case Prop of
		?STATUSFILTER ->
			[{"show all monitors",""},{"show only monitors that had errors or warnings","error or warning"},{"show only monitors that had errors","error"},
			{"show only monitors that had warnings","warning"},{"show only monitors that were OK","good"}];
		?PRECISION ->
			[{"automatic","default"},{"minute","60"},{"2 minutes","120"},{"5 minutes","300"},{"10 minutes","600"},{"15 minutes","900"},
			{"30 minutes","1800"},{"hour","3600"},{"2 hours","7200"},{"6 hours","21600"},{"12 hours","43200"},{"day","86400"}];
		?VMAX ->
			[{"automatic",""},{"1","1"},{"5","5"},{"10","10"},{"20","20"},{"50","50"},{"100","100"},{"1000","1000"},{"5000","5000"},
			{"10000","10000"},{"20000","20000"},{"1000000","1000000"},{"10000000","10000000"}];
		?SCHEDFILTER->
			F = fun(X)->
				Id = proplists:get_value(id,X),
				Name = proplists:get_value(name,X),
				{Name,atom_to_list(Id)}
				end,
			[{"every day,all day","all"}] ++ lists:map(F,api_schedule:get_infos());
		_->
			[]
	end.

get_template_property()->
	[
	#property{name=?DETAILED,title="Show Detail", description="",type=bool,editable=true,advance=true},	
	#property{name=?STATUSFILTER,title="Show Monitors",type=scalar,editable=true,advance=true},
	#property{name=?SCHEDFILTER,title="Schedule Filter", description="schedule for the monitor to be enabled - for example, \"weekdays, 9-6\" enables the monitor to run from 9am to 6pm, Monday - Friday",type=schedule,editable=true,advance=true},
	#property{name=?BESTCASECALC,title="Uptime Calculation", description="",type=bool,editable=true,advance=true},		
	#property{name=?PRECISION,title="Time Scale",type=scalar,editable=true,advance=true},
	#property{name=?VMAX,title="Vertical Scale",type=scalar,editable=true,advance=true},
	#property{name=?TITLE,title="Title",type=text,editable=true,advance=true},
	#property{name=?DESCRIPTION,title="Description",type=text,editable=true,advance=true}
	].
