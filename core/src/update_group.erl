%% ---
%%update_group
%%
%%---
-module(update_group,[BASE,Monitor]).
-extends(action).
-compile(export_all).
-include("monitor.hrl").

new(Monitor)->
	Obj = action:new(),
	{?MODULE,Obj,Monitor}.

get_monitor()->
	Monitor.

execute()->
	spawn(fun()->THIS:update_group() end).
	
update_group()->
	{ok,{_,Id}} = Monitor:get_property(?ID),
	Monitors = Monitor:getGroupsMonitors(false),
	Ret = [X:runUpdate(X,false,false)||X<-Monitors],
	Monitor:set_attribute(?LAST_UPDATE,sv_datetime:now()),
	monitor_status_store:save(Monitor:get_app(),Id,Monitor:get_status_info(Monitor)),
	case lists:member(false,Ret) of
		true ->
			false;
		_->
			true
	end.
