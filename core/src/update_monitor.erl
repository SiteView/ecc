%% ---
%%update_monitor
%%
%%---
-module(update_monitor,[BASE,Monitor]).
-compile(export_all).
-extends(action).

-include("monitor.hrl").

new(Monitor)->
	Obj = action:new(),
	{?MODULE,Obj,Monitor}.
	
get_monitor()->
	Monitor.
%%
%%
%%
execute()->
	%%{ok,{monitor,M}}= THIS:get_monitor(),
	M = Monitor,
	case M:get_parent() of
		{ok,{parent,Parent}}->
			case Parent:groupSchedulerActive(false) of
				true->
					{ok,group_scheduler_active};
				_->
					M:runUpdate(M,false,false)
			end;
		_->
			
			M:runUpdate(M,false,false)
	end.

getFullGroupName(M)->
	S = case M:get_property(groupid) of
			{ok,{groupid,Gid}}->
				{ok,{id,Id}} = M:get_property(id),
					atom_to_list(Gid)++ atom_to_list(Id) + " ";
			_->
				""
		end,
	{ok,{name,Name}}=M:get_property(name),
	list_to_atom(S++atom_to_list(Name)).
	
on_timeout()->
	M = Monitor,
	M:set_attribute(running,false),
	M:set_attribute(?CATEGORY,?NO_DATA),
	M:set_attribute(?STATE_STRING,"timeout"),
	ok.