%% ---
%% update_device
%%
%%---
-module(update_device,[BASE,Monitor]).
-extends(action).
-compile(export_all).


new(Monitor)->
	Obj = action:new(),
	Obj:set_monitor(Monitor),
	Obj:set_attribute(runType,1),
	{?MODULE,Obj,Monitor}.
%%
%%
execute()->
	ok.

