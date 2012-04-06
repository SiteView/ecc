-module(tr069base,[BASE]).
-extends(atomic_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").


new()->
	Obj = atomic_monitor:new(),    	
	{?MODULE,Obj}.

    
	
get_template_property() ->
    BASE:get_template_property() ++ 
	[
	].
	
