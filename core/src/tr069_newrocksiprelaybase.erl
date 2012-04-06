-module(tr069_newrocksiprelaybase,[BASE]).
-extends(tr069base).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").


new() ->
    Obj = tr069base:new(),
    Obj:set_attribute(lastMeasurementTime,0),	
	{?MODULE,Obj}.


get_template_property() ->
    BASE:get_template_property().




    	
