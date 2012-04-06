-module(multiContent_base,[BASE]).
-extends(application_base).
-compile(export_all).

-include("monitor_template.hrl").
-include("monitor.hrl").

new()->
	Obj = application_base:new(),
	Obj:set_attribute(countersInError,0),
	{?MODULE,Obj}.

getHostname()->"".

getTemplateFile()->"".

getCounters(This,S)->
    This:getMultiContentBaseCounters(This,S,false).

getAvailableCounters(This)->
    io:format("****ok~n"),
    This:getMultiContentBaseCounters(This,"",true).

getMultiContentBaseCounters(_,S,false) when length(S) > 0->
	S;
getMultiContentBaseCounters(This,_,Flag)->
	R = This:getTemplateContent(This:getTemplatePath(), This:getTemplateFile(),Flag),
	[{atom_to_list(X#counters.id),atom_to_list(X#counters.name)} || X<-R].




    





