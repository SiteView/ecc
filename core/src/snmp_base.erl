%% ---
%% snmp_base
%%
%%---
-module(snmp_base,[BASE]).
-extends(application_base).
-compile(export_all).
-include("monitor_template.hrl").
-include("monitor.hrl").

new()->
	Obj = application_base:new(),
	{?MODULE,Obj}.

getHostname()->"".

getActiveCounters(This)->
	Cnt = This:getCountersContent(),
	Max = This:getMaxCounters(),
	if
		length(Cnt) > Max ->
			Max;
		true ->
			length(Cnt)
	end.

getCounters(This,S)->
	THIS:getSNMPCounters(This,S,false).


getAvailableCounters(This)->
	THIS:getSNMPCounters(This,"",true).

getDefaultPort()->
	161.

getSNMPCounters(_,S,false) when length(S) > 0->
	S;
getSNMPCounters(This,_,Flag)->
	R = This:getTemplateContent(This:getTemplatePath(), This:getTemplateFile(),Flag),
	[{atom_to_list(X#counters.id),atom_to_list(X#counters.name) ++ ":" ++ atom_to_list(X#counters.id)} || X<-R].
	
getAppServerTestOID()->"".

getSNMPData()->ok.

oid2string([])->"";
oid2string([I|T])->
	case T of
		[]->
			integer_to_list(I);
		_->
			integer_to_list(I) ++ "." ++ oid2string(T)
	end.

get_template_property()->
	BASE:get_template_property() ++ 
	[
	 #property{name=countersInError,title="counters in error",type=numeric,editable=true,configurable=false,state=true,order=100}
	].