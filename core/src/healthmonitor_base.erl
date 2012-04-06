%% @author Cheng kaiyang <kaiyang.cheng@dragonflow.com>
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc Health Monitor Base.
%% 
%% Description: Erlang siteview isn't surport this monitor sofar
-module(healthmonitor_base,[BASE]).
-extends(atomic_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

new()->
	{?MODULE,atomic_monitor:new()}.
	
gethostname()->"".

getLogProperties(This)->
	[numerrors|This:getLogProperties(This)].
	
setstatus(This)->
	case This:errorcheck() of
		[]->
			THIS:set_attribute(?STATE_STRING,"no errors"),
			THIS:set_property(numerrors,0);
		L->
			Mes=createErrorMeg(L),
			THIS:set_attribute(?STATE_STRING,Mes),
			THIS:set_property(numerrors,length(L))
	end.

createErrorMeg([])->[];
createErrorMeg([F|R])->
	if
        length(R)>0 ->
			F++createErrorMeg(R);
		true ->
			F++", "++createErrorMeg(R)
	end.

getCostInLicensePoints()->0.

get_template_property()->
	BASE:get_template_property() ++ 
	[
	 #property{name=numerrors,title="numErrors",type=numeric,editable=true,configurable=false,state=true,order=100}
	].