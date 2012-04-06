%% ---
%% aix system infomation Monitor
%%
%%---

%% @copyright 2010 Dragonflow
%% @author xianfang.shi
%% @version 1.0

-module(aix_sysinfo_monitor,[BaseObj]).
-extends(atomic_monitor).
-compile(export_all).

-include("monitor.hrl").
-include("monitor_template.hrl").

new()->
	Obj = atomic_monitor:new(),
	{?MODULE,Obj}.

	
defaultTitle(Params)->
	BaseObj:defaultTitle(Params) ++ ": " ++ case proplists:get_value(host,Params) of undefined->"";V->V end.

update() ->
	{ok,{machine,Machine}} = THIS:get_property(machine),
	Mach = machine:getMachineByName(Machine),
	case Mach of
		[]->
			THIS:set_attribute(os_name,"n/a"),
			THIS:set_attribute(uptime,"n/a"),
			THIS:set_attribute(date,"n/a"),
			THIS:set_attribute(time,"n/a"),
			THIS:set_attribute(status,400),
			{error,machine_not_found};
		_->
			[OsName,Uptime,Date,Time] = realdata_agent:get_sysinfo(Mach),
	
			THIS:set_attribute(os_name, OsName),
			THIS:set_attribute(uptime, Uptime),
			THIS:set_attribute(date, Date),
			THIS:set_attribute(time, Time),
			THIS:set_attribute(status,200),
			{ok,update}
	end.
	
verify(Params)->
	 Errs = 
    case BaseObj:verify(Params) of
		{error,Be}->
			Be;
		_->
			[]
	end,   
	if length(Errs)>0 ->
	    {error,Errs};
    true ->
	    {ok,""}
	end.
	 
get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{status, '!=', 200}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{status, '>',  200}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{status, '==', 200}]
	end.
	
get_template_property()->
	BaseObj:get_template_property() ++
	[
	#property{name=status,title="Status",type=numeric,configurable=false,state=true},
	#property{name=os_name,title="OS",type=text,configurable=false,state=true},
	#property{name=uptime,title="Uptime",type=text,state=true,configurable=false},
	#property{name=date,title="Date",type=text,state=true,configurable=false},
	#property{name=time,title="time",type=text,state=true,configurable=false}
	].