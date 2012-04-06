%% ---
%% aix cpu infomation Monitor
%%
%%---

%% @copyright 2010 Dragonflow
%% @author xianfang.shi
%% @version 1.0

-module(aix_cpu_monitor,[BaseObj]).
-extends(browsable_base).
-compile(export_all).

-include("monitor.hrl").
-include("monitor_template.hrl").

new()->
	Obj = browsable_base:new(),
	{?MODULE,Obj}.

	
defaultTitle(Params)->
	BaseObj:defaultTitle(Params) ++ ": " ++ case proplists:get_value(host,Params) of undefined->"";V->V end.

update() ->
	{ok,{machine,Machine}} = THIS:get_property(machine),
	{ok,{_,Counters}} = THIS:get_property(browse),
	Mach = machine:getMachineByName(Machine),
	
	THIS:set_attribute(countersInError,0),
	
	case Mach of
		[]->
			lists:map(fun({X,_})->
						THIS:set_attribute(X,"n/a")
					end,Counters),
			THIS:set_attribute(countersInError,length(Counters)),
			{ok,update};
		_->
			CtrData = realdata_agent:get_data(Mach,Counters),
			lists:map(fun({K,V})->
						THIS:set_attribute(K,V)
					end,CtrData),
			{ok,update}
	end.
	% THIS:set_attribute("processor_count",2),
	% THIS:set_attribute("total_cpu",5),
	% THIS:set_attribute("user_percent",2),
	% THIS:set_attribute("system_percent",3),
	% THIS:set_attribute("wait_percent",0),
	% THIS:set_attribute("queue_length",1),
	% THIS:set_attribute("processes_total",40),
	% THIS:set_attribute("processes_zombies",2),
	% THIS:set_attribute("processes_blocked",2),
	% {ok,update}.
	
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
			[{countersInError, '>', 0}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError, '>',  0}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError, '==', 0}]
	end.
	
remoteCommandLineAllowed()->
	true.
	
getBrowseData(Params)->
	[
	{"processor_count","processor count"},
	{"total_cpu","Total %"},
	{"user_percent","User %"},
	{"system_percent","System %"},
	{"wait_percent","Wait %"},
	{"queue_length","Queue Length"},
	{"processes_total","Processes Total"},
	{"processes_zombies","Processes Zombies"},
	{"processes_blocked","Processes Blocked"}
	].
	
get_template_property()->
	BaseObj:get_template_property() ++
	[
	#property{name=machine,title="Server",type=server,editable=true,order=1}
	% #property{name=processor_count,title="processor",type=numeric,configurable=false,state=true},
	% #property{name=user_percent,title="User %",type=numeric,state=true,configurable=false},
	% #property{name=system_percent,title="System %",type=numeric,state=true,configurable=false},
	% #property{name=wait_percent,title="Wait %",type=numeric,state=true,configurable=false},
	% #property{name=queue_length,title="Queue Length",type=numeric,state=true,configurable=false},
	% #property{name=processes_total,title="Processes Total",type=numeric,state=true,configurable=false},
	% #property{name=processes_zombies,title="Processes Zombies",type=numeric,state=true,configurable=false},
	% #property{name=processes_blocked,title="Processes Blocked",type=numeric,state=true,configurable=false}
	].