%% ---
%% aix memory infomation Monitor
%%
%%---

%% @copyright 2010 Dragonflow
%% @author xianfang.shi
%% @version 1.0

-module(aix_memory_monitor,[BaseObj]).
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
			{error,update};
		_->
			CtrData = realdata_agent:get_data(Mach,Counters),
			lists:map(fun({K,V})->
						THIS:set_attribute(K,V)
					end,CtrData),
			{ok,update}
	end.
	% THIS:set_attribute("page_in",12),
	% THIS:set_attribute("page_out",13),
	% THIS:set_attribute("ram_total",1073741824),
	% THIS:set_attribute("ram_free",644245094),
	% THIS:set_attribute("vm_max",1610612736),
	% THIS:set_attribute("vm_free",1288490188.8),
	% THIS:set_attribute("processes_swapped",1),
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
	{"page_in","Page In"},
	{"page_out","Page Out"},
	{"ram_total","Total(RAM)"},
	{"ram_free","Free(RAM)"},
	{"ram_free_percent","Free(RAM)(%)"},
	{"vm_max","Max Size(VM)"},
	{"vm_free","Free(VM)"},
	{"vm_free_percent","Free(VM)(%)"},
	{"processes_swapped","Processes Swapped"}
	].
	
get_template_property()->
	BaseObj:get_template_property() ++
	[
	#property{name=machine,title="Server",type=server,editable=true,order=1}
	% #property{name=page_in,title="Page In",type=numeric,configurable=false,state=true},
	% #property{name=page_out,title="Page Out",type=numeric,configurable=false,state=true},
	% #property{name=ram_total,title="Total(RAM)",type=numeric,configurable=false,state=true},
	% #property{name=ram_free,title="Free(RAM)",type=numeric,configurable=false,state=true},
	% #property{name=vm_max,title="Max Size(VM)",type=numeric,configurable=false,state=true},
	% #property{name=vm_free,title="Free(VM)",type=numeric,configurable=false,state=true},
	% #property{name=processes_swapped,title="Processes Swapped",type=numeric,configurable=false,state=true}
	].