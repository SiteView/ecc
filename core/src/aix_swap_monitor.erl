%% ---
%% aix swap infomation Monitor
%%
%%---

%% @copyright 2010 Dragonflow
%% @author xianfang.shi
%% @version 1.0

-module(aix_swap_monitor,[BaseObj]).
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
	% THIS:set_attribute(swap_writes,2),
	% THIS:set_attribute(swap_reads,3),
	% THIS:set_attribute(swap_total,536870912),
	% THIS:set_attribute(swap_used,1024),
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
	{"swap_writes","Writes/s"},
	{"swap_reads","Reads/s"},
	{"swap_total","Total"},
	{"swap_used","Used"},
	{"swap_used_percent","Used(%)"}
	].
	
get_template_property()->
	BaseObj:get_template_property() ++
	[
	#property{name=machine,title="Server",type=server,editable=true,order=1}
	% #property{name=swap_writes,title="Writes/s",type=numeric,configurable=false,state=true},
	% #property{name=swap_reads,title="Reads/s",type=numeric,configurable=false,state=true},
	% #property{name=swap_total,title="Total",type=numeric,configurable=false,state=true},
	% #property{name=swap_used,title="Used",type=numeric,configurable=false,state=true}
	].