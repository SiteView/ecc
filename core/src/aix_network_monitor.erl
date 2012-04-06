%% ---
%% aix network infomation Monitor
%%
%%---

%% @copyright 2010 Dragonflow
%% @author xianfang.shi
%% @version 1.0

-module(aix_network_monitor,[BaseObj]).
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
			
	% THIS:set_attribute(logins,8),
	% THIS:set_attribute(connection_established,50),
	% THIS:set_attribute(connection_time_wait,20),
	% THIS:set_attribute(connection_close_wait,10),
	% THIS:set_attribute(pkts_in,10),
	% THIS:set_attribute(pkts_out,20),
	% THIS:set_attribute(pkts_error_in,2),
	% THIS:set_attribute(pkts_error_out,0),
	% THIS:set_attribute(nic_0_pkts,10),
	% THIS:set_attribute(nic_0_errors,0),
	% THIS:set_attribute(nic_0_name,"en0"),
	% THIS:set_attribute(nic_1_pkts,"N/A"),
	% THIS:set_attribute(nic_1_errors,"N/A"),
	% THIS:set_attribute(nic_1_name,"N/A"),	
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
	 
getBrowseData(Params)->
	[
	{"logins","Logins"},
	{"connection_established","Established"},
	{"connection_time_wait","Time_Wait"},
	{"connection_close_wait","Close_Wait"},
	{"pkts_in","In Pkts/s"},
	{"pkts_out","Out Pkts/s"},
	{"pkts_error_in","In Errors/s"},
	{"pkts_error_out","Out Errors/s"},
	{"nic_0_pkts","NIC 0 Pkts"},
	{"nic_0_errors","NIC 0 errors"},
	{"nic_0_name","NIC 0 Name"},
	{"nic_1_pkts","NIC 1 Pkts"},
	{"nic_1_errors","NIC 1 errors"},
	{"nic_1_name","NIC 1 Name"}
	].
	 
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
	
get_template_property()->
	BaseObj:get_template_property() ++
	[
	#property{name=machine,title="Server",type=server,editable=true,order=1}
	% #property{name=logins,title="Logins",type=numeric,configurable=false,state=true},
	% #property{name=connection_established,title="Established",type=numeric,state=true,configurable=false},
	% #property{name=connection_time_wait,title="Time_Wait",type=numeric,state=true,configurable=false},
	% #property{name=connection_close_wait,title="Close_Wait",type=numeric,state=true,configurable=false},
	% #property{name=pkts_in,title="In Pkts/s",type=numeric,state=true,configurable=false},
	% #property{name=pkts_out,title="Out Pkts/s",type=numeric,state=true,configurable=false},
	% #property{name=pkts_error_in,title="In Errors/s",type=numeric,state=true,configurable=false},
	% #property{name=pkts_error_out,title="Out Errors/s",type=numeric,state=true,configurable=false},
	% #property{name=nic_0_pkts,title="NIC 0 Pkts",type=numeric,state=true,configurable=false},
	% #property{name=nic_0_errors,title="NIC 0 errors",type=numeric,state=true,configurable=false},
	% #property{name=nic_0_name,title="NIC 0 Name",type=numeric,state=true,configurable=false},
	% #property{name=nic_1_pkts,title="NIC 1 Pkts",type=numeric,state=true,configurable=false},
	% #property{name=nic_1_errors,title="NIC 1 errors",type=numeric,state=true,configurable=false},
	% #property{name=nic_1_name,title="NIC 1 Name",type=numeric,state=true,configurable=false}
	].