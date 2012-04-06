%% ---
%% aix disk infomation Monitor
%%
%%---

%% @copyright 2010 Dragonflow
%% @author xianfang.shi
%% @version 1.0

-module(aix_disk_monitor,[BaseObj]).
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
	{"disk_writes","Writes/s"},
	{"disk_reads","Reads/s"},
	{"disk_io_0_name","Name Of Disk Io 0"},
	{"disk_io_0_value","Value Of Disk Io 0"},
	{"disk_io_1_name","Name Of Disk Io 1"},
	{"disk_io_1_value","Value Of Disk Io 1"},
	{"disk_io_2_name","Name Of Disk Io 2"},
	{"disk_io_2_value","Value Of Disk Io 2"},
	{"disk_io_3_name","Name Of Disk Io 3"},
	{"disk_io_3_value","Value Of Disk Io 3"},
	{"disk_0","Disk 0"},
	{"disk_0_mount","Disk 0 Mount"},
	{"disk_0_size","Disk 0 Size"},
	{"disk_0_used","Disk 0 Used"},
	{"disk_0_available","Disk 0 Available"},
	{"disk_0_capacity","Disk 0 Capacity"},
	{"disk_1","Disk 1"},
	{"disk_1_mount","Disk 1 Mount"},
	{"disk_1_size","Disk 1 Size"},
	{"disk_1_used","Disk 1 Used"},
	{"disk_1_available","Disk 1 Available"},
	{"disk_1_capacity","Disk 1 Capacity"},
	{"disk_2","Disk 2"},
	{"disk_2_mount","Disk 2 Mount"},
	{"disk_2_size","Disk 2 Size"},
	{"disk_2_used","Disk 2 Used"},
	{"disk_2_available","Disk 2 Available"},
	{"disk_2_capacity","Disk 2 Capacity"},
	{"disk_3","Disk 3"},
	{"disk_3_mount","Disk 3 Mount"},
	{"disk_3_size","Disk 3 Size"},
	{"disk_3_used","Disk 3 Used"},
	{"disk_3_available","Disk 3 Available"},
	{"disk_3_capacity","Disk 3 Capacity"}
	].
	
get_template_property()->
	BaseObj:get_template_property() ++
	[
	#property{name=machine,title="Server",type=server,editable=true,order=1}
	% #property{name=disk_writes,title="Writes/s",type=numeric,configurable=false,state=true},
	% #property{name=disk_reads,title="Reads/s",type=numeric,configurable=false,state=true},
	% #property{name=disk_count,title="Disk Count",type=numeric,configurable=false,state=true}
	].