%%
%% f5_monitor
%%
%%
-module(f5_monitor,[BASE]).
-extends(browsable_snmp_base).
-compile(export_all).

-include("monitor.hrl").
-include("monitor_template.hrl").
-include_lib("snmp/include/snmp_types.hrl").

-define(F5_MIB,"LOAD-BAL-SYSTEM-MIB.mib").
-define(F5_OID,"LOAD-BAL-SYSTEM-MIB.oid").

new()->
	Obj = browsable_snmp_base:new(),
	{?MODULE,Obj}.


getScalarValues(Prop,Params)->
	case Prop of
		mibfile->
			%%case browsable_mib:containsMIB(?F5_MIB) of
			%%	true->
					[{?F5_MIB,?F5_MIB}];
			%%	false->
			%%		[{"No MIBs Available","No MIBs Available"}]
			%%end;
		_->
			BASE:getScalarValues(Prop,Params)
	end.


get_classifier(error)->
	Count = THIS:getMaxCounter(),
	Cls = case THIS:get_property(error_classifier) of
				{ok,{error_classifier,Classifier}}->
					Classifier;
				_->
					[{countersInError,'>',0}]
			end,
	if 
		length(Cls) < Count ->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,Count - length(Cls)));
		true ->
			Cls
	end;
get_classifier(warning)->
	Count = THIS:getMaxCounter(),
	Cls =case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'>',0}]
	end,
	if 
		length(Cls)< Count->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,Count - length(Cls)));
		true ->
			Cls
	end;
get_classifier(good)->
	Count = THIS:getMaxCounter(),
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'==',0}]
	end,
	if 
		length(Cls)<Count->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,Count - length(Cls)));
		true ->
			Cls
	end.
