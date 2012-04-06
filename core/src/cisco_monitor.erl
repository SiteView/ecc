%%
%% cisco_monitor
%%
%%
-module(cisco_monitor,[BASE]).
-extends(browsable_snmp_base).
-compile(export_all).

-include("monitor.hrl").
-include("monitor_template.hrl").

-define(CISCOWORKS_MIB,"CISCOWORKS-MIB.mib").

new()->
	Obj = browsable_snmp_base:new(),
	{?MODULE,Obj}.


getScalarValues(Prop,Params)->
	case Prop of
		mibfile->
			case browsable_mib:containsMIB(?CISCOWORKS_MIB) of
				true->
					[{"CISCOWORKS-MIB.mib","CISCOWORKS-MIB.mib"}];
				false->
					[{"No MIBs Available","No MIBs Available"}]
			end;
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
