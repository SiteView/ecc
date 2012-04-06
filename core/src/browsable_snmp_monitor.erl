%% ---
%% browsable_snmp_monitor
%%
%%---
-module(browsable_snmp_monitor,[BASE]).
-extends(browsable_snmp_base).
-compile(export_all).
-export([new/0, getScalarValues/2, get_classifier/1]).

%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for browsable snmp monitor
new()->
	Base = browsable_snmp_base:new(),
	{?MODULE,Base}.

%% @spec getScalarValues(Prop,Params)-> Result
%% Result = list()
%% @doc get version list, oid list, percentageBase list, scale list for ui's combobox
getScalarValues(Prop,Params)->
	case Prop of
		mibfile->
			Mibs = browsable_mib:getCompiledMIBs(),			
			[{X,X}||X<-Mibs];
		_->			
			BASE:getScalarValues(Prop,Params)
	end.

%% @spec get_classifier(error) -> List
%% List = [Tuple]
%% Tule = {status, Logic, Value}
%% Logic = '!=' | '==' | '>' | '<' | 'contain'
%% Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error)->
	
	Cls = case THIS:get_property(error_classifier) of
				{ok,{error_classifier,Classifier}}->
					Classifier;
				_->
					[{countersInError,'>',0}]
			end,
	if 
		length(Cls) < 10 ->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end;
get_classifier(warning)->
	Cls =case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'>',0}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end;
get_classifier(good)->
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'==',0}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end.
