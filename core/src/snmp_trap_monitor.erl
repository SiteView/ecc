%% 
%% snmp_trap_monitor
%%
-module(snmp_trap_monitor,[BASE]).
-extends(snmp_trap_base).
-compile(export_all).

-include("monitor.hrl").
-include("monitor_template.hrl").

-export([new/0,update/0,get_classifier/1]).

%% @spec new() -> Obj
%% where
%% Obj = term()
%% @doc create a new instance for snmp trap monitor
new()->
	Obj = snmp_trap_base:new(),
	{?MODULE,Obj}.

%% @spec update() -> Result
%% Result = term()
%% @doc update is the run function called by schedule to test the snmp trap information
update()->
	snmp_ex2_manager:start_link(),
	snmp_ex2_manager:set_trap_handle(THIS),
	BASE:update(),
	ok.

%% @spec get_classifier(error) -> List
%% List = [Tuple]
%% Tule = {status, Logic, Value}
%% Logic = '!=' | '==' | '>' | '<' | 'contain'
%% Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{matchCount,'>',0}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{matchCount,'>',0}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{matchCount,'==',0}]
	end.

