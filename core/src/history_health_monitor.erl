%% @author Cheng kaiyang <kaiyang.cheng@dragonflow.com>
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc History Health Monitor.
%% 
%% Description: Erlang siteview isn't surport this monitor sofar
-module(history_health_monitor,[BASE]).
-extends(healthmonitor_base).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

new()->
	Obj = healthmonitor_base:new(),
	{?MODULE,Obj}.
	
update()->
	try 
        THIS:set_attribute(?STATE_STRING,"This monitor is obsolete"),
		THIS:set_attribute(numerrors,0)
	catch
		error:X->X,
		THIS:set_attribute(?STATE_STRING,httputils:exception_to_String(X))
	end.
	
get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{numerrors,'>',0}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{numerrors,'>',0}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{numerrors,'==',0}]
	end.
	
