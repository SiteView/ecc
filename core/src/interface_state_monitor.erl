-module(interface_state_monitor,[BASE]).
-compile(export_all).
-extends(browsable_base). 

-include("monitor.hrl").
-include("monitor_template.hrl").

-define(MaxCounters,10).

new() ->
    Base = browsable_base:new(),
    {?MODULE,Base}.

update() ->
    Info = case THIS:get_property(browse) of
	       {ok,{_,Counters}} ->		  
			   FinalCounters = nnm_monitor_util:filter_device_ports(Counters),		  
			   Data = network_flow:get_interface_state_data(FinalCounters),
			   set_counters(Data),
			   %io:format("Data:~p~n",[Data]),
			   Infos = network_flow:get_interface_state_desc(Data),	
			   %io:format("Infos:~p~n",[Infos]),	  		   
			   THIS:set_attribute(?STATE_STRING,Infos);
	       _ ->
		   	   []
	   end,
    Info.

set_counters(Data) ->
    [THIS:set_attribute(Id,State) || {Id,_Text,State} <- Data].

get_classifier(error)->
    Cls = case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}} ->		  
			Classifier;
		_->
			[{'contersInError','>',0}]
	end,
	nnm_monitor_util:build_classifier(Cls,?MaxCounters);

get_classifier(warning)->
    Cls = case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}} ->
			Classifier;
		_->
			[{'contersInError','>',0}]
	end,
	nnm_monitor_util:build_classifier(Cls,?MaxCounters);

get_classifier(good)->
    Cls = case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}} ->
		    Classifier;
		_->
		    [{'contersInError','>',0}]
    end,
    nnm_monitor_util:build_classifier(Cls,?MaxCounters).

getBrowseData(_Params)->
    nnm_monitor_util:get_counters().

verify(Params)->
    Errs=case BASE:verify(Params) of
		{error,Be}->
		    Be;
		_->
		    []
    end,
    if	length(Errs)>0 
		->
		    {error,Errs};
		true ->
		    {ok,""}
    end.