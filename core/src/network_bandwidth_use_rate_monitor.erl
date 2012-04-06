-module(network_bandwidth_use_rate_monitor,[BASE]).
-compile(export_all).
-extends(browsable_base).

-include("monitor.hrl").
-include("monitor_template.hrl").

-define(MaxCounters,10).

new()->
    Obj = browsable_base:new(),
    {?MODULE,Obj}.

update()->
    Info = case THIS:get_property(browse) of
	       {ok,{_,Counters}} ->		   
			   FinalCounters = nnm_monitor_util:filter_device_ports(Counters),
			   Data = network_flow:get_bandwidth_rate_data(FinalCounters),
			   %io:format("Network Bandwidth Use Rate Data:~p~n",[Data]),
			   Infos = network_flow:get_bandwidth_rate_data_desc(Data),
	  		   %io:format("Network Bandwidth Use Rate Infos:~p~n",[Infos]),
			   [THIS:set_attribute(ID,Value) || {ID,_,Value} <- Data],
			   THIS:set_attribute(?STATE_STRING,Infos);
	       _ ->
		   	   []
	   end,
    Info.

get_classifier(error)->
    Cls = case THIS:get_property(error_classifier) of
	      {ok,{error_classifier,Classifier}} ->		  
		  	  Classifier;
	      _->
		  	  [{'contersInError','>',0}]
	end,
    nnm_monitor_util:build_classifier(Cls,?MaxCounters);

get_classifier(warning)->
    Cls = case THIS:get_property(warning_classifier) of
	      {ok,{warning_classifier,Classifier}} ->
		      Classifier;
	      _->
		      [{'contersInError','>',0}]
	end,
    nnm_monitor_util:build_classifier(Cls,?MaxCounters);

get_classifier(good)->
    Cls = case THIS:get_property(good_classifier) of
	 	  {ok,{good_classifier,Classifier}} ->
	          Classifier;
	 	  _->
	          [{'contersInError','==',0}]
    end,
    nnm_monitor_util:build_classifier(Cls,?MaxCounters).

getBrowseData(_)->
    nnm_monitor_util:get_counters().

verify(Params)->
    Errs=case BASE:verify(Params) of
		{error,Be}->
		    Be;
		_->
		    []
    end,
    if length(Errs)>0 
		->
		    {error,Errs};
		true ->
		    {ok,""}
    end.
