-module(interface_use_rate_monitor,[BASE]).
-compile(export_all).
-extends(browsable_base).

-include("monitor.hrl").
-include("monitor_template.hrl").

new()->
    Obj = browsable_base:new(),
    {?MODULE,Obj}.

update()->
	Info = case THIS:get_property(browse) of
		{ok,{_,Counters}} ->			
			Data = network_flow:get_interface_use_data(Counters),
			Infos = network_flow:get_interface_use_rate_desc(Data),
			[THIS:set_attribute(ID,Value) || {ID,_Device,Value} <- Data],
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
	Cls;

get_classifier(warning)->
    Cls = case THIS:get_property(warning_classifier) of
	      {ok,{warning_classifier,Classifier}} ->
		      Classifier;
	      _->
		      [{'contersInError','>',0}]
	end,
	Cls;

get_classifier(good)->
    Cls = case THIS:get_property(good_classifier) of
	 	  {ok,{good_classifier,Classifier}} ->
	          Classifier;
	 	  _->
	          [{'contersInError','==',0}]
    end,
    Cls.

getBrowseData(_)->
    nnm_monitor_util:get_devices().
    
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