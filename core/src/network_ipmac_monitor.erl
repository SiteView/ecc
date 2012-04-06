-module(network_ipmac_monitor,[BASE]).
-extends(atomic_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

new()->
    Obj = atomic_monitor:new(),
    {?MODULE,Obj}.

update() ->
    Data = network_flow:get_ipmac_abnormal_data(),
    %io:format("Data:~p~n",[length(Data)]),
    THIS:set_attribute(abnormalCount,length(Data)),
	Infos = network_flow:get_ipmac_data_desc(Data),
	%io:format("Infos:~p~n",[Infos]),
	THIS:set_attribute(?STATE_STRING,Infos).    

get_classifier(error)->
    case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
		    Classifier;
		_->
		    [{'abnormalCount', '>', 0}]
    end;
get_classifier(warning)->
    case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
		    Classifier;
		_->
		    [{'abnormalCount', '>', 0}]
    end;
get_classifier(good)->
    case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
		    Classifier;
		_->
		    [{'abnormalCount', '==', 0}]
    end.

get_template_property()->
    BASE:get_template_property() ++ 
	[
	 #property{name=abnormalCount,title="Abnormal Count",type=numeric,editable=true,configurable=false,state=true,order=100}
	].
