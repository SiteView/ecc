-module(network_frame_monitor,[BASE]).
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
			   Formulas = get_classifier(error)++get_classifier(warning)++get_classifier(good),
			   SelectedFormulas = nnm_monitor_util:unique(nnm_monitor_util:get_selected_formulas(Formulas)),
			   SelectedFormulaTypes = nnm_monitor_util:unique([nnm_monitor_util:get_formula_type(Item) || Item <- SelectedFormulas]),
			   Data = network_flow:get_formulas_data(SelectedFormulaTypes,FinalCounters,frame),
			   %io:format("Data:~p~n",[Data]),
			   Infos = nnm_monitor_util:get_data_desc(Data),
	  		   %io:format("Infos:~p~n",[Infos]),
			   [THIS:set_attribute(Formula,Value) || {Formula,Value,_Unit} <- network_flow:get_formula_value(Data,SelectedFormulas)],
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
			[{'max_total','>',50}]
	end,
    nnm_monitor_util:build_classifier(Cls,?MaxCounters);

get_classifier(warning)->
    Cls = case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}} ->
			Classifier;
		_->
			[{'max_total','>',50}]
	end,
    nnm_monitor_util:build_classifier(Cls,?MaxCounters);

get_classifier(good)->
    Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}} ->
		    Classifier;
		_->
		    [{'max_total','>',20}]
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

get_template_property()->
    BASE:get_template_property() ++ 
	[	 		 
		   #property{name=max_total,title="Max Total",type=numeric,editable=true,configurable=false,state=true,order=100}
		 , #property{name=max_sent,title="Max Sent",type=numeric,editable=true,configurable=false,state=true,order=101}
		 , #property{name=max_received,title="Max Received",type=numeric,editable=true,configurable=false,state=true,order=102}
		 , #property{name=min_total,title="Min Total",type=numeric,editable=true,configurable=false,state=true,order=103}
		 , #property{name=min_sent,title="Min sent",type=numeric,editable=true,configurable=false,state=true,order=104}
		 , #property{name=min_received,title="Min Received",type=numeric,editable=true,configurable=false,state=true,order=105}
		 , #property{name=avg_total,title="Avg Total",type=numeric,editable=true,configurable=false,state=true,order=106}
		 , #property{name=avg_sent,title="Avg sent",type=numeric,editable=true,configurable=false,state=true,order=107}
		 , #property{name=avg_received,title="Avg Received",type=numeric,editable=true,configurable=false,state=true,order=108}
		 , #property{name=sum_total,title="Sum Total",type=numeric,editable=true,configurable=false,state=true,order=109}
		 , #property{name=sum_sent,title="Sum sent",type=numeric,editable=true,configurable=false,state=true,order=110}
		 , #property{name=sum_received,title="Sum Received",type=numeric,editable=true,configurable=false,state=true,order=111}		 
	].
