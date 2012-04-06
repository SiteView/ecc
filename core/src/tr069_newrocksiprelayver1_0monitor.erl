-module(tr069_newrocksiprelayver1_0monitor,[BASE]).
-extends(tr069_newrocksiprelayver1base).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

-define(MAX,17).

new() ->
    Obj = tr069_newrocksiprelayver1base:new(),
	{?MODULE,Obj}.
	
	
get_template_property() ->
    BASE:get_template_property().	
	
get_classifier(error)->  
	Cls = case THIS:get_property(error_classifier) of
				{ok,{error_classifier,Classifier}}->
					Classifier;
				_->
					[{countersInError,'>',0}]
			end,
	io:format("Cls~p~n",[Cls]),
	if      	
		length(Cls)<17->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,?MAX - length(Cls)));
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
		length(Cls)<17->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,?MAX - length(Cls)));
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
		length(Cls)<17->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,?MAX - length(Cls)));
		true ->
			Cls
	end.