%%
%% Agent Monitor
%%

%% @author lianbing.wang@dragonflow.com
%% @copyright 2010 siteview
%% @version 1.0
%% @doc agent monitor
-module(agent_monitor,[BASE]).
-extends(browsable_base).
-compile(export_all).

-include("monitor.hrl").
-include("monitor_template.hrl").

new() ->
    Base = browsable_base:new(),    
    {?MODULE,Base}.

update() ->
    {ok,{_,AgentIp}} = THIS:get_property(agentIpAddress),
    {ok,{_,AgentPort}} = THIS:get_property(agentPort),
    case agent:test(AgentIp,AgentPort) of
        true ->	    	    
	    	Info = case THIS:get_property(browse) of
				{ok,{_,Browse}} -> 
		    		Counters = lists:map(fun({_ID,_}) -> _ID end,Browse),
		    		Infos = agent:get_info(AgentIp,AgentPort,Counters),
		    		set_counter_val(Counters,Infos),
		    		THIS:set_attribute(countersInError,length(agent:get_errors(Infos))),
		    		%io:format("~p~n",[output(Browse,Infos)]),
		    		THIS:set_attribute(?STATE_STRING,output(Browse,Infos));
				_ ->
		    		[]
	    	end,
	    Info;
	_ -> 
	    THIS:set_attribute(countersInError,1),
	    %%THIS:set_attribute(?NO_DATA,true),
	    THIS:set_attribute(?CATEGORY,error),
	    THIS:set_attribute(status,"error"),
	    THIS:set_attribute(?STATE_STRING,"Agent not available")	
    end.

output(Browse,Infos)->
    OkValues = [KeyValues || {ok,[KeyValues]} <- Infos],
    Content = [{SelectedCounterText,_Value} || {Counter,_Value} <-OkValues,{SelectedCounter,SelectedCounterText} <-Browse,Counter == list_to_atom(SelectedCounter)],    
    build(Content).

build([])->
    [];
build([{Text,Value}|T]) ->
    lists:concat([nnm_monitor_util:to_list(Text) ++ "=" ++ nnm_monitor_util:to_list(Value)] ++ [","++ nnm_monitor_util:to_list(_Text) ++ " = " ++ nnm_monitor_util:to_list(_Value) || {_Text,_Value} <- T]).   

set_counter_val([],_) ->
    [];
set_counter_val(Counters,Infos) ->
    List = [KeyValues || {ok,[KeyValues]} <- Infos],
    OkCounters = [ atom_to_list(OkCounter) || {OkCounter,_Value} <- List],
    ErrorCounters =  Counters -- OkCounters,
    [THIS:set_attribute(C,"") || C <- ErrorCounters],
    [THIS:set_attribute(atom_to_list(C),V) || {C,V} <- List].

get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'>',0}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'>',0}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier; 
		_->
			[{countersInError,'==',0}]
	end.

get_template_property() ->
    BASE:get_template_property() ++ 
    [
    	#property{name=agentIpAddress,title="Agent IP Address",type=text,order=1,description = "The Agent IP Address"},
    	#property{name=agentPort,title="Agent Port",default=3344,type=numeric,order=2,description = "The Agent Port"}
    ].   

getBrowseData(Params)->
    [Address,Port] = [proplists:get_value(agentIpAddress,Params),proplists:get_value(agentPort,Params)],
    case lists:any(fun(V)-> V==undefined end,[Address,Port]) of
    	true ->
	    	[];
    	false->
	    	agent:get_counters(Address,Port)
    end.

verify(Params)->
    Errs =
	case proplists:get_value(agentIpAddress,Params) of
		Address when Address==undefined orelse Address==""->
			[{agentIpAddress,"The agent IP address cannot be blank."}];
		_->
			[]
	end ++
	case proplists:get_value(agentPort,Params) of
	    Port when Port==undefined orelse Port==""->
			[{agentPort,"The agent port cannot be blank."}];
	    _->
			[]
	end ++
    case BASE:verify(Params) of
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

defaultTitle(Params)->
	BASE:defaultTitle(Params) ++ "(" ++ case proplists:get_value(agentIpAddress,Params) of undefined->"";V->V end ++ ")".
    


