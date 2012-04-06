%% Author: Administrator
%% Created: 2011-11-22
%% Description: TODO: Add description to ecc8_webspereWebApp_monitor
-module(ecc8_webspereWebApp_monitor,[BASE]).
-extends(atomic_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").
new()->
	Base = atomic_monitor:new(),
	{?MODULE,Base}.

%% @spec update() -> Result
%% Result = term()
%% @doc update is the run function called by schedule to test the  directory monitor
update() ->    
	THIS:set_attribute(numLoadedServlets,"n/a"),
	THIS:set_attribute(numReloads,"n/a"),
	THIS:set_attribute(totalRequests,"n/a"),
	THIS:set_attribute(concurrentRequests,"n/a"),
	THIS:set_attribute(responseTime,"n/a"),
	THIS:set_attribute(numErrors,"n/a"),
       
	{ok,{_,Server}}=THIS:get_property(server), 
    {ok,{_,Url}}=THIS:get_property(url), 
	Paras="_Server="++Server++"$_Url="++Url,
    RL= monitorc2erlang:getrefreshedmonitor(Paras, "ecc8monitor/websphere.dll", "WebApp"),
	LL=string:tokens(RL, "$"),
	case length(LL)>1 of
		true ->
			updatevalues(LL,"");
		_ ->
			THIS:set_attribute(?STATE_STRING,RL)
%% 			THIS:set_attribute(?STATE_STRING,iconv:convert("utf-8","gbk",RL))
	end
   . 
updatevalues([],S)->
	THIS:set_attribute(?STATE_STRING,S),
	ok;
%%
%%"numLoadedServlets=$numReloads=$totalRequests=786$concurrentRequests=$responseTime=error data$numErrors=$"
%% 
updatevalues([H|E],S)->
	TVset=string:tokens(H, "="),
	case length(TVset) of
		2 ->
		  	[K,V]=TVset;
		_ ->
			[K]=TVset,
			V="n/a"
	end,
	V1=try
		   list_to_integer(V) 
	   catch _:_ ->
				 try
					 list_to_float(V)
				 catch _:_ ->
						   V
				 end
	   end,   
	Tem=case K of
		"numLoadedServlets" ->
			THIS:set_attribute(numLoadedServlets,V1),
			S++"num Loaded Servlets="++V++",";
		 "numReloads" ->
			THIS:set_attribute(numReloads,V1),
			S++"num Reloads="++V++",";
		 "totalRequests" ->
			THIS:set_attribute(totalRequests,V1),
			S++"total Requests="++V++",";
		 "concurrentRequests" ->
			THIS:set_attribute(concurrentRequests,V1),
			S++"concurrent Requests="++V++",";	
		 "responseTime" ->
			THIS:set_attribute(responseTime,V1),
			S++"response Time="++V++",";	
		 "numErrors" ->
			 THIS:set_attribute(numErrors,V1),
			 S++"numErrors="++V
		end,		
	updatevalues(E,Tem).
	
verify(Params)->
	Errs =
	case proplists:get_value(server,Params) of
		undefined->
			[{server,"machineName is null"}];
		[]->
			[{server,"machineName is null"}];
		_->
			[]
	end ++
	case proplists:get_value(url,Params) of
		undefined->
			[{url,"url is null"}];
		[]->
			[{url,"url is null"}];
		_->
			[]
	end ++ 
	case BASE:verify(Params) of
		{error,Be}->
			Be;
		_->
			[]
	end,
	if
		length(Errs)>0->
			{error,Errs};
		true ->
			{ok,""}
	end.	
	
	
%% @spec get_classifier(error) -> List
%% @type List = [Tuple]
%% @type Tule = {status, Logic, Value}
%% @type Logic = '!=' | '==' | '>' | '<' | 'contain
%% @type Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{numLoadedServlets,'>',90}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{numLoadedServlets,'>',80}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{numLoadedServlets,'<',10}]
	end.
%% @spec getLogProperties(This)->list()
%% @doc get properties need to log
%%
getLogProperties(This)->
	Temp = This:get_template_property(),
    [X#property.name || X<-Temp,X#property.state=:=true].
%%
%% 
get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=server,title="Machine Name",type=text,editable=true,order=1,description="Machine Name"},
	#property{name=url,title="perfservlet url",type=text,editable=true,order=3,description="perfservlet url"},
	#property{name=numLoadedServlets,title="num Loaded Servlets",type=numeric,order=99,configurable=false,state=true,baselinable=true},
	#property{name=numReloads,title="num Reloads",type=numeric,order=100,configurable=false,state=true,baselinable=true},
	#property{name=totalRequests,title="total Requests",type=numeric,order=101,configurable=false,state=true,baselinable=true},
	#property{name=concurrentRequests,title="concurren tRequests",type=numeric,order=102,configurable=false,state=true,baselinable=true},
    #property{name=responseTime,title="response Time",type=numeric,order=103,configurable=false,state=true,baselinable=true},
    #property{name=numErrors,title="num Errors",type=numeric,order=104,configurable=false,state=true,baselinable=true}
	].
