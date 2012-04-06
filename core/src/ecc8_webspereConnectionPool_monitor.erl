%% Author: Administrator
%% Created: 2011-11-23
%% Description: TODO: Add description to ecc8_webspereConnectionPool_monitor
-module(ecc8_webspereConnectionPool_monitor,[BASE]).
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
	THIS:set_attribute(numCreates,"n/a"),
	THIS:set_attribute(numDestroys,"n/a"),
	THIS:set_attribute(numAllocates,"n/a"),
	THIS:set_attribute(numReturns,"n/a"),
	THIS:set_attribute(poolSize,"n/a"),
	THIS:set_attribute(concurrentWaiters,"n/a"),
	THIS:set_attribute(avgWaitTime,"n/a"),
	THIS:set_attribute(percentUsed,"n/a"),
	THIS:set_attribute(percentMaxed,"n/a"),
	THIS:set_attribute(prepStmtCacheDiscards,"n/a"),
       
	{ok,{_,Server}}=THIS:get_property(server), 
    {ok,{_,Url}}=THIS:get_property(url), 
	Paras="_Server="++Server++"$_Url="++Url,
    RL= monitorc2erlang:getrefreshedmonitor(Paras, "ecc8monitor/websphere.dll", "ConnectionPool"),
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
%%"numCreates=0$numDestroys=0$numAllocates=$numReturns=$poolSize=0$concurrentWaiters=0$avgWaitTime=error data$percentUsed=
%% 0$percentMaxed=$prepStmtCacheDiscards=$"
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
		"numCreates" ->
			THIS:set_attribute(numCreates,V1),
			S++"connection Creates count="++V++",";
		 "numDestroys" ->
			THIS:set_attribute(numDestroys,V1),
			S++"connection Destroys count="++V++",";
		 "numAllocates" ->
			THIS:set_attribute(numAllocates,V1),
			S++"connection Allocates count="++V++",";
		 "numReturns" ->
			THIS:set_attribute(numReturns,V1),
			S++"connection Returns count="++V++",";	
		 "poolSize" ->
			THIS:set_attribute(poolSize,V1),
			S++"pool Size="++V++",";	
		 "concurrentWaiters" ->
			THIS:set_attribute(concurrentWaiters,V1),
			S++"connection current Waiters count="++V++",";	
		 "avgWaitTime" ->
			THIS:set_attribute(avgWaitTime,V1),
			S++"avg Wait Time="++V++",";	
		 "percentUsed" ->
			THIS:set_attribute(percentUsed,V1),
			S++"percent Used="++V++",";	
		 "percentMaxed" ->
			THIS:set_attribute(percentMaxed,V1),
			S++"percent Maxed="++V++",";	
		 "prepStmtCacheDiscards" ->
			 THIS:set_attribute(prepStmtCacheDiscards,V1),
			 S++"prepStmtCacheDiscards="++V
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
			[{numCreates,'>',90}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{numCreates,'>',80}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{numCreates,'<',10}]
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
	#property{name=numCreates,title="connection Creates count",type=numeric,order=99,configurable=false,state=true,baselinable=true},
	#property{name=numDestroys,title="connection Destroys count",type=numeric,order=100,configurable=false,state=true,baselinable=true},
	#property{name=numAllocates,title="connection Allocates count",type=numeric,order=101,configurable=false,state=true,baselinable=true},
	#property{name=numReturns,title="connection Returns count",type=numeric,order=102,configurable=false,state=true,baselinable=true},
    #property{name=poolSize,title="pool Size",type=numeric,order=103,configurable=false,state=true,baselinable=true},
	#property{name=concurrentWaiters,title="connection current Waiters count",type=numeric,order=104,configurable=false,state=true,baselinable=true},
	#property{name=avgWaitTime,title="avg Wait Time",type=numeric,order=105,configurable=false,state=true,baselinable=true},
	#property{name=percentUsed,title="percent Used",type=numeric,order=106,configurable=false,state=true,baselinable=true},
	#property{name=percentMaxed,title="percent Maxed",type=numeric,order=107,configurable=false,state=true,baselinable=true},
	#property{name=prepStmtCacheDiscards,title="prepStmtCacheDiscards",type=numeric,order=108,configurable=false,state=true,baselinable=true}
	].

