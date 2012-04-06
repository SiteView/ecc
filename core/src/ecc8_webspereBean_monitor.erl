%% Author: Administrator
%% Created: 2011-11-23
%% Description: TODO: Add description to ecc8_webspereBean_monitor
-module(ecc8_webspereBean_monitor,[BASE]).
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
	THIS:set_attribute(creates,"n/a"),
	THIS:set_attribute(removes,"n/a"),
	THIS:set_attribute(activates,"n/a"),
	THIS:set_attribute(passivates,"n/a"),
	THIS:set_attribute(instantiates,"n/a"),
	THIS:set_attribute(destroys,"n/a"),
	THIS:set_attribute(loads,"n/a"),
	THIS:set_attribute(stores,"n/a"),
	THIS:set_attribute(concurrentActives,"n/a"),
	THIS:set_attribute(concurrentLives,"n/a"),
	THIS:set_attribute(totalMethodCalls,"n/a"),
	THIS:set_attribute(avgMethodRt,"n/a"),
	THIS:set_attribute(avgCreateTime,"n/a"),
	THIS:set_attribute(avgRemoveTime,"n/a"),
	THIS:set_attribute(activeMethods,"n/a"),
	THIS:set_attribute(getsFromPool,"n/a"),
	THIS:set_attribute(getsFound,"n/a"),
	THIS:set_attribute(returnsToPool,"n/a"),
	THIS:set_attribute(returnsDiscarded,"n/a"),
	THIS:set_attribute(drainsFromPool,"n/a"),
	THIS:set_attribute(avgDrainSize,"n/a"),
	THIS:set_attribute(poolSize,"n/a"),
       
	{ok,{_,Server}}=THIS:get_property(server), 
    {ok,{_,Url}}=THIS:get_property(url), 
	Paras="_Server="++Server++"$_Url="++Url,
    RL= monitorc2erlang:getrefreshedmonitor(Paras, "ecc8monitor/websphere.dll", "Bean"),
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
%%"globalTransBegun=$globalTransInvolved=$localTransBegun=$activeGlobalTrans=$=$=$=
%% $=$=$=$=
%% $=$=$=$=$=$=$=$=$"
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
		"creates" ->
			THIS:set_attribute(creates,V1),
			S++"creates="++V++",";
		 "removes" ->
			THIS:set_attribute(removes,V1),
			S++"removes="++V++",";
		 "activates" ->
			THIS:set_attribute(activates,V1),
			S++"activates="++V++",";
		 "passivates" ->
			THIS:set_attribute(passivates,V1),
			S++"passivates="++V++",";	
		 "instantiates" ->
			THIS:set_attribute(instantiates,V1),
			S++"instantiates="++V++",";	
		 "destroys" ->
			THIS:set_attribute(destroys,V1),
			S++"destroys="++V++",";	
		 "loads" ->
			THIS:set_attribute(loads,V1),
			S++"loads="++V++",";	
		 "stores" ->
			THIS:set_attribute(stores,V1),
			S++"stores="++V++",";	
		 "concurrentActives" ->
			THIS:set_attribute(concurrentActives,V1),
			S++"concurrent Actives="++V++",";	
		 "concurrentLives" ->
			THIS:set_attribute(concurrentLives,V1),
			S++"concurrent Lives="++V++",";
		 "totalMethodCalls" ->
			THIS:set_attribute(totalMethodCalls,V1),
			S++"total Method Calls="++V++",";
		 "avgMethodRt" ->
			THIS:set_attribute(avgMethodRt,V1),
			S++"avgMethodRt="++V++",";
		 "avgCreateTime" ->
			THIS:set_attribute(avgCreateTime,V1),
			S++"avgCreateTime="++V++",";	
		 "avgRemoveTime" ->
			THIS:set_attribute(avgRemoveTime,V1),
			S++"avg Remove Time="++V++",";	
		 "activeMethods" ->
			THIS:set_attribute(activeMethods,V1),
			S++"active Methods="++V++",";	
		 "getsFromPool" ->
			THIS:set_attribute(getsFromPool,V1),
			S++"gets From Pool="++V++",";	
		 "getsFound" ->
			THIS:set_attribute(getsFound,V1),
			S++"gets Found="++V++",";	
		 "returnsToPool" ->
			THIS:set_attribute(returnsToPool,V1),
			S++"returns To Pool="++V++",";	
		 "returnsDiscarded" ->
			THIS:set_attribute(returnsDiscarded,V1),
			S++"returns Discarded="++V++",";	
		 "drainsFromPool" ->
			THIS:set_attribute(drainsFromPool,V1),
			S++"drains From Pool="++V++",";	
		 "avgDrainSize" ->
			THIS:set_attribute(avgDrainSize,V1),
			S++"avg Drain Size="++V++",";	
		 "poolSize" ->
			 THIS:set_attribute(poolSize,V1),
			 S++"pool Size="++V
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
			[{creates,'>',90}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{creates,'>',80}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{creates,'<',10}]
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
	#property{name=creates,title="creates",type=numeric,order=99,configurable=false,state=true,baselinable=true},
	#property{name=removes,title="removes",type=numeric,order=100,configurable=false,state=true,baselinable=true},
	#property{name=activates,title="activates",type=numeric,order=101,configurable=false,state=true,baselinable=true},
	#property{name=passivates,title="passivates",type=numeric,order=102,configurable=false,state=true,baselinable=true},
    #property{name=instantiates,title="instantiates",type=numeric,order=103,configurable=false,state=true,baselinable=true},
	#property{name=destroys,title="destroys",type=numeric,order=104,configurable=false,state=true,baselinable=true},
	#property{name=loads,title="loads",type=numeric,order=105,configurable=false,state=true,baselinable=true},
	#property{name=stores,title="stores",type=numeric,order=106,configurable=false,state=true,baselinable=true},
	#property{name=concurrentActives,title="concurrent Actives",type=numeric,order=107,configurable=false,state=true,baselinable=true},
	#property{name=concurrentLives,title="concurrent Lives",type=numeric,order=108,configurable=false,state=true,baselinable=true},
	#property{name=totalMethodCalls,title="total Method Calls",type=numeric,order=109,configurable=false,state=true,baselinable=true},
	#property{name=avgMethodRt,title="avgMethodRt",type=numeric,order=110,configurable=false,state=true,baselinable=true},
	#property{name=avgCreateTime,title="avg Create Time",type=numeric,order=111,configurable=false,state=true,baselinable=true},
	#property{name=avgRemoveTime,title="avg Remove Time",type=numeric,order=112,configurable=false,state=true,baselinable=true},
	#property{name=activeMethods,title="active Methods",type=numeric,order=113,configurable=false,state=true,baselinable=true},
	#property{name=getsFromPool,title="gets From Pool",type=numeric,order=114,configurable=false,state=true,baselinable=true},
	#property{name=getsFound,title="gets Found",type=numeric,order=115,configurable=false,state=true,baselinable=true},
	#property{name=returnsToPool,title="returns To Pool",type=numeric,order=116,configurable=false,state=true,baselinable=true},
	#property{name=returnsDiscarded,title="returns Discarded",type=numeric,order=117,configurable=false,state=true,baselinable=true},
	#property{name=drainsFromPool,title="drains From Pool",type=numeric,order=118,configurable=false,state=true,baselinable=true},
	#property{name=avgDrainSize,title="avg Drain Size",type=numeric,order=119,configurable=false,state=true,baselinable=true},
	#property{name=poolSize,title="pool Size",type=numeric,order=120,configurable=false,state=true,baselinable=true}
	].


