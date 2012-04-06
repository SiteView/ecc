%% Author: Administrator
%% Created: 2011-11-23
%% Description: TODO: Add description to ecc8_webspereTransaction_monitor
-module(ecc8_webspereTransaction_monitor,[BASE]).
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
	THIS:set_attribute(globalTransBegun,"n/a"),
	THIS:set_attribute(globalTransInvolved,"n/a"),
	THIS:set_attribute(localTransBegun,"n/a"),
	THIS:set_attribute(activeGlobalTrans,"n/a"),
	THIS:set_attribute(activeLocalTrans,"n/a"),
	THIS:set_attribute(globalTranDuration,"n/a"),
	THIS:set_attribute(localTranDuration,"n/a"),
	THIS:set_attribute(globalBeforeCompletionDuration,"n/a"),
	THIS:set_attribute(globalPrepareDuration,"n/a"),
	THIS:set_attribute(globalCommitDuration,"n/a"),
	THIS:set_attribute(localBeforeCompletionDuration,"n/a"),
	THIS:set_attribute(localCommitDuration,"n/a"),
	THIS:set_attribute(numOptimization,"n/a"),
	THIS:set_attribute(globalTransCommitted,"n/a"),
	THIS:set_attribute(localTransCommitted,"n/a"),
	THIS:set_attribute(globalTransRolledBack,"n/a"),
	THIS:set_attribute(localTransRolledBack,"n/a"),
	THIS:set_attribute(globalTransTimeout,"n/a"),
	THIS:set_attribute(localTransTimeout,"n/a"),
       
	{ok,{_,Server}}=THIS:get_property(server), 
    {ok,{_,Url}}=THIS:get_property(url), 
	Paras="_Server="++Server++"$_Url="++Url,
    RL= monitorc2erlang:getrefreshedmonitor(Paras, "ecc8monitor/websphere.dll", "Transaction"),
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
		"globalTransBegun" ->
			THIS:set_attribute(globalTransBegun,V1),
			S++"global Trans Begun="++V++",";
		 "globalTransInvolved" ->
			THIS:set_attribute(globalTransInvolved,V1),
			S++"global Trans Involved="++V++",";
		 "localTransBegun" ->
			THIS:set_attribute(localTransBegun,V1),
			S++"local Trans Begun="++V++",";
		 "activeGlobalTrans" ->
			THIS:set_attribute(activeGlobalTrans,V1),
			S++"active Global Trans="++V++",";	
		 "activeLocalTrans" ->
			THIS:set_attribute(activeLocalTrans,V1),
			S++"active Local Trans="++V++",";	
		 "globalTranDuration" ->
			THIS:set_attribute(globalTranDuration,V1),
			S++"global Tran Duration="++V++",";	
		 "localTranDuration" ->
			THIS:set_attribute(localTranDuration,V1),
			S++"local Tran Duration="++V++",";	
		 "globalBeforeCompletionDuration" ->
			THIS:set_attribute(globalBeforeCompletionDuration,V1),
			S++"global Before Completion Duration="++V++",";	
		 "globalPrepareDuration" ->
			THIS:set_attribute(globalPrepareDuration,V1),
			S++"global Prepare Duration="++V++",";	
		 "globalCommitDuration" ->
			THIS:set_attribute(globalCommitDuration,V1),
			S++"global Commit Duration="++V++",";
		 "localBeforeCompletionDuration" ->
			THIS:set_attribute(localBeforeCompletionDuration,V1),
			S++"local Before Completion Duration="++V++",";
		 "localCommitDuration" ->
			THIS:set_attribute(localCommitDuration,V1),
			S++"local Commit Duration="++V++",";
		 "numOptimization" ->
			THIS:set_attribute(numOptimization,V1),
			S++"num Optimization="++V++",";	
		 "globalTransCommitted" ->
			THIS:set_attribute(globalTransCommitted,V1),
			S++"global Trans Committed="++V++",";	
		 "localTransCommitted" ->
			THIS:set_attribute(localTransCommitted,V1),
			S++"local Trans Committed="++V++",";	
		 "globalTransRolledBack" ->
			THIS:set_attribute(globalTransRolledBack,V1),
			S++"global Trans RolledBack="++V++",";	
		 "localTransRolledBack" ->
			THIS:set_attribute(localTransRolledBack,V1),
			S++"local TransRolledBack="++V++",";	
		 "globalTransTimeout" ->
			THIS:set_attribute(globalTransTimeout,V1),
			S++"global Trans Timeout="++V++",";	
		 "localTransTimeout" ->
			 THIS:set_attribute(localTransTimeout,V1),
			 S++"local Trans Timeout="++V
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
			[{globalTransBegun,'>',90}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{globalTransBegun,'>',80}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{globalTransBegun,'<',10}]
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
	#property{name=globalTransBegun,title="global Trans Begun",type=numeric,order=99,configurable=false,state=true,baselinable=true},
	#property{name=globalTransInvolved,title="global Trans Involved",type=numeric,order=100,configurable=false,state=true,baselinable=true},
	#property{name=localTransBegun,title="local Trans Begun",type=numeric,order=101,configurable=false,state=true,baselinable=true},
	#property{name=activeGlobalTrans,title="active Global Trans",type=numeric,order=102,configurable=false,state=true,baselinable=true},
    #property{name=activeLocalTrans,title="active Local Trans",type=numeric,order=103,configurable=false,state=true,baselinable=true},
	#property{name=globalTranDuration,title="global Tran Duration",type=numeric,order=104,configurable=false,state=true,baselinable=true},
	#property{name=localTranDuration,title="local Tran Duration",type=numeric,order=105,configurable=false,state=true,baselinable=true},
	#property{name=globalBeforeCompletionDuration,title="global Before CompletionD uration",type=numeric,order=106,configurable=false,state=true,baselinable=true},
	#property{name=globalPrepareDuration,title="global Prepare Duration",type=numeric,order=107,configurable=false,state=true,baselinable=true},
	#property{name=globalCommitDuration,title="global Commit Duration",type=numeric,order=108,configurable=false,state=true,baselinable=true},
	#property{name=localBeforeCompletionDuration,title="local Before Completion Duration",type=numeric,order=109,configurable=false,state=true,baselinable=true},
	#property{name=localCommitDuration,title="local Commit Duration",type=numeric,order=110,configurable=false,state=true,baselinable=true},
	#property{name=numOptimization,title="num Optimization",type=numeric,order=111,configurable=false,state=true,baselinable=true},
	#property{name=globalTransCommitted,title="global Trans Committed",type=numeric,order=112,configurable=false,state=true,baselinable=true},
	#property{name=localTransCommitted,title="local Trans Committed",type=numeric,order=113,configurable=false,state=true,baselinable=true},
	#property{name=globalTransRolledBack,title="global Trans RolledBack",type=numeric,order=114,configurable=false,state=true,baselinable=true},
	#property{name=localTransRolledBack,title="local Trans RolledBack",type=numeric,order=115,configurable=false,state=true,baselinable=true},
	#property{name=globalTransTimeout,title="global Trans Timeout",type=numeric,order=116,configurable=false,state=true,baselinable=true},
	#property{name=localTransTimeout,title="local Trans Timeout",type=numeric,order=117,configurable=false,state=true,baselinable=true}
	].
