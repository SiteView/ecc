%% Author: Administrator
%% Created: 2011-11-23
%% Description: TODO: Add description to ecc8_webspereServletSessions_monitor
-module(ecc8_webspereServletSessions_monitor,[BASE]).
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
	THIS:set_attribute(createdSessions,"n/a"),
	THIS:set_attribute(invalidatedSessions,"n/a"),
	THIS:set_attribute(sessionLifeTime,"n/a"),
	THIS:set_attribute(activeSessions,"n/a"),
	THIS:set_attribute(liveSessions,"n/a"),
	{ok,{_,Server}}=THIS:get_property(server), 
    {ok,{_,Url}}=THIS:get_property(url), 
	Paras="_Server="++Server++"$_Url="++Url,
    RL= monitorc2erlang:getrefreshedmonitor(Paras, "ecc8monitor/websphere.dll", "ServletSessions"),
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
%%"createdSessions=$invalidatedSessions=$sessionLifeTime=$activeSessions=$liveSessions=0$"
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
		"createdSessions" ->
			THIS:set_attribute(createdSessions,V1),
			S++"created Sessions="++V++",";
		 "invalidatedSessions" ->
			THIS:set_attribute(invalidatedSessions,V1),
			S++"invalidated Sessions="++V++",";
		 "sessionLifeTime" ->
			THIS:set_attribute(sessionLifeTime,V1),
			S++"session LifeTime="++V++",";
		 "activeSessions" ->
			THIS:set_attribute(activeSessions,V1),
			S++"active Sessions="++V++",";		
		 "liveSessions" ->
			 THIS:set_attribute(liveSessions,V1),
			 S++"live Sessions="++V
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
			[{createdSessions,'>',90}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{createdSessions,'>',80}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{createdSessions,'<',10}]
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
	#property{name=createdSessions,title="created Sessions",type=numeric,order=99,configurable=false,state=true,baselinable=true},
	#property{name=invalidatedSessions,title="invalidated Sessions",type=numeric,order=100,configurable=false,state=true,baselinable=true},
	#property{name=sessionLifeTime,title="session LifeTime",type=numeric,order=101,configurable=false,state=true,baselinable=true},
	#property{name=activeSessions,title="active Sessions",type=numeric,order=102,configurable=false,state=true,baselinable=true},
    #property{name=liveSessions,title="live Sessions",type=numeric,order=103,configurable=false,state=true,baselinable=true}
	].