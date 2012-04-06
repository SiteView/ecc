%% Author: Administrator
%% Created: 2011-11-22
%% Description: TODO: Add description to ecc8_MQSeries_monitor
-module(ecc8_MQSeries_monitor,[BASE]).
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
	THIS:set_attribute(szCurrentDepth,"n/a"),
	THIS:set_attribute(szMaximumDepth,"n/a"),
       
	{ok,{_,Machine}}=THIS:get_property(machineName), 
    {ok,{_,Login}}=THIS:get_property(login), 
	{ok,{_,Password}}=THIS:get_property(password), 
	{ok,{_,MQQueueManager}}=THIS:get_property(mQQueueManager), 
	{ok,{_,MQQueueName}}=THIS:get_property(mQQueueName), 
	Paras="_MachineName="++Machine++"$_login="++Login++"$_password="
        ++Password++"$_MQQueueManager="++MQQueueManager++"$_MQQueueName="++MQQueueName,
    RL= monitorc2erlang:getrefreshedmonitor(Paras, "ecc8monitor/ibmmqseries.dll", "GetMQInfo"),
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
%%"Total=25.000000$Used=0.062500$UsedPercent=0.250000$FreePercent=99.750000$FreeSpace=24.937500$"
%% 
updatevalues([H|E],S)->
	[K,V]=string:tokens(H, "="),
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
		"szCurrentDepth" ->
			THIS:set_attribute(szCurrentDepth,V1),
			S++"Current Depth="++V++",";
		 "szMaximumDepth" ->
			 THIS:set_attribute(szMaximumDepth,V1),
			 S++"Maximum Depth="++V
		end,		
	updatevalues(E,Tem).
	
verify(Params)->
	Errs =
	case proplists:get_value(machineName,Params) of
		undefined->
			[{machineName,"machineName is null"}];
		[]->
			[{machineName,"machineName is null"}];
		_->
			[]
	end ++
	case proplists:get_value(mQQueueManager,Params) of
		undefined->
			[{mQQueueManager,"MQ Queue Manager is null"}];
		[]->
			[{mQQueueManager,"MQ Queue Manager is null"}];
		_->
			[]
	end ++ 
	case proplists:get_value(mQQueueName,Params) of
		undefined->
			[{mQQueueName,"MQ Queue Name is null"}];
		[]->
			[{mQQueueName,"MQ Queue Name is null"}];
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
			[{szCurrentDepth,'>',100}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{szCurrentDepth,'>',50}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{szCurrentDepth,'<',10}]
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
	#property{name=machineName,title="Machine Name",type=text,editable=true,order=1,description="Machine Name"},
	#property{name=login,title="user login",type=text,editable=true,order=3,description="user login"},
	#property{name=password,title="Pass Word",type=password,editable=true,order=4,description="PassWord"},
	#property{name=mQQueueManager,title="MQ Queue Manager",type=text,editable=true,order=5,description="MQ Queue Manager"},
	#property{name=mQQueueName,title="MQ Queue Name",type=text,editable=true,order=6,description="MQ Queue Name"},
	#property{name=szCurrentDepth,title="Current Depth",type=numeric,order=99,configurable=false,state=true,baselinable=true},
	#property{name=szMaximumDepth,title="Maximum Depth",type=numeric,order=100,configurable=false,state=true,baselinable=true}
	].




