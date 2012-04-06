%% Author: Administrator
%% Created: 2011-11-18
%% Description: TODO: Add description to filtereventlog_monitor
-module(ecc8_filtereventlog_monitor,[BASE]).
-extends(server_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").
new()->
	Base = server_monitor:new(),
	{?MODULE,Base}.

getmhost(Host)->
	string:strip(Host, left, $\\).
%% @spec update() -> Result
%% Result = term()
%% @doc update is the run function called by schedule to test the  directory monitor
update() ->    
	THIS:set_attribute(checkcount,"n/a"),
	THIS:set_attribute(filtercount,"n/a"),
       
	{ok,{_,Machine}}=THIS:get_property(machine), 
    {ok,{_,LogName}}=THIS:get_property(logName), 
	{ok,{_,EventType}}=THIS:get_property(eventType), 
	{ok,{_,CodeList}}=THIS:get_property(codeList), 
	{ok,{_,SourceList}}=THIS:get_property(sourceList), 
    RL=case Machine of
		  ""->
			  Paras="_MachineName=127.0.0.1$_UserAccount=$_PassWord=$"
              ++"_logName="++LogName++"$_eventType="++EventType
	          ++"$_codeList="++CodeList++"$_sourceList="++SourceList,
			  monitorc2erlang:getrefreshedmonitor(Paras, "ecc8monitor/WMIWatcher.dll", "FilterEventLogInfo");
		  _->
			  case machine:getNTMachine(Machine) of
				  []->
					  THIS:set_attribute(?NO_DATA,true),
					  THIS:set_attribute(?CATEGORY,?NO_DATA),
					  THIS:set_attribute(?STATE_STRING,"machine fail!"),
					  [];
				[M|_]->
				 Paras="_MachineName="++getmhost(M#machine.host)++"$_UserAccount="
			     ++M#machine.login++"$_PassWord="++M#machine.passwd
			     ++"_logName="++LogName++"$_eventType="++EventType
	             ++"$_codeList="++CodeList++"$_sourceList="++SourceList,
			    monitorc2erlang:getrefreshedmonitor(Paras, "ecc8monitor/WMIWatcher.dll", "FilterEventLogInfo")
			  end 
	  end,
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
updatevalues([H|E],S)->
	[K,V]=string:tokens(H, "="),
	Tem=case K of
		"checkcount" ->
			S++"check Count="++V++",";
		 "filtercount" ->
			 S++"filter Count="++V
		end,	
	THIS:set_attribute(list_to_atom(K),list_to_integer(V)),
	updatevalues(E,Tem).
	
	
	
	
	
%% @spec getScalarValues(Property,Params) -> ValueList
%% where
%% Property = atom()
%% Params = [{PropertyName,PropertyValue}]
%% PropertyName = atom()
%% PropertyValue = string()
%% ValueList = [{Scalarname,Scalarvalue}]
%% Scalarname = string()
%% Scalarvalue = string()
%% @doc Set scalar properties value.
getScalarValues(Prop,Params)->
%% 	{"4.x","4.x"},{"5.x","5.x"},
	case Prop of
		logName ->
			[{"System Log","System"},{"Application Log","Application"},{"Security Log","Security"},{"DNS Log","DNS"}];
		eventType ->
			[{"All","1"},{"Error","2"},{"Warning","3"},{"Error or Warning","4"},{"Info","5"}];
		_ ->
			BASE:getScalarValues(Prop, Params)
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
			[{checkcount,'>',0}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{checkcount,'>',0}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{checkcount,'==',0}]
	end.
%%
%% 
get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=logName,title="Log Name List",type=scalar,editable=true,order=1,description="log list of windows"},
	#property{name=eventType,title="Windows Event Type",type=scalar,editable=true,order=2,description="Windows Event Type"},
	#property{name=codeList,title="Event ID",type=text,editable=true,order=3,description="Event ID"},
	#property{name=sourceList,title="source List",type=text,editable=true,order=4,description="source List"},
	#property{name=checkcount,title="check Count",type=numeric,order=99,configurable=false,state=true,baselinable=true},
	#property{name=filtercount,title="filter Count",type=numeric,order=100,configurable=false,state=true,baselinable=true}
	].

