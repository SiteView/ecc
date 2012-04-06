%% Author: Administrator
%% Created: 2011-11-21
%% Description: TODO: Add description to ecc8_oracletablespace_monitor
-module(ecc8_oracletablespace_monitor,[BASE]).
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
	THIS:set_attribute(usedPercent,"n/a"),
	THIS:set_attribute(used,"n/a"),
	THIS:set_attribute(freePercent,"n/a"),
	THIS:set_attribute(freeSpace,"n/a"),
	THIS:set_attribute(total,"n/a"),
       
	{ok,{_,Machine}}=THIS:get_property(machineName), 
    {ok,{_,ServiceName}}=THIS:get_property(serviceName), 
	{ok,{_,UserAccount}}=THIS:get_property(userAccount), 
	{ok,{_,PassWord}}=THIS:get_property(passWord), 
	{ok,{_,TableSpaceName}}=THIS:get_property(tableSpaceName), 
	Paras="_MachineName="++Machine++"$_ServiceName="++ServiceName++"$_UserAccount="
        ++UserAccount++"$_PassWord="++PassWord++"$_TableSpaceName="++TableSpaceName,
    RL= monitorc2erlang:getrefreshedmonitor(Paras, "ecc8monitor/oracle.dll", "OracleDB"),
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
	Tem=case K of
		"Total" ->
			S++"Total Space(MB)="++V++",";
		 "Used" ->
			 S++"Used Space(MB)="++V++",";
		 "UsedPercent" ->
			 S++"Used Percent(%)="++V++",";
		 "FreePercent" ->
			 S++"Free Percent(%)="++V++",";
		 "FreeSpace" ->
			 S++"Free Space(MB)="++V
		end,	
	K1=string:to_lower(string:sub_string(K, 1, 1))++string:sub_string(K, 2, length(K)),
	THIS:set_attribute(list_to_atom(K1),list_to_float(V)),
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
	case proplists:get_value(serviceName,Params) of
		undefined->
			[{serviceName,"odbc data source is null"}];
		[]->
			[{serviceName,"odbc data source is null"}];
		_->
			[]
	end ++ 
		case proplists:get_value(tableSpaceName,Params) of
		undefined->
			[{tableSpaceName,"tableSpaceName is null"}];
		[]->
			[{tableSpaceName,"tableSpaceName is null"}];
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
		serviceName ->
		    Odbcsource= monitorc2erlang:getrefreshedmonitor("","ecc8monitor/oracle.dll","SYSTEMDSN"),
			RL=string:tokens(Odbcsource, "$"),
			buildKeyValue([],RL);
%% 		tableSpaceName ->
%% 			ServiceName=proplists:get_value(serviceName,Params),
%% 			UserAccount=proplists:get_value(userAccount,Params),
%% 			PassWord=proplists:get_value(passWord,Params),
%% 			Param1="_ServiceName="++ServiceName++"$_UserAccount="
%%                     ++UserAccount++"$_PassWord="++PassWord,
%% 			Tablespaces= monitorc2erlang:getrefreshedmonitor(Param1,"ecc8monitor/oracle.dll","OracleTableName"),
%% 			RL=string:tokens(Tablespaces, "$"),
%% 			buildKeyValue([],RL);
		_ ->
			BASE:getScalarValues(Prop, Params)
	end.
buildKeyValue(S,[])->
	S;
buildKeyValue(S,[H|E])->
	[K,V]=string:tokens(H, "="),
	Temp=S++[{K,V}],
	buildKeyValue(Temp,E).
	
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
			[{usedPercent,'>',90}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{usedPercent,'>',80}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{usedPercent,'>=',0}]
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
	#property{name=serviceName,title="odbc data source",type=scalar,editable=true,order=2,description="odbc data source"},
	#property{name=userAccount,title="user Account",type=text,editable=true,order=3,description="userAccount"},
	#property{name=passWord,title="Pass Word",type=password,editable=true,order=4,description="PassWord"},
%% 	#property{name=tableSpaceName,title="Table Space Name",type=scalar,editable=true,order=5,description="Table Space Name"},
	#property{name=tableSpaceName,title="Table Space Name",type=text,editable=true,order=5,description="Table Space Name"},
	#property{name=usedPercent,title="UsedPercent",type=numeric,order=99,configurable=false,state=true,baselinable=true},
	#property{name=used,title="Used",type=numeric,order=100,configurable=false,state=true,baselinable=true},
	#property{name=freePercent,title="Free Percent",type=numeric,order=101,configurable=false,state=true,baselinable=true},
	#property{name=freeSpace,title="Free Space",type=numeric,order=102,configurable=false,state=true,baselinable=true},
	#property{name=total,title="Total",type=numeric,order=103,configurable=false,state=true,baselinable=true}
	].



