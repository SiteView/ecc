%% Author: Administrator
%% Created: 2011-11-22
%% Description: TODO: Add description to ecc8_oracleinfo_monitor
-module(ecc8_oracleinfo_monitor,[BASE]).
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
	THIS:set_attribute(cursor,"n/a"),
	THIS:set_attribute(session,"n/a"),
	THIS:set_attribute(transaction,"n/a"),
	THIS:set_attribute(lock,"n/a"),
	THIS:set_attribute(deadLock,"n/a"),
    THIS:set_attribute(bufHitRate,"n/a"),
	THIS:set_attribute(libHitRate,"n/a"),
	THIS:set_attribute(time,"n/a"),
	{ok,{_,Machine}}=THIS:get_property(machineName), 
    {ok,{_,ServiceName}}=THIS:get_property(serviceName), 
	{ok,{_,UserAccount}}=THIS:get_property(userAccount), 
	{ok,{_,PassWord}}=THIS:get_property(passWord), 
	{ok,{_,MachStr}}=THIS:get_property(machStr), 
	Paras="_MachineName="++Machine++"$_ServiceName="++ServiceName++"$_UserAccount="
        ++UserAccount++"$_PassWord="++PassWord++"$_MachStr="++MachStr,
    RL= monitorc2erlang:getrefreshedmonitor(Paras, "ecc8monitor/oracle.dll", "GetOracleInfo"),
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
%%"SV_Cursor=2$SV_Session=8$SV_Transaction=0$SV_Lock=0$SV_DeadLock=0$SV_BufHitRate=94.457527$SV_LibHitRate=99.928757$SV_ti
%% me=2$"
%% 
updatevalues([H|E],S)->
	[K,V]=string:tokens(H, "="),
	Tem=case K of
		"SV_Cursor" ->
			S++"Cursor Count="++V++",";
		 "SV_Session" ->
			 S++"Session Count="++V++",";
		 "SV_Transaction" ->
			 S++"Transaction Count/s="++V++",";
		 "SV_Lock" ->
			 S++"Lock count="++V++",";
		 "SV_DeadLock" ->
			 S++"DeadLock count="++V++",";
		 "SV_BufHitRate" ->
			 S++"Buf Hit Rate(%)="++V++",";
		 "SV_LibHitRate" ->
			 S++"Lib Hit Rate(%)="++V++",";
		 "SV_time" ->
			 S++"monitor used time(s)="++V
		end,	
	K1=string:to_lower(string:sub_string(K, 4, 4))++string:sub_string(K, 5, length(K)),
	V1=try
		   list_to_integer(V) 
	   catch _:_ ->
				 try
					 list_to_float(V)
				 catch _:_ ->
						   V
				 end
	   end,   
	THIS:set_attribute(list_to_atom(K1),V1),
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
			[{cursor,'>',90}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{cursor,'>',80}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{cursor,'>=',0}]
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
	#property{name=machStr,title="Mach Str",type=text,editable=true,order=5,description="Mach Str"},
	#property{name=cursor,title="Cursor Count",type=numeric,order=99,configurable=false,state=true,baselinable=true},
	#property{name=session,title="Session Count",type=numeric,order=100,configurable=false,state=true,baselinable=true},
	#property{name=transaction,title="Transaction Count/s",type=numeric,order=101,configurable=false,state=true,baselinable=true},
	#property{name=lock,title="Lock count",type=numeric,order=102,configurable=false,state=true,baselinable=true},
	#property{name=deadLock,title="DeadLock count",type=numeric,order=103,configurable=false,state=true,baselinable=true},
	#property{name=bufHitRate,title="Buf Hit Rate(%)",type=numeric,order=104,configurable=false,state=true,baselinable=true},
	#property{name=libHitRate,title="Lib Hit Rate(%)",type=numeric,order=105,configurable=false,state=true,baselinable=true},
	#property{name=time,title="monitor used time(s)",type=numeric,order=106,configurable=false,state=true,baselinable=true}
	].

