%% Author: Administrator
%% Created: 2011-11-22
%% Description: TODO: Add description to ecc8_oracleperfmon_monitor
-module(ecc8_oracleperfmon_monitor,[BASE]).
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
	THIS:set_attribute(procMemPer,"n/a"),
	THIS:set_attribute(totalPhysicalIO,"n/a"),
	THIS:set_attribute(physicalIO1m,"n/a"),
	THIS:set_attribute(totalLogicalIO,"n/a"),
	THIS:set_attribute(logicalIO1m,"n/a"),
    THIS:set_attribute(sortsDisk,"n/a"),
	THIS:set_attribute(sortsDisk1m,"n/a"),
	THIS:set_attribute(sortsMem,"n/a"),
	THIS:set_attribute(sortsMem1m,"n/a"),
	THIS:set_attribute(userCommits,"n/a"),
	THIS:set_attribute(userCommits1m,"n/a"),
	THIS:set_attribute(userRollbacks,"n/a"),
	THIS:set_attribute(userRollbacks1m,"n/a"),
	{ok,{_,Machine}}=THIS:get_property(machineName), 
    {ok,{_,ServiceName}}=THIS:get_property(serviceName), 
	{ok,{_,UserAccount}}=THIS:get_property(userAccount), 
	{ok,{_,PassWord}}=THIS:get_property(passWord), 
	{ok,{_,MachStr}}=THIS:get_property(machStr), 
	{ok,{_,ProcessName}}=THIS:get_property(processName), 
	Paras="_MachineName="++Machine++"$_ServiceName="++ServiceName++"$_UserAccount="
        ++UserAccount++"$_PassWord="++PassWord++"$_MachStr="++MachStr++"$_ProcessName="++ProcessName,
    RL= monitorc2erlang:getrefreshedmonitor(Paras, "ecc8monitor/oracle.dll", "GetProcMemUsePercent"),
	LL=string:tokens(RL, "$"),
	case length(LL)>1 of
		true ->
			updatevalues(LL,"");
		_ ->
			THIS:set_attribute(?STATE_STRING,RL)
	end
   . 
updatevalues([],S)->
	THIS:set_attribute(?STATE_STRING,S),
	ok;
%%
%%"ProcMemPer=90.44$TotalPhysicalIO=5163$=2$=98808$=551$=0
%% $=0$=10690$=89$=0$=0$=0$=0$"
%% 
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
		"ProcMemPer" ->
			THIS:set_attribute(procMemPer,V1),
			S++"Proc Mem Per(%)="++V++",";
		 "TotalPhysicalIO" ->
			THIS:set_attribute(totalPhysicalIO,V1),
			S++"Total Physical IO="++V++",";
		 "PhysicalIO1m" ->
			THIS:set_attribute(physicalIO1m,V1),
			S++"Physical IO/m="++V++",";
		 "TotalLogicalIO" ->
			THIS:set_attribute(totalLogicalIO,V1),
			S++"Total Logical IO="++V++",";	
		 "LogicalIO1m" ->
			THIS:set_attribute(logicalIO1m,V1),
			S++"Logical IO/m="++V++",";	
		 "SortsDisk" ->
			THIS:set_attribute(sortsDisk,V1),
			S++"Sorts Disk="++V++",";
		 "SortsDisk1m" ->
			THIS:set_attribute(sortsDisk1m,V1),
			S++"Sorts Disk/m="++V++",";
		 "SortsMem" ->
			THIS:set_attribute(sortsMem,V1),
			S++"Sorts Mem="++V++",";	
		 "SortsMem1m" ->
			THIS:set_attribute(sortsMem1m,V1),
			S++"Sorts Mem/m="++V++",";
		 "UserCommits" ->
			THIS:set_attribute(userCommits,V1),
			S++"User Commits="++V++",";
		 "UserCommits1m" ->
			THIS:set_attribute(userCommits1m,V1),
			S++"User Commits/m="++V++",";
		 "UserRollbacks" ->
			THIS:set_attribute(userRollbacks,V1),
			S++"User Rollbacks="++V++",";		
		 "UserRollbacks1m" ->
			 THIS:set_attribute(userRollbacks1m,V1),
			 S++"UserRollbacks/m="++V
		end,		
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
			[{procMemPer,'>',90}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{procMemPer,'>',80}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{procMemPer,'>=',0}]
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
	#property{name=processName,title="Process Name",type=text,editable=true,order=5,description="Process Name"},
	#property{name=machStr,title="Mach Str",type=text,editable=true,order=6,description="Mach Str"},
	#property{name=procMemPer,title="Proc Mem Per(%)",type=numeric,order=99,configurable=false,state=true,baselinable=true},
	#property{name=totalPhysicalIO,title="Total Physical IO",type=numeric,order=100,configurable=false,state=true,baselinable=true},
	#property{name=physicalIO1m,title="Physical IO 1m",type=numeric,order=101,configurable=false,state=true,baselinable=true},
	#property{name=totalLogicalIO,title="total Logical IO",type=numeric,order=102,configurable=false,state=true,baselinable=true},
	#property{name=logicalIO1m,title="Logical IO 1m",type=numeric,order=103,configurable=false,state=true,baselinable=true},
	#property{name=sortsDisk,title="Sorts Disk",type=numeric,order=104,configurable=false,state=true,baselinable=true},
	#property{name=sortsDisk1m,title="Sorts Disk 1m",type=numeric,order=105,configurable=false,state=true,baselinable=true},
	#property{name=sortsMem,title="Sorts Mem",type=numeric,order=106,configurable=false,state=true,baselinable=true},
	#property{name=sortsMem1m,title="Sorts Mem 1m",type=numeric,order=107,configurable=false,state=true,baselinable=true},
	#property{name=userCommits,title="User Commits",type=numeric,order=108,configurable=false,state=true,baselinable=true},
	#property{name=userCommits1m,title="User Commits 1m",type=numeric,order=109,configurable=false,state=true,baselinable=true},
	#property{name=userRollbacks,title="User Rollbacks",type=numeric,order=110,configurable=false,state=true,baselinable=true},
	#property{name=userRollbacks1m,title="User Rollbacks 1m",type=numeric,order=111,configurable=false,state=true,baselinable=true}
	].



