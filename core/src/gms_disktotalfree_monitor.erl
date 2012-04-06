%% Author: Administrator
%% Created: 2011-10-9
%% Description: TODO: Add description to flowserverlivechanneluser 
-module(gms_disktotalfree_monitor,[BASE]).
-extends(server_monitor).
-define(SNAME, "wmi").
-compile(export_all).
-include("monitor_template.hrl").
-include("monitor.hrl").

-define(MAX_COUNTER,20).

%% @spec new() -> ok
%% @doc Start the monitor instance.
new()->
	Base = server_monitor:new(),
	{?MODULE,Base}.
%% @spec getHostname(This) -> string()
%% where
%% This = instance()
%% @doc Get Machine.
getHostname(This)->
	{ok,{_,Machine}}=This:get_property(machine),
	Machine.
%% @spec getMaxCounters() -> integer()
%% @doc Get Max counters number.
getMaxCounter()->?MAX_COUNTER.
get_wmi_node()->
	case server_conf:getWmiNode() of
		undefined->
			{ok, Host} = inet:gethostname(),
			list_to_atom( ?SNAME ++ "@" ++ Host);
		Node->
			Node
	end.
getmhost(Host)->
	string:strip(Host, left, $\\).
update()->
	{ok,{_,Machine}}=THIS:get_property(machine),
	case Machine of
		""->
			Host="127.0.0.1",
			User=" ",
			Passwd=" ",
	         THIS:getCounterValues(Host,User,Passwd);  
		_->
			case machine:getNTMachine(Machine) of
				[]->
					THIS:set_attribute(?NO_DATA,true),
					THIS:set_attribute("status","error"),
					THIS:set_attribute(status,"error"),
				    THIS:set_attribute(?CATEGORY,?NO_DATA),
					THIS:set_attribute(?STATE_STRING,"connect target error!"),
					[];
				[M|_]->
                    THIS:getCounterValues(getmhost(M#machine.host),M#machine.login,M#machine.passwd)
			end
  end.
getRet(Ret,[H|E])->
	TRet=Ret++H,
	getRet(TRet,E);
getRet(Ret,[])->
    Ret.
getCounterValues(Host,User,Passwd) ->
    THIS:set_attribute(totalFreeSpace,"n/a"),
    THIS:set_attribute("status","error"),
	THIS:set_attribute(status,"error"),
    THIS:set_attribute("total free space(MB)","n/a"),
    Ssql=["select FreeMegabytes from Win32_PerfRawData_PerfDisk_LogicalDisk where Name='_Total'"],
	case rpc:call(THIS:get_wmi_node(), wmic, wmic, [Host,User,Passwd,Ssql]) of
            {ok, Result} ->
				case Result of
					[{_SQL,{_,empty}}]->
					  THIS:set_attribute(?NO_DATA,true),
			          THIS:set_attribute(?CATEGORY,?NO_DATA);
					[{_SQL,{_,[{{_Name1,Value1},{_,_}}]}}] ->
						TempV=integer_to_list(Value1),
						V=Value1,
						THIS:set_attribute(totalFreeSpace,V),
						THIS:set_attribute("total free space(MB)",V),
						THIS:set_attribute("status","ok"),
                        THIS:set_attribute(status,"ok"),
						THIS:set_attribute(?STATE_STRING,"total free space(MB)"++"="++TempV)
				        
				end;	
%% 				Result;		    
			_Error ->
				THIS:set_attribute(?NO_DATA,true),
			    THIS:set_attribute(?CATEGORY,?NO_DATA),
				THIS:set_attribute(?STATE_STRING,"call wmi node error!"),
				[]
	       end .     


%% @spec verify(Params) -> {ok, []} | {error, Reason}
%%  Params = [term()]
%%  Reason = string()
%% @doc verify is the function called by schedule to verify the parameter of monitor inputed by user
verify(Params) ->
    Errs = 
    case BASE:verify(Params) of
		{error,Be}->
			Be;
		_->
			[]
	end,   
	if length(Errs)>0 ->
	    {error,Errs};
    true ->
	    {ok,""}
	end.
getLogProperties(This)->
	Temp = This:get_template_property(),
	[X#property.title|| X<-Temp,X#property.state=:=true].
getCostInLicensePoints()->
	1.
get_classifier(error)->
	Cls = case THIS:get_property(error_classifier) of
				{ok,{error_classifier,Classifier}}->
					Classifier;
				_->
					[{status,'!=',"ok"}]
			end,
	if 
		length(Cls) < 10 ->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end;
get_classifier(warning)->

	Cls =case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'!=',"ok"}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end;
get_classifier(good)->
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'==',"ok"}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end.
get_template_property()->
	BASE:get_template_property() ++  
	[
	#property{name=totalFreeSpace,title="total free space(MB)",type=numeric,configurable=false,state=true,upIsBad=false,baselinable=true},
	#property{name=status,title="status",configurable=false,state=true}
    ].
