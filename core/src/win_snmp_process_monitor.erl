%%
%% memory monitor
%%

%% @author jun.xiao@dragonflow.com
%% @copyright 2011-2012 Dragonflow
%% @version 1.0
%% @doc win memory monitor by snmp
-module(win_snmp_process_monitor,[BASE]).
-compile(export_all).
-extends(browsable_base).
-include("monitor.hrl").
-include("monitor_template.hrl").

-export([new/0,update/0,get_classifier/1,get_template_property/0,verify/1]).


%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for memory monitor
new()->
	Base = browsable_base:new(),
	Base:set_property(pHostname,""),
	Base:set_property(pComm,""),
	{?MODULE,Base}.
	

%% @spec update() -> Result
%%  Result = term()
%% @doc update is the run function called by schedule to test the  memory monitor
update() ->
	{ok,{_,Browse}} = THIS:get_property(browse),
	{ok,{_,Hostname}} = THIS:get_property(pHostname),
	{ok,{_,Comm}} = THIS:get_property(pComm),
	ErIp = textutils:ipStr2ErIp(Hostname),
	Session = snmp_session:new(ErIp,161,"V2",Comm,"MD5","","","","","",5000),
	Statdesc = getResult(Browse,Session),
	THIS:set_attribute(?STATE_STRING,Statdesc).



getResult([H|T],Session)->
	{PID,PIDANDNAME} = H,
	io:format("PID**~p~n",[PID]),
	R = Session:g(lists:append([1,3,6,1,2,1,25,4,2,1,7],[list_to_integer(PID)])),
	{ok,{Y,_,[{_,_,_,X,_}]},_} = R,
	ProcessStat = case Y of noSuchName ->"notExist";
			        _ ->
			      case X of 1 -> "runing";
					2 -> "runnable";
					3 -> "notRunnable";
					4 -> "invalid";
					_ -> "unknown"
			      end
		      end,
	io:format("PIDANDNAME:~p,X:~p",[PIDANDNAME,X]),
	THIS:set_attribute(PIDANDNAME,X),
	Statdesc = "Process " ++ PIDANDNAME++" is:"++ProcessStat++"   \r\n",
	Statdesc ++ getResult(T,Session);
getResult([],_)->
	[].


getBrowseData(Params)->
	ErIp = textutils:ipStr2ErIp(proplists:get_value(pHostname,Params)),
	Session = snmp_session:new(ErIp,161,"V2",proplists:get_value(pComm,Params),"MD5","","","","","",5000),
	ProcessPidList = Session:get_table_col([1,3,6,1,2,1,25,4,2,1,1]),
	ProcessNameList = Session:get_table_col([1,3,6,1,2,1,25,4,2,1,2]),
	ProcessPids = [X||{_,_,_,X,_}<-ProcessPidList],
	ProcessNames = [X||{_,_,_,X,_}<-ProcessNameList],
	%io:format("makeBrowseData********************~p~n",[makeBrowseData(ProcessPids,ProcessNames)]),
	makeBrowseData(ProcessPids,ProcessNames).

makeBrowseData([H|T],[H1|T1])->
	lists:append([{integer_to_list(H),"[PID:"++integer_to_list(H)++"] "++H1++" status"}],makeBrowseData(T,T1));
makeBrowseData([],[])->
	[];
makeBrowseData(_,_)->
	[].

getLogProperties(This)->
	{ok,{_,Counters}} = THIS:get_property(browse),
	Temp = This:get_template_property(),
	[X#property.name || X<-Temp,X#property.state=:=true] ++ [X||{_,X}<- Counters].


%% @spec get_template_property() -> list()
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
get_template_property() ->
    BASE:get_template_property() ++ 
    [
	#property{name=pHostname,title="host name(or ip address)",type=text,order=3,editable=true},
	#property{name=pComm,title="public communicator",type=text,order=4,editable=true}
    ].


%% @spec get_classifier(Param) -> List
%% Param = atom()
%% List = [Tuple]
%% Tuple = {Status, Logic, Value}
%% Status = 'error'|'warning'| 'good' 
%% Logic = '!=' | '==' | '>' | '<' | 'contain'
%% Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error)->
	case THIS:get_property(error_classifier) of
			{ok,{error_classifier,Classifier}}->
				Classifier;
			_->
				[{percentfull,'>=',95}]
	end;

get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{percentfull,'>=',90}]
	end;

get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{percentfull,'>=',0}]
	end.


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