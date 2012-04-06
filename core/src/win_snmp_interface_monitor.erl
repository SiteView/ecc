%%
%% memory monitor
%%

%% @author jun.xiao@dragonflow.com
%% @copyright 2011-2012 Dragonflow
%% @version 1.0
%% @doc win memory monitor by snmp
-module(win_snmp_interface_monitor,[BASE]).
-compile(export_all).
-extends(atomic_monitor).
-include("monitor.hrl").
-include("monitor_template.hrl").

-export([new/0,update/0,get_classifier/1,get_template_property/0,verify/1]).


%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for memory monitor
new()->
	Base = atomic_monitor:new(),
	Base:set_property(pHostname,""),
	Base:set_attribute(pSpeed,0),
	Base:set_attribute(pTotalInOct,0),
	Base:set_attribute(pTotalOutOct,0),
	{?MODULE,Base}.
	

%% @spec update() -> Result
%%  Result = term()
%% @doc update is the run function called by schedule to test the  memory monitor
update() ->
	{ok,{_,CpuUsed}} = THIS:get_property(pHostname),
	{ok,{_,Comm}} = THIS:get_property(pComm),
	ErIp = textutils:ipStr2ErIp(CpuUsed),
	Session = snmp_session:new(ErIp,161,"V2",Comm,"MD5","","","","","",5000),
	MacAddResult = Session:get_table_col([1,3,6,1,2,1,2,2,1,6]),
	MacAddVar = [X||{_,_,_,X,_}<-MacAddResult],
	IfInOctetsResult = Session:get_table_col([1,3,6,1,2,1,2,2,1,10]),
	IfInOctetsVar = [X||{_,_,_,X,_}<-IfInOctetsResult],
	IfOutOctetsResult = Session:get_table_col([1,3,6,1,2,1,2,2,1,16]),
	IfOutOctetsVar = [X||{_,_,_,X,_}<-IfOutOctetsResult],
	{TotalInOct,TotalOutOct} = asy(MacAddVar,IfInOctetsVar,IfOutOctetsVar),
	{_,{attTotalInOct,LastTotalInOct}} = THIS:get_attribute(attTotalInOct),
	{_,{attTotalOutOct,LastTotalOutOct}} = THIS:get_attribute(attTotalOutOct),
	{IsDtok,{attNow,LastUpdatetime}} = THIS:get_attribute(attNow),
	io:format("***LastTotalInOct***~p~n",[LastTotalInOct]),
	io:format("****LastTotalOutOct**~p~n",[LastTotalOutOct]),
	io:format("****LastUpdatetime**~p~n",[LastUpdatetime]),
	case IsDtok of
		error ->
			THIS:set_attribute(?STATE_STRING,"need refresh again.");
		_ ->
			UpdateTime = (sv_datetime:now() - LastUpdatetime)/1000,
			Stat = round(((TotalInOct+TotalOutOct)-(LastTotalInOct+LastTotalOutOct))/UpdateTime),
			THIS:set_attribute(pSpeed,Stat),
			THIS:set_attribute(pTotalInOct,TotalInOct-LastTotalInOct),
			THIS:set_attribute(pTotalOutOct,TotalOutOct-LastTotalOutOct),
			io:format("***TotalSpeed***~p~n",[Stat]),
			io:format("***TotalInOct***~p~n",[TotalInOct-LastTotalInOct]),
			io:format("****TotalOutOct**~p~n",[TotalOutOct-LastTotalOutOct]),
			THIS:set_attribute(?STATE_STRING,io_lib:format("Server TotalSpeed is ~p Byte/s~n,TotalInOct is ~p Byte~n,TotalOutOct is ~p Byte~n, in ~p s",[Stat,TotalInOct-LastTotalInOct,TotalOutOct-LastTotalOutOct,UpdateTime]))
		end,
			
	THIS:set_attribute(attTotalInOct,TotalInOct),
	THIS:set_attribute(attTotalOutOct,TotalOutOct),
	THIS:set_attribute(attNow,sv_datetime:now()).

asy([H|T],[H1|T1],[H2|T2])->
	case H of
		[] ->  
			asy(T,T1,T2);
		_  -> 
			asy(T,T1,T2,H1,H2,[])
	end.
asy([],[],[],TotalInOct,TotalOutOct,_)->
	{TotalInOct,TotalOutOct};
asy([H|T],[H1|T1],[H2|T2],TotalInOct,TotalOutOct,ExistList)->
	case H of
		[] ->  
			asy(T,T1,T2,TotalInOct,TotalOutOct,ExistList);
		_  ->	
			case lists:keyfind(H,1,ExistList) of 
				false ->
					asy(T,T1,T2,H1+TotalInOct,H2+TotalOutOct,lists:append(ExistList,[{H,""}]));
				_ ->
					asy(T,T1,T2,TotalInOct,TotalOutOct,ExistList)
				end
	end.



%% @spec get_template_property() -> list()
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
get_template_property() ->
    BASE:get_template_property() ++ 
    [
	#property{name=pHostname,title="host name(or ip address)",type=text,order=3,editable=true},
	#property{name=pComm,title="public communicator",type=text,order=4,editable=true},
	#property{name=pSpeed,title="TotalSpeed(Byte/s)",type=numeric,order=1,configurable=false,state=true},
	#property{name=pTotalInOct,title="TotalInOct(Bytes)",type=numeric,order=2,configurable=false,state=true},
	#property{name=pTotalOutOct,title="TotalOutOct(Bytes)",type=numeric,order=3,configurable=false,state=true}
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
				[{pSpeed,'>=',1000000}]
	end;

get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{pSpeed,'>=',500000}]
	end;

get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{pSpeed,'>=',450000}]
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