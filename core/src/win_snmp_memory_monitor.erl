%%
%% memory monitor
%%

%% @author jun.xiao@dragonflow.com
%% @copyright 2011-2012 Dragonflow
%% @version 1.0
%% @doc win memory monitor by snmp
-module(win_snmp_memory_monitor,[BASE]).
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
	Base:set_property(pComm,""),
	{?MODULE,Base}.
	

%% @spec update() -> Result
%%  Result = term()
%% @doc update is the run function called by schedule to test the  memory monitor
update() ->
	{ok,{_,CpuUsed}} = THIS:get_property(pHostname),
	{ok,{_,Comm}} = THIS:get_property(pComm),
	ErIp = textutils:ipStr2ErIp(CpuUsed),
	Session = snmp_session:new(ErIp,161,"V2",Comm,"MD5","","","","","",5000),
	Var = Session:get_table_col([1,3,6,1,2,1,25,2,3]),
	Memindexlist = [ string:substr(Diskvar,12,1) || {_,Diskvar,_,[1,3,6,1,2,1,25,2,1,2],_}<-Var],
	Virmemindexlist = [ string:substr(Diskvar,12,1) || {_,Diskvar,_,[1,3,6,1,2,1,25,2,1,3],_}<-Var],
	[Memresult] = asys(Memindexlist,Var,[]),
	[VirMemresult] = asys(Virmemindexlist,Var,[]),
	MemDes = io_lib:format("~p,    ~pMB,    ~pMB used,    ~p%used.    \r\n",Memresult),
	VirmemDes = io_lib:format("~p,    ~pMB,    ~pMB used,    ~p%used.    \r\n",VirMemresult),
	[_,_,_,Percent] = Memresult,
	THIS:set_attribute(pPercentFull,Percent),
	Stat = lists:append(MemDes,VirmemDes),
	THIS:set_attribute(?STATE_STRING,Stat).
asys([H|T],Var,Resultlist)->
		Templist = lists:append([1,3,6,1,2,1,25,2,3,1,3],H),%Mem desc
		Templist2 = lists:append([1,3,6,1,2,1,25,2,3,1,4],H),%cu
		Templist3 = lists:append([1,3,6,1,2,1,25,2,3,1,5],H),%total size
		Templist4 = lists:append([1,3,6,1,2,1,25,2,3,1,6],H),%used size
		%Memdes =lists:append(Resultlist, [ [X++",    ",integer_to_list((Q*W*100 div (1024*1024))div 100)++"MB,    ",integer_to_list((Q*W*100 div (1024*1024*1024))div 100)++"GB,    ",integer_to_list((Q*U*100 div (1024*1024))div 100)++"MB used,    ",integer_to_list(U*100 div W)++"  %used. \n"] || {_,Y,_,X,_}<-Var,{_,Z,_,Q,_}<-Var,{_,C,_,W,_}<-Var,{_,V,_,U,_}<-Var,Y==Templist,Z==Templist2,C==Templist3,V==Templist4]),
		Res =lists:append(Resultlist, [ [X,(Q*W*100 div (1024*1024))div 100,Q*U*100 div (1024*1024)div 100,U*100 div W] || {_,Y,_,X,_}<-Var,{_,Z,_,Q,_}<-Var,{_,C,_,W,_}<-Var,{_,V,_,U,_}<-Var,Y==Templist,Z==Templist2,C==Templist3,V==Templist4]),
		case T of
			[]->
				io:format("asys end ********************~n"),
				Res;
			_->	
				asys(T,Var,Res)
		end.



%% @spec get_template_property() -> list()
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
get_template_property() ->
    BASE:get_template_property() ++ 
    [
	#property{name=pHostname,title="host name(or ip address)",type=text,order=3,editable=true},
	#property{name=pComm,title="public communicator",type=text,order=4,editable=true},
	#property{name=pPercentFull,title="percentfull",type=numeric,order=1,configurable=false,state=true,baselinable=true}
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