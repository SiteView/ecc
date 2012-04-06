%%
%% memory monitor
%%

%% @author jun.xiao@dragonflow.com
%% @copyright 2011-2012 Dragonflow
%% @version 1.0
%% @doc memory monitor
-module(win_snmp_disk_monitor,[BASE]).
-compile(export_all).
-extends(browsable_base).
-include("monitor.hrl").
-include("monitor_template.hrl").
-include_lib("snmp/include/snmp_types.hrl").
-include("snmp_ecc.hrl").

-export([new/0,update/0,get_classifier/1,get_template_property/0,get_memory_info_win32/1,get_memory_info_linux/1,verify/1,getBrowseData/1]).


%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for memory monitor
new()->
	Base = browsable_base:new(),
	Base:set_property(pCpuUsed,""),
	Base:set_property(pComm,""),
	{?MODULE,Base}.

is_wmi(Host)->
	OsNum = machine:getOS(Host),
	case OsNum of
		1 ->
			case machine:getNTMachine(Host) of
				[]->
					false;
				[Mach|_]->
					Mach#machine.method == "WMI"
			end;
		_ ->
			false
	end.
	

%% @spec update() -> Result
%%  Result = term()
%% @doc update is the run function called by schedule to test the  memory monitor
update() ->
	{ok,{_,Browse}} = THIS:get_property(browse),
	{ok,{_,CpuUsed}} = THIS:get_property(pCpuUsed),
	{ok,{_,Comm}} = THIS:get_property(pComm),
	ErIp = textutils:ipStr2ErIp(CpuUsed),
	Session = snmp_session:new(ErIp,161,"V2",Comm,"MD5","","","","","",5000),
	Var = Session:get_table_col([1,3,6,1,2,1,25,2,3]),
	Diskindexlist = [ string:substr(X,12,1) ||{X,_}<-Browse],
	Stat = asys(Diskindexlist,Var,[]),
	THIS:set_attribute(?STATE_STRING,Stat).
                			
asys([H|T],Var,Resultlist)->
		IndexList = lists:append([1,3,6,1,2,1,25,2,3,1,2],H),%disk index
		Templist = lists:append([1,3,6,1,2,1,25,2,3,1,3],H),%disk desc
		Templist2 = lists:append([1,3,6,1,2,1,25,2,3,1,4],H),%cu
		Templist3 = lists:append([1,3,6,1,2,1,25,2,3,1,5],H),%total size
		Templist4 = lists:append([1,3,6,1,2,1,25,2,3,1,6],H),%used size
		Diskdes =lists:append(Resultlist, [ [X++",    ",integer_to_list((Q*W*100 div (1024*1024))div 100)++"MB,    ",integer_to_list((Q*W*100 div (1024*1024*1024))div 100)++"GB,    ",integer_to_list((Q*U*100 div (1024*1024))div 100)++"MB used,    ",integer_to_list(U*100 div W)++"  %used. \n"] || {_,Y,_,X,_}<-Var,{_,Z,_,Q,_}<-Var,{_,C,_,W,_}<-Var,{_,V,_,U,_}<-Var,Y==Templist,Z==Templist2,C==Templist3,V==Templist4]),
		[[_,_,_,_,DiskUsedPercent]] =[ [X,(Q*W*100 div (1024*1024))div 100,(Q*W*100 div (1024*1024*1024))div 100,(Q*U*100 div (1024*1024))div 100,U*100 div W] || {_,Y,_,X,_}<-Var,{_,Z,_,Q,_}<-Var,{_,C,_,W,_}<-Var,{_,V,_,U,_}<-Var,Y==Templist,Z==Templist2,C==Templist3,V==Templist4],
		{ok,{_,Counters}} = THIS:get_property(browse),
		{_,BrowseAttribute} = lists:keyfind(IndexList, 1, Counters),
		THIS:set_attribute(BrowseAttribute,DiskUsedPercent),
		case T of
			[]->
				lists:flatten(Diskdes);
			_->	
				asys(T,Var,Diskdes)
		end.

getLogProperties(This)->
	{ok,{_,Counters}} = THIS:get_property(browse),
	Temp = This:get_template_property(),
	[X#property.name || X<-Temp,X#property.state=:=true] ++ [X||{_,X}<- Counters].

getBrowseData(Params)->
	io:format(proplists:get_value(pCpuUsed,Params)),
	io:format(proplists:get_value(pComm,Params)),
	ErIp = textutils:ipStr2ErIp(proplists:get_value(pCpuUsed,Params)),
	Session = snmp_session:new(ErIp,161,"V2",proplists:get_value(pComm,Params),"MD5","","","","","",5000),
	Var = Session:get_table_col([1,3,6,1,2,1,25,2,3]),
	Diskindexlist = [ Diskvar || {_,Diskvar,_,[1,3,6,1,2,1,25,2,1,4],_}<-Var],
	Diskindexitem = [ string:substr(Diskvar,12,1) || {_,Diskvar,_,[1,3,6,1,2,1,25,2,1,4],_}<-Var],
	Diskdeslist = [ lists:append([1,3,6,1,2,1,25,2,3,1,3],X) || X<-Diskindexitem],io:format("****~p",[browseDataAsys(Diskindexlist,Diskdeslist,Var)]),
	browseDataAsys(Diskindexlist,Diskdeslist,Var).

browseDataAsys([],[],_)->
		[];
browseDataAsys([H|T],[H1|T1],Var)->
		Result =[{X,Y}||{_,Z,_,Y,_}<-Var,Z==H1,X<-[H]],
		lists:append(Result,browseDataAsys(T,T1,Var)).
		
	


%% @spec get_memory_info_win32(Data) -> ok
%% Data = string()
%% @doc  Get windows machine memory information
get_memory_info_win32(Data) ->
    List = string:tokens(Data,"\r\n"),
    Ptime = get_preftime(List),
    Pfreq = get_perffreq(List),
    P26 = get_26(List),
    P40 = get_40(List),	
	FRACTION = get_FRACTION(List),
    BASE = get_BASE(List),
	Used = (100 * P26) / ((P26 * BASE)/FRACTION),
	FreeM = (((P26 * BASE)/FRACTION) - P26) / erlang:list_to_integer("100000",16),
    
	THIS:set_attribute(percentfull,Used),
	THIS:set_attribute(percent_used,(FreeM/(1-Used) - FreeM) /(FreeM/(1-Used))),
	THIS:set_attribute(free,FreeM),
	
	{ok,{pLastMeasurement,LastMeasurement}} = THIS:get_attribute(pLastMeasurement),
	{ok,{pLastPageFaults,LastPageFaults}} = THIS:get_attribute(pLastPageFaults),
	if LastMeasurement /= 0,LastPageFaults /= 0 ->
	    F = (P40 - LastPageFaults)/((Ptime - LastMeasurement) / Pfreq),
		THIS:set_attribute(pPageFaultsPerSecond,F);
	true ->
        io:format("test")
    end,			    
	THIS:set_attribute(pLastMeasurement,P40),
	THIS:set_attribute(pLastPageFaults,Ptime).

%% @spec get_memory_info_linux(Data) -> ok
%% Data = string()
%% @doc  Get unix machine memory information
get_memory_info_linux(Data) ->	
	List = string:tokens(Data," "),
	[Total] = lists:sublist(List,7,1),
	[Free] = lists:sublist(List,9,1),
	[Used] = lists:sublist(List,8,1),
	THIS:set_attribute(percentfull,Used/Total),
	THIS:set_attribute(percent_used,Used),
	THIS:set_attribute(free,Free).


%% @spec get_template_property() -> list()
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
get_template_property() ->
    BASE:get_template_property() ++ 
    [
	#property{name=pCpuUsed,title="host name(or ip address)",type=text,order=3,editable=true},
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
	Cls = case THIS:get_property(error_classifier) of
				{ok,{error_classifier,Classifier}}->
					Classifier;
				_->
					[{percentfull,'>=',95}]
			end,
	if 
		length(Cls) < 3 ->
			Cls ++ lists:map(fun(X)->{'percentfull','',''} end,lists:seq(1,3 - length(Cls)));
		true ->
			Cls
	end;
get_classifier(warning)->
	Cls =case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{percentfull,'>=',90}]
	end,
	if 
		length(Cls)<3->
			Cls ++ lists:map(fun(X)->{'percentfull','',''} end,lists:seq(1,3 - length(Cls)));
		true ->
			Cls
	end;
get_classifier(good)->
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{percentfull,'>=',0}]
	end,
	if 
		length(Cls)<3->
			Cls ++ lists:map(fun(X)->{'percentfull','',''} end,lists:seq(1,3 - length(Cls)));
		true ->
			Cls
	end.

get_preftime(List) ->
    get_preftime_t(List,length(List)).
get_preftime_t(L,0) -> L;
get_preftime_t(Li,Num) ->
    [A|B] = Li,
	Term  = string:str(A,"PrefTime"),    
    if Term > 0 ->
	    L2 = string:tokens(A," "),
        [E] = lists:sublist(L2,length(L2),1),	
        get_preftime_t(list_to_integer(E),0);
	true ->
	    get_preftime_t(B,Num-1)
    end.	

get_perffreq(List) ->
    get_perffreq_t(List,length(List)).
get_perffreq_t(L,0) -> L;
get_perffreq_t(Li,Num) ->
    [A|B] = Li,
	Term  = string:str(A,"PerfFreq"),    
    if Term > 0 ->
	    L2 = string:tokens(A," "),
        [E] = lists:sublist(L2,length(L2),1),	
        get_preftime_t(list_to_integer(E),0);
	true ->
	    get_preftime_t(B,Num-1)
    end.	


get_26(List) ->
    get_26_t(List,length(List)).
get_26_t(L,0) -> L;
get_26_t(Li,Num) ->
    [A|B] = Li,
	Term  = string:str(A,"26:"),    
    if Term > 0 ->
	    L2 = string:tokens(A," "),
        [E] = lists:sublist(L2,2,1),	
        get_preftime_t(list_to_integer(E),0);
	true ->
	    get_preftime_t(B,Num-1)
    end.
	
get_40(List) ->
    get_40_t(List,length(List)).
get_40_t(L,0) -> L;
get_40_t(Li,Num) ->
    [A|B] = Li,
	Term  = string:str(A,"40:"),    
    if Term > 0 ->
	    L2 = string:tokens(A," "),
        [E] = lists:sublist(L2,2,1),	
        get_preftime_t(list_to_integer(E),0);
	true ->
	    get_preftime_t(B,Num-1)
    end.	


get_FRACTION(List) ->
    get_FRACTION_t(List,length(List)).
get_FRACTION_t(L,0) -> L;
get_FRACTION_t(Li,Num) ->
    [A|B] = Li,
	Term  = string:str(A,"PERF_RAW_FRACTION"),    
    if Term > 0 ->
	    L2 = string:tokens(A," "),
        [E] = lists:sublist(L2,2,1),	
        get_preftime_t(list_to_integer(E),0);
	true ->
	    get_preftime_t(B,Num-1)
    end.

get_BASE(List) ->
    get_BASE_t(List,length(List)).
get_BASE_t(L,0) -> L;
get_BASE_t(Li,Num) ->
    [A|B] = Li,
	Term  = string:str(A,"PERF_RAW_BASE"),    
    if Term > 0 ->
	    L2 = string:tokens(A," "),
        [E] = lists:sublist(L2,2,1),	
        get_preftime_t(list_to_integer(E),0);
	true ->
	    get_preftime_t(B,Num-1)
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

%-record(machine,{id,name,host,login="",passwd="",os="nt",status="",method="",sshcommand="",sshport=22,version="",keyfile="",sshauthmethod=""}).
get_memory_cmd(Machine,Osfamily) ->
	case Osfamily of 
	    win32 ->
		    case Machine#machine.os of
			"nt" ->
		        Cmd = "tools\\perfex.exe" ++ " =4" ++  " -connect " ++ "\\\\" ++ Machine#machine.host ++ "-u" ++ Machine#machine.login ++ "-p" ++ Machine#machine.passwd,
			    {"nt",Cmd};
			"linux" ->
                Cmd = "tools\\plink.exe" ++ Machine#machine.login ++ "@" ++  Machine#machine.host ++ " -ssh" ++" -P" ++Machine#machine.sshport ++" -pw" ++ Machine#machine.passwd ++ "  free -om",
			    {"linux",Cmd}
			end;	
        unix ->
            io:format("get cpu cmd Unix")
    end. 			    	