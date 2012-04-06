%%
%% memory monitor
%%

%% @author lei.lin@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc memory monitor
-module(memory_monitor,[BASE]).
-compile(export_all).
-extends(server_monitor).
-include("monitor.hrl").
-include("monitor_template.hrl").

-export([new/0,update/0,get_classifier/1,get_template_property/0,get_memory_info_win32/1,get_memory_info_linux/1,verify/1]).


%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for memory monitor
new()->
	Base = server_monitor:new(),
	Base:set_attribute(pPercentFull,0),
	Base:set_attribute(percent_used,0),
	Base:set_attribute(pPageFaultsPerSecond,0),
	Base:set_attribute(pLastMeasurement,0),
	Base:set_attribute(pLastPageFaults,0),
	Base:set_attribute(pPageFaultsPerSecond,0),
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
%%io:format("-------------------------------------------------------------------update Memory"),
	THIS:set_attribute(pPercentFull,"n/a"),
	THIS:set_attribute(percent_used,"n/a"),
	THIS:set_attribute(pPageFaultsPerSecond,"n/a"),     
    {ok,{_,Host}} = THIS:get_property(machine),
	%[Machine] = machine:getMachine(Host),
	%%io:format("MEMORY Host  Host  Host ~p~n",[Host]),
    {Osfamily, Osname} = os:type(),
	{ok,{pLastMeasurement,LastMeasurement}} = THIS:get_attribute(pLastMeasurement),
	{ok,{pLastPageFaults,LastPageFaults}} = THIS:get_attribute(pLastPageFaults), 
    {Status,ResultList} = platform:getMemoryFull(Host,LastPageFaults,LastMeasurement),
    %%io:format("__________platform:getMemoryFull____________:~p~n",[{Status,ResultList}]),
    case Status of
    ok ->
        case machine:getSnmpMachine(Host) of
            {ok, Ma=#machine{}} ->
                [PercentUsed,TotalVirMemory,FreeVirMem,PagesPerSec] = ResultList,
                THIS:set_attribute(pPercentFull,PercentUsed),
                THIS:set_attribute(pFreeSpace, FreeVirMem/1024/1024),
                %%THIS:set_attribute(pPageFaultsPerSecond, PagesPerSec),
                S1 = integer_to_list(round(FreeVirMem/1024/1024)),
                S2 = integer_to_list(round(PercentUsed)) ++ "% used      " ++ S1 ++ "MB free",
                THIS:set_attribute(?STATE_STRING,S2);
            _ ->
	%%	case Host of
	%%		"\\\\"++_HOST ->
	%%			Host1 = Host;
	%%		_ ->
	%%			Host1 = "\\\\"++Host
	%%	end,
	        case is_wmi(Host) of
                    true ->
                        %%[PercentUsed,TotalVirMemory,FreeVirMem,PagesPerSec].
                            [PercentUsed,TotalVirMemory,FreeVirMem,PagesPerSec] = ResultList,
                            THIS:set_attribute(pPercentFull,PercentUsed),
                            THIS:set_attribute(pFreeSpace, FreeVirMem),
                            THIS:set_attribute(pPageFaultsPerSecond, PagesPerSec),
                            S1 = integer_to_list(round(FreeVirMem)),
                            S2 = integer_to_list(round(PercentUsed)) ++ "% used      " ++ S1 ++ "MB free" ++ "," ++ integer_to_list(round(PagesPerSec)) ++ " pages/sec",
                            THIS:set_attribute(?STATE_STRING,S2);
                    _ ->
                        [L2] = lists:sublist(ResultList,1,1),
                        [Al1] = lists:sublist(ResultList,2,1), 
                        [Al2] = lists:sublist(ResultList,3,1), 
                        L4 =  Al2 - Al1,
			
                        [L5] =   lists:sublist(ResultList,4,1),
                        [L6] =   lists:sublist(ResultList,5,1),
                        [L1] = lists:sublist(ResultList,6,1),
                        [L] = lists:sublist(ResultList,7,1),   
                        [L7] = lists:sublist(ResultList,8,1),    
                        S1 = integer_to_list(round(L4 / 1048576)),
		    
                        if L5 /= -1 ,L6 /=0 ->
                            F1 = L6 / L7,
                            TF = L5 / F1,
                            if TF < 0.001 ->
                            F = 0.0;
                            true ->
                            F = TF
                            end;
                        true ->
                            F = -1
                        end,                 
                        THIS:set_attribute(pLastPageFaults,L1),
                            THIS:set_attribute(pLastMeasurement,L),
                        if L2 == -1 ->
                            THIS:set_attribute(pPercentFull,"n/a"), 
                            THIS:set_attribute(pFreeSpace,"n/a"),
                            THIS:set_attribute(pPageFaultsPerSecond,"n/a"),
                            THIS:set_attribute(pMeasurement,0),
                            THIS:set_attribute(?NO_DATA,true), 
                                THIS:set_attribute(?STATE_STRING,"no data"), 
                                THIS:set_attribute(?CATEGORY,?NO_DATA);
                        true ->
                            THIS:set_attribute(pPercentFull,L2),
                            THIS:set_attribute(pFreeSpace, L4 / 1048576),
                            THIS:set_attribute(pMeasurement, 0),
                            if F == -1 ->
                            S2 = integer_to_list(round(L2)) ++ "% used," ++ S1 ++ "MB free", 
                            THIS:set_attribute(pPageFaultsPerSecond,0);
                            true ->
                            THIS:set_attribute(pPageFaultsPerSecond,F),
                            S2 = integer_to_list(round(L2)) ++ "% used," ++ S1 ++ "MB free" ++ "," ++ integer_to_list(round(F)) ++ " pages/sec"
                            end,
                            THIS:set_attribute(?STATE_STRING,S2)
                        end
                    end
        end;
    _ ->
        THIS:set_attribute(pPercentFull,"n/a"), 
        THIS:set_attribute(pFreeSpace,"n/a"),
        THIS:set_attribute(pPageFaultsPerSecond,"n/a"),
        THIS:set_attribute(pMeasurement,0),         
        THIS:set_attribute(?NO_DATA,true),
        THIS:set_attribute(?STATE_STRING,ResultList), 
		THIS:set_attribute(?CATEGORY,?NO_DATA),
		{error,get_data}
    end.
	%Cmd = get_memory_cmd(Machine,Osfamily),
	%case Osfamily of
	%    win32 ->
	%	    Data = os:cmd(Cmd),
	%		case Machine#machine.os of
	%		    "nt" ->
    %                get_memory_info_win32(Data);
	%			"linux" ->
    %                get_memory_info_linux(Data)
	%		end;
    %    unix ->
    %        io:format("unix")
    %end.			
                			
			

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
    %使用量 = （free/(1-使用率) - free）/总量
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
	%THIS:set_attribute(?STATE_STRING,)

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
        #property{name=pPercentFull,title="percentfull",type=numeric,order=1,configurable=false,state=true,baselinable=true},
		#property{name=pFreeSpace,title="MB free(MB)",type=numeric,order=2,configurable=false,state=true,upIsBad=false,baselinable=true},
		#property{name=pPageFaultsPerSecond,title="pages/sec(pages/sec)",type=numeric,order=3,configurable=false,state=true,baselinable=true}
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
get_classifier(important)->
	io:format("------------------------in memory"),
	Cls = case THIS:get_property(important_classifier) of
		{ok,{important_classifier,Classifier}}->
			io:format("------------------------in memory Classifier ~p",[Classifier]),
			Classifier;
		_->
			[{percentfull,'>=',85}]
	end,
	if 
		length(Cls)<3->
			Cls ++ lists:map(fun(X)->{'percentfull','',''} end,lists:seq(1,3 - length(Cls)));
		true ->
			Cls
	end;
get_classifier(minor)->
	Cls = case THIS:get_property(minor_classifier) of
		{ok,{minor_classifier,Classifier}}->
			Classifier;
		_->
			[{percentfull,'>=',70}]
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