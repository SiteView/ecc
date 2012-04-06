%%
%% memory monitor
%%

%% @author jun.xiao@dragonflow.com
%% @copyright 2011-2012 Dragonflow
%% @version 1.0
%% @doc memory monitor
-module(win_snmp_cpu_monitor,[BASE]).
-compile(export_all).
-extends(browsable_base).
-include("monitor.hrl").
-include("monitor_template.hrl").

-export([new/0,update/0,get_classifier/1,get_template_property/0,verify/1,getIp/1]).


%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for memory monitor
new()->
	Base = browsable_base:new(),
	Base:set_property(pCpuUsed,""),
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
	ErIp=getIp(CpuUsed),
%	ErIp = textutils:ipStr2ErIp(CpuUsed),
	Session = snmp_session:new(ErIp,161,"V2",Comm,"MD5","","","","","",5000),
	Var = Session:get_table_col([1,3,6,1,2,1,25,3,3,1,2]),
	BrowKeylist  = [X||{X,_}<-Browse],
	%io:format("BrowKeylist***************:~p~n",[Browse]),
	%io:format("getResult***************:~p~n",[getResult(BrowKeylist,Var)]),
	Stat = ["CPU core:"++X ++"%used,  "||X<-getResult(BrowKeylist,Var)],
	THIS:set_attribute(?STATE_STRING,Stat).
 
getResult([],_)->[];
getResult([H|T],Var)->
	CpuUsed = [integer_to_list(X)||{_,Z,_,X,_}<-Var,Z==H],
	[CpuUsedNum] = CpuUsed,
	{ok,{_,Counters}} = THIS:get_property(browse),
	{_,BrowseAttribute} = lists:keyfind(H, 1, Counters),
	THIS:set_attribute(BrowseAttribute,list_to_integer(CpuUsedNum)),
	lists:append(CpuUsed,getResult(T,Var)).

getLogProperties(This)->
	{ok,{_,Counters}} = THIS:get_property(browse),
	Temp = This:get_template_property(),
	[X#property.name || X<-Temp,X#property.state=:=true] ++ [X||{_,X}<- Counters].


%% @spec get_template_property() -> list()
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
get_template_property() ->
    BASE:get_template_property() ++ 
    [
	#property{name=pCpuUsed,title="host name(or ip address)",type=text,order=3,editable=true},
	#property{name=pComm,title="public communicator",type=text,order=4,editable=true}
    ].

getBrowseData(Params)->
	IpStr=proplists:get_value(pCpuUsed,Params), 
	ErIp=getIp(IpStr),
	Session = snmp_session:new(ErIp,161,"V2",proplists:get_value(pComm,Params),"MD5","","","","","",5000),
	Var = Session:get_table_col([1,3,6,1,2,1,25,3,3,1,2]),
	
	BDLIST = [{X,"core"}||{_,X,_,_,_}<-Var],
	makeBrowseData(BDLIST,length(BDLIST)).

%If IP return with the format IP, if the hostname make it to IP
getIp(IpStr)->
	ErIp = 
	case ip_utils:check_ip(IpStr) of
		{ok,is_ip} ->  
			textutils:ipStr2ErIp(IpStr);
		_ ->
		 
			
			{ok,{_,_,_,_,_,N}}=inet:gethostbyname(IpStr), 
			[H|T]=N, 
			case erlang:size(H) of
				4 ->
					H_Str=integer_to_list(element(1,H))++"."++ integer_to_list(element(2,H))++"."++ integer_to_list(element(3,H))++"."++ integer_to_list(element(4,H)),
					textutils:ipStr2ErIp(H_Str);
				_->{0,0,0,0}				
			end
			
	end.

makeBrowseData([H|T],Num)->
	{X,Y} = H,
	Term = [{X,Y++integer_to_list(Num)++"#"}],
	lists:append(Term,makeBrowseData(T,Num-1));
makeBrowseData(_,0)->
		[].


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