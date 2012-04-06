%%
%% service_monitor
%%

%% @author lei.lin@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc service monitor
-module(service_monitor,[BASE]).
-extends(server_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").
-define(MEMORY_PAGESIZE,20).

-export([new/0,update/0,getScalarValues/2,get_classifier/1,get_template_property/0]).


%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for service monitor
new()->
	Base = server_monitor:new(),
	Base:set_attribute(processCPU,0),
	Base:set_attribute(processMemory,0),
	Base:set_attribute(processCount,0),
        Base:set_attribute(processCount,0),
	Base:set_attribute(lastMeasurement,"n/a"),
	Base:set_attribute(checkMemory,false),
	Base:set_attribute(lastMeasurementTime,"n/a"),
	{?MODULE,Base}.
	
%% @spec update() -> Result
%% Result = term()
%% @doc update is the run function called by schedule to test the  service monitor
update()->
	{ok,{_,Service}} = THIS:get_property(service),
	{ok,{_,Machine}} = THIS:get_property(machine),
	ProcessName = case THIS:get_property(process) of
						{ok,{_,N1}} ->
							case regexp:match(string:to_lower(N1),"\\.exe$") of
								{match,_,_}->
									string:substr(N1,1,length(N1)-4);
								_->
									N1
							end;
						_->
							""
					end,
	%~ io:format("ProcessName:~p~n",[ProcessName]),
	% = THIS:get_property(process),
	{ok,{_,Host}} = THIS:get_property(machine),	
	% 新增SNMP进程检测
	case machine:getSnmpMachine(Host) of
	{ok, Ma=#machine{}} ->
	    io:format("SNMPsucess:~p~n",[Host]),
		case snmp_machine:getwin32_processStatus(Ma,ProcessName) of
			{ok, StatusResult} ->
			    io:format("StatusResult:~p~n",[StatusResult]),
				Status = 
				if
					StatusResult == 1 ->
					"running";
				true ->
					"stopped"
				end,
				StatusString = 	Status++
				case Status of
					"running"->
						case snmp_machine:getwin32_cpuUsed(Ma,ProcessName) of
							{ok, CpuResult} ->
								 io:format("CpuResult:~p~n",[CpuResult]),
								 lists:flatten(io_lib:format(",~p CpuUsed",[CpuResult]));
							true->
								 ""
						end
				end,	
				io:format("StatusString:~p~n",[StatusString]),
				THIS:set_attribute(?STATE_STRING,StatusString);
			_ -> {error,error}
		end;
	_ ->

		IsWindows = case THIS:getPlatform() of "nt"->true;_->false end,%%platform:isWindows(THIS:getPlatform()),

		if IsWindows ->
			ChkM = false;
		true->        
			case THIS:get_property(checkMemory) of
				{ok,{_,ChkM1}} ->  
					ChkM = ChkM1;
				_ ->
					ChkM = false
			end
		end,
		UseProcessName = Service=="(using Process Name)", 
		%%io:format("UseProcessName = ~p~n",[UseProcessName]),
		Result1 =
		if
			IsWindows andalso UseProcessName ->
				LastMeasurement = case THIS:get_attribute(lastMeasurement) of {ok,{_,"n/a"}}->0;{ok,{_,Tmp1}}->Tmp1;_->0 end,
				LastMeasurementTime = case THIS:get_attribute(lastMeasurementTime) of {ok,{_,"n/a"}}->0;{ok,{_,Tmp2}}->Tmp2;_->0 end,
				Ret = platform:processUsed(Machine,ProcessName,LastMeasurement,LastMeasurementTime),

				THIS:set_attribute(lastMeasurement,lists:nth(1,Ret)),

				THIS:set_attribute(lastMeasurementTime,lists:nth(3,Ret)),

				[lists:nth(2,Ret),lists:nth(1,Ret),lists:nth(5,Ret)];
			true ->
				PageSize =
				if
					(not IsWindows) andalso ChkM ->
						Cmds1 = case machine:getCommandString('serviceMonitor',Machine) of
									{ok,Tmp2}->
										Tmp2;
									_->
										""
								end,
						Cmds2 = 
						if
							length(Cmds1)>0->
								case machine:getCommandSetting(Machine,'serviceMonitor','pageSize') of
									{error,_}->
										"";
									{ok,Tmp1}->
										Tmp1
								end;
							true ->
								machine:getCommandSetting(Machine,'processDetail','pageSize')
						end,
						case Cmds2 of
								"compute"->
									-2;
								_->
									Cmds2
							end;
					
					true ->
						0
				end,           
				Ret2 = platform:checkProcess(iconv:convert("utf-8", "gbk", Service),Machine,PageSize,true,THIS,[]),
				case lists:nth(3,Ret2) of
					{false,false}->
						[lists:nth(1,Ret2),-1,-1];
					_->
						[lists:nth(1,Ret2),-1,lists:nth(3,Ret2)]
				end
		end,
		
		
		Result = list_to_tuple(Result1),	
		if
			IsWindows andalso UseProcessName orelse ChkM ->
				if
					element(2,Result)==-1 ->
						THIS:set_attribute(processCPU,"n/a"),
						THIS:set_attribute(?NO_DATA,true),
						THIS:set_attribute(?MEASUREMENT,0);
					true ->
						THIS:set_attribute(processCPU,element(2,Result)),
						THIS:set_attribute(?MEASUREMENT,element(2,Result))
				end,
				if
					element(3,Result)==-1->
						THIS:set_attribute(processMemory,"n/a"),
						THIS:set_attribute(?NO_DATA,true);
					true->
						THIS:set_attribute(processMemory,element(3,Result))
				end;
			true->
				pass
		end,
		THIS:set_attribute(processCount,element(1,Result)),
		Status = 
		if
			element(1,Result)>0->
				"running";
			element(1,Result) =:= -1, IsWindows->
				"stopped";
			element(1,Result) =:= -2, IsWindows ->
				"paused";
			true->
				"not found"
				%%if
				%%	element(4,Result)==1->
				%%		"not found";
				%%	true->
				%%		"error code:" ++ integer_to_list(element(4,Result))
				%%end
		end,
		THIS:set_attribute(status,Status),
		StatusString = Status ++
		case Status of
			"running"->
				if
					element(1,Result)>1->
						lists:flatten(io_lib:format(",~p processes",[element(1,Result)]));
					true->
						""
				end ++
				if
					element(2,Result)=/=-1->
						lists:flatten(io_lib:format(",~p % cpu",[round(element(2,Result)*100)/100]));
					true->
						""
				end ++
				if
					(IsWindows andalso length(ProcessName)>0) orelse ChkM ->
						if
							element(3,Result)=/=-1->
								Msize = round(element(3,Result) /1048576 * 100)/100,
								lists:flatten(io_lib:format(",~p MB memory",[Msize]));
							true->
								""
						end;
					true ->
						""
				end;
			_->
				""
		end,
	    THIS:set_attribute(?STATE_STRING,StatusString)
    end.
        

getLogProperties()->
	[processCPU,processMemory,processCount,lastMeasurement].

%% @spec getScalarValues(Prop,Params) -> Result
%% Result = [term()]
%% @doc getScalarValues is the run function called by schedule to get drop-downlist box
getScalarValues(Prop,Params)->
	case Prop of
		service->
			case proplists:get_value(machine,Params) of
				undefined->
					[];
				Host->
					Ret = case platform:getProcesses(Host) of
								{error,_}->
									[];
								{_,Temp}->
									[{iconv:convert("gbk", "utf-8", X),iconv:convert("gbk", "utf-8", X)}||
										X<-lists:sort(fun(A,B)->string:to_upper(A)<string:to_upper(B) end,Temp)]
							end,
					% IsWindows = platform:isWindows(),
					case machine:getOS(Host) of
						1->
							Ret++[{"(using Process Name)","(using Process Name)"}];
						_->
							Ret
					end
			end;
		process->
			case proplists:get_value(machine,Params) of
				undefined->
					[];
				Host->
					Ret = 
					case Host of
						""->
							proxy:process(1,"localhost","administrator","1");
						_->
							case machine:getMachine(Host) of
								[]->
									{error,[]};
								[M|_]->
									io:format("Method:~p~n",[M#machine.method]),
									case M#machine.method of
										"Snmp" -> getwin32_process(Host);
									     _ -> proxy:process(1,M#machine.host,M#machine.login,M#machine.passwd)
									end
							end
					end,
					io:format("Ret:~p~n",[Ret]),
					case Ret of
						{error,_}->[];
						{ok,Pstr}->
							PLists =
							lists:foldl(fun(X,R)->
									case lists:member(X,R) of
										true->
											R;
										_->
											[X|R]
									end end,[],string:tokens(Pstr,"$")),
							[{X,X}||X<-lists:sort(fun(A,B)->string:to_upper(A)<string:to_upper(B) end,PLists)];
						{snmp,Result} -> Result
					end
			end;
		_->
			BASE:getScalarValues(Prop,Params)
	end.

%% @spec defaultTitle(Params) -> List
%% Params = [term()]
%% List = string()
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	BASE:defaultTitle(Params) ++ "(" ++ case proplists:get_value(service,Params) of undefined->"";V->V end ++ ")".


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
			[{status,'!=',"running"}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'!=',"running"}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'==',"running"}]
	end.


%% @spec get_template_property() -> List
%% List = [term()]
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
get_template_property()->
	Params = case THIS:get_property(?PAGE_PARAMS) of
		{ok,{_,V1}}->
			V1;
		_->
			[]
		end,
	TargetPlatform = case proplists:get_value(machine,Params) of
		undefined->
			1;
		V2->
			machine:getOS(V2)
	end,
	BASE:get_template_property() ++
	case TargetPlatform of
		1 ->
			[
			#property{name=service,title="Service",type=scalar,allowother=true,description="the NT service to monitor"},
			#property{name=process,title="Process Name",type=scalar,description="optional process name for process count and cpu usage (example: httpd)."}
			];
		_->
			[
			#property{name=service,title="Process",type=scalar,allowother=true,description="the process to monitor"},
			#property{name=checkMemory,title="Measure Process Memory Use",type=bool,advance=true,order=1,description="when selected, measure amount of virtual memory used by process"}
			]
	end ++ 
	[
	%% #property{name=service,title="Service",type=scalar,allowother=true,description="the NT service to monitor"},
	%% #property{name=process,title="Process Name",type=text,order=8,advance=true,description="optional process name for process count and cpu usage (example: httpd); use a string or a <a href=/SiteView/docs/regexp.htm>regular expression</a>"},
	#property{name=status,title="status",type=text,configurable=false,state=true},
	#property{name=processCPU,title="cpu",type=numeric,configurable=false,state=true,baselinable=true},
	#property{name=processMemory,title="memory",type=numeric,configurable=false,state=true,baselinable=true},
	#property{name=processCount,title="processes",type=numeric,configurable=false,state=true}
	].
	
%% 获取进程信息	
getwin32_process(Host) ->
	case machine:getSnmpMachine(Host) of
        {ok, Ma=#machine{}} ->
			   case snmp_machine:getwin32_processname(Host) of
					{ok, Result} ->
							{snmp,Result};
					_ -> {error,error}
			   end;
		_ ->
			error
	end.	


