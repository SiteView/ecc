%%
%% service_monitor
%%

%% @author lei.lin@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc process monitor
-module(process_monitor,[BASE]).
-extends(server_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").
-define(MEMORY_PAGESIZE,20).

-export([new/0,update/0,getScalarValues/2,get_classifier/1,get_template_property/0]).


%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for process monitor
new()->
	Base = server_monitor:new(),
	Base:set_attribute(processCPU,0),
	Base:set_attribute(processMemory,0),
	Base:set_attribute(processCount,0),
	Base:set_attribute(lastMeasurement,"n/a"),
	Base:set_attribute(lastMeasurementTime,"n/a"),
	{?MODULE,Base}.
	
%% @spec update() -> Result
%% Result = term()
%% @doc update is the run function called by schedule to test the  process monitor
update()->
    {ok,{_,Machine}} = THIS:get_property(machine),
	
    ProcessName =   case THIS:get_property(process) of
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
		    
    IsWindows = case THIS:getPlatform() of
			"nt"->true;
			_->false
		end,
    if IsWindows ->
           ChkM = false;
       true->        
	    {ok,{_,ChkM}} = THIS:get_property(checkMemory)
    end,
    
	
    Result1 =
	if
		IsWindows   ->
			LastMeasurement = case THIS:get_attribute(lastMeasurement) of {ok,{_,"n/a"}}->0;{ok,{_,Tmp1}}->Tmp1;_->0 end,
			LastMeasurementTime = case THIS:get_attribute(lastMeasurementTime) of {ok,{_,"n/a"}}->0;{ok,{_,Tmp2}}->Tmp2;_->0 end,
			
			Ret = platform:processUsed(Machine,ProcessName,LastMeasurement,LastMeasurementTime),

			THIS:set_attribute(lastMeasurement,lists:nth(1,Ret)),
			THIS:set_attribute(lastMeasurementTime,lists:nth(3,Ret)),

			[lists:nth(2,Ret),lists:nth(1,Ret),lists:nth(5,Ret)];
		true ->
			io:format("not windows"),
			[0,-1,-1,-1]
			
	end,
	
	io:format("Result1 = ~p~n",[Result1]),
	Result = list_to_tuple(Result1),	
	if
		IsWindows orelse ChkM ->
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
	end,

	StatusString = 
	case Status of
		"running"->
			if
				element(1,Result)>0->
					lists:flatten(io_lib:format("~p processes",[element(1,Result)]));
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
							io:format("Machine = ~p~n", [Machine]),
							Msize = case Machine of % remote wmi get process data error
									  "" -> % host
									    round(element(3,Result) /1048576 * 100)/100; % 1048576 = 1024*1024
								      _-> % remote wmi get data error
										round(element(3,Result) /1073741824 * 100)/100 % 1073741824 = 1024*1024*1024
									end,
							lists:flatten(io_lib:format(",~p MB memory",[Msize]));
						true->
							""
					end;
				true ->
					""
			end;
		_->
			"not find"
	end,
	THIS:set_attribute(?STATE_STRING,StatusString).



getLogProperties()->
	[processCPU,processMemory,processCount,lastMeasurement].

%% @spec getScalarValues(Prop,Params) -> Result
%% Result = [term()]
%% @doc getScalarValues is the run function called by schedule to get drop-downlist box
getScalarValues(Prop,Params)->
	case Prop of
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
									proxy:process(1,M#machine.host,M#machine.login,M#machine.passwd)
							end
					end,
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
							[{X,X}||X<-lists:sort(fun(A,B)->string:to_upper(A)<string:to_upper(B) end,PLists)]
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
	BASE:defaultTitle(Params) ++ "(" ++ case proplists:get_value(process,Params) of
						undefined->"";
						V->V 
					    end ++ ")".


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
			[{processCount,'==',0}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{processCount,'>=',2}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{processCount,'==',1}]
	end.


%% @spec get_template_property() -> List
%% List = [term()]
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
get_template_property()->
	BASE:get_template_property() ++ 
	[
	%% #property{name=service,title="Service",type=scalar,allowother=true,description="the NT service to monitor"},
	#property{name=process,title="process name",type=scalar,editable=true,order=2},
	%% #property{name=status,title="status",type=text,configurable=false,state=true},
	#property{name=processCPU,title="cpu",type=numeric,configurable=false,state=true,baselinable=true},
	#property{name=processMemory,title="memory",type=numeric,configurable=false,state=true,baselinable=true},
	#property{name=processCount,title="processes",type=numeric,configurable=false,state=true}
	].


