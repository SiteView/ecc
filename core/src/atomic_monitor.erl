%% 
%% @doc monitor base class
%% @author shixianfang<xianfang.shi@dragonflow.com>
%% @version {0.1}
-module(atomic_monitor,[BASE]).
-extends(monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").
-include("ecc_monitor_node.hrl").

%% @spec new()->Object
%% @doc atomic_monitor construct class
%% 
new()->
	Base = monitor:new(),
	%%Base:set_attribute(monitorsInGroup,0),
	%%Base:set_attribute(monitorsInError,0),
	Base:set_attribute(maxErrorCount,0),
	Base:set_attribute(monitorSkips,0),
	Base:set_attribute(running,false),
	Base:set_attribute(?CATEGORY,?NO_DATA),
	Base:set_attribute(?MEASUREMENT,0),
	Base:set_attribute(progressString,""),
	Base:set_attribute(currentFrequency,0),
	Base:set_attribute(?SAMPLE,0),
	{?MODULE,Base}.
	
init(This,Data)->
	case proplists:get_value(id,Data) of
		undefined->
			pass;
		Id->
			dbcs_base:set_app(proplists:get_value(?APP,Data)),
			siteview:remove_object(Id),
			This:remove_attribute(baseline_removed),
			BASE:init(This,Data),
			%%io:format("~p~n",[Id]),
			
			siteview:set_object(Id,monitor,element(1,This),This)
	end.

%% @spec update()->ok
%% @doc monitor update
%%
update()->ok.


getCostInLicensePoints()->1.


%% @spec testUpdate(This)->(ok|error)
%% @doc update test
%%
testUpdate(This)->
	Ret = This:update(),
	This:runClassifiers(This),
	Ret.
	
%% @spec recalculateFrequency(Flag)->{ok,Resean} | {error,Resean}
%% @doc recalculate monitor's frequency
%%
recalculateFrequency(Category)->
	{ok,{_,Freq}} = THIS:get_property(?FREQUENCY),
	{ok,{_,ErrFreq}} = THIS:get_property(?ERROR_FREQUENCY),
    
	% io:format("category:~p,error freuquency:~p~n",[Category,ErrFreq]),
	if
		ErrFreq=/= 0 andalso Category=/=good ->
			THIS:set_attribute(currentFrequency,ErrFreq*1000);
		true ->
			THIS:set_attribute(currentFrequency,Freq*1000)
	end.
			
%% @spec startMonitor(This)-> (ok | error)
%% @doc start the monitor 
%%
startMonitor(This)->
	BASE:startMonitor(This),
	This:startRunner(This),
	case This:get_attribute(action) of
		{error,_}->
			{ok,{_,Id}} = This:get_property(id),
			% monitor_status_store:start_link(),
			case monitor_status_store:read(This:get_app(),Id) of
				{ok,StData}->
					lists:map(fun({K,V})->
						This:set_attribute(K,V)
						end,StData);
				_->
					pass
			end,
			
			Action = update_monitor:new(This),
			This:set_attribute(action,Action),
			This:set_attribute('_machine',This:getHostname()),
			DisabledReason = This:whyDisabled(),
			case DisabledReason of
				account_disable->
					{error,account_disable};
				_->
					
					This:set_attribute(okFrequency,This:get_property_as_number(?FREQUENCY)*1000),
					case This:get_property(?ERROR_FREQUENCY) of
						{ok,{?ERROR_FREQUENCY,_}}->
							This:set_attribute(errorFrequency,This:get_property_as_number(?ERROR_FREQUENCY)*1000);
						_->
							error
					end,
					Category = case This:get_attribute(?CATEGORY) of
								{ok,{?CATEGORY,CG}}->
									CG;
								_->
									good
								end,
					This:recalculateFrequency(Category),
					{ok,{_,Freq}} = This:get_attribute(currentFrequency),
					Now = sv_datetime:now(),
					LastUpdate = case This:get_attribute_as_number(?LAST_UPDATE) of
									Val when Val > Now ->
										Now;
									Val ->
										Val
								end,
					This:set_attribute(?LAST_UPDATE,LastUpdate),
					ForceRefresh= case This:get_property(?FORCE_REFRESH) of
									{ok,{?FORCE_REFRESH,_}}->
										true;
									_->
										false
									end,
					CurrentMillis = sv_datetime:now(),
					% Interval = LastUpdate + Freq*1000 -CurrentMillis,
					UpdateTime=
					if 
						DisabledReason == "" orelse ForceRefresh->
							if 
								LastUpdate == 0 ->
									MonitorDelayBetweenRefresh = This:get_property_as_number(?MONITOR_DELAY_BETWEEN_REFRESH,1000),
									% RandomDelay = 2 + random:uniform(100) * This:get_property_as_number(?INITIAL_MONITOR_DELAY,60),
									MonitorSeq = 
									case get(monitor_sequence) of
										undefined->put(monitor_sequence,1),1;
										V->put(monitor_sequence,V+1),V+1
									end,
									RandomDelay = 2 + MonitorSeq * This:get_property_as_number(?INITIAL_MONITOR_DELAY,200) + random:uniform(100) * 2,	
									CurrentMillis + RandomDelay +MonitorDelayBetweenRefresh - Freq;
									% This:set_attribute(?LAST_UPDATE,CurrentMillis + RandomDelay +MonitorDelayBetweenRefresh);
								% Interval < 5000 ->
								%	RandomDelay = 2 + random:uniform(20) * This:get_property_as_number(?INITIAL_MONITOR_DELAY,600),
									% This:set_attribute(?LAST_UPDATE,CurrentMillis - This:get_attribute_as_number(currentFrequency) + RandomDelay*1000);
								%	CurrentMillis - Freq + RandomDelay*1000;
								true->
									LastUpdate
							end;
						true->
							LastUpdate
					end,
					Schedule = This:get_schedule(),
					Scheduler = case siteview:get_monitor_scheduler() of
									{ok,Sche}->
										Sche;
									_->
										{error,no_found_scheduler}
								end,
					CF = This:get_attribute_as_number(currentFrequency),
					% io:format("StartMonitor:~p~n",[UpdateTime]),
					if 
						% Schedule =/= null,Schedule#schedule.type=:="absolute",ForceRefresh =:= false ->
							% case Scheduler:scheduleAbsolutePeriodicAction(Action,Schedule,UpdateTime) of
								% {ok,_}->
									% {ok,started};
								% {error,R1}->
									% {error,R1};
								% Err1->
									% {error,Err1}
							% end;
						CF =/= 0 ->
							% io:format("scheduler:scheduleRepeatedPeriodicAction:~p,~p~n",[CF,ForceRefresh]),
							case Scheduler:scheduleRepeatedPeriodicAction(Action,CF,UpdateTime)of
								{ok,_}->
									{ok,started};
								{error,R2}->
									{error,R2};
								_->
									{error,start_failure}
							end;
						true->
							io:format("scheduler do nothing~p,~p,~p~n",[CF,Schedule,ForceRefresh]),
							{error,not_scheduler}
					end
			end;
		_->
			{ok,already_running}
	end.
	
unschedule(This)->
	case siteview:get_monitor_scheduler() of
		{ok,Sche}->
			case This:get_attribute(action) of
				{ok,{action,Action}}->
					case Sche:unschedule(Action) of
						{ok,Ret}->
							This:remove_attribute(action),
							Action:delete(),
							{ok,Ret};
						Err->
							Err
					end;
				_->
					io:format("stopMonitor not found action~n"),
					{ok,no_action}
			end;
		_->
			{error,no_found_scheduler}
	end.

%% @spec stopMonitor(This)->({ok,Result}|{error,Reason})
%% @doc stop monitor action
%%							
stopMonitor(This)->
	Scheduler = case siteview:get_monitor_scheduler() of
					{ok,Sche}->
						Sche;
					_->
						{error,no_found_scheduler}
				end,
	case This:get_attribute(action) of
		{ok,{action,Action}}->
			%io:format("stopMonitor:~p~n",[Action]),
			Scheduler:unschedule(Action),
			This:remove_attribute(action),

			Action:delete();
		_->
			io:format("stopMonitor not found action~n")
	end,
	This:stopRunner(This),
	BASE:stopMonitor(This),
	This:set_attribute(maxErrorCount,0),
	This:set_attribute(monitorSkips,0),
	This:set_attribute(running,false),
	This:set_attribute(?CATEGORY,?NO_DATA),
	This:set_attribute(?MEASUREMENT,0),
	This:set_attribute(progressString,""),
	This:set_attribute(currentFrequency,0),
	This:set_attribute(?SAMPLE,0),
	{ok,stopped}.
	
delete()->
	try
	Scheduler = case siteview:get_monitor_scheduler() of
					{ok,Sche}->
						Sche;
					_->
						{error,no_found_scheduler}
				end,
	case THIS:get_attribute(action) of
		{ok,{action,Action}}->
			Scheduler:unschedule(Action),
			THIS:remove_attribute(action),
			Action:delete();
		_->
			pass
	end
	catch
	_:_->error
	end,
	BASE:delete(). 

%% @spec runUpdate(This,Bool,Aysnc)-> (true|false)
%% @doc run update action 
%%
runUpdate(This,Bool,Aysnc)->
	runUpdate(This,This:get_attribute(running),This:isDisabled(),This:isSuspended(),Bool,Aysnc).

runUpdate(This,{ok,{running,true}},false,false,_,_)->
	This:inc_attribute(monitorSkips),
	{ok,{_,SkipCount}} = This:get_attribute(monitorSkips),
	{ok,{_,Id}} = This:get_property(?ID),
	{ok,{_,Name}} = This:get_property(?NAME),
	?ERROR_LOG2("skipped #~w , monitor still running:~w(~s)",[SkipCount,Id,Name]),
	{ok,{monitorSkips,Mskips}} = This:get_attribute(monitorSkips),
	if 
		Mskips >= ?MAX_MONITORS_SKIPS ->
			This:setDisabledProperties(lists:flatten(io_lib:format("Disabled due to Skips > ~p",[?MAX_MONITORS_SKIPS]))),
			This:set_property(?DISABLED_DESCRIPTION,lists:flatten(io_lib:format("Disabled due to Skips > ~p",[?MAX_MONITORS_SKIPS]))),
			This:set_property(?DISABLED,true),
			false;
		true->
			false
	end;
runUpdate(This,_,_,_,_,Aysnc)->
	This:set_attribute(monitorSkips, 0),
	ForceRefresh= case This:get_property(?FORCE_REFRESH) of
						{ok,{?FORCE_REFRESH,_}}->
							true;
						_->
							false
						end,
	%%DsDesc = case This:get_property(?DISABLED_DESCRIPTION) of
	%		{ok,{?DISABLED_DESCRIPTION,Val}}->
	%			Val;
	%		_->
	%			""
	%		end,
	%%Pos = string:rstr(DsDesc,"Disabled due to Skips >"),
	%%if
	%%	Pos > 0 ->
	%%		This:remove_property(?DISABLED_DESCRIPTION),
	%%		This:remove_property(?DISABLED);
	%%	true->
	%%		do_nothing
	%%end,
	Suspended = This:isSuspended(),
	Disabled = This:isDisabled(),
	% 
	{ok,{_,Freq}} = This:get_attribute(currentFrequency),
	
	if
		 Suspended =:= true ->
            This:set_attribute(?STATE_STRING,"Monitor runs are currently suspended by general setting"),
            This:set_attribute(?CATEGORY, nodata),
			This:set_attribute(progressString,"Monitor runs are currently suspended"),
			false;
		Disabled =:= true,ForceRefresh =:= false->
			This:setDisabledProperties(This:whyDisabled()),
			This:set_attribute(progressString,This:whyDisabled()),
			false;
		true->
			ScheduleEnabled = schedule_property:is_enabled(This:get_schedule(),Freq),
			if 
				ScheduleEnabled=:=false, ForceRefresh=:=false ->
					This:set_attribute(progressString,"Monitor will not run due to current schedule.\nTo run monitor push the \"Run Once\" link on the Detail Page\n\n"),
					false;
				true ->
					This:set_attribute(running,true),
					% This:set_attribute(?STATE_STRING,"running..."),
					try
						case Aysnc of
							true->
								io:format("**********************************~n"),
								This:runExclusively(This);
							_->
								This:run(This,hi)
								% This:run_rpc(This)
						end,
						true
					catch
					E1:E2->
						This:set_attribute(running,false),
						?ERROR_LOG2("ERROR:atomic_monitor:monitorUpdate_RPC catch exception:~p~n",[erlang:get_stacktrace()]),
						false
					end
					
			end

	end.

%% @spec startRunner(This)->ok
%% @doc start a monitor process
startRunner(_)->
%%	Pid = spawn(fun()-> This:run(This) end),
%%	This:set_attribute(run_id,Pid),
	ok.

%% @spec stopRunner(This)->ok
%% @doc stop the monitor process
stopRunner(_)->ok.
%%	case This:get_attribute(run_id) of
%%		{ok,{run_id,Pid}}->
%%			Pid ! {self(),stop},
%%			receive
%%				{Pid,{ok,_}}->
%%					{ok,stoppped};
%%				_->
%%					{error,fail}
%%			after 1000->
%%				{error,timeout}
%%			end;
%%		_->
%%			{error,no_running}
%%	end.

%% @spec runExclusively(This)-> ({ok,Result} | {error,Reason})
%%
%%
runExclusively(This)->
%%	{ok,{run_id,Rid}} = This:get_attribute(run_id),
%%	Rid ! {self(),run}.
	try
	rpc:cast(node(),monitor_runner,run,[This])
	catch
	Err->?ERROR_LOG2("atomic_monitor:runExclusively:~p",[Err])
	end.
	

%% @spec runOwnRules()-> true | false
%% @doc whether run subclass's rule
%%
runOwnRules()->
	case THIS:get_property(?RUN_OWNRULES) of
		{ok,_}->
			true;
		_->
			false
	end.

%% @spec reportStatus()->(true | false)
%% @doc report monitor's status
%%
reportStatus()->
	case THIS:get_attribute(alwaysReportStatus) of
		{ok,{alwaysReportStatus,Val}}->
			Val;
		_->
			false
	end.

%% @spec monitorUpdate(This)->(ok|{error,Reason})
%% @doc update monitor
%%
monitorUpdate(This)->
	This:remove_property(?NO_DATA),
	% io:format("atomic_monitor:monitorUpdate:~p~n",[This:get_attribute(currentStatus)]),
	{ok,{_,Id}} = This:get_property(id),
	try
		case This:update() of
			{error,{Code,Msg}}->
				% This:set_attribute(?OPERATION_ERROR_CODE,Code),
				% This:set_attribute(?OPERATION_ERROR_MESSAGE,Msg),
				error;
			{error,_}->
				This:set_attribute(running,false),
				% THIS:set_property(?CATEGORY,error),
				error;
			_->
				ok
		end
	catch
	EE:Err->
		This:set_attribute(running,false),
		% This:set_attribute(?STATE_STRING,"error"),
		% This:set_attribute(?STATE_STRING,"no data"), 
		% This:set_attribute(?CATEGORY,?NO_DATA),
		?ERROR_LOG2("ERROR:monitor(~p) atomic_monitor:monitorUpdate catch exception:~p:~p~n~p~n",[Id,EE,Err,erlang:get_stacktrace()]),
		{error,exception_catch}
	end.
	
is_local(This)->
	case This:get_attribute(platform) of
		{error,_}->
			{ok,{_,Class}} = This:get_property(?CLASS),
			Pf = monitor_template:get_template_platform(Class),
			This:set_attribute(platform,Pf),
			lists:member(local,Pf);
		{ok,{_,Pfc}}->
			lists:member(local,Pfc)
	end.

monitorUpdate_RPC(This)->
	This:remove_property(?NO_DATA),
	{ok,{_,Id}} = This:get_property(id),
	try
	case This:is_local(This) of
		true->
			This:monitorUpdate(This);
		_->
			{ok,{_,Class}} = This:get_property(?CLASS),
			Pf = monitor_template:get_template_platform(Class),
			case monitor_proxy_server:get_node(This:get_app(),Id,Pf) of
				{ok,Node}->
					% io:format("->atomic_monitor:monitorUpdate_RPC:~p~n",[Id]),
					% io:format("monitor:attributes:~p~n",[This:get_attribute(action)]),
					Data = zlib:zip(term_to_binary({This:get_properties(),This:get_attributes()})),
					% Ret = rpc:call(Node,monitor_runner,run_rpc,[{This:get_properties(),This:get_attributes()},[]]),
					Ret = rpc:call(Node,monitor_runner,run_rpc,[Data,[]]),
							% io:format("==RPC CALL RET:~p~n",[Ret]),
						case Ret of
							{ok,Bin}->
								{P,A} = binary_to_term(zlib:unzip(Bin)),
								This:remove_properties(),
								This:add_properties(P),
								This:remove_attributes(),
								This:add_attributes(A),
								% io:format("monitor:attributes:~p~n",[This:get_attribute(action)]),
								% io:format("<-atomic_monitor:monitorUpdate_RPC:~p~n",[Id]),
								ok;
							{error,RpcErr}->
								% THIS:set_property(?CATEGORY,error),
								% This:set_attribute(?STATE_STRING,RpcErr),
								?ERROR_LOG2("ERROR:monitor(~p) atomic_monitor:monitorUpdate_RPC catch exception:~p~n",[Id,RpcErr]),
								Ret
						end;
						
				Else->
					App = This:get_app(),
					try
					monitor_local_stat:add_ref({App,Id}),
					Rr = This:monitorUpdate(This),
					monitor_local_stat:release({App,Id}),
					Rr
					% io:format("~p~n",[Else])
					catch
					E1:E2->
						monitor_local_stat:release({App,Id}),
						This:set_attribute(running,false),
						% This:set_attribute(?STATE_STRING,"error"),
						% This:set_attribute(?STATE_STRING,"no data"), 
						% This:set_attribute(?CATEGORY,?NO_DATA),
						?ERROR_LOG2("ERROR:monitor(~p) atomic_monitor:monitorUpdate_RPC catch exception:~p:~p~n~p~n",[Id,E1,E2,erlang:get_stacktrace()]),
						{error,exception_catch}
					end
			end
	end
	catch
	EE:Err->
		This:set_attribute(running,false),
		% This:set_attribute(?STATE_STRING,"error"),
		% This:set_attribute(?STATE_STRING,"no data"), 
		% This:set_attribute(?CATEGORY,?NO_DATA),
		?ERROR_LOG2("ERROR:monitor(~p) atomic_monitor:monitorUpdate_RPC catch exception:~p:~p~n~p~n",[Id,EE,Err,erlang:get_stacktrace()]),
		{error,exception_catch}
	end.

	
%% @spec run(This,_)-> (ok | {error,Reason})
%% @doc run action
%% 
run(This,_)->	 
	{ok,{id,Id}}= This:get_property(id),
	UpdateTime = sv_datetime:now(),
	dbcs_base:set_app(This:get_app(),true), %% set app 
	
	Ret = This:getDependencies([]),
	DependonOk = 
	if 
		length(Ret) > 0 ->
			% THIS:sleep(1000),
			case This:getDependsOnSignal([],This,true) of
				{true,_}->
					true;
				_->
					This:set_attribute(progressString,"<br><br>check dependon fail</b>"),
					false
			end;
		true->
			true
	end,
	case DependonOk of
		true->
			THIS:inc_attribute(?SAMPLE),
			This:set_attribute(currentStatus,"starting the monitor update..."),
			{ok,{_,Name}}=This:get_property(?NAME),
			Ps1 = "STARTING MONITOR: "++ Name,
			This:set_attribute(progressString,Ps1),
			
			RunOwnRules = This:runOwnRules(),
			% This:monitorUpdate(This),
			case This:monitorUpdate_RPC(This) of
				ok->
					This:set_attribute(currentStatus,"monitor update done..."),
					This:set_attribute(?LAST_UPDATE,UpdateTime),
					ReportStatus = This:reportStatus(),
					if
						RunOwnRules =:= false;RunOwnRules=:=true,ReportStatus=:= true ->
							This:runClassifiers(This);
						true->
							do_nothing
					end;
				_->
					This:set_attribute(?STATE_STRING,"no data"), 
					This:set_attribute(?CATEGORY,?NO_DATA)
			end,
			

			{ok,{?CATEGORY,Category}} = This:get_attribute(?CATEGORY),
			VerfiyErr = case This:get_property(?VERFIY_ERROR) of
							{ok,{_,true}}->
								true;
							_->
								false
						end,
			%% io:format("atomic_monitor:run:~p~n",[This:get_attribute(currentStatus)]),
			if 
				RunOwnRules=:= false,Category=:=error,VerfiyErr=:= true->
					% io:format("verfiy error monitor~n"),
					% This:monitorUpdate(This),
					case This:monitorUpdate_RPC(This) of
						ok->
							This:runClassifiers(This);
						_->
							This:set_attribute(?STATE_STRING,"no data"), 
							This:set_attribute(?CATEGORY,?NO_DATA)
					end;
				true->
					do_nothing
			end,
			{ok,{?CATEGORY,C2}} = This:get_attribute(?CATEGORY),
			
			{ok,{_,StateStr}} = This:get_attribute(?STATE_STRING),
			Ps2 = Ps1 ++ "<br><b>Monitor:" ++ atom_to_list(C2) ++"," ++ StateStr ++"</b>",
			This:set_attribute(progressString,Ps2),

			Ps3 = Ps2++"<br>Checking for Alerts",
			This:set_attribute(progressString,Ps3),

			This:save_result(This,C2),
			if 
				RunOwnRules =:= false ->
					This:runActionRules(This,C2);
				true->
					do_nothing
			end,

			Ps4=Ps3++"<br>Monitor done.<br>",
			This:set_attribute(progressString,Ps4),
			%dbcs_baseline:try_save_baseline_update_data(This),	
			This:set_attribute(running,false),
			This:set_attribute(?LAST_UPDATE,UpdateTime),
			monitor_status_store:save(Id,This:get_status_info(This)),
			This:reschedule(This,C2,sv_datetime:now());
		_->
			This:set_attribute(running,false)
	end.

%% @spec reschedule(This,Category,LastTime)->(ok|{error,Reason})
%% @doc reschedule monitor
%%
reschedule(This,Category,LastTime)->
	{ok,{_,Freq}} = This:get_attribute(currentFrequency),
	This:recalculateFrequency(Category),
	{ok,{_,CurFreq}}=This:get_attribute(currentFrequency),
	if
		Freq=/=CurFreq->
			{ok,{_,Action}}=This:get_attribute(action),
			Scheduler = case siteview:get_monitor_scheduler() of
									{ok,Sche}->
										Sche;
									_->
										undefined
								end,
			Scheduler:unschedule(Action),

			if
				CurFreq=/=0 ->
					Scheduler:scheduleRepeatedPeriodicAction(Action,CurFreq,LastTime+CurFreq);
				true ->
					error
			end;
		true ->
			error
	end.

%% @spec run(This)->ok
%% @doc monitor's process
%%
% run(This)->
	% receive
		% {_,run}->
			%{ok,{?NAME,Name}} = This:get_property(?NAME),
			%This:set_attribute(progressString,io_lib:format("STARTING MONITOR: ~p",[Name])),
			% THIS:inc_property(?SAMPLE),
			% This:set_attribute(currentStatus,"starting the monitor update..."),
			% {ok,{_,Name}}=This:get_property(?NAME),
			% Ps1 = "STARTING MONITOR: "++ Name,
			% This:set_attribute(progressString,Ps1),

			% This:monitorUpdate(This),

			% This:set_attribute(currentStatus,"monitor update done..."),
			% This:set_attribute(?LAST_UPDATE,sv_datetime:now()),
			% RunOwnRules = This:runOwnRules(),
			% ReportStatus = This:reportStatus(),
			% if
				% RunOwnRules =:= false;RunOwnRules=:=true,ReportStatus=:=true->
					% This:runClassifiers(This);
				% true->
					% do_nothing
			% end,
			% Ret = This:getDependencies([]),
			% if 
				% length(Ret) > 0 ->
					% THIS:sleep(1000),
					% case This:getDependsOnSignal([],This,true) of
						% {true,_}->
							% true;
						% _->
							% run(This)
					% end;
				% true->
					% true
			% end,
			% {ok,{?CATEGORY,Category}} = This:get_attribute(?CATEGORY),
			% VerfiyErr = case This:get_property(?VERFIY_ERROR) of
							% {ok,true}->
								% true;
							% _->
								% false
						% end,
			%io:format("atomic_monitor:run:~p~n",[This:get_attribute(currentStatus)]),
			% if 
				% RunOwnRules=:= false,Category=:=error,VerfiyErr=:= true->
					% This:monitorUpdate(This),
					% This:runClassifiers(This);
				% true->
					% do_nothing
			% end,
			
			% {ok,{?CATEGORY,C2}} = This:get_attribute(?CATEGORY),
	
			% {ok,{_,StateStr}} = This:get_attribute(?STATE_STRING),
			% Ps2 = Ps1 ++ "<br><b>Monitor:" ++ atom_to_list(C2) ++"," ++ StateStr ++"</b>",
			% This:set_attribute(progressString,Ps2),

			% Ps3 = Ps2++"<br>Checking for Alerts",
			% This:set_attribute(progressString,Ps3),

			% This:save_result(This,C2),

			% if 
				% RunOwnRules =:= false ->
					% This:runActionRules(This,Category);
				% true->
					% do_nothing
			% end,
			% Ps4=Ps3++"<br>Monitor done.<br>",

			% This:set_attribute(progressString,Ps4),
			
			% This:set_attribute(running,false),

			% This:set_attribute(?LAST_UPDATE,sv_datetime:now()),
			% This:reschedule(This,C2,sv_datetime:now()),

			% run(This);
		% {From,stop}->
			% From ! {self(),{ok,stopped}};
		% _->
			% error,
			% run(This)
	% end.

%% @spec isSuspended()->false
%% @doc whether monitor is suspended
%%
isSuspended()->
	case preferences:get(general,suspend_monitors) of
		{ok,[{_,Susp}]} ->
			Susp;
		_ ->
			false
	end.
	
isRunning()->
	case THIS:get_attribute(running) of
		{ok,{_,true}}->
			true;
		_->
			false
	end.

%% @spec setDisabledProperties(S)->({ok,Result}|{error,Reason})
%% @doc set disable properties
%%
setDisabledProperties(S)->
	THIS:inc_attribute(disabledTotal),
	THIS:set_attribute(?CATEGORY,nodata),
	THIS:set_attribute(?STATE_STRING,S),
	%%THIS:set_property(?MEASUREMENT,0),
	THIS:resetCategoryProperties(good),
	THIS:resetCategoryProperties(warning),
	THIS:resetCategoryProperties(error).



%% @spec getDependsOnSignal(Idlist,This,Flag)->({true,Result}|{false,Reason})
%% @doc get dependecies result
%%
getDependsOnSignal(Idlist,This,Flag)->
	Deps = This:getDependencies(false), %%%Find dependent objects
	Len = length(Deps),
	if 
		Len =:= 0 ->
			{true,no_depends};
		true->
			FullID=This:getFullID(),
			case lists:member(FullID,Idlist) of
				true->
					{false,circular_depends}; %%Cycle dependent
				_->
					NewIdlist = Idlist ++ [FullID],
					%%io:format("getDependsOnSignal:~p,~p,~p~n",[Deps,NewIdlist,FullID]),
					%%Result = [R||X<-Deps,R=This:check_depends(X,NewIdlist,This,Flag)],
					Result = This:check_depends(Deps,NewIdlist,This,Flag),
					case lists:member(false,Result) of
						true->
							{false,depends_error};
						false->
							{true,depends_ok}
					end
			end
	end.

%% @spec check_depends([Dep|T],Idlist,This,Flag)->List
%% @doc check dependecies result
%%
check_depends([],_,_,_)->[];
check_depends([{"none",_}|_],_,_,_)->[];
check_depends([Dep|T],Idlist,This,Flag)->
	io:format("check_depends:~p~n",[Dep]),
	{M,Cond} = Dep,
	Categ = case Cond of
				"error"->
					?ERROR_CATEGORY;
				"warning"->
					?WARNING_CATEGORY;
				"nodata"->
					?NO_DATA;
				_->
					?GOOD_CATEGORY
			end,
	% M:runUpdate(M,true,false),
	%%platform:sleep(5000),
	case M:get_attribute(?CATEGORY) of
		{ok,{?CATEGORY,Categ}}->
			io:format("check_depends1:~p~n",[Categ]),
			case M:getDependsOnSignal(Idlist,M,Flag) of
				{false,_}->
					This:setDependsOnProperties(Categ),
					[false];
				{true,_} ->
					[true]
			end;
		_->
			io:format("check_depends2  check_depends2:~p~n",[Categ]),
			{ok,{?CATEGORY,Categry1}} =  M:get_attribute(?CATEGORY),
			This:setDependsOnProperties(Categry1),
			[false]
	end ++ This:check_depends(T,Idlist,This,Flag).

%% @spec setDependsOnProperties(Category)->({ok,Result}|{error,Reason})
%% @doc set dependeices check result to properties
%% 
setDependsOnProperties(Category)->
	THIS:inc_attribute(disabledTotal),
	io:format("Category  Category =~p~n",[Category]),
	DepDes = case Category of
				?ERROR_CATEGORY->
					?DEPENDS_PREFIX ++ "not in " ++ atom_to_list(?ERROR_CATEGORY);
				?WARNING_CATEGORY->
					?DEPENDS_PREFIX ++ "not in " ++ atom_to_list(?WARNING_CATEGORY);
				?NO_DATA->
					?DEPENDS_PREFIX ++ "not in " ++ atom_to_list(?NO_DATA);
				_->
					?DEPENDS_PREFIX ++"is not in " ++ atom_to_list(?GOOD_CATEGORY)
			  end,		
	setDisabledProperties(DepDes).

%% @spec reload(This)->(ok|error)
%% @doc reload monitor
%%
reload(This)->
	{ok,{?ID,Id}} = This:get_property(?ID),
	MonitorData = dbcs_monitor:get_monitor(Id),
	This:stopMonitor(This),
	This:init(This,MonitorData),
	This:startMonitor(This).


%% @spec isDispatcher()->false
%%
%%
isDispatcher()->false.


%% @spec getStateProperties(This,_)->list()
%% @doc get state properties
getStateProperties(This,_)->
	Temp = This:get_template_property(),
	[X || X<-Temp,X#property.state=:=true].

%% @spec getLogProperties(This)->list()
%% @doc get properties need to log
%%
getLogProperties(This)->
	Temp = This:get_template_property(),
%% 	Temp = lists:sort(fun(X,Y)->(X#property.order < Y#property.order) or (X#property.order =:= Y#property.order)  end, Temp1),
	lists:foldl(fun(X,R)->
		if
			X#property.type == counters orelse X#property.type==browsable ->
				case This:get_property(X#property.name) of
					{ok,{_,Conts}}->
						R ++ [I||{_,I}<-Conts];
					_->
						R
				end;
			true->
				R
		end
	end,[],Temp) ++ [X#property.name || X<-Temp,X#property.state=:=true].
	
%% @spec getLogProperties(This)->list()
%% @doc get properties need to log
%%
getPrimaryStateProperties(This)->
	Temp = This:get_template_property(),
	[X#property.name || X<-Temp,X#property.primarystate=:=true].


getMonitorSkips()->
	{ok,{monitorSkips,Skips}} = THIS:get_attribute(monitorSkips),
	Skips.


isMultiThreshold()->false.

verify(Params)->
	Errs = 
	case proplists:get_value(?FREQUENCY,Params) of
		undefined->
			[{?FREQUENCY,"frequency must set"}];
		Freq->
			if
				not is_number(Freq)->
					[{?FREQUENCY,"frequency must be a number"}];
				Freq < ?MIN_MONITOR_INTERVAL ->
					[{?FREQUENCY,"Freqency was less than "++ integer_to_list(?MIN_MONITOR_INTERVAL) ++" seconds"}];
				true ->
					[]
			end
	end ++
	case proplists:get_value(?ERROR_FREQUENCY,Params) of
		undefined->
			[];
		0->
			[];
		Ef->
			if
				not is_number(Ef)->
					[{?ERROR_FREQUENCY,"error frequency must be a number"}];
				Ef < ?MIN_MONITOR_INTERVAL ->
					[{?ERROR_FREQUENCY,lists:flatten(io_lib:format("~p was less than ~p seconds",[?ERROR_FREQUENCY,?MIN_MONITOR_INTERVAL]))}];
				true ->
					[]
			end
	end,
	if
		length(Errs)>0 ->
			{error,Errs};
		true ->
			{ok,""}
	end.


getMeasurement(This,Prop)->
	This:getMeasurement(This,Prop,0).

getMeasurement(This,Prop,Default)->

	case This:get_attribute(Prop) of
		{ok,{Prop,Val}}->
			if 
				is_float(Val)->
					Val;
				is_integer(Val)->
					Val;
				true ->
					case string:to_float(Val) of
						{error,_}->
							case string:to_integer(Val) of
								{error,_}->
									Default;
								{V1,_}->
									V1;
								_->
									Default
							end;
						{V2,_}->
							V2;
						_->
							Default
					end
			end;
		_->
			Default

	end.
		
getScalarValues(Prop,Params)->
	case Prop of
		schedule->
			F = fun(X)->
				Id = proplists:get_value(id,X),
				Name = proplists:get_value(name,X),
				{Name,atom_to_list(Id)}
				end,
			[{"every day,all day","all"}] ++ lists:map(F,api_schedule:get_infos());
		proxy_node->
			Pfs = case THIS:get_platform(THIS,Params) of
					{ok,Platform}->
						Platform;
					_->
						[nt]
				end,

			case lists:member(platform:platformName(),Pfs) of
				true->
					[{"XP Proxy","XP"}];
				_->
					[]
			%%end ++
			%%case ecc_monitor_node:get_nodes() of
			%%	{ok,Nodes}->
			%%		S1 = sets:from_list(Pfs),
			%%		
			%%		F = fun(X)->
			%%			L = sets:size(sets:intersection(S1,sets:from_list(X#monitor_node.platform))),
			%%			if
			%%				L>0->
			%%					true;
			%%				true ->
			%%					false
			%%			end
			%%		end,
			%%			
			%%		lists:map(fun(X)->{X#monitor_node.name,X#monitor_node.name} end,lists:filter(F,Nodes));
			%%	_->
			%%		[]
			end;
		_->
			BASE:getScalarValues(Prop,Params)
	end.

get_platform(_,Params)->
	Key =  case proplists:get_value(?CLASS,Params) of undefined->undefined;CC->list_to_atom(CC) end,
	%% io:format("get_platform:~p,~p~n",[Key,Params]),
	case file:consult("conf/monitor_template.conf") of
		{ok,MTs}->
			case lists:keysearch(Key,1,MTs) of
				{value,M}->
					case proplists:get_value(platform,element(3,M)) of
						undefined->
							{ok,[nt]};
						V->
							{ok,V}
					end;
				_->
					{error,monitor_class_not_found}
			end;
		_->
			{error,template_file_error}
	end.

%% @spec get_run_info()->list()
%% @doc get monitor's runing state
%%
get_run_info()->
	State = 
	case THIS:get_attribute(?STATE_STRING) of
		{ok,V2}->
			[V2];
		_->
			[{?STATE_STRING,"n/a"}]
	end,
	ProgressString = 
	case THIS:get_attribute(progressString) of
		{ok,V3}->
			[V3];
		_->
			[{progressString,"n/a"}]
	end,
	Running = 
	case THIS:get_attribute(running) of
		{ok,V4}->
			[V4];
		_->
			[{running,false}]
	end,
	Machine = 
	case THIS:get_attribute('_machine') of
		{ok,V5}->
			[V5];
		_->
			[{'_machine',""}]
	end,
	%~ io:format("category get run info ~p~n",[THIS:get_attribute(category)]),
	BASE:get_run_info() ++ State ++ ProgressString ++ Running ++ Machine.

%%Threshold template
%%
%%
get_template_state(This,_)->
	Porps = This:get_template_property(),
	[X || X<-Porps,X#property.state=:=true].
	
get_status_info(This)->
	State = 
	case THIS:get_attribute(?STATE_STRING) of
		{ok,V2}->
			[V2];
		_->
			[{?STATE_STRING,"n/a"}]
	end,
	ProgressString = 
	case THIS:get_attribute(progressString) of
		{ok,V3}->
			[V3];
		_->
			[{progressString,"n/a"}]
	end,
	LogProps = lists:foldl(fun(X,R)->
			case This:get_attribute(X) of
				{ok,Vv}->
					R ++ [Vv];
				_->
					R ++ [{X,"n/a"}]
			end end, [], This:getLogProperties(This)),
	BASE:get_status_info(BASE) ++ State ++ ProgressString ++ LogProps.	


get_template_property()->
	BASE:get_template_property() ++
	[
	% #property{name=monitor_node,title="Select Node",type=scalar,order=1,advance=true,editable=true,default="",description="Select node where the monitor to run."},
	% #property{name='_proxy',title="Select Node",type=scalar,order=100,advance=false,editable=true,default="default",description="Select node where the monitor to run."},
	#property{name=schedule,title="Schedule",type=schedule,editable=true,advance=true, default="all",description="schedule for the monitor to be enabled - for example, \"weekdays, 9-6\" enables the monitor to run from 9am to 6pm, Monday - Friday"},
	#property{name=activate_baseline,title="Activate Baseline",type=bool,editable=true,advance=true,default=false,description="activate monitor's baseline function, will lose current thresholds"}
	].