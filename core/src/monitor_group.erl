%% ---
%%monitor group
%%
%%---
-module(monitor_group,[BASE]).
-extends(monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

new()->
	Obj = monitor:new(),
	Obj:set_attribute(updategroup,null),
	Obj:set_attribute(childs,[]),
	Obj:set_attribute(runing,false),
	{?MODULE,Obj}.

init(This,Data)->
	BASE:init(This,Data),
	case proplists:get_value(id,Data) of
		undefined->
			pass;
		Id->
			%%io:format("~p~n",[Id]),
			dbcs_base:set_app(proplists:get_value(?APP,Data),true),
			siteview:set_object(Id,group,element(1,This),This)
	end.
	
startGroup()->
	THIS:startMonitor(),
	THIS:start_scheduler().

stopGroup()->
	THIS:stop_scheduler(),
	THIS:stopMonitor().

remove_child(Child)->
	{ok,{childs,Childs}}=THIS:get_attribute(childs),
	NewChild = [X||X<-Childs,X=/=Child],
	THIS:set_attribute(childs,NewChild),
	{ok,remove_child_ok}.

remove_childs()->
	{ok,{childs,Childs}}=THIS:get_attribute(childs),
	[X:delete()||X<-Childs],
	THIS:set_attribute(childs,[]),
	{ok,remove_childs_ok}.

add_child(Child)->
	{ok,{childs,Childs}}=THIS:get_attribute(childs),
	case lists:member(Child,Childs) of
		true ->
			{ok,already_exist};
		_->
			THIS:set_attribute(childs,Childs++[Child]),
			{ok,add_child_ok}
	end.

get_childs()->
	App = dbcs_base:get_app(),
	% io:format("App:~p~n",[App]),
	{ok,{childs,Childs}}=THIS:get_attribute(childs),
	case App of
		undefined->
			Childs;
		_->
			F = fun(X)->
				case X:get_property(?APP) of
					{ok,{_,Val}}->
						Val == App;
					_->
						false
				end
			end,
			lists:filter(F,Childs)
	end.

getMonitors(Flag)->
	THIS:getMonitors(THIS:get_childs(),Flag).

getMonitors([],_)->[];
getMonitors([M|T],true)->	
	case M:get_property(?CLASS) of
		{ok,{?CLASS,group}}->
			getMonitors(T++M:get_childs(),true);
		_->
			[M] ++ getMonitors(T,true)
	end;
getMonitors([M|T],_)->	
	case M:get_property(?CLASS) of
		{ok,{?CLASS,group}}->
			getMonitors(T,false);
		_->
			[M] ++ getMonitors(T,false)
	end.

reload()->
	THIS:stopGroup(),
	{ok,{?ID,Id}} = THIS:get_property(?ID),
	dbcs_base:set_app(THIS:get_app()),
	THIS:remove_childs(),
	%%THIS:remove_properties(),
	% THIS:set_property(?ID,Id),
	GrpData = dbcs_group:get_group(Id),
	THIS:init(THIS,GrpData),
	THIS:load_group(),
	THIS:startGroup(),
	{ok,reload_ok}.
	reload_self()->
	THIS:stop_scheduler(),
	{ok,{?ID,Id}} = THIS:get_property(?ID),
	dbcs_base:set_app(THIS:get_app(),true),
	GrpData = dbcs_group:get_group(Id),
	THIS:init(THIS,GrpData),
	THIS:start_scheduler().

delete()->
	{ok,{childs,Childs}}=THIS:get_attribute(childs),
	THIS:stopChild(Childs),
	THIS:remove_childs(),
	THIS:set_parent(null),
	BASE:delete().

stopMonitor()->
	case THIS:get_attribute(childs) of
		{ok,{childs,Monitors}}->
			%%io:format("stopChild:~p~n",[Monitors]),
			THIS:stopChild(Monitors);
		_->
			?ERROR_LOG2("stopMonitor not found childs:~p~n",[THIS:get_property(id)]),
			io:format("stopMonitor not found childs:~p~n",[THIS:get_property(id)])
	end,
	BASE:stopMonitor(THIS),
	{ok,stopped}.

stopChild([])->{ok,stop_ok};
stopChild([C|T])->
	%%io:format("stopChild:~p,~p~n",[C,C:get_property(id)]),
	case C:get_property(?CLASS) of
		{ok,{?CLASS,group}}->
			C:stopGroup();
		{ok,{?CLASS,device}}->
			C:stopGroup();
		_->
			C:stopMonitor(C)
	end,
	THIS:stopChild(T).


%%
%%
%%TODO:start report
startMonitor()->
	BASE:startMonitor(THIS),
	THIS:set_attribute(?CATEGORY,good),
	{ok,{childs,Monitors}} = THIS:get_attribute(childs),
	% read old status data
	{ok,{_,Id}} = THIS:get_property(id),
	case monitor_status_store:read(THIS:get_app(),Id) of
				{ok,StData}->
					lists:map(fun({K,V})->
						THIS:set_attribute(K,V)
						end,StData);
				_->
					pass
			end,
	
	% io:format("startMonitor:~p~n",[Monitors]),
	%% Monitors = THIS:get_monitors(),
	%% THIS:set_attribute(monitors,Monitors),
	F = fun(X)->
		try
			case X:get_property(?CLASS) of
				{ok,{?CLASS,group}}->
					X:startGroup();
				{ok,{?CLASS,device}}->
					X:startGroup();
				_->
					X:startMonitor(X)
			end
		catch
		Err:Msg->
			?ERROR_LOG2("~w:~w~n~p",[Err,Msg,erlang:get_stacktrace()])
		end
		end,
	lists:foreach(F,Monitors),
	{ok,started}.


%%start_scheduler()->{error,Resean} | {ok,schedule_ok}
%%
%%
start_scheduler()->
	case THIS:get_property(?FREQUENCY) of
		{ok,{?FREQUENCY,Val}}->
			THIS:start_scheduler(Val);
		_->
			ok
	end.

start_scheduler(Freq) when is_integer(Freq),Freq>0,Freq>?MIN_GROUP_INTERVAL->
	L = Freq * 1000,
	case THIS:get_attribute(updategroup) of
		{ok,{updategroup,null}}->
			UpdateGroup = update_group:new(THIS),
			{ok,Scheduler} = siteview:get_monitor_scheduler(),
			THIS:set_attribute(updategroup,UpdateGroup),
			L1 = THIS:get_attribute_as_number(?LAST_UPDATE),
			L2 = sv_datetime:now(),
			%L3 = L1+L-L2,
			if 
				L1 > L2 ->
					Scheduler:scheduleRepeatedPeriodicAction(UpdateGroup,L,L2);
				L1 < 0 ->
					L5 = (5+random:uniform(100))*1000,
					Scheduler:scheduleRepeatedPeriodicAction(UpdateGroup,L,L2 - L + L5);
				L < 5000 ->
					L6 = (2+random:uniform(200))*1000,
					Scheduler:scheduleRepeatedPeriodicAction(UpdateGroup,L,L2 - L + L6);
				true ->
					Scheduler:scheduleRepeatedPeriodicAction(UpdateGroup,L,L1)
			end;
		_->
			{error,scheduler_already}
	end;
start_scheduler(_)->{error,frequency_error}.	

stop_scheduler()->
	case THIS:get_attribute(updategroup) of
		{ok,{updategroup,UpdateGroup}} when UpdateGroup=/= null ->
			Scheduler = case siteview:get_monitor_scheduler() of
							{ok,Sche}->
								Sche;
							_->
								?DEBUG_LOG("not found scheduler,system shutdown now!"),
								{error,no_found_scheduler}
						end,
			Scheduler:unschedule(UpdateGroup),
			THIS:set_attribute(updategroup,null),
			UpdateGroup:delete();
		_->
			nothing
	end.

save_monitor()->
	{ok,{childs,Monitors}} = THIS:get_attribute(childs),
	[dbcs_monitor:update_monitor(X:get_properties())||X<-Monitors].

%%get_monitors()->
%%	Mdatas = dbcs_monitor:get_all(),
%%	lists:map(fun(X)->{value,{class,Class}}=lists:keysearch(class,1,X),C = Class:new([]),C:init(C,X),C end, Mdatas).
	%%[C || X<- Mdatas,{value,{class,Class}}=lists:keysearch(class,1,X),C = Class:new([]),C:init(X)].
	%%{ok,Ids} = file:list_dir("monitor"),
	%%[THIS:file_to_monitor("monitor/"++X)||X<-Ids].

%%
%%
%%
groupSchedulerActive(Flag)->
	case THIS:get_property(frequency) of
		{ok,{frequency,Freq}}->
			Freq>0;
		_->
			case THIS:get_parent() of
				{ok,{parent,null}}->
					false;
				{ok,{parent,Parent}}->
					if 
						Flag=:=true->
							THIS:groupSchedulerActive(Parent,Flag);
						true->
							false
					end;
				_->
					false
			end
	end.

%%
%%
%%
groupSchedulerActive(Parent,Flag)->
	case Parent:get_property(frequency) of
		{ok,{frequency,Freq}}->
			Freq>0;
		_->
			if
				Flag =:= true->
					case Parent:get_parent() of 
						{ok,{parent,null}}->
							false;
						{ok,{_,Pp}}->
							THIS:groupSchedulerActive(Pp,Flag);
						_->
							false
					end;
				true->
					false
			end
	end.

file_to_monitor(File)->
	{ok,[M|_]}=file:consult(File),
	{value,{class,Class}}=lists:keysearch(class,1,M),
	P=Class:new([]),
	P:init(M),
	P.

save()->
	dbcs_group:update_group(THIS:get_properties()).


%%load_group()->{ok,load_group_ok}
%%
load_group()->
	{ok,{id,Id}} = THIS:get_property(id),
	dbcs_base:set_app(THIS:get_app()),
	MonitorDatas = dbcs_group:get_childs(Id),
	%% io:format("Datas:~p~n",[MonitorDatas]),
	Monitors =[THIS:createObject(X)||X<-MonitorDatas], %% lists:map(fun(X)->M=THIS:create_object(X),M end,MonitorDatas),
	%% io:format("load_group,~p~n",[Monitors]),
	THIS:set_attribute(childs,lists:filter(fun(X)->case X of {error,_}-> false;_->true end end,Monitors)),
	F = fun(X)->
			case X of 
				{error,_}->
					error;
				_->
					X:set_owner(THIS),
					case X:get_property(?CLASS) of
						{ok,{?CLASS,group}}->
							X:load_group();
						{ok,{?CLASS,device}}->
							X:load_group();
						_->
							ok
					end
			end
		end,
	lists:foreach(F,Monitors),
	{ok,load_group_ok}.

load_group(Id)->
	MonitorDatas = dbcs_group:get_childs(Id),
	%%io:format("Datas:~p~n",[MonitorDatas]),
	Monitors =[THIS:createObject(X)||X<-MonitorDatas], %%lists:map(fun(X)->M=THIS:create_object(X),M end,MonitorDatas),
	%%io:format("load_group,~p~n",[Monitors]),
	THIS:set_attribute(childs,lists:filter(fun(X)->case X of {error,_}-> false;_->true end end,Monitors)),
	F = fun(X)->
			case X of 
				{error,_}->
					error;
				_->
					X:set_owner(THIS),
					case X:get_property(?CLASS) of
						{ok,{?CLASS,group}}->
							X:load_group();
						{ok,{?CLASS,device}}->
							X:load_group();
						_->
							ok
					end
			end
		end,
	lists:foreach(F,Monitors),
	{ok,load_group_ok}.
	
getGroupsMonitors(Flag) when Flag=:=true;Flag=:=false ->
	{ok,{childs,Childs}} = THIS:get_attribute(childs),
	THIS:getGroupsMonitors(Childs,Flag);
getGroupsMonitors(_)->
	{error,parameter_must_be_true_or_false}.

getGroupsMonitors([],_)->[];
getGroupsMonitors([M|T],Flag)->
	case M:get_property(?CLASS) of
		{ok,{?CLASS,group}}->
			{ok,{childs,Childs}} = M:get_attribute(childs),
			case Flag of
				true->
					[M] ++ THIS:getGroupsMonitors(Childs,Flag) ++ THIS:getGroupsMonitors(T,Flag);
				_->
					[M] ++ THIS:getGroupsMonitors(T,Flag)
			end;
		{ok,{?CLASS,device}}->
			{ok,{childs,Childs}} = M:get_attribute(childs),
			case Flag of
				true->
					[M] ++ THIS:getGroupsMonitors(Childs,Flag) ++ THIS:getGroupsMonitors(T,Flag);
				_->
					[M] ++ THIS:getGroupsMonitors(T,Flag)
			end;
		_->
			[M] ++ THIS:getGroupsMonitors(T,Flag)
	end.

getParentActionRules()->
	case THIS:get_owner() of
		{ok,{parent,Parent}}->
			THIS:getRules(2) ++ Parent:getParentActionRules();
		_->
			THIS:getRules(2)
	end.
	
runUpdate(This,Flag,Aysnc)->
	This:set_attribute(runing,true),
	Childs = This:get_childs(),
	Ret = [X:runUpdate(X,false,Aysnc)||X<-Childs],
	This:set_attribute(runing,false),
	case lists:member(false,Ret) of
		true ->
			false;
		_->
			true
	end.
	
	
verify(Params)->
	Errs = 
	case proplists:get_value(?FREQUENCY,Params) of
		undefined->
			[{?FREQUENCY,"frequency must set"}];
		Freq->
			if
				not is_number(Freq)->
					[{?FREQUENCY,"frequency must be a number"}];
				Freq < ?MIN_MONITOR_INTERVAL andalso Freq=/=0->
					[{?FREQUENCY,lists:flatten(io_lib:format("~p was less than ~p seconds",[Freq,?MIN_MONITOR_INTERVAL]))}];
				true ->
					[]
			end
	end ++
	case proplists:get_value(?NAME,Params) of
		undefined->
			[{?NAME,"name must set"}];
		""->
			[{?NAME,"name can not be empty"}];
		_->
			[]
	end,
	if
		length(Errs)>0 ->
			{error,Errs};
		true ->
			{ok,""}
	end.

%% @spec get_run_info()->list()
%% @doc get monitor's runing state
%%
get_run_info()->
	{ok,{childs,Childs}} = THIS:get_attribute(childs),
	{Total,Err,_,_} = THIS:get_group_monitors(Childs,{0,0,0,0}),
	State = [{?STATE_STRING,lists:flatten(io_lib:format("~p in group,~p in error",[Total,Err]))}],
	BASE:get_run_info() ++ State.
	
get_status_info()->
	{ok,{childs,Childs}} = THIS:get_attribute(childs),
	{Total,Err,_,_} = THIS:get_group_monitors(Childs,{0,0,0,0}),
	State = [{?STATE_STRING,lists:flatten(io_lib:format("~p in group,~p in error",[Total,Err]))}],
	BASE:get_status_info(THIS) ++ State.

get_template_property()->
	[
	#property{name=?NAME,title="Group Name",type=text,editable=true},
	#property{name=?DESCRIPTION,title="Description",type=text,editable=true,advance=true},
	#property{name=?DEPENDS_ON,title="Depends On",type=scalar,editable=true,advance=true},
	#property{name=?DEPENDS_CONDITION,title="Depends Condition",type=scalar,editable=true,advance=true},
	#property{name=?FREQUENCY,title="Refresh Group every",type=frequency,advance=true,editable=true}
	].