%% ---
%%siteview_group
%%
%%---
-module(siteview_group,[BASE]).
-extends(monitor_group).
-compile(export_all).
-define(CONFDIR,"conf").
-include("monitor.hrl").

%% -import(historyreport, [createHistoryReportObject/1]).

new()->
	Obj = monitor_group:new(),
	Obj:set_attribute(childs,[]),
	% Obj:set_property(id,'0'),
	{?MODULE,Obj}.

start_siteview() -> 
	THIS:start_httpserver(),
	io:format("load scheduler....~n"),
	THIS:start_scheduler(),
	
	Sch = THIS:get_monitor_scheduler(),
	Sch:suspendScheduler(),	
%% 	SchReport = THIS:get_report_scheduler(),
%% 	SchReport:suspendScheduler(),
	
	io:format("load monitor....~n"),
	THIS:startMonitor(),
	THIS:signalReload(),
	io:format("start scheduler....~n"),
	
    %%
	THIS:start_report_scheduler(),
	io:format("locad report scheduler......\~n"),
	ScheduleReport=get_report_scheduler(),
	ScheduleReport:suspendScheduler(),	
    ScheduleReport:resumeScheduler(),
%% 	THIS:startreport(),
	%%
	
	Sch:resumeScheduler(),	
%%  	SchReport:resumeScheduler(),	
  	THIS:loadHistory(true),
%% 	timer:sleep(60000),
	ok.
startreport() ->
	ScheduleReport=get_report_scheduler(),
	%%scheduleRepeatedAction(Action,ScheduleSpec,LastTime)780
	Action=reportscheduletest,
 	ScheduleReport:scheduleRepeatedAction(Action,"weekday  U,M,T,W,R,F,S  42180",sv_datetime:now()).
stop_siteview()->
	Sch = THIS:get_monitor_scheduler(),
	Sch:suspendScheduler(),	
	THIS:stop_httpserver(),
	THIS:stopGroup(),
	THIS:unload_rules(),
	Sch:stopScheduler(),	
	ScheduleReport=get_report_scheduler(),
	ScheduleReport:stopScheduler().

start_httpserver()->
	ok.

start_script()->
	ok.

start_groupscheduler()->ok.

%%%%%%%%%%%%%%%%%%%%%%%% historyreport interface %%%%%%%%%%%%%%%%%%%%%%%

loadHistory(Flag) ->	
	HistoryReports = buidReportApps(),
%% 	io:format("HistoryReports:~p ~n", [HistoryReports]),
%% 	historyreport:createHistoryReportObject(""),
	F = fun(X)->
		M = historyreport_static:createHistoryReportObject(X),
%% 		M:sleep(60000),
		{_,{_,Id}}=lists:keysearch(id, 1, X),
		if
			Flag == true ->
				M:schedule();
%% 			    case is_atom(M:get_app()) of
%% 		 			true->
%% 						put(hostname, M:get_app());	
%% 					_->
%% 					put(hostname, list_to_atom(M:get_app()))
%%            		end;
%% 				siteview:set_object(Id,report,historyreport,M);
			true ->
				ok	
		end
%% 		historyreport:init("", "")
	end,
	lists:foreach(F, HistoryReports).
buidReportApps()->
	Apps = app:all(),
	get_Report_App(Apps).
get_Report_App([])->
	[];
get_Report_App([App|E]) ->
	put(hostname,App),
	api_report:get_all()++get_Report_App(E).

%
start_report_scheduler()->
	ReportScheduler = scheduler:new('Report Scheduler'),
	ReportScheduler:startScheduler(),
	THIS:set_attribute(report_scheduler, ReportScheduler),
	ok.

%
get_report_scheduler()->
	case THIS:get_attribute(report_scheduler) of
		{ok,{report_scheduler,Sche}}->
			Sche;
		_->
			{error,not_found_scheduler}
	end.

start_scheduler()->
	MonitorScheduler = scheduler:new('Monitor Scheduler'),
	THIS:load_rules(),
	MonitorScheduler:startScheduler(),
	THIS:set_attribute(monitor_scheduler,MonitorScheduler),
	
%% 	THIS:start_report_scheduler(),
%% 	THIS:loadHistory(true),
	
	ok.

%%%%%%%%%%%%%%%%%%%%%%%% historyreport interface %%%%%%%%%%%%%%%%%%%%%%%

stop_httpserver()->
	ok.

get_group_ids()->
	[filename:basename(X,".mg")||X<-filelib:wildcard("groups/*.mg")].

load_groups()->ok.
	
load_settings()->
	{ok,[Conf|_]}=file:consult("groups/master.config"),
	THIS:init(Conf).
	
	
load_rules()->
	Apps = app:all(),
	THIS:load_rules(Apps).
	
load_rules([])->ok;
load_rules([App|T])->
	put(hostname,App),
	Rules = dbcs_rule:get_all(),
	F = fun(X)->
		AId = proplists:get_value(id,X),
		case siteview:get_object(AId) of
			[]->
				Obj = rule:new(),
				Obj:init(Obj,X);
			_->
				pass
		end
	end,
	lists:map(F,Rules),
	THIS:load_rules(T).
	
unload_rules()->
	Rules = siteview:get_object_by_type(rule),
	[X:delete() || X<-Rules],
	ok.

save_settings()->
	file:write_file("groups/master.config",io_lib:format("~p.",[THIS:get_properties()])).

create_monitor_count_log()->ok.

%%
%%
%%
get_monitor_scheduler()->
	case THIS:get_attribute(monitor_scheduler) of
		{ok,{monitor_scheduler,Sche}}->
			Sche;
		_->
			{error,not_found_scheduler}
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
	
%%adjustGroups()->
%%
%%
adjustGroups()->
	{ok,{childs,Groups}} = THIS:get_attribute(childs),
	[THIS:remove_group(X)||X<-Groups],
	% SId = THIS:getServerID(),
	{ok,{_,SId}} = THIS:get_property(id), 
%% 	io:format("siteviewgroup id :~p ~n", [SId]),
	GrpData =get_db_childs(SId),
%%     io:format("adjustGroups groups:~p~n",[GrpData]),
	F = fun(G,R)->
		try
			case proplists:get_value(?CLASS,G) of
				group->
					 X=THIS:load_group(G),
					 X:startGroup(),
					 R ++ [R];
				_->
					R
			end
		catch
		Err:Msg->
			?ERROR_LOG2("adjustGroups ~w:~w~n~p",[Err,Msg,erlang:get_stacktrace()])
		end
	end,
	NewGroups = lists:foldl(F,[],GrpData),
	% NewGroups = lists:map(fun(G)-> X=THIS:load_group(G),X:startGroup(),X end,GrpData),
	NewGroups.
	
get_db_childs(Id)->
	Apps = app:all(),
	get_db_childs(Id,Apps).

get_db_childs(_,[])->[];	
get_db_childs(SId,[App|T])->
	put(hostname,App),
	siteview:check_install_date(),
	dbcs_group:get_childs(SId) ++ get_db_childs(SId,T).


%%load_group(Id)-> monitor_group:new()
%%
%%
load_group(Id) when is_atom(Id)->
	Grp = monitor_group:new(),
	% Grp:set_property(id,Id),
	% Grp:set_property(name,Id),
	GrpData = dbcs_group:get_group(Id),
	Grp:init(Grp,GrpData),
	Grp:load_group(Id),
	THIS:add_group(Grp),
	Grp:set_owner(THIS),
	Grp;
load_group(Data) when is_list(Data)->
	Grp = monitor_group:new(),
	Grp:init(Grp,Data),
	Grp:load_group(proplists:get_value(id,Data)),
	THIS:add_group(Grp),
	Grp:set_owner(THIS),
	Grp;
load_group(_)->
	{error,args_error}.

%%remove_group(Group)->{ok,remove_group_ok}
%%
%%
remove_group(Group)when is_atom(Group)->
	{ok,{childs,Groups}} = THIS:get_attribute(childs),
	App = dbcs_base:get_app(),
	NewGroups = [X||X<-Groups,R = X:get_property(id),R =/= {ok,{id,Group}} andalso App=/=X:get_app()],
	THIS:set_attribute(childs,NewGroups),
	{ok,remove_group_ok};
remove_group(Group)->
	Id = Group:get_property(id),
	App = Group:get_app(),
	{ok,{childs,Groups}} = THIS:get_attribute(childs),
	%%NewGroups = [X||X<-Groups,R = X:get_property(id),R =/= Id],
	NewGroups = lists:filter(fun(X)->R = X:get_property(id),R =/= Id andalso App=/=X:get_app() end,Groups),
	THIS:set_attribute(childs,NewGroups),
	Group:delete(),
	{ok,remove_group_ok}.

%%add_group(Group)->{ok,add_group_ok}
%%
%%
add_group(Group)->
	{ok,{childs,Groups}} = THIS:get_attribute(childs),
	Id = Group:get_property(id),
	App = Group:get_app(),
	NewGroups = [X||X<-Groups,X:get_property(id) =/= Id andalso App=/=X:get_app()] ++ [Group],
	THIS:set_attribute(childs,Groups ++[Group]),
	{ok,add_group_ok}.


%%get_group(Id)->monitor_group | {error,Resean}
%%
%%
get_group(Id)when is_atom(Id)->
	App = dbcs_base:get_app(),
	{ok,{childs,Groups}} = THIS:get_attribute(childs),
	MyGroup = [X||X<-Groups,R = X:get_property(id),R == {ok,{id,Id} andalso App==X:get_app()}],
	if
		length(MyGroup) > 0 ->
			[G|_] = MyGroup,
			G;
		true->
			{error,not_found_group}
	end;
get_group(_)->
	{error,error_id}.


signalReload()->
	THIS:adjustGroups().

reload()->
	THIS:signalReload().

getMonitors()->
	{ok,{childs,Childs}} = THIS:get_attribute(childs),
	THIS:getMonitors(Childs).

getMonitors([])->[];
getMonitors([M|T])->
	M:getMonitors(true) ++ THIS:getMonitors(T).

getServerID()->
	case file:consult(?CONFDIR ++"/server.conf") of
		{ok,Data}->
			proplists:get_value(serverID,Data);
		_->
			'99'
	end.
	
getGroupsMonitors(Flag) when Flag=:=true;Flag=:=false ->
	THIS:getGroupsMonitors(THIS:get_childs(),Flag);
getGroupsMonitors(_)->
	{error,parameter_must_be_true_or_false}.
	
load_app(App) when is_atom(App)->
	put(hostname,App),
	siteview:check_install_date(),
	{ok,{_,SId}} = THIS:get_property(id), 
	GrpData = dbcs_group:get_childs(SId),
	F = fun(G)->
		try
		 X=THIS:load_group(G),
		 X:startGroup()
		catch
		Err:Msg->
			?ERROR_LOG2("adjustGroups ~w:~w~n~p",[Err,Msg,erlang:get_stacktrace()])
		end
	end,
	lists:map(F,GrpData);
load_app(App) when is_list(App)->
	load_app(list_to_atom(App));
load_app(_)->{error,parameter_error}.
	
	