-module(test1).
-compile(export_all).

-define(PRINT,io:format).

-include("alert.hrl").
-include("monitor.hrl").

test()->
	% 找根节点
	[R|_] = api_siteview:get_nodes(),
	RId = element(1,R),
	?PRINT("--------TEST START-------------~n"),
	
	% 在根节点的下面添加一个组
	{ok,Group}=api_group:create(RId,[{name,"_TEST"},{class,group},{frequency,0},{depends_on,"none"},{depends_condition,"good"}]),
	GId = proplists:get_value(id,Group),
	timer:sleep(1000),
	?PRINT("[OK]-Create Group:~p~n",[GId]),
	
	% 更新组信息
	Group2 = lists:keyreplace(name,1,Group,{name,"_TEST_"}),
	{ok,Grp} = api_group:update(Group2),
	timer:sleep(1000),
	?PRINT("[OK]-Update Group:~p~n",[GId]),
	
	% 再建一个组
	{ok,Grp2}=api_group:create(RId,[{name,"_TEST2_"},{class,group},{frequency,0},{depends_on,"none"},{depends_condition,"good"}]),
	GId2 = proplists:get_value(id,Grp2),
	timer:sleep(1000),
	?PRINT("[OK]-Create Group:~p~n",[GId2]),
	
	% 建一个监测器
	{ok,M1} = api_monitor:create(GId,[{class,memory_monitor},{name,"Memory"},
												{frequency,600},{disabled,false},{verfiy_error,false},{error_frequency,0},
												{depends_on,"none"},{depends_condition,"good"},{schedule,"all"},{machine,[]}]),
	MId1 = proplists:get_value(id,M1),
	timer:sleep(1000),
	?PRINT("[OK]-Create Monitor:~p~n",[MId1]),
	
	%更新监测器
	M2 = lists:keyreplace(name,1,M1,{name,"_TEST_Memory"}),
	{ok,M3} = api_monitor:update(M2),
	timer:sleep(1000),
	?PRINT("[OK]-Update Monitor:~p~n",[MId1]),
	
	%移动监测器
	{ok,_} = api_monitor:move(MId1,GId2),
	timer:sleep(1000),
	?PRINT("[OK]-Move Monitor(~p) To:~p To:~n",[MId1,GId2]),
	
	%复制监测器
	{ok,M4} = api_monitor:copy(MId1,GId),
	MId2 = proplists:get_value(id,M4),
	timer:sleep(1000),
	?PRINT("[OK]-Copy Monitor:~p To:~p,New Monitor:~p~n",[MId1,GId2,MId2]),
	
	% 取监测器信息
	MInf = api_monitor:info(MId1),
	?PRINT("[OK]-Get Monitor Info:~p ~n",[MInf]),

	% 监测器运行信息
	RInf = api_monitor:get_run_info(MId1),
	?PRINT("[OK]-Get Monitor Run Info:~p ~n",[RInf]),
	
	% 监测器阀值
	?PRINT("[OK]-Get Monitor Classifier:~p ~n",[api_monitor:get_classifier(MId1,good)]),
	?PRINT("[OK]-Get Monitor Classifier:~p ~n",[api_monitor:get_classifier(MId1,warning)]),
	?PRINT("[OK]-Get Monitor Classifier:~p ~n",[api_monitor:get_classifier(MId1,error)]),
	?PRINT("[OK]-Get Monitor Classifier:~p ~n",[api_monitor:get_all_classifier(MId2)]),
	
	% 禁用监测器
	timer:sleep(1000),
	{ok,_}=api_monitor:disable(MId2,""),
	?PRINT("[OK]-Disable Monitor:~p ~n",[MId2]),
	
	% 启用监测器
	timer:sleep(1000),
	{ok,_}=api_monitor:enable(MId2),
	?PRINT("[OK]-Enable Monitor:~p ~n",[MId2]),
	
	% 刷新监测器
	timer:sleep(1000),
	true = api_monitor:refresh(MId1),
	?PRINT("[OK]-Refresh Monitor:~p ~n",[MId1]),
	
	% 取计数器
	timer:sleep(1000),
	Counters = api_monitor:getBrowseData("add",[{class,"browsableNTCounter_monitor"},{machine,""},
									{pFile,"templates.perfmon/browsable/winntsys.xml"}]),
	?PRINT("[OK]-Get Monitor Counters length:~p ~n",[length(Counters)]),
	
	% 取阀值属性
	{ok,StateValue} = api_monitor:getStatePropertyObjects(MId1),
	?PRINT("[OK]-Get Monitor(~p) State Object:~p ~n",[MId1,StateValue]),
	
	% 浏览
	[_,_|_] = api_monitor:browse('memory_monitor',[]),
	?PRINT("[OK]-Browse Monitor~n"),
	
	% 统计信息
	timer:sleep(1000),
	{ok,StatInfo} = api_monitor:get_stat(),
	?PRINT("[OK]-Get Monitor Stat:~p~n",[StatInfo]),
	
	% 正在运行的监测器
	timer:sleep(1000),
	{ok,RunningMonitor}  = api_monitor:get_running(),
	?PRINT("[OK]-Get Monitor Running:~p~n",[RunningMonitor]),
	
	% 最近运行的监测器
	timer:sleep(1000),
	{ok,RecentMonitor}  = api_monitor:get_running(),
	?PRINT("[OK]-Get Monitor Recent:~p~n",[RecentMonitor]),
	
	% 取监测器日志
	{ok,Log} = api_monitor:get_log(erlang:date(),MId1),
	?PRINT("[OK]-Get Monitor(~p) Log:~p~n",[MId1,Log]),
	
	% 禁用该监测器的告警
	timer:sleep(1000),
	{ok,_} = api_monitor:disable_alert(MId1,"",60),
	?PRINT("[OK]-Disable Monitor(~p) Alert.~n",[MId1]),
	
	% 启用监测器的告警
	timer:sleep(1000),
	{ok,_} = api_monitor:enable_alert(MId1),
	?PRINT("[OK]-Enable Monitor(~p) Alert.~n",[MId1]),
	
	% 取监测的主机名
	{ok,_} = api_monitor:get_hostname(MId1),
	?PRINT("[OK]-Get Host Name of Monitor(~p).~n",[MId1]),
	
	% 禁用组
	timer:sleep(1000),
	{ok,_} = api_group:disable(GId,""),
	?PRINT("[OK]-Disable Group:~p~n",[GId]),
	
	% 启用组
	timer:sleep(1000),
	{ok,_} = api_group:enable(GId),
	?PRINT("[OK]-Enable Group:~p~n",[GId]),
	
	% 禁用组里的监测器
	timer:sleep(1000),
	{ok,_} = api_group:disable_monitors(GId,""),
	?PRINT("[OK]-Disable Monitors of Group:~p~n",[GId]),
	
	% 启用组里的监测器
	timer:sleep(1000),
	{ok,_} = api_group:enable_monitors(GId),
	?PRINT("[OK]-Enable Monitors of Group:~p~n",[GId]),
	
	% 取对象名字
	GName = "_TEST_" = api_siteview:get_object_name(GId),
	?PRINT("[OK]-Get Name Of Group(~p):~p~n",[GId,GName]),
	
	% 取对象的父亲节点的id
	?PRINT("[OK]-Get Parent Id Of Monitor(~p):~p~n",[MId1,api_siteview:get_parent_id(MId1)]),
	
	% 取对象的父亲节点的名字
	?PRINT("[OK]-Get Parent Name Of Monitor(~p):~p~n",[MId1,api_siteview:get_parent_name(MId1)]),
	
	% 取对象全名字
	?PRINT("[OK]-Get Full Name Of Monitor(~p):~p~n",[MId1,api_siteview:get_full_name(MId1)]),
	
	% 取对象的路径
	?PRINT("[OK]-Get Path Of Monitor(~p):~p~n",[MId1,api_siteview:get_object_path(MId1)]),
	
	% 取所有的组和监测器
	timer:sleep(1000),
	Total =length(api_siteview:getAllGroupsMonitors()),
	?PRINT("[OK]-Get All Groups And Monitors,Total:~p~n",[Total]),
	
	% 取所有的组
	timer:sleep(1000),
	Total2 =length(api_siteview:getAllGroups()),
	?PRINT("[OK]-Get All Groups,Total:~p~n",[Total2]),
	
	% 创建告警
	timer:sleep(1000),
	Target = lists:flatten(io_lib:format("<~w>",[GId])),
	{ok,Alert} = api_alert:create([{name,"Alert_TEST_"},{class,rule},{target,Target},{action,mailto},
								{action_param,#mail_alert{sendto=[],other="xianfang.shi@dragonflow.com",template="Default"}},
								{category,error},{condition,{always,1}},{enabled,true},{name_match,""},
								{status_match,""},{type_match,"any"}]),
	AId = proplists:get_value(id,Alert),
	?PRINT("[OK]-Create Alert:~p~n",[AId]),
	
	%取所有告警
	timer:sleep(1000),
	[_|_] = api_alert:get_all(),
	?PRINT("[OK]-Get All Alert.~n"),
	
	%更新告警
	timer:sleep(1000),
	Alert2 = lists:keyreplace(name,1,Alert,{name,"_Alert_TEST_"}),
	{ok,_}=api_alert:update(Alert2),
	?PRINT("[OK]-Update Alert:~p.~n",[AId]),
	
	% 取告警可变属性（scalar）的值
	timer:sleep(1000),
	AltScalar = api_alert:get_scalar_property(mailto,to,[]),
	?PRINT("[OK]-Get Scalar Property of Alert:~p~n",[AltScalar]),
	
	% 告警测试
	timer:sleep(1000),
	{ok,_} = api_alert:alert_test(MId1,AId),
	?PRINT("[OK]-Alert Test:~p,Monitor:~p~n",[AId,MId1]),
	
	% 告警日志
	timer:sleep(1000),
	{ok,AlertLog} = api_alert:get_log(erlang:date()),
	?PRINT("[OK]-Get Alert Log,size:~p~n",[length(AlertLog)]),
	
	% 禁用所有告警
	timer:sleep(1000),
	{ok,_} = api_alert:disable_all(),
	?PRINT("[OK]-Disable All Alerts.~n"),
	
	% 启用所有告警
	timer:sleep(1000),
	{ok,_} = api_alert:enable_all(),
	?PRINT("[OK]-Enable All Alerts.~n"),
	
	% 写告警模板
	{ok,_} = api_alert:write_template_file(mailto,"test","monitor : <name>"),
	?PRINT("[OK]-Write Alert Template.~n"),
	
	% 告警模板列表
	?PRINT("[OK]-Get List of Alert Template:~p~n",[api_alert:get_template_file_list(mailto)]),
	
	% 读取告警模板
	{ok,_} = api_alert:read_template_file(mailto,"test"),
	?PRINT("[OK]-Read Alert Template.~n"),
	
	% 删除告警模板
	{ok,_} = api_alert:remove_template_file(mailto,"test"),
	?PRINT("[OK]-Remove Alert Template.~n"),
	
	% 查询告警日志
	{eof,_} = api_alert:query_log(sv_datetime:prev_date(erlang:date()),{1,1,1},erlang:date(),{23,59,59},
									[{id,'=',AId}],1,10000),
	?PRINT("[OK]-Query Alert Log:~p.~n",[AId]),
	
	% 删除告警
	{ok,_} = api_alert:delete(AId),
	?PRINT("[OK]-Delete Alert:~p.~n",[AId]),
	
	% 取得Windows主机
	?PRINT("[OK]-Get Windows Machine length:~p.~n",[length(api_machine:get_ntmachine())]),
	
	% 取得Unix主机
	?PRINT("[OK]-Get Unix Machine length:~p.~n",[length(api_machine:get_unixmachine())]),
	
	% 创建主机
	{ok,MachId} = api_machine:create_machine(#machine{}),
	?PRINT("[OK]-Create Machine:~p.~n",[MachId]),
	
	% 更新主机
	{ok,_} = api_machine:update_machine(#machine{id=MachId,name="MACH_TEST"}),
	?PRINT("[OK]-Update Machine:~p.~n",[MachId]),
	
	% 删除主机
	{ok,_} = api_machine:delete_machine(MachId),
	?PRINT("[OK]-Delete Machine:~p.~n",[MachId]),
	
	% 取监测器集合列表
	MsetList = api_monitor_set:get_monitorset_list(),
	?PRINT("[OK]-Get List of MonitorSet, length:~p.~n",[length(MsetList)]),
	
	% 取监测器集合信息,配置监测器集合，从集合创建监测器
	case lists:keysearch("PingGroup.mset",1,MsetList) of
		{value,_}->
			{ok,Mset} = api_monitor_set:get_monitorset("PingGroup.mset"),
			?PRINT("[OK]-Get Content of MonitorSet:~p.~n",[Mset]),
			{ok,Mset2} = api_monitor_set:configure_monitorset("PingGroup.mset",[{"%ip%","127.0.0.1"}]),
			?PRINT("[OK]-Configure MonitorSet:~p.~n",[Mset2]),
			MsIds = lists:foldl(fun(X,RR)->
								{ok,Mdata} = api_monitor_set:create_monitor_from_monitorset(GId,X),
								XMId = proplists:get_value(id,Mdata),
								?PRINT("[OK]-Create Monitor from MonitorSet:~p.~n",[XMId]),
								RR ++ [XMId]
							end,[],Mset2#monitor_set.monitors),
			timer:sleep(5000),
			lists:map(fun(X)->
						{ok,_} = monitor_delete(X),
						?PRINT("[OK]-Delete Monitor that Create from MonitorSet:~p.~n",[X])
					end, MsIds),
			ok;
		_->
			?PRINT("[WARN]-MonitorSet 'PingGroup.mset' is missing, MonitorSet test is ignored.")
	end,
	
	% 监测器模板列表
	Mts = api_monitor_template:get_templates(),
	?PRINT("[OK]-Get List of Monitor Template,Length:~p~n",[length(Mts)]),
	
	% 取监测器模板内容
	lists:map(fun(X)->
			MtPs = api_monitor_template:get_template(element(1,X)),
			?PRINT("[OK]-Get Monitor Template(~p),Find Propertis:~p~n",[element(1,X),length(MtPs)]),
			ok
		end,Mts),
	
	% 取服务器列表
	Servers = api_monitor_template:get_servers([{"class","service_monitor"}]),
	?PRINT("[OK]-Get Servers,Length:~p~n",[length(Servers)]),
	
	% 取State属性
	api_monitor_template:get_template_state(memory_monitor,[]),
	StProps = api_monitor_template:get_template_state(MId1,memory_monitor,[]),
	?PRINT("[OK]-Get State Property,Length:~p~n",[length(StProps)]),
	
	% 创建Schedule 
	{ok,SchId} = api_schedule:create({"","range",{"enabled",[],[]},{"enabled",[],[]},
									{"enabled",[],[]},{"enabled",[],[]},{"enabled",[],[]}
									,{"enabled",[],[]},{"enabled",[],[]}}),
	?PRINT("[OK]-Create New Schedule:~p~n",[SchId]),
	
	% 更新Schedule
	ok = api_schedule:update(SchId,{"_","range",{"enabled",[],[]},{"enabled",[],[]},
									{"enabled",[],[]},{"enabled",[],[]},{"enabled",[],[]}
									,{"enabled",[],[]},{"enabled",[],[]}}),
	?PRINT("[OK]-Update Schedule:~p~n",[SchId]),
	
	% 取Schedule列表
	[_|_] = api_schedule:get_infos(),
	?PRINT("[OK]-Get List Of Schedule.~n"),
	
	% 取Schedule信息
	[_|_] = api_schedule:get_info(SchId),
	?PRINT("[OK]-Get Info Of Schedule.~n"),
	
	% 取Schedule 名称
	{ok,_} = api_schedule:get_schedulename(SchId),
	?PRINT("[OK]-Get Name Of Schedule.~n"),
	
	% 通过名字取Schedule信息
	[_|_] = api_schedule:get_info_by_name("_"),
	?PRINT("[OK]-Get Schedule By Name.~n"),
	
	% Schedule是否已存在
	ok =  api_schedule:name_existed("_"),
	?PRINT("[OK]-Schedule Name Existed.~n"),
	
	% Schedule是否使用中
	SchUsed = api_schedule:schedule_used(SchId),
	?PRINT("[OK]-Schedule is in using:~p.~n",[SchUsed]),
	
	% 删除Schedule
	ok = api_schedule:delete(SchId),
	?PRINT("[OK]-Delete Schedule:~p.~n",[SchUsed]),
	
	% 删除监测器
	timer:sleep(1000),
	{ok,_} = monitor_delete(MId1),
	{ok,_} = monitor_delete(MId2),
	?PRINT("[OK]-Delete Monitor:~p,~p~n",[MId1,MId2]),
	
	%删除组
	timer:sleep(1000),
	{ok,_} = api_group:delete(GId),
	{ok,_} = api_group:delete(GId2),
	?PRINT("[OK]-Delete Group:~p,~p~n",[GId,GId2]),
	
	?PRINT("--------TEST END-------------~n").
	
monitor_delete(Id)->
	case api_monitor:delete(Id) of
		{error,monitor_is_running}->
			?PRINT("[OK]-Monitor(~p) Is Running,Wait 1 Seconds.~n",[Id]),
			timer:sleep(1000),
			monitor_delete(Id);
		Else->
			Else
	end.
	
t2()->
	Ps = erlang:processes(),
	lists:foldl(fun(X,R)->
				case erlang:process_info(X,status) of
					{_,running}->
						io:format("runing_process:~p,function:~p,init call:~p~n",[X,erlang:process_info(X,current_function),erlang:process_info(X,current_function)]),
						R;
					{_,runnable}->
						io:format("runnable:~p,function:~p~,init call:~p~n",[X,erlang:process_info(X,current_function)]),
						R;
					_->
						pass,
						R
				end
			end,[],Ps),
	ok.