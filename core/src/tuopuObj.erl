%% Author: Administrator
%% Created: 2010-1-8
%% Description: TODO: Add description to tuopuObj
-module(tuopuObj,[BASE]).
-extends(siteview_object).
-compile(export_all).

-define(RECEIVE_TIME_OUT, 30*1000).
-define(DEBUG_INFO, debug_info).
-define(CURRENT_STATE, current_state).
-define(REG_NAME, java_mail_box).

-include("monitor.hrl").
-include("monitor_template.hrl").

%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for tuopuObj
new()->
	Obj = siteview_object:new(),
	Obj:set_attribute(rootpath,""),
	Obj:set_attribute(name,""),
	Obj:set_attribute(apppath,""),
	{?MODULE,Obj}.

%% @makeTuopu
%% Obj = term()
%% @doc makeTuopu
makeTuopu()->
	{ok,{_, RootPath}} = THIS:get_attribute(rootpath),
	{ok,{_, AppPath}} = THIS:get_attribute(apppath),
	{ok,{_, Name}} = THIS:get_attribute(name), 	
	THIS:set_property(app_,db_ecc:domain(get(hostname))),
	GroupInfoForTuopu = api_siteview:getGroupInfoForTuopu(),
	MonitorInfoForTuopu = api_siteview:getMonitorInfoForTuopu(),
 	MachineInfoForTuopu = api_siteview:getMachineInfoForTuopu(),
%% 	io:format("makeTuopu1: ~p ~n", [MachineInfoForTuopu]),
%% 	io:format("makeTuopu2: ~p ~n", [MonitorInfoForTuopu]),
%% 	io:format("makeTuopu3: ~p ~n", [GroupInfoForTuopu]),
	Request = [{rootpath, RootPath}] ++ [{apppath, AppPath}] ++ [{name, Name}] ++ GroupInfoForTuopu ++ MonitorInfoForTuopu ++ MachineInfoForTuopu,
	
	Java_Node = siteview:get_java_node(),
	io:format("makeTuopu2: ~p ~n", [Java_Node]),
%% 	io:format("makeTuopu3333: ~p ~n", ["ddddd"]),
	Response = rpc(java_mail_box, Java_Node, {"com.dragonflow.erlangecc.monitor.TuopuMonitor", "update", Request}),	
	io:format("makeTuopu: ~p ~n", [Response]),
	ok.

%%@spec rpc(RegName, Node, Msg) -> Response
%%@type RegName = atom()
%%@type Node = atom()
%%@type Msg = [tuple()]
%%@doc remote process calling for the java node with messages.
rpc(RegName, Node, Msg) ->
	THIS:set_attribute(?DEBUG_INFO, "remote process call java node ..."),
	Ping = net_adm:ping(Node),
	{RegName, Node} ! Msg,	
	receive
		{ok, _, Ret} ->	
			qsort(Ret);
		{error, _From, Ret} ->
			Ret;
		{error, Reason} ->
			[{error, Reason}]
	after ?RECEIVE_TIME_OUT ->
			case Ping of
				pong ->
					[{error, "time is out. "}];
				pang ->
					[{error, "Connect Java Node Error! "}]
			end
	end.

%% @spec qsort
%% Obj = term()
%% @doc qsort
qsort([]) -> [];
qsort([Pivot|T]) ->
	qsort([X || X <- T, element(2,X) < element(2, Pivot)])
	++ [Pivot] ++
	qsort([X || X <- T, element(2,X) >= element(2,Pivot)]).