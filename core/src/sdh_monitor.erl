%%%@author hao.huang@dragonflow.com
%%%@copyright dragonflow
%%%@description SDH monitor

-module(sdh_monitor,[BASE]).
-compile(export_all).
-extends(server_monitor).
-include("monitor.hrl").
-include("monitor_template.hrl").

-record(emsAlarms, {eventType, eventInfo}).
-record(tca_eventInfo, {notificationId,objectName,nativeEMSName,objectType,emsTime,neTime,isClearable,perceivedSeverity,layerRate,granularity,pmParameterName,pmLocation,thresholdType,value,unit,additionalInfo}).
-record(alarm_eventInfo,{notificationId,objectName,nativeEMSName,nativeProbableCause,objectType,emsTime,neTime,isClearable,layerRate,probableCause,probableCauseQualifier,perceivedSeverity,serviceAffecting,affectedTPList,additionalText,additionalInfo,x733EventType,objectTypeQualifier}).
-record(objectName, {ems,managedelement,ptp,ctp}).
-record(additionalInfo, {alarmserialno,alarmreason,productname,equipmentname,affirmstate,detailinfo,slaveshelf}).

new()->
	Base = server_monitor:new(),
	{?MODULE,Base}.
	 
update() ->
	{ok,{_,Host}} = THIS:get_property(machine),
	{ok,{_,Port}} = THIS:get_property(pCorbaport),
	{ok,{_,User}} = THIS:get_property(pCorbauser),
	{ok,{_,Password}} = THIS:get_property(pCorbapassword),
	case getSDHAlarms(Host,Port,User,Password) of
		{ok,[]}		-> 
		THIS:set_attribute(?STATE_STRING,"Wait for data to return...");
		{ok,Alarms} -> 
			Result = get_Alarms_info(Alarms,[]),
			THIS:set_attribute(?STATE_STRING,Result);
		{error,rpcerror} ->	
		THIS:remove_attribute(?CATEGORY),
		THIS:set_attribute(?CATEGORY,error),
		io:format("category category category category ~p~n",[THIS:get_attribute(?CATEGORY)]),
		io:format("category category category category ~p~n",[THIS:get_attribute(category)]),
		THIS:set_attribute(?STATE_STRING,"SDH Node Error");
		{error,nodedown} ->
		THIS:remove_attribute(?CATEGORY),		
		THIS:set_attribute(?CATEGORY,error),
		io:format("category category category category ~p~n",[THIS:get_attribute(?CATEGORY)]),
		io:format("category category category category ~p~n",[THIS:get_attribute(category)]),
		THIS:set_attribute(?STATE_STRING,"SDH Node Down");
		{error,Reason} -> 
		THIS:remove_attribute(?CATEGORY),
		THIS:set_attribute(?CATEGORY,error),
		THIS:set_attribute(?STATE_STRING,Reason)
	end.

get_Alarms_info([],ACC) -> ACC;
get_Alarms_info([Alarm|Alarms],ACC) ->
	case Alarm#emsAlarms.eventType of
		"NT_ALARM" 	-> get_Alarms_info(Alarms, lists:flatten([ACC , nt_alarm_info(Alarm)]));
		"NT_TCA"	-> get_Alarms_info(Alarms, lists:flatten([ACC , nt_tca_info(Alarm)]));
		_ -> get_Alarms_info(Alarms,ACC)
	end;
get_Alarms_info(Alarms,ACC) when is_record(Alarms,emsAlarms) -> get_Alarms_info([Alarms],ACC).

nt_alarm_info(Alarm) ->
	lists:flatten([
		"AlarmSerialNo:",((Alarm#emsAlarms.eventInfo)#alarm_eventInfo.additionalInfo)#additionalInfo.alarmserialno,
		"ProductName:",((Alarm#emsAlarms.eventInfo)#alarm_eventInfo.additionalInfo)#additionalInfo.productname,
		"EquipmentName:",((Alarm#emsAlarms.eventInfo)#alarm_eventInfo.additionalInfo)#additionalInfo.equipmentname,
		"\n",
		"nativeEMSName:",iconv:convert("gbk", "utf-8",(Alarm#emsAlarms.eventInfo)#alarm_eventInfo.nativeEMSName),
		"\n",
		"AlarmReason:",iconv:convert("gbk", "utf-8", ((Alarm#emsAlarms.eventInfo)#alarm_eventInfo.additionalInfo)#additionalInfo.alarmreason)
	]).

nt_tca_info(Alarm) ->
	lists:flatten([
		"ProductName:",((Alarm#emsAlarms.eventInfo)#tca_eventInfo.additionalInfo)#additionalInfo.productname,
		"EquipmentName:",((Alarm#emsAlarms.eventInfo)#tca_eventInfo.additionalInfo)#additionalInfo.equipmentname,
		"\n",
		"nativeEMSName:",iconv:convert("gbk", "utf-8",(Alarm#emsAlarms.eventInfo)#tca_eventInfo.nativeEMSName)
	]).

get_template_property() ->
    BASE:get_template_property() ++ 
    [
		#property{name=pCorbaport,title="Corba Interface Port",type=numeric,order=2,description="Corba Interface Port"},
		#property{name=pCorbauser,title="Corba Interface Username",type=text,order=3,description="Corba Interface Username"},
		#property{name=pCorbapassword,title="Corba Interface Password",type=password,order=4,description="Corba Interface Password"}
    ].
	
getSDHAlarms(Host,Port,User,Password) ->
	case get_db_node(sdh) of
		{ok,Node} ->
			case rpc:call(Node,api_sdh_device,getEMSAlarms,[Host,Port,User,Password]) of
				[]	-> {ok,[]};
				{ok,Alarms} -> {ok,Alarms};
				{error,Reason} -> {error,Reason};
				{badrpc,_} -> {error,rpcerror};
				Any -> {error,Any}
			end;
		Error -> Error
	end.

%% ��ȡ��ݿ�ڵ����
get_db_node() ->
%% 	{ok,Hostname} = inet:gethostname(),
	Node = server_conf:get_db_node(),
	case net_adm:ping(Node) of
		pang -> {error,"Db node down"};
		pong -> {ok,Node}
	end.

get_db_node(Key) ->
	Conf = "conf/nodes.conf",
	case file:consult(Conf) of
		{ok,Data} ->
			Fun = fun([],_) -> get_db_node();
					([Item|Items],Self) ->
						case Item of
							{Key,Node} -> {ok,Node};
							_ -> Self(Items,Self)
						end
				end,
			Fun(Data,Fun);
		_ -> get_db_node()
	end.
	
verify(Params)->
	Errs =
	case proplists:get_value(pCorbaport,Params)   of
		""->
			[{port,"port missing"}];
		Port when not is_number(Port)->
			[{port,"port must be a number"}];
		X->
			case X > 0 of
			true ->
				[];
			_ ->
				[{port,"port must be greater than 0"}]
			end
	end ++
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

