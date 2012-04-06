-module(api_sdh_query).
-export([query_alarms/1,query_alarms/3]).

-record(emsAlarms, {eventType, eventInfo}).
-record(tca_eventInfo, {notificationId,objectName,nativeEMSName,objectType,emsTime,neTime,isClearable,perceivedSeverity,layerRate,granularity,pmParameterName,pmLocation,thresholdType,value,unit,additionalInfo}).
-record(alarm_eventInfo,{notificationId,objectName,nativeEMSName,nativeProbableCause,objectType,emsTime,neTime,isClearable,layerRate,probableCause,probableCauseQualifier,perceivedSeverity,serviceAffecting,affectedTPList,additionalText,additionalInfo,x733EventType,objectTypeQualifier}).
-record(objectName, {ems,managedelement,ptp,ctp}).
-record(additionalInfo, {alarmserialno,alarmreason,productname,equipmentname,affirmstate,detailinfo,slaveshelf}).

%~ api_sdh_query:query_alarms([{sdhserver,'==',"10.184.249.250"}]).
%% Get the name of the database node
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

query_alarms(Query) -> query_alarms(Query,1,20).
query_alarms(Query,Index,Pc) ->
	FQ = fun([],_,ACC) -> ACC;
			([Item|Items],Self,ACC) ->
				case Item of
					{_,_,Value} when Value =/= [] ->
						Self(Items,Self,[Item | ACC]);
					_	-> Self(Items,Self,ACC)
				end
		end,
	FQuery = FQ(Query,FQ,[]),
	case get_db_node(sdh) of
		{ok,Node} ->
			case rpc:call(Node,api_sdh_device,queryAlarms,[FQuery]) of
				[]	-> {ok,0,0,[]};
				{ok,Alarms} -> 
					All = get_Alarms_info(Alarms,[]),
					Sort = sort_Alarms_info(All),
					Len = length(Sort),
					PCount = case (Len - (Index - 1) * Pc) >= Pc of
							true -> Pc;
							_ -> case (Len - (Index - 1) * Pc) >= 0 of
									true -> Len - (Index - 1) * Pc;
									_ -> 0
								 end
						end,
					Page = lists:sublist(Sort,(Index - 1) * Pc + 1, PCount),
					{ok,Len,PCount,Page};
				{error,Reason} -> {error,Reason};
				{badrpc,_} -> {error,rpcerror};
				Any -> {error,Any}
			end;
		Err -> Err
	end.

sort_Alarms_info(Alarms) ->
	SortFun = fun(Alarm1,Alarm2) ->
			OneTime = case Alarm1#emsAlarms.eventType of
				"NT_ALARM"	-> 
					EmsTime1 = (Alarm1#emsAlarms.eventInfo)#alarm_eventInfo.emsTime,
					Etime1 = string:substr(EmsTime1,1,14),
					list_to_integer(Etime1);
				"NT_TCA"	-> 
					EmsTime1 = (Alarm1#emsAlarms.eventInfo)#tca_eventInfo.emsTime,
					Etime1 = string:substr(EmsTime1,1,14),
					list_to_integer(Etime1);
				_ -> 0
			end,
			TwoTime = case Alarm2#emsAlarms.eventType of
				"NT_ALARM"	-> 
					EmsTime2 = (Alarm2#emsAlarms.eventInfo)#alarm_eventInfo.emsTime,
					Etime2 = string:substr(EmsTime2,1,14),
					list_to_integer(Etime2);
				"NT_TCA"	-> 
					EmsTime2 = (Alarm2#emsAlarms.eventInfo)#tca_eventInfo.emsTime,
					Etime2 = string:substr(EmsTime2,1,14),
					list_to_integer(Etime2);
				_ -> 0
			end,
			OneTime > TwoTime
		end,
	lists:sort(SortFun,Alarms).

get_Alarms_info([],ACC) -> ACC;
get_Alarms_info([Alarm|Alarms],ACC) ->
	case is_record(Alarm,emsAlarms) of
		true ->
			case Alarm#emsAlarms.eventType of
				"NT_ALARM" 	-> get_Alarms_info(Alarms, [nt_alarm_info(Alarm) | ACC]);
				"NT_TCA"	-> get_Alarms_info(Alarms, [nt_tca_info(Alarm) | ACC]);
				_ -> get_Alarms_info(Alarms,ACC)
			end;
		_	 -> get_Alarms_info(Alarms,ACC)
	end;
get_Alarms_info(Alarms,ACC) when is_record(Alarms,emsAlarms) -> get_Alarms_info([Alarms],ACC).

nt_alarm_info(Alarm) ->
	NativeEMSName = iconv:convert("gbk", "utf-8",(Alarm#emsAlarms.eventInfo)#alarm_eventInfo.nativeEMSName),
	Alarmreason = iconv:convert("gbk", "utf-8", ((Alarm#emsAlarms.eventInfo)#alarm_eventInfo.additionalInfo)#additionalInfo.alarmreason),
	Detailinfo = iconv:convert("gbk", "utf-8", ((Alarm#emsAlarms.eventInfo)#alarm_eventInfo.additionalInfo)#additionalInfo.detailinfo),
	Alarm#emsAlarms {
		eventInfo = (Alarm#emsAlarms.eventInfo)#alarm_eventInfo {
			nativeEMSName = list_to_binary(NativeEMSName),
			additionalInfo = ((Alarm#emsAlarms.eventInfo)#alarm_eventInfo.additionalInfo)#additionalInfo {
				alarmreason = list_to_binary(Alarmreason),
				detailinfo = list_to_binary(Detailinfo)
			}
		}
	}.

nt_tca_info(Alarm) ->
	NativeEMSName = iconv:convert("gbk", "utf-8",(Alarm#emsAlarms.eventInfo)#tca_eventInfo.nativeEMSName),
	Alarm#emsAlarms {
		eventInfo = (Alarm#emsAlarms.eventInfo)#tca_eventInfo {
			nativeEMSName = list_to_binary(NativeEMSName)
		}
	}.
