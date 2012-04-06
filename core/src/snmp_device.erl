-module(snmp_device,[Id, Dis, Metrics, MonitorObj]).
-compile(export_all).
-include_lib("snmp/include/snmp_types.hrl").

-define(ETSNAME, ecc_snmp_device_ets).

%% ETS option , save global variable
%%make_a_set() ->
%%    io:format("*****PidAtom = ~p~n", [PidAtom]),
%%    Tab = ets:new(PidAtom,[public,named_table]).
    
    
%%get_attribute(Item) ->
%%    case ets:lookup(PidAtom, Item) of
%%        undefined ->
%%            {error, undefine_item};
%%        [{Item, Value}] ->
%%            {ok, Value};
%%        Other ->
%%            {error, Other}
%%    end.
    
%%set_attribute(Item, Value) ->
%%    ets:insert(PidAtom,{Item, Value}).
    
%%delete() ->
%%    ets:delete(?ETSNAME).
    
init() ->
    %%make_a_set(),
    MonitorObj:set_attribute(initialRTPropertyValue, "-1"),
    MonitorObj:set_attribute(numRequests, -1),
    %%MonitorObj:set_attribute(identifier, Id),
    %%MonitorObj:set_attribute(displayName, Dis),
    %%MonitorObj:set_attribute(metrics, Metrics),
    MonitorObj:set_attribute(propertyNametoMetricLabel, []),
    MonitorObj:set_attribute(rTPropertyNametoMetricLabel, []),
    MonitorObj:set_attribute(rTPropertyNametoGraphLabel, []).
    
%%******************************
%%**********i****************
%%******************************
setSession(Snmpsession) ->
    MonitorObj:set_attribute(session, Snmpsession).

setRealTimeDataWindow(L) ->
    MonitorObj:set_attribute(realTimeDataWindow, L).
    
refreshMetrics() ->
    case MonitorObj:get_attribute(session) of
        {ok,{_,Session}} ->
            %%io:format("yes yes yes yes yes yes yes~n"),
            Ids = cycDel(Metrics, Session),
            %%io:format("***************Ids = ~p~n", [Ids]),
            Ids;
        Other ->
            %%io:format("no no no no no no no~n"),
            []
    end.
    
    
cycDel([],Session) ->
    [];
cycDel([{OIDStr,Metric}|T],Session) ->
    OID = [list_to_integer(X) || X <- string:tokens(OIDStr,".")],
    Ret = Session:get_table_col(OID),
    F = fun(X)->
            %%Str = getOctetString(X#varbind.value),
            Index = lists:last(X#varbind.oid),
            case proplists:get_value(metricName, Metric) of
                undefined ->
                    MetricName = [];
                MetricName ->
                    MetricName
            end,
            case proplists:get_value(units, Metric) of
                Units = undefined ->
                    [];
                Units ->
                    Units
            end,
            if
                Index =:= 0 ->
                    TbIdx = "";
                true ->
                    TbIdx = integer_to_list(Index)
            end,
            if
                MetricName =/= [] andalso Units =/= [] ->
                    %%set_attribute(MetricName ++ TbIdx ++ "(" ++ Units ++ ")", X#varbind.value),
                    %%io:format("*******************~nClassDis = ~p~nClassValue = ~p~n", [MetricName ++ TbIdx ++ "(" ++ Units ++ ")",get_attribute(MetricName ++ TbIdx ++ "(" ++ Units ++ ")")]),
                    {MetricName ++ TbIdx ++ "(" ++ Units ++ ")", X#varbind.value};
                true ->
                    []
            end
            
            %%X
        end,
    lists:map(F,Ret) ++
    cycDel(T,Session);
cycDel([H|T],Session) ->
    cycDel(T,Session).
    
    
getOctetString(Str)->
	case lists:last(Str) of
		0->
			string:substr(Str,1,length(Str)-1);
		_->
			Str
	end.
    
    
    
    
    

    
    
