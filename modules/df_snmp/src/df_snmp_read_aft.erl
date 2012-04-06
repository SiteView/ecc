-module(df_snmp_read_aft).

-compile(export_all).


-include_lib("df_snmp_record.hrl").
-include_lib("snmp/include/snmp_types.hrl").

main(Devid_list,Timeout,Pid_log) ->
    Csc_Devid_list = getCscDevid(Devid_list,[]),
    SP_list        = df_snmp_devidsp:getDevidSP01(Devid_list,[]),
    SP_list_logic  = lists:umerge(lists:usort(SP_list),getLogicSP(Csc_Devid_list,[])),
    AftData_list1  = readAftByDtp(SP_list_logic, Timeout, Pid_log),
    io:format("test readAftByDtp ~n~p~n",[AftData_list1]),
    AftData_list2  = getAftLogic1(AftData_list1,SP_list,[]),
    io:format("test getAftLogic1 ~n~p~n",[AftData_list2]),
    SP_list1 = lists:subtract(SP_list, Csc_Devid_list),
    AftData_list3  = readAftByQtp(SP_list1, Timeout, Pid_log),
    io:format("test readAftByQtp ~n~p~n",[AftData_list3]),
    AftData_list4  = lists:keymerge(1,lists:usort(AftData_list2),
                                lists:usort(AftData_list3)),
    io:format("test AftData_list4 ~n~p~n",[AftData_list4]),
    AftData_list5  = getAft_list(AftData_list4,[]),
    io:format("test AftData_list5 ~n~p~n",[AftData_list5]),
    Aft_list       = lists:map(fun({SP,Aft}) -> getAft(SP,Aft,[]) end, AftData_list5),
    getDevid_list_aft(Devid_list,Aft_list,[]).

getCscDevid([],Devid_list_new) -> Devid_list_new;

getCscDevid([Devid|Devid_list],Devid_list_new) ->
    SysOid = Devid#deviceInfo.sysOid,
    case lists:prefix([1,3,6,1,4,1,9],SysOid) of
        true  ->
            getCscDevid(Devid_list,[Devid|Devid_list_new]);
        false ->
            getCscDevid(Devid_list,Devid_list_new)
    end.

getLogicSP([],SP_list_logic) -> SP_list_logic;
getLogicSP([Csc_devid|Csc_Devid_list],SP_list_logic) ->
    LogicCommunity = Csc_devid#deviceInfo.logicCommunity,
    SP             = Csc_devid#deviceInfo.sp,
    SPs_logic      = lists:map(fun(LC) -> SP#snmpPara{community = LC} end,                                  LogicCommunity),
    getLogicSP(Csc_Devid_list,lists:umerge(lists:usort(SPs_logic),SP_list_logic)).


getDevidSP([],SP_list) -> SP_list;
getDevidSP([Devid|Devid_list],SP_list) ->
    Type = Devid#deviceInfo.devType,
    if
        Type=:=0;Type=:=1;Type=:=2 ->
            getDevidSP(Devid_list,[Devid#deviceInfo.sp|SP_list]);
        true -> 
            getDevidSP(Devid_list,SP_list)
    end.



readAftByDtp(SP_list, Timeout, Pid_log) ->
    AftData_list = test_gn:main(SP_list, [1,3,6,1,2,1,17,4,3,1,2]),
    lists:map(fun({SP,Afts}) -> getOneAft(SP,Afts) end, AftData_list).

readAftByQtp(SP_list, Timeout, Pid_log) ->
    AftData_list = df_snmp_agn:main(SP_list,[1,3,6,1,2,1,17,7,1,2,2,1,2], Timeout),
    lists:map(fun({SP,Afts}) -> getOneAft(SP,Afts) end, AftData_list).
    

    
    
getOneAft(SP,Afts) ->
    {
        SP,
        lists:usort(
            lists:map(fun({Oid,Port}) -> 
                        {_,Mac} = get_endof_list(Oid,6) ,
                        {Port,Mac} end, Afts ))
    }.

get_endof_list(List,Num) ->
    Len = length(List),
    lists:split(Len-Num,List).

getAft_list([],Aft_list) -> Aft_list;
getAft_list([{SP,AftData}|AftData_list],[{SP,Aft}|Aft_list]) ->
    Aft_new = lists:umerge(lists:usort(AftData),Aft),
    getAft_list(AftData_list,[{SP,Aft_new}|Aft_list]);
getAft_list([{SP,AftData}|AftData_list],Aft_list) ->
    getAft_list(AftData_list,[{SP,AftData}|Aft_list]).

getAft(SP,[],Aft_new) ->{SP,Aft_new};

getAft(SP,[{Ifport,Mac}|Aft],Aft_new) ->
    case Aft_new of
        [{Ifport,M_list}|Aft_new_sub] ->
            getAft(SP,Aft,[{Ifport,[Mac|M_list]}|Aft_new_sub]);
        Aft_new_sub ->
            getAft(SP,Aft,[{Ifport,[Mac]}|Aft_new_sub])
    end.


getAftLogic1(_,[],AftData_list_new) -> AftData_list_new;
getAftLogic1(AftData_list,[SP|SP_list],AftData_list_new) ->
    Afts = getAftLogic2(AftData_list,{SP,[]}),
    getAftLogic1(AftData_list,SP_list,lists:umerge([Afts],AftData_list_new)).


getAftLogic2([],Afts) -> Afts;
getAftLogic2([{SP1,Aft}|Afts],{SP2,Afts_new}) ->
    IP2 = SP2#snmpPara.server,
    case SP1#snmpPara.server of
        IP2 -> 
            getAftLogic2(Afts,{SP2,lists:umerge(lists:usort(Aft),Afts_new)});
        _ ->
            getAftLogic2(Afts,{SP2,Afts_new})
    end.


getDevid_list_aft([],_,Devid_list_new) -> Devid_list_new;

getDevid_list_aft([Devid|Devid_list],Aft_list,Devid_list_new) ->
    case lists:keysearch(Devid#deviceInfo.sp,1,Aft_list) of
        false    -> 
            getDevid_list_aft(Devid_list,Aft_list,[Devid|Devid_list_new]);
        {value,{_,Afts}} ->
            Devid_new = Devid#deviceInfo{aft_list=Afts},
            getDevid_list_aft(Devid_list,Aft_list,[Devid_new|Devid_list_new])
    end.

    


