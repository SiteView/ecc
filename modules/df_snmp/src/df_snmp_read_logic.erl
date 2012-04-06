-module(df_snmp_read_logic).

-compile(export_all).


-include_lib("df_snmp_record.hrl").
-include_lib("snmp/include/snmp_types.hrl").

main(Devid_list,Timeout,Pid_log) ->
    SP_list = getCscSP(Devid_list,[]),
    LC_list = readLogicCommunity(SP_list,Timeout,Pid_log),
    getLogicCommunity(Devid_list,LC_list,[]).
    
getCscSP([],SP_list) ->
    SP_list;
getCscSP([Devid|Devid_list],SP_list) ->
    SysOid = Devid#deviceInfo.sysOid,
    case lists:prefix([1,3,6,1,4,1,9],SysOid) of
        true  ->
            getCscSP(Devid_list,[Devid#deviceInfo.sp|SP_list]);
        false ->
            getCscSP(Devid_list,SP_list)
    end.

readLogicCommunity(SP_list,Timeout,Pid_log) ->
    LogicCom_list = df_snmp_agn:main(SP_list,[1,3,6,1,2,1,47,1,2,1,1,4], Timeout),
    lists:map(fun({SP,LCs}) -> getOneLC(SP,LCs,[]) end,LogicCom_list).

getOneLC(SP,[],LC_new) ->
    {SP,LC_new};
getOneLC(SP,[{_,LC}|LCs],LC_new) ->
    V = cutString(LC,$@),
    if
        V=:="1";V=:="1002";V=:="1003";V=:="1004";V=:="1005" ->
            getOneLC(SP,LCs,LC_new);
        true ->
            getOneLC(SP,LCs,[LC|LC_new])
    end.

cutString([],_) -> "";

cutString([Str|Strs],Sign) ->
    if
        Str =:= Sign ->
            Strs;
        true         ->
            cutString(Strs,Sign)
    end.

getLogicCommunity([],_,Devid_list_new) -> Devid_list_new;
getLogicCommunity([Devid|Devid_list],LC_list,Devid_list_new) ->
    case lists:keysearch(Devid#deviceInfo.sp,1,LC_list) of
        false ->
            getLogicCommunity(Devid_list,LC_list,[Devid|Devid_list_new]);
        {value,{_,LCs}} ->
            Devid_new = Devid#deviceInfo{logicCommunity=LCs},
            getLogicCommunity(Devid_list,LC_list,[Devid_new|Devid_list_new])
    end.
























