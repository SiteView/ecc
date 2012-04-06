-module(df_snmp_read_inf).

-compile(export_all).


-include_lib("df_snmp_record.hrl").
-include_lib("snmp/include/snmp_types.hrl").


main(Devid_list, Timeout, Pid_log) ->
    SP_list1       = getDevidSP(Devid_list,[]),
    df_log:writeLog(Pid_log,"DevidSP is ~p~n",[SP_list1]),
    Infinx_list    = readInfinx_list(SP_list1, Timeout, Pid_log),
    df_log:writeLog(Pid_log,"Infinx is ~p~n",[Infinx_list]),
    SP_list2       = getSP_list(Infinx_list),
    Devid_list_new = getDevid_list_inf(Devid_list, Infinx_list, []),
    delDevid(Devid_list_new,lists:subtract(SP_list1,SP_list2)).



delDevid(Devid_list,[]) -> Devid_list;
delDevid(Devid_list,SP_list) ->
    lists:map(fun(Devid) -> delDevid1(Devid,SP_list) end, Devid_list).

delDevid1(Devid,SP_list) ->
    SP = Devid#deviceInfo.sp,
    case lists:member(SP,SP_list) of
        true  -> Devid#deviceInfo{snmpFlag=0};
        false -> Devid
    end.

readInfinx_list(SP_list, Timeout, Pid_log) ->
    InfNum_list   = df_snmp_agn:main(SP_list,[1,3,6,1,2,1,2,1],Timeout),
    io:format("getSP_list(InfNum_list)=~p~n",[getSP_list(InfNum_list)]),
    InfTypes_list = df_snmp_agn:main(getSP_list(InfNum_list),
                                     [1,3,6,1,2,1,2,2,1,3],Timeout),
    %%io:format("the InfTypes_list is ~p~n",[InfTypes_list]),
    InfPort_list  = df_snmp_agn:main(getSP_list(InfTypes_list),
                                     [1,3,6,1,2,1,17,1,4,1,2],Timeout),
    %%io:format("the InfPort_list is ~p~n",[InfPort_list]),
    InfDesc_list  = df_snmp_agn:main(getSP_list(InfTypes_list),
                                     [1,3,6,1,2,1,2,2,1,2],Timeout),
    df_log:writeLog(Pid_log,"the InfDesc_list is ~p~n",[InfDesc_list]),
    InfMac_list   = df_snmp_agn:main(getSP_list(InfDesc_list),
                                     [1,3,6,1,2,1,2,2,1,6],Timeout),
    %%format("the InfMac_list is ~p~n",[InfMac_list]),
    InfSpeed_list = df_snmp_agn:main(getSP_list(InfMac_list),
                                     [1,3,6,1,2,1,2,2,1,5],Timeout),
    getInfinx_list(getSP_list(InfSpeed_list),InfNum_list,InfTypes_list,InfPort_list,InfDesc_list,InfMac_list,InfSpeed_list,[]).


getDevid_list_inf([],_,Devid_list_new) -> Devid_list_new;

getDevid_list_inf([Devid|Devid_list],Infinx_list,Devid_list_new) ->
    case lists:keysearch(Devid#deviceInfo.sp,1,Infinx_list) of
        false    -> 
            getDevid_list_inf(Devid_list,Infinx_list,[Devid|Devid_list_new]);
        {value,{_,Inf_list}} ->
            Mac  = Devid#deviceInfo.baseMac,
            Macs = getMacs(Inf_list#inf.infinx_list,[Mac]),
            Devid_new = Devid#deviceInfo{mac_list=Macs,inf_list=Inf_list},
            getDevid_list_inf(Devid_list,Infinx_list,[Devid_new|Devid_list_new])
    end.


getMacs([],Macs) -> Macs;
getMacs([InfInx|InfInx_list],Macs) ->
    getMacs(InfInx_list,lists:umerge([InfInx#infInx.ifmac],Macs)).

getInfinx_list([],_,_,_,_,_,_,InfInx_list) ->
    InfInx_list;

getInfinx_list([SP|SP_list],InfNum_list,InfTypes_list,InfPort_list,
                    InfDesc_list,InfMac_list,InfSpeed_list,InfInx_list) ->
    case getV(InfNum_list,SP) of
        [{_,InfNum}|_] ->
            Infinxes  = getInfinxes(
                                    #inf{ifip=SP#snmpPara.server,ifnum=InfNum},
                                    getV(InfTypes_list,SP),getV(InfPort_list,SP),
                                    getV(InfDesc_list,SP),getV(InfMac_list,SP),
                                    getV(InfSpeed_list,SP)),
            getInfinx_list(SP_list,InfNum_list,InfTypes_list,InfPort_list,
                    InfDesc_list,InfMac_list,InfSpeed_list,[{SP,Infinxes}|InfInx_list]);    
        _ -> getInfinx_list(SP_list,InfNum_list,InfTypes_list,InfPort_list,
                    InfDesc_list,InfMac_list,InfSpeed_list,InfInx_list)    
    end.

getInfinxes(Infinxes,[],_,_,_,_) -> Infinxes;

getInfinxes(Infinxes,[{Ifindex,IfType}|InfTypes],InfPorts,InfDescs,InfMacs,InfSpeeds) ->
    Infinx  = getInfinx(#infInx{ifindex=Ifindex,ifType=IfType},
                                InfPorts,InfDescs,InfMacs,InfSpeeds),
    case Infinx#infInx.ifmac of
        [] -> Infinxes_new = Infinxes;
        _  ->
            Infinx_list  = [Infinx|Infinxes#inf.infinx_list],
            Infinxes_new = Infinxes#inf{infinx_list=Infinx_list}
    end,
    getInfinxes(Infinxes_new,InfTypes,InfPorts,InfDescs,InfMacs,InfSpeeds).
    
getInfinx(Infinx,InfPorts,InfDescs,InfMacs,InfSpeeds) ->
    Ifindex = Infinx#infInx.ifindex,
    IfDesc  = getIf(Ifindex, InfDescs),
    IfMac   = getIf(Ifindex, InfMacs),
    IfSpe   = getIf(Ifindex, InfSpeeds),
    IfPort  = getOid(Ifindex, InfPorts),
    Infinx#infInx{
                    ifdesc  = IfDesc,
                    ifmac   = IfMac,
                    ifport  = IfPort,
                    ifspeed = IfSpe
                  }.
    
getIf(_,[]) -> false;
getIf(Ifindex, [{Oid,V}|Ifs]) ->
    case Oid of
        Ifindex -> V;
        _       -> getIf(Ifindex,Ifs)
    end.

getOid(Ifindex,[]) -> Ifindex;
getOid(Ifindex,[{Oid,V}|Ifs]) ->
    case V of
        Ifindex -> Oid;
        _       -> getOid(Ifindex,Ifs)
    end.

getSP_list(SV_list) ->
    lists:map(fun({SP,_}) -> SP end, SV_list).

getDevidSP([],SP_list) -> SP_list;

getDevidSP([Devid|Devid_list],SP_list) ->
    Type = Devid#deviceInfo.devType,
    Flag = Devid#deviceInfo.snmpFlag,
    if
        Flag=:=1 ->
            if
                Type=:=0;Type=:=1;Type=:=2 ->
                    getDevidSP(Devid_list,[Devid#deviceInfo.sp|SP_list]);
                true ->
                    getDevidSP(Devid_list,SP_list)
            end;
        true -> 
            getDevidSP(Devid_list,SP_list)
    end.
    
getV(SV_list,SP) ->
    case lists:keysearch(SP,1,SV_list) of
        {value,{_,Vs}} -> 
            lists:map(fun({Oid,Value}) -> {lists:last(Oid),Value} end , Vs);
        false         -> []
    end.















