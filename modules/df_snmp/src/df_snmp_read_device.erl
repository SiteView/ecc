-module(df_snmp_read_device).

-compile(export_all).


-include_lib("df_snmp_record.hrl").
-include_lib("snmp/include/snmp_types.hrl").


main(SP_list,Timeout,DTD) ->
    Devid_list1    = get_devid_SP(SP_list),
    
    Sys_list       = readSys_list(SP_list,Timeout),
    Devid_list2    = get_deviceInfo(Devid_list1,[],Sys_list,DTD),
    
    Mac_list       = readMac_list(getSP_list(Sys_list),Timeout),
    IP_list        = readIP_list(getSP_list(Mac_list),Timeout),
    Msk_list       = readMsk_list(getSP_list(IP_list),Timeout),
    Inf_list       = readInf_list(getSP_list(Msk_list),Timeout),
    IMI_list       = processIPMskInf(getSP_list(Inf_list),IP_list,Msk_list,Inf_list,[]),
    Devid_list3    = get_device_other(Devid_list2,[],Mac_list,IMI_list),
    Devid_list_new = delDevid(Devid_list3,lists:subtract(SP_list,getSP_list(IMI_list))),
    Devid_list_new.

get_devid_SP([]) -> [];
get_devid_SP(SP_list) ->
    io:format("the SP_list  ~p ~n~n",[SP_list]),
    lists:map(fun(SP) -> #deviceInfo
    {snmpFlag=1,sp=SP,community_get = SP#snmpPara.community} end, SP_list).

getSP_Devid([],SP_list) ->
    lists:usort(SP_list);
getSP_Devid([Devid|Devid_list],SP_list) ->
    case Devid#deviceInfo.snmpFlag of
        1 ->
            getSP_Devid(Devid_list,[Devid#deviceInfo.sp|SP_list]);
        _ ->
            getSP_Devid(Devid_list,SP_list)
    end.
    
getSP_list(SV_list) ->
    lists:map(fun({SP,_}) -> SP end, SV_list).    


get_deviceInfo([],Devid_list_new,_,_)-> Devid_list_new;

get_deviceInfo([Devid|Devid_list],Devid_list_new,Sys_list,DTD) ->
    SP = Devid#deviceInfo.sp,
    case lists:keysearch(SP,1,Sys_list) of
        {value,{_,Sys}} ->
            Devid_new = get_device_sys(Devid,Sys,DTD),
            get_deviceInfo(Devid_list,[Devid_new|Devid_list_new],Sys_list,DTD);
        _        ->
            get_deviceInfo(Devid_list,Devid_list_new,Sys_list,DTD)
    end.

get_device_sys(Devid_new,[],_) -> Devid_new;

get_device_sys(Devid,[{Oid,Value}|Sys],DTD) ->
    if 
        Oid =:= [1,3,6,1,2,1,1,2,0];Oid =:= [1,3,6,1,2,1,1,2] ->
            case dict:find(Value,DTD) of
                {ok,{DevType, DevTypeName, DevModel, DevFactory}} ->
                    Devid_new = Devid#deviceInfo{sysOid=Value,devType=DevType,
                    devTypeName=DevTypeName,devModel=DevModel,devFactory=DevFactory},
                    get_device_sys(Devid_new,Sys,DTD);
                _ ->
                    Devid_new = Devid#deviceInfo{sysOid=Value},
                    get_device_sys(Devid_new,Sys,DTD)
            end;
        Oid =:= [1,3,6,1,2,1,1,5,0];Oid =:= [1,3,6,1,2,1,1,5] ->
            Devid_new = Devid#deviceInfo{sysName=Value},
            get_device_sys(Devid_new,Sys,DTD);
        Oid =:= [1,3,6,1,2,1,1,7,0];Oid =:= [1,3,6,1,2,1,1,7] ->
            Devid_new = Devid#deviceInfo{sysSvcs=Value},
            get_device_sys(Devid_new,Sys,DTD);
        true -> get_device_sys(Devid,Sys,DTD)
    end.

readSys_list(SP_list,Timeout) ->
    Oids = [1,3,6,1,2,1,1],
    df_snmp_agn:main(SP_list,Oids,Timeout).
    
readMac_list(SP_list,Timeout) ->
    Oids = [1,3,6,1,2,1,17,1,1],
    df_snmp_agn:main(SP_list,Oids,Timeout).
    
readIP_list(SP_list,Timeout) ->
    Oids = [1,3,6,1,2,1,4,20,1,1],
    df_snmp_agn:main(SP_list,Oids,Timeout).
    
readMsk_list(SP_list,Timeout) ->
    Oids = [1,3,6,1,2,1,4,20,1,3],
    df_snmp_agn:main(SP_list,Oids,Timeout).
    
readInf_list(SP_list,Timeout) ->
    Oids = [1,3,6,1,2,1,4,20,1,2],
    df_snmp_agn:main(SP_list,Oids,Timeout).
    

processIPMskInf([],_,_,_,IMI_list) ->
    IMI_list;

processIPMskInf([SP|SP_list],IP_list,Msk_list,Inf_list,IMI_list) ->
    IPs  = search(SP,IP_list),
    Msks = search(SP,Msk_list),
    Infs = search(SP,Inf_list),
    IMI  = processIPMskInf(IPs,Msks,Infs,[]),
    processIPMskInf(SP_list,IP_list,Msk_list,Inf_list,[{SP,IMI}|IMI_list]).

processIPMskInf(IPs,Msks,Infs,IMI)
    when IPs=:=[];Msks=:=[];Infs=:=[] -> IMI;

processIPMskInf([{_,IP}|IPs],[{_,Msk}|Msks],[{_,Inf}|Infs],IMI) ->
    [IP1|_] = IP,
    %%Msk1    = lists:last(Msk),
    %%io:format("~n~ntest for IP~p  ~p   ~p~n~n",[IP1,IP,Msk]),
    if
        IP1=:=0;IP1=:=127 ->
            processIPMskInf(IPs,Msks,Infs,IMI);
        %%Msk1>128 ->
          %%  processIPMskInf(IPs,Msks,Infs,IMI);
        is_integer(IP1) ->
            processIPMskInf(IPs,Msks,Infs,[{{list_to_tuple(IP),list_to_tuple(Msk)},Inf}|IMI]);
        true ->
            processIPMskInf(IPs,Msks,Infs,IMI)
    end.
    
get_device_other([],Devid_list_new,_,_) -> Devid_list_new;
    
get_device_other([Devid|Devid_list],Devid_list_new,Mac_list,IMI_list) ->
    SP  = Devid#deviceInfo.sp,
    case search(SP,Mac_list) of
        [{_,Mac}|_] -> Devid_new = Devid#deviceInfo{baseMac = Mac};
        _              -> Devid_new = Devid
    end,
    case search(SP,IMI_list) of
        []       -> Devid_new1 = Devid_new;
        IMI      -> 
            Devid_new1 = Devid_new#deviceInfo{ipmskinf_list=lists:usort(IMI)}
    end,
    get_device_other(Devid_list,[Devid_new1|Devid_list_new],Mac_list,IMI_list).


    
search(SP,V_list) ->
    case lists:keysearch(SP,1,V_list) of
        {value,{_,V}} -> V;
        false          -> []
    end.



delDevid(Devid_list,[]) -> Devid_list;
delDevid(Devid_list,SP_list) ->
    lists:map(fun(Devid) -> delDevid1(Devid,SP_list) end, Devid_list).

delDevid1(Devid,SP_list) ->
    SP = Devid#deviceInfo.sp,
    case lists:member(SP,SP_list) of
        true  -> Devid#deviceInfo{snmpFlag=0};
        false -> Devid
    end.






















