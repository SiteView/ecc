-module(df_snmp_read_arp).

-compile(export_all).


-include_lib("df_snmp_record.hrl").
-include_lib("snmp/include/snmp_types.hrl").


main(Devid_list, ScanPro, Pid_log) -> 
    Read_arp = ScanPro#scanPro.read_arp,
    io:format("test here ~p~n",[Read_arp]),
    case Read_arp of
        "0"   -> SP_list = df_snmp_devidsp:getDevidSP(Devid_list,[]);
        "012" -> SP_list = df_snmp_devidsp:getDevidSP012(Devid_list,[]);
        _     -> SP_list = df_snmp_devidsp:getDevidSP02(Devid_list,[])
    end,
    io:format("test here2 ~n"),
    Arp_list = readArp_list(SP_list, ScanPro#scanPro.timeout, Pid_log),
    df_log:writeLog(Pid_log,"arp is ~p~n",[Arp_list]),
    getDevid_list_arp(Devid_list,Arp_list,[]).

readArp_list(SP_list, Timeout, Pid_log) ->
    ArpData_list = df_snmp_agn:main(SP_list,[1,3,6,1,2,1,4,22,1,2], Timeout),
    df_log:writeLog(Pid_log,"test for ArpData_list = ~p~n",[ArpData_list]),
    lists:map(fun({SP,Arps}) -> getOneArp(SP,Arps,[]) end,ArpData_list).

getOneArp(SP,[],Arp) ->
    Arp_new = lists:usort(Arp),
    getArp(SP,Arp_new,[]);

getOneArp(SP,[{Oid,Mac}|Arps],Arp) ->
    [Ifindex|IP] = lists:subtract(Oid,[1,3,6,1,2,1,4,22,1,2]),
    getOneArp(SP,Arps,[{Ifindex,{Mac,list_to_tuple(IP)}}|Arp]).

getArp(SP,[],Arp_new) ->{SP,Arp_new};

getArp(SP,[{Ifindex,MI}|Arp],Arp_new) ->
    case Arp_new of
        [{Ifindex,MI_list}|Arp_new_sub] ->
            getArp(SP,Arp,[{Ifindex,[MI|MI_list]}|Arp_new_sub]);
        Arp_new_sub ->
            getArp(SP,Arp,[{Ifindex,[MI]}|Arp_new_sub])
    end.

getDevid_list_arp([],_,Devid_list_new) -> Devid_list_new;

getDevid_list_arp([Devid|Devid_list],Arp_list,Devid_list_new) ->
    case lists:keysearch(Devid#deviceInfo.sp,1,Arp_list) of
        false    -> 
            getDevid_list_arp(Devid_list,Arp_list,[Devid|Devid_list_new]);
        {value,{_,Arps}} ->
            Devid_new = Devid#deviceInfo{arp_list=Arps},
            getDevid_list_arp(Devid_list,Arp_list,[Devid_new|Devid_list_new])
    end.

getDevidSP02([],SP_list) -> SP_list;
getDevidSP02([Devid|Devid_list],SP_list) ->
    Type = Devid#deviceInfo.devType,
    if
        Type=:=0;Type=:=2 ->
            getDevidSP02(Devid_list,[Devid#deviceInfo.sp|SP_list]);
        true -> 
            getDevidSP02(Devid_list,SP_list)
    end.
    






















