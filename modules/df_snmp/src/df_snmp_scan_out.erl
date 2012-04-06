-module(df_snmp_scan_out,[ScanPro,Pid_log]).

-compile(export_all).

-include_lib("df_snmp_record.hrl").
-include_lib("snmp/include/snmp_types.hrl").


new(ScanPro,Pid_log) ->
    Obj = instance(ScanPro,Pid_log),
    Obj.



discover_arp(IP_list,Devid_list,IP_visited_list) ->
    {SP_list, IP_visited_list_new1} = getSP_list(IP_list, ScanPro, IP_visited_list),
    Seed_list = snmpping(SP_list,ScanPro,Pid_log),
    {IP_visited_list_new2, Devid_list_cur} 
        = scanSeeds_arp(Seed_list,IP_visited_list_new1,ScanPro),
    {IP_list_new,Devid_list_new} = getIP_list_arp
                                    (Devid_list_cur,ScanPro, Pid_log),
    {
        IP_list_new,
        lists:umerge(Devid_list_new,Devid_list),
        IP_visited_list_new2
    }.
    
%discover_ipmask(IP_list,Devid_list,IP_visited_list,IP_Mask_list) ->
%    {SP_list, IP_visited_list_new1} = getSP_list(IP_list, ScanPro, IP_visited_list),
%    Seed_list = snmpping(SP_list,ScanPro,Pid_log),
    


getSP_list(IP_list, ScanPro, IP_visited_list) ->
    df_snmp_sp:getSP_list(IP_list, ScanPro, IP_visited_list).

snmpping(SP_list,ScanPro,Pid_log) -> 
    Timeout  = ScanPro#scanPro.timeout,
    Ping_len = ScanPro#scanPro.ping_len,
    df_snmp_snmpping:main(SP_list,Timeout,Pid_log,Ping_len).

%% get the IPMask_cur,Devid_list_cur,and update IP_visited
%% the return is all usort list
scanSeeds_arp([],IP_visited_list_new1,_) -> {IP_visited_list_new1,[]};
scanSeeds_arp(Seed_list,IP_visited_list_new1, ScanPro) ->
    Timeout = ScanPro#scanPro.timeout,
    DTD     = ScanPro#scanPro.dtd,
    Devid_list_cur = df_snmp_read_device:main(Seed_list,Timeout,DTD),
    {IP_list_cur, _IPMask_list_new} = getIPMsk_list(Devid_list_cur),  
    IP_visited_list_new2 = lists:umerge(IP_list_cur,IP_visited_list_new1),
    {IP_visited_list_new2,lists:usort(Devid_list_cur)}.
    
    
getIPMsk_list(Devid_list_org) ->
    Devid_list = getDevice012(Devid_list_org,[]),
    IMI_list    = lists:umerge(lists:map(fun(Devid) -> Devid#deviceInfo.ipmskinf_list end, 
                                Devid_list)),
    IM_list     = lists:usort(lists:map(fun(IMI) -> {IM,_} = IMI, IM end, IMI_list)),
    IP_list     = lists:usort(lists:map(fun({IP,_}) -> IP end, IM_list)),
    IP_Msk_list = lists:usort(lists:map(fun(IM) -> df_util:getWeb_ad(IM) end,IM_list)),
    {IP_list,IP_Msk_list}.


getIP_list_arp(Devid_list, ScanPro, Pid_log) -> 
    df_log:writeLog(Pid_log,"test for getIP_list_arp1"),
    Devid_list_new = df_snmp_read_arp:main(Devid_list, ScanPro, Pid_log),   
    df_log:writeLog(Pid_log,"test for getIP_list_arp"),
    Arp_list       = getArp_list(Devid_list_new,[]),
    df_log:writeLog(Pid_log,"scan by Arp_list is ~p~n",[Arp_list]),
    IP_list        = getIP_list_fromarp(Arp_list,[]),   
    df_log:writeLog(Pid_log,"test for scan by Arp IP_list is ~p~n", [IP_list]),
    {IP_list,Devid_list_new}.

getArp_list([],Arp_list) -> Arp_list;
getArp_list([DeviceInfo|Device_list],Arp_list) ->
    case DeviceInfo#deviceInfo.arp_list of
        []  -> getArp_list(Device_list,Arp_list);
        Arp ->
            IP = (DeviceInfo#deviceInfo.sp)#snmpPara.server,
            getArp_list(Device_list,[{IP,Arp}|Arp_list])
    end.

getIP_list_fromarp([],IP_list) -> IP_list;
getIP_list_fromarp([{_,Arp}|Arp_list],IP_list) ->
    IP_list_cur = getIP_list_fromarp1(Arp,[]),
    getIP_list_fromarp(Arp_list,lists:umerge(IP_list_cur,IP_list)).

getIP_list_fromarp1([],IP_list) -> lists:usort(IP_list);
getIP_list_fromarp1([{_,MI_list}|Arp],IP_list) ->
    IP_list_cur = getIP_list_fromarp2(MI_list,[]),
    getIP_list_fromarp1(Arp,lists:umerge(IP_list_cur,IP_list)).

getIP_list_fromarp2([],IP_list) -> lists:usort(IP_list);
getIP_list_fromarp2([{Mac,IP}|IM_list],IP_list) ->
    {IP1,_,_,_} = IP,
    if
        IP1=:=127;IP1=:=224;IP1=:=0;IP1=:=255;
        Mac=:=[];Mac=:=[0,0,0,0,0,0];
        Mac=:=[255,255,255,255,255,255] ->
            getIP_list_fromarp2(IM_list,IP_list);
        true -> 
            getIP_list_fromarp2(IM_list,[IP|IP_list])
    end.



getDevice012([], Devid_list) -> Devid_list;

getDevice012([Devid|Devid_list_org],Devid_list) ->
    Type = Devid#deviceInfo.devType,
    if 
        Type=:=0;Type=:=1;Type=:=2 ->
            getDevice012(Devid_list_org,[Devid|Devid_list]);
        true ->
            getDevice012(Devid_list_org,Devid_list)
    end.










