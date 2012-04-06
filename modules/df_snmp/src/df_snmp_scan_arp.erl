-module(df_snmp_scan_arp).

-compile(export_all).

-include_lib("df_snmp_record.hrl").
-include_lib("snmp/include/snmp_types.hrl").


test() ->
    Pid_log = df_log:openLog(),
    snmp_ex2_manager:start_link(),
    ScanPro = df_snmp_readdoc_scanPro:main(),
    Device_list = scanBySeeds([{192,168,3,245}],4,ScanPro,Pid_log),
    snmp_ex2_manager:stop().

start2() ->
    Begin = now(),
    snmp_ex2_manager:start_link(),
    ScanPro = df_snmp_readdoc_scanPro:main(),
    Pid_log  = df_log:openLog(),
    Obj     = df_snmp_scan_out:new(ScanPro,Pid_log),
    Result  = Obj:discover_arp([{192,168,3,68},{192,168,0,251},{192,168,0,252},{192,168,0,253},{192,168,0,254}],[],[]),
    df_log:writeLog(Pid_log, "result is ~p~n",[Result]),
    End = now(),
    df_log:writeLog(Pid_log, "over time~p~n",[df_util:timeSub(Begin,End)]),
    snmp_ex2_manager:stop().
    
start() ->
    main([{192,168,3,68},{192,168,0,251},{192,168,0,252},{192,168,0,253},{192,168,0,254}],4). 

main(IP_list,Layers) ->
    Begin = now(),
    Pid_log = df_log:openLog(),
    snmp_ex2_manager:start_link(),
    ScanPro = df_snmp_readdoc_scanPro:main(),
    Device_list = scanBySeeds(IP_list,Layers,ScanPro,Pid_log), 
    %%Devid_list = df_snmp_readDevice:main(SP_list,3000,DTD),
    %%df_log:writeLog(Pid_log, "the devid list is ~p~n",[Devid_list]),
    df_snmp_write_device:writeDevice(Device_list),
    df_snmp_write_inf:main(Device_list),
    df_snmp_write_aft:main(Device_list),
    df_snmp_write_arp:main(Device_list),
    End = now(),
    df_log:writeLog(Pid_log, "over time~p~n",[df_util:timeSub(Begin,End)]),
    snmp_ex2_manager:stop(),
    df_log:closeLog(Pid_log),
    Device_list.
    
    

read_IP_fromDoc(IP_list,Read_ip_list) when Read_ip_list=:="0" ->
    IP_list;

read_IP_fromDoc(IP_list1,_) ->
    {ok,IP_list2} = file:consult("log/IP_list.txt"),
    IP_list = lists:umerge(lists:usort(IP_list1),lists:usort(IP_list2)),
    IP_list.
    
write_IP_toDoc(IP_list,Read_ip_list) when Read_ip_list=:="2" ->
    case file:open("log/IP_list.txt",write) of
        {ok,F} ->
            lists:foreach(fun(IP)->io:format(F,"~w.~n",[IP]) end ,IP_list),
            file:close(F);
        _      -> false
    end;
write_IP_toDoc(_,_) -> true.

scanBySeeds(IP_list_org,Layers,ScanPro,Pid_log) ->
    IP_list = read_IP_fromDoc(IP_list_org,ScanPro#scanPro.read_ip_list),
    df_log:writeLog(Pid_log,"test here for scan_arp1~n"),
    {IP_visited_list,Devid_SP_list,Devid_list} = 
        scanBySeeds(lists:usort(IP_list),Layers,ScanPro,{[],[],[]}, Pid_log),
    df_log:writeLog(Pid_log,"test here for scan_arp2~n"),
    Devid_list_new = df_snmp_read:main(Devid_list,ScanPro,Pid_log),
    write_IP_toDoc(lists:map(fun(SP)->SP#snmpPara.server end,Devid_SP_list),
                    ScanPro#scanPro.read_ip_list),
    df_log:writeLog(Pid_log,"~p visited ip ~n visited ip ~n",[length(IP_visited_list)]),
    df_log:writeLog(Pid_log,"~ndevid sp is~p~n",[Devid_SP_list]),
    df_log:writeLog(Pid_log,"~n~ndiscovered ~w devids :~n~p~n",[length(Devid_list_new), Devid_list_new]),
    Devid_list_new.
    

scanBySeeds([],_,_,R,_) -> R;
scanBySeeds(_,Layers,_,R,_) when is_integer(Layers),Layers<1 -> R;
scanBySeeds(IP_list,Layers,ScanPro,
            {IP_visited_list,Devid_SP_list,Devid_list}, Pid_log) ->
    df_log:writeLog(Pid_log,"scan by seed Layer ~p~n",[Layers]),
    io:format("scan by seed Layer ~p~n",[Layers]),
    
    {SP_list, IP_visited_list_new1} = getSP_list(IP_list, ScanPro, IP_visited_list),
    
    io:format("begin to snmpping~n"),
    df_log:writeLog(Pid_log,"begin to snmpping"),
    Seed_list = snmpping(SP_list,ScanPro,Pid_log),
    df_log:writeLog(Pid_log,"end snmpping"),
    
    io:format("begin to scanSeeds~n"),
    df_log:writeLog(Pid_log,"begin to scanSeeds"),
    {IP_visited_list_new2, Devid_list_cur} 
        = scanSeeds(Seed_list,IP_visited_list_new1,ScanPro),
    %%df_log:writeLog(Pid_log,"end  scanSeeds"),
    df_log:writeLog(Pid_log,"begin to getIP_list_arp"),
    
    {IP_list_new,Devid_list_new} = getIP_list_arp
                                    (Devid_list_cur,ScanPro, Pid_log),
    df_log:writeLog(Pid_log,"end getIP_list_arp"),
    
    scanBySeeds(
                    IP_list_new, Layers-1, ScanPro, 
                    {
                        IP_visited_list_new2,
                        lists:umerge(Seed_list,Devid_SP_list),
                        %%lists:umerge(IPMask_list_cur,IPMask_list),
                        lists:umerge(Devid_list_new,Devid_list)
                    },
                    Pid_log
               ).


getSP_list(IP_list, ScanPro, IP_visited_list) ->
    df_snmp_sp:getSP_list(IP_list, ScanPro, IP_visited_list).


getCommunity(IP, {Community,ComList}) ->
    case getCommunity(IP,ComList) of
        false -> Community;
        R     -> R
    end;
    
getCommunity(_, []) ->
    false;
    
getCommunity(IP,[{IP_list,Com}|ComList]) ->
    case lists:member(IP,IP_list) of
        true  -> Com;
        false -> getCommunity(IP,ComList)
    end.


snmpping(SP_list,ScanPro,Pid_log) -> 
    Timeout  = ScanPro#scanPro.timeout,
    Ping_len = ScanPro#scanPro.ping_len,
    df_snmp_snmpping:main(SP_list,Timeout,Pid_log,Ping_len).


%% get the IPMask_cur,Devid_list_cur,and update IP_visited
%% the return is all usort list
scanSeeds([],IP_visited_list_new1,_) -> {IP_visited_list_new1,[]};
scanSeeds(Seed_list,IP_visited_list_new1, ScanPro) ->
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




read_device_type() ->
    {ok,DeviceList} = file:consult("log/devicetype.dat"),
    get_types(DeviceList,dict:new()).

get_types([],DTD) -> DTD;

get_types([DL|NewDeviceList],DTD) ->
    case DL of
        {SysOids,A,B,C,D} ->
            get_types(NewDeviceList,dict:store(SysOids,{A,B,C,D},DTD));
        _ -> 
            get_types(NewDeviceList,DTD)
    end.


















