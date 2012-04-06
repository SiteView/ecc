-module(df_snmp_scan).

-compile(export_all).

-include_lib("df_snmp_record.hrl").
-include_lib("snmp/include/snmp_types.hrl").


start() ->
    %%process_flag(trap_exit,true),
    Begin = now(),
    Pid_log = df_log:openLog(),
    snmp_ex2_manager:start_link(),
    ScanPro = df_snmp_readdoc_scanPro:main(),
    
    %%scanBySeeds([{192,168,2,23},{192,168,0,254},{192,168,0,253},{192,168,0,252},{192,168,0,251}],3,ScanPro,Pid_log),
    %%scanBySeeds([{192,168,3,68},{192,168,4,2},{192,168,0,253}],5,ScanPro,Pid_log),
    %%IP_list = getIP_list([{192,168,3,1}]),
                        %%{{192,168,0,1},{255,255,255,0}},{{192,168,1,1},{255,255,255,0}},
                        %%{{192,168,0,1},{255,255,255,0}},{{192,168,159,1},{255,255,255,0}}]),
                        %%{{192,168,4,1},{255,255,255,0}},{{192,168,5,1},{255,255,255,0}}]),
    %%IP_list_new = read_IP_fromDoc(IP_list),

    Device_list = scanBySeeds([{192,168,3,68},{192,168,0,251},{192,168,0,252},{192,168,0,253},{192,168,0,254}],4,ScanPro,Pid_log), 
    %%Devid_list = df_snmp_readDevice:main(SP_list,3000,DTD),
    %%df_log:writeLog(Pid_log, "the devid list is ~p~n",[Devid_list]),
    df_snmp_write_device:writeDevice(Device_list),
    df_snmp_write_inf:main(Device_list),
    df_snmp_write_aft:main(Device_list),
    df_snmp_write_arp:main(Device_list),
    End = now(),
    df_log:writeLog(Pid_log, "over time~p~n",[timeSub(Begin,End)]),
    df_log:closeLog(Pid_log),
    snmp_ex2_manager:stop().
    
    
    
timeSub({A1,A2,A3},{B1,B2,B3}) ->
    {B1-A1,B2-A2,B3-A3}.
    

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
    {IP_visited_list,Devid_SP_list,IPMask_list,Devid_list} = 
        scanBySeeds(lists:usort(IP_list),Layers,ScanPro,{[],[],[],[]}, Pid_log),
    Devid_list_new = df_snmp_read:main(Devid_list,ScanPro,Pid_log),
    write_IP_toDoc(lists:map(fun(SP)->SP#snmpPara.server end,Devid_SP_list),
                    ScanPro#scanPro.read_ip_list),
    df_log:writeLog(Pid_log,"~p visited ip ~n visited ip ~n",[length(IP_visited_list)]),
    df_log:writeLog(Pid_log,"~ndevid sp is~p~n",[Devid_SP_list]),
    df_log:writeLog(Pid_log,"~ndiscovered subnet is~p~n",[IPMask_list]),
    df_log:writeLog(Pid_log,"~n~ndiscovered ~w devids :~n~p~n",[length(Devid_list_new), Devid_list_new]),
    Devid_list_new.
    

scanBySeeds([],_,_,R,_) -> R;
scanBySeeds(_,Layers,_,R,_) when is_integer(Layers),Layers<1 -> R;
scanBySeeds(IP_list,Layers,ScanPro,
            {IP_visited_list,Devid_SP_list,IPMask_list,Devid_list}, Pid_log) ->
    df_log:writeLog(Pid_log,"scan by seed Layer ~p~n",[Layers]),
    io:format("scan by seed Layer ~p~n",[Layers]),
    
    {SP_list, IP_visited_list_new1} = getSP_list(IP_list, ScanPro, IP_visited_list),
    
    io:format("begin to snmpping~n"),
    Seed_list = snmpping(SP_list,ScanPro,Pid_log),
    
    io:format("begin to scanSeeds~n"),
    {IP_visited_list_new2, IPMask_list_cur, Devid_list_cur} 
        = scanSeeds(Seed_list,{IP_visited_list_new1,IPMask_list},ScanPro),
    
    
    IP_list_new = getIP_list(IPMask_list_cur),
    
    scanBySeeds(
                    IP_list_new, Layers-1, ScanPro, 
                    {
                        IP_visited_list_new2,
                        lists:umerge(Seed_list,Devid_SP_list),
                        lists:umerge(IPMask_list_cur,IPMask_list),
                        lists:umerge(Devid_list_cur,Devid_list)
                    },
                    Pid_log
               ).


getSP_list(IP_list, ScanPro, IP_visited_list) -> 
    {IP_list_new,IP_visited_list_new} = checkIP_list(IP_list, IP_visited_list),
    Pid = self(),
    Pid_SP = spawn_link(fun() -> createSP(ScanPro,Pid) end),
    lists:foreach(fun(IP) -> Pid_SP!{ip,IP} end,IP_list_new),
    {receiveSP_list([]), IP_visited_list_new}.
    
receiveSP_list(SP_list) ->
    receive
        {sp,SP} -> receiveSP_list([SP|SP_list]);
        createSP_over    -> lists:usort(SP_list);
        _       -> receiveSP_list(SP_list)
    after 100 -> lists:usort(SP_list)
    end.
    
checkIP_list(IP_list, IP_visited_list) ->
    {
        lists:subtract(IP_list, IP_visited_list),
        lists:umerge(IP_list,IP_visited_list)
    }.
    
createSP(ScanPro,Pid) ->
    {Community,ComList} = ScanPro#scanPro.com_list,
    Timeout = ScanPro#scanPro.timeout,
    Retry   = ScanPro#scanPro.retry,
    Port    = ScanPro#scanPro.port,
    receive
        {ip,IP} -> 
            Com = getCommunity(IP, {Community,ComList}),
            SP  = #snmpPara{
                                server    = IP,
                                community = Com,
                                port      = Port,
                                timeout   = Timeout,
                                retry     = Retry
                            },
            Pid!{sp,SP},
            createSP(ScanPro,Pid);
        _              -> createSP(ScanPro,Pid)
        after 100 -> Pid!createSP_over
    end.

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
scanSeeds(Seed_list,{IP_visited_list_new1,IPMask_list}, ScanPro) ->
    Timeout = ScanPro#scanPro.timeout,
    DTD     = ScanPro#scanPro.dtd,
    Devid_list_cur = df_snmp_read_device:main(Seed_list,Timeout,DTD),
    {IP_list_cur, IPMask_list_new} = getIPMsk_list(Devid_list_cur),
    IP_visited_list_new2 = lists:umerge(IP_list_cur,IP_visited_list_new1),
    IPMsk_list_cur       = checkIPMask_list(IPMask_list_new,IPMask_list),
    {IP_visited_list_new2, IPMsk_list_cur, lists:usort(Devid_list_cur)}.
    
    
getIPMsk_list(Devid_list_org) ->
    Devid_list = getDevice012(Devid_list_org,[]),
    IMI_list    = lists:umerge(lists:map(fun(Devid) -> Devid#deviceInfo.ipmskinf_list end, 
                                Devid_list)),
    IM_list     = lists:usort(lists:map(fun(IMI) -> {IM,_} = IMI, IM end, IMI_list)),
    IP_list     = lists:usort(lists:map(fun({IP,_}) -> IP end, IM_list)),
    IP_Msk_list = lists:usort(lists:map(fun(IM) -> df_util:getWeb_ad(IM) end,IM_list)),
    {IP_list,IP_Msk_list}.

getDevice012([], Devid_list) -> Devid_list;

getDevice012([Devid|Devid_list_org],Devid_list) ->
    Type = Devid#deviceInfo.devType,
    if 
        Type=:=0;Type=:=1;Type=:=2 ->
            getDevice012(Devid_list_org,[Devid|Devid_list]);
        true ->
            getDevice012(Devid_list_org,Devid_list)
    end.

checkIPMask_list(IPMask_list_cur,IPMask_list) ->
    lists:subtract(lists:umerge(IPMask_list_cur,IPMask_list),IPMask_list).
    
%%getIP_list(IPMask_list) ->
    %%lists:append(lists:map(fun(IPMask) -> df_util:getIP_list(IPMask) end, IPMask_list)).

getIP_list(IPMask_list) ->
    df_util:getIP_lists(IPMask_list).



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












