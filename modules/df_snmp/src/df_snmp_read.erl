-module(df_snmp_read).

-compile(export_all).


-include_lib("df_snmp_record.hrl").
-include_lib("snmp/include/snmp_types.hrl").


    

main(Devid_list, ScanPro, Pid_log) ->
    %%Infinx_list = readInfinx_list(getDevidSP(Devid_list,[]),Timeout, Pid_log),
    %%Arp_list    = readArp_list(getDevidSP02(Devid_list,[]),Timeout, Pid_log),
    %%Aft_list    = df_snmp_read_aft:main(getDevid01(Devid_list,[]),Timeout, Pid_log)
    %%Devid_list1 = getDevid_list_inf(Devid_list,Infinx_list,[]),
    Devid_list1 = df_snmp_read_logic:main(Devid_list,ScanPro#scanPro.timeout, Pid_log),
    Devid_list2 = df_snmp_read_inf:main(Devid_list1,ScanPro#scanPro.timeout, Pid_log),
    Devid_list3 = df_snmp_read_aft:main(Devid_list2,ScanPro#scanPro.timeout, Pid_log),
    Devid_list4 = df_snmp_read_arp:main(Devid_list3,ScanPro, Pid_log),
    case ScanPro#scanPro.read_router of
        "0" -> Devid_list5 = Devid_list4;
        _   -> 
            Devid_list5 = df_snmp_read_router:main
                            (Devid_list3, ScanPro, Pid_log)
    end,
    Devid_list5.
    

main1(Devid_list, ScanPro, Pid_log) ->
    Devid_list1 = df_snmp_read_logic:main(Devid_list,ScanPro#scanPro.timeout, Pid_log),
    Devid_list2 = df_snmp_read_inf:main(Devid_list1,#scanPro.timeout, Pid_log),
    df_snmp_read_aft:main(Devid_list2,#scanPro.timeout, Pid_log).















