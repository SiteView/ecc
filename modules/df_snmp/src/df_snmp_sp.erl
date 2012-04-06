-module(df_snmp_sp).

-compile(export_all).

-include_lib("df_snmp_record.hrl").
-include_lib("snmp/include/snmp_types.hrl").


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













