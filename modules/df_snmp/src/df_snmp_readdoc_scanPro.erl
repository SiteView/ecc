-module(df_snmp_readdoc_scanPro).

-compile(export_all).


-include_lib("df_snmp_record.hrl").


test() ->
    ScanPro = main(),
    io:format("com_list is ~p~n",[ScanPro#scanPro.com_list]),
    io:format("timeout is ~p~n",[ScanPro#scanPro.timeout]),
    io:format("retry is ~p~n",[ScanPro#scanPro.retry]),
    io:format("port is ~p~n",[ScanPro#scanPro.port]),
    io:format("ping_len is ~p~n",[ScanPro#scanPro.ping_len]),
    io:format("read_ip_list is ~p~n",[ScanPro#scanPro.read_ip_list]),
    io:format("readimfromother is ~p~n",[ScanPro#scanPro.readimfromother]),
    io:format("read_arp is ~p~n",[ScanPro#scanPro.read_arp]).

main() ->
    ScanPro = #scanPro{dtd=read_device_type()},
    case file:consult("conf/scanPro.conf") of
        {ok,S_list} ->
            io:format("test for S_list ~p~n",[S_list]),
            getScanPro(S_list,ScanPro);
        Err ->
            io:format("read scanPro.conf is wrong ~p~n",[Err]),
            ScanPro
    end.

getScanPro([],ScanPro) -> ScanPro;
getScanPro([S|S_list],ScanPro) ->
    case S of
        {comList,SS}         -> ScanPro_new = ScanPro#scanPro{com_list=SS};
        {timeout,SS}         -> ScanPro_new = ScanPro#scanPro{timeout=SS};
        {retry,SS}           -> ScanPro_new = ScanPro#scanPro{retry=SS};
        {port,SS}            -> ScanPro_new = ScanPro#scanPro{port=SS};
        {ping_len,SS}        -> ScanPro_new = ScanPro#scanPro{ping_len=SS};
        {seed_type,SS}       -> ScanPro_new = ScanPro#scanPro{seed_type=SS};
        {read_ip_list,SS}    -> ScanPro_new = ScanPro#scanPro{read_ip_list=SS};
        {readimfromother,SS} -> ScanPro_new = ScanPro#scanPro{readimfromother=SS};
        {read_arp,SS}        -> ScanPro_new = ScanPro#scanPro{read_arp=SS};
        {read_router,SS}     -> ScanPro_new = ScanPro#scanPro{read_router=SS};
        {read_ospf,SS}       -> ScanPro_new = ScanPro#scanPro{read_ospf=SS};
        {read_vrrp,SS}       -> ScanPro_new = ScanPro#scanPro{read_vrrp=SS};
        {read_bgp,SS}        -> ScanPro_new = ScanPro#scanPro{read_bgp=SS};
        _                    -> ScanPro_new = ScanPro
    end,
    getScanPro(S_list,ScanPro_new).

read_device_type() ->
    {ok,DeviceList} = file:consult("conf/devicetype.dat"),
    get_types(DeviceList,dict:new()).

get_types([],DTD) -> DTD;

get_types([DL|NewDeviceList],DTD) ->
    case DL of
        {SysOids,A,B,C,D} ->
            get_types(NewDeviceList,dict:store(SysOids,{A,B,C,D},DTD));
        _ -> 
            get_types(NewDeviceList,DTD)
    end.












