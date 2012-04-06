-module(df_snmp_write_device).

-compile(export_all).

-include_lib("df_snmp_record.hrl").
-include_lib("snmp/include/snmp_types.hrl").



writeDevice(Device_list) ->
    {ok,F} = file:open("log/DeviceInfos.txt",write),
    lists:foreach(fun(DeviceInfo) -> writeOneDevice(F,DeviceInfo) end,Device_list),
    file:close(F).

writeOneDevice(F,DeviceInfo) ->
    SPLIT_MAIN = "[::]",
    writeIP(F,(DeviceInfo#deviceInfo.sp)#snmpPara.server),
    io:format(F,SPLIT_MAIN ++ "~p",[DeviceInfo#deviceInfo.snmpFlag]),
    io:format(F,SPLIT_MAIN ++ DeviceInfo#deviceInfo.community_get,""),
    io:format(F,SPLIT_MAIN ++ DeviceInfo#deviceInfo.community_set,""),
    io:format(F,SPLIT_MAIN,""),
    writeList(F,DeviceInfo#deviceInfo.sysOid,"."),
    io:format(F,SPLIT_MAIN ++ "~p",[DeviceInfo#deviceInfo.devType]),
    io:format(F,SPLIT_MAIN ++ DeviceInfo#deviceInfo.devFactory,""),
    io:format(F,SPLIT_MAIN ++ "~s",[DeviceInfo#deviceInfo.devModel]),
    io:format(F,SPLIT_MAIN ++ DeviceInfo#deviceInfo.devTypeName,""),
    io:format(F,SPLIT_MAIN,""),
    writeMac(F,DeviceInfo#deviceInfo.baseMac),
    io:format(F,SPLIT_MAIN ++ DeviceInfo#deviceInfo.sysName,""),
    io:format(F,SPLIT_MAIN ++ "~p",[DeviceInfo#deviceInfo.sysSvcs]),
    io:format(F,SPLIT_MAIN,""),
    writeIMI(F,DeviceInfo#deviceInfo.ipmskinf_list),
    io:format(F,SPLIT_MAIN,""),
    writeMac_list(F,DeviceInfo#deviceInfo.mac_list),
    io:format(F,"~n","").


writeIMI(_,[]) -> true;
writeIMI(F,[IMI|IMI_list]) ->
    SPLIT_SUB  = "[:]",
    case IMI of
        {{IP,Mask},Inf} ->
            writeIP(F,IP),
            io:format(F,"/",""),
            writeIP(F,Mask),
            io:format(F,"/~p",[Inf]);
         _             ->   
            io:format("test imi wrong~p~n",[IMI]),
            wrong
    end,
    if
        IMI_list=:=[] -> true;
        true -> 
            io:format(F,SPLIT_SUB,""),
            writeIMI(F,IMI_list)
    end.

writeIP(F,IP) ->
    case IP of 
        {IP1,IP2,IP3,IP4} ->
            io:format(F,"~p.~p.~p.~p",[IP1,IP2,IP3,IP4]);
        _ ->
            io:format("test ip wrong~p~n",[IP]),
            wrong
    end.

writeMac(_,[]) -> true;
writeMac(F,[M|Mac]) ->
    B = df_util:c10_16(M rem 16),
    A = df_util:c10_16(M div 16 rem 16),
    io:format(F,A++B,""),
    writeMac(F,Mac).

writeList(_,[],_) -> true;
writeList(F,[L|List],Sign) ->
    io:format(F,"~p",[L]),
    case List of
        [] -> true;
        _  -> 
            io:format(F,Sign,""),
            writeList(F,List,Sign)
    end.

writeMac_list(_,[]) -> true;
writeMac_list(F,[Mac|Mac_list]) ->
    SPLIT_SUB  = "[:]",
    writeMac(F,Mac),
    case Mac_list of
        [] -> true;
        _  -> 
            io:format(F,SPLIT_SUB,""),
            writeMac_list(F,Mac_list)
    end.






