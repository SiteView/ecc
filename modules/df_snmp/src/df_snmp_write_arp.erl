-module(df_snmp_write_arp).

-compile(export_all).

-include_lib("df_snmp_record.hrl").
-include_lib("snmp/include/snmp_types.hrl").


main(Device_list) ->
    Arp_list = getArp_list(Device_list,[]),
    case file:open("log/Arp_ORG.txt",write) of
        {ok,F} ->
            writeArp_list(F,Arp_list),
            file:close(F);
        _      ->
            io:format("fail to write Arp_ORG.txt~n")
    end.
    

getArp_list([],Arp_list) -> Arp_list;
getArp_list([DeviceInfo|Device_list],Arp_list) ->
    case DeviceInfo#deviceInfo.arp_list of
        []  -> getArp_list(Device_list,Arp_list);
        Arp ->
            IP = (DeviceInfo#deviceInfo.sp)#snmpPara.server,
            getArp_list(Device_list,[{IP,Arp}|Arp_list])
    end.

writeArp_list(F,Arp_list) ->
    lists:foreach(fun(Arp) -> writeArp(F,Arp) end, Arp_list).


writeArp(F,{IP,Arps}) ->
    SPLIT_TOP  = "::",
    writeIP(F,IP),
    io:format(F,SPLIT_TOP,""),
    writeArps(F,Arps),
    io:format(F,"~n","").
    
writeArps(F,[{Port,IPMacs}|Arps]) ->
    SPLIT_MAIN = ":",
    SPLIT_SUB  = ";",
    case IPMacs of
        [] -> 
            case Arps of
                [] ->
                    true;
                _  ->
                    io:format(F,SPLIT_SUB,""),
                    writeArps(F,Arps)
            end;
        _  ->
            io:format(F,"~p" ++ SPLIT_MAIN, [Port]),
            writeIPMacs(F,IPMacs),
            case Arps of
                [] ->
                    true;
                _  ->
                    io:format(F,SPLIT_SUB,""),
                    writeArps(F,Arps)
            end
    end.

writeIPMacs(_,[]) -> true;
writeIPMacs(F,[{Mac,IP}|IPMacs]) ->
    SPLIT_SUB  = ",",
    writeIP(F,IP),
    io:format(F,"-",""),
    writeMac(F,Mac),
    case IPMacs of
        [] -> true;
        _  -> 
            io:format(F,SPLIT_SUB,""),
            writeIPMacs(F,IPMacs)
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
    writeMac(F,Mac);
writeMac(_,_) -> false.








