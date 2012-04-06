-module(df_snmp_write_aft).

-compile(export_all).

-include_lib("df_snmp_record.hrl").
-include_lib("snmp/include/snmp_types.hrl").


main(Device_list) ->
    Aft_list = getAft_list(Device_list,[]),
    case file:open("log/Aft_ORG.txt",write) of
        {ok,F} ->
            writeAft_list(F,Aft_list),
            file:close(F);
        _      ->
            io:format("fail to write Aft_ORG.txt~n")
    end.
    

getAft_list([],Aft_list) -> Aft_list;
getAft_list([DeviceInfo|Device_list],Aft_list) ->
    case DeviceInfo#deviceInfo.aft_list of
        []  -> getAft_list(Device_list,Aft_list);
        Aft ->
            IP = (DeviceInfo#deviceInfo.sp)#snmpPara.server,
            getAft_list(Device_list,[{IP,Aft}|Aft_list])
    end.

writeAft_list(F,Aft_list) ->
    lists:foreach(fun(Aft) -> writeAft(F,Aft) end, Aft_list).


writeAft(F,{IP,Afts}) ->
    SPLIT_TOP  = "::",
    writeIP(F,IP),
    io:format(F,SPLIT_TOP,""),
    writeAfts(F,Afts),
    io:format(F,"~n","").
    
writeAfts(F,[{Port,Macs}|Afts]) ->
    SPLIT_MAIN = ":",
    SPLIT_SUB  = ";",
    case Macs of
        [] -> 
            case Afts of
                [] ->
                    true;
                _  ->
                    io:format(F,SPLIT_SUB,""),
                    writeAfts(F,Afts)
            end;
        _  ->
            io:format(F,"~p" ++ SPLIT_MAIN, [Port]),
            writeMacs(F,Macs),
            case Afts of
                [] ->
                    true;
                _  ->
                    io:format(F,SPLIT_SUB,""),
                    writeAfts(F,Afts)
            end
    end.

writeMacs(_,[]) -> true;
writeMacs(F,[Mac|Macs]) ->
    SPLIT_SUB  = ",",
    writeMac(F,Mac),
    case Macs of
        [] -> true;
        _  -> 
            io:format(F,SPLIT_SUB,""),
            writeMacs(F,Macs)
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








