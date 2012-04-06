-module(df_snmp_write_inf).

-compile(export_all).

-include_lib("df_snmp_record.hrl").
-include_lib("snmp/include/snmp_types.hrl").


main(Device_list) ->
    Inf_list = getInf_list(Device_list,[]),
    case file:open("log/InfProps.txt",write) of
        {ok,F} ->
            writeInfs(F,Inf_list),
            file:close(F);
        _      ->
            io:format("fail to write InfProps.txt~n")
    end.

getInf_list([],Inf_list) -> Inf_list;
getInf_list([DeviceInfo|Device_list],Inf_list) ->
    case DeviceInfo#deviceInfo.inf_list of
        []  -> getInf_list(Device_list,Inf_list);
        Inf -> getInf_list(Device_list,[Inf|Inf_list])
    end.

writeInfs(F,Inf_list) ->
    lists:foreach(fun(Inf) -> writeInf(F,Inf) end,Inf_list).
    
writeInf(F,Inf) ->
    SPLIT_TOP  = "[:::]",
    writeIP(F,Inf#inf.ifip),
    io:format(F,SPLIT_TOP ++ "~p" ++ SPLIT_TOP, [Inf#inf.ifnum]),
    writeInfInx_list(F,Inf#inf.infinx_list),
    io:format(F,"~n","").

writeInfInx_list(_,[]) -> true;
writeInfInx_list(F,[InfInx|InfInx_list]) ->
    SPLIT_MAIN = "[::]",
    SPLIT_SUB  = "[:]",
    io:format(F,"~p" ++ SPLIT_SUB,[InfInx#infInx.ifindex]),
    io:format(F,"~p" ++ SPLIT_SUB,[InfInx#infInx.ifType]),
    writeMac(F,InfInx#infInx.ifmac),
    io:format(F,SPLIT_SUB,""),
    io:format(F,"~p" ++ SPLIT_SUB,[InfInx#infInx.ifport]),
    io:format(F,InfInx#infInx.ifdesc ++ SPLIT_SUB,""),
    io:format(F,"~p",[InfInx#infInx.ifspeed]),
    case InfInx_list of
        [] -> true;
        _  -> 
            io:format(F,SPLIT_MAIN,""),
            writeInfInx_list(F,InfInx_list)
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







