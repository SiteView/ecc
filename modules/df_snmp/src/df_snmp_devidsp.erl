-module(df_snmp_devidsp).

-compile(export_all).

-include_lib("df_snmp_record.hrl").

getDevidSP012([],SP_list) -> SP_list;
getDevidSP012([Devid|Devid_list],SP_list) ->
    Type = Devid#deviceInfo.devType,
    Flag = Devid#deviceInfo.snmpFlag,
    if
        Flag=:=1 ->
            if
                Type=:=0;Type=:=1;Type=:=2 ->
                    getDevidSP012(Devid_list,[Devid#deviceInfo.sp|SP_list]);
                true ->
                    getDevidSP012(Devid_list,SP_list)
            end;
        true -> 
            getDevidSP012(Devid_list,SP_list)
    end.   

getDevidSP01([],SP_list) -> SP_list;
getDevidSP01([Devid|Devid_list],SP_list) ->
    Type = Devid#deviceInfo.devType,
    Flag = Devid#deviceInfo.snmpFlag,
    if
        Flag=:=1 ->
            if
                Type=:=0;Type=:=1 ->
                    getDevidSP01(Devid_list,[Devid#deviceInfo.sp|SP_list]);
                true ->
                    getDevidSP01(Devid_list,SP_list)
            end;
        true -> 
            getDevidSP01(Devid_list,SP_list)
    end.   

getDevidSP02([],SP_list) -> SP_list;
getDevidSP02([Devid|Devid_list],SP_list) ->
    Type = Devid#deviceInfo.devType,
    Flag = Devid#deviceInfo.snmpFlag,
    if
        Flag=:=1 ->
            if
                Type=:=0;Type=:=2 ->
                    getDevidSP02(Devid_list,[Devid#deviceInfo.sp|SP_list]);
                true ->
                    getDevidSP02(Devid_list,SP_list)
            end;
        true -> 
            getDevidSP02(Devid_list,SP_list)
    end. 

getDevidSP([],SP_list) -> SP_list;
getDevidSP([Devid|Devid_list],SP_list) ->
    Flag = Devid#deviceInfo.snmpFlag,
    if
        Flag=:=1 ->
            getDevidSP(Devid_list,[Devid#deviceInfo.sp|SP_list]);
        true -> 
            getDevidSP(Devid_list,SP_list)
    end. 










