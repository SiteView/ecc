-module(test).

-compile(export_all).


test(Len) ->
    for_up(1,Len,fun(I) -> crePro(I) end).
    
crePro(I) ->
    spawn(fun() -> process(I) end).

process(I) ->
    io:format("process ~p~n",[I]).

for_up(Max, Max, F) -> [F(Max)];
for_up(I, Max, F)   -> [F(I)|for_up(I+1, Max, F)].



start2() ->
    snmp_ex2_manager:astop(),
    snmp_ex2_manager:start_link(),
    ScanPro =
    {
        scanPro,
        {
            "public",
            [{[{192,168,0,254},{192,168,0,253},{192,168,0,252},{192,168,0,251}],"public1"}]
        },
        3000,2,161
    } ,
    Pid_log = df_log:openLog(),
    io:format("test begin~n"),
    %%IP_list1     = df_snmp_scan:getIP_list([{{192,168,0,1},{255,255,255,0}},{{192,168,1,1},{255,255,255,0}}]),
    %%{SP_list1,_} = df_snmp_scan:getSP_list(IP_list1, ScanPro, []),
    %%IP_list2     = df_snmp_scan:getIP_list([{{192,168,3,1},{255,255,255,128}},{{192,168,3,133},{255,255,255,240}},{{192,168,3,181},{255,255,255,224}},{{192,168,3,208},{255,255,255,192}}]),
    IP_list2     = df_snmp_scan:getIP_list([{{192,168,4,1},{255,255,255,0}}]),
    {SP_list2,_} = df_snmp_scan:getSP_list(IP_list2, ScanPro, []),
    df_log:writeLog(Pid_log, "SP_list2 = ~p~n",[SP_list2]),
    IP_list3     = df_snmp_scan:getIP_list([{{192,168,4,1},{255,255,255,0}},{{192,168,5,1},{255,255,255,0}}]),
    {SP_list3,_} = df_snmp_scan:getSP_list(IP_list3, ScanPro, []),
    df_snmp_scan:snmpping(SP_list2,ScanPro,Pid_log),
    receive
    after 4000 -> []
    end,
    %%df_snmp_scan:snmpping(SP_list2,ScanPro,Pid_log),
    df_log:closeLog(Pid_log).


start() ->
    %%read_device_type().
    pro(),
    pro().
    
    
pro() ->
    spawn(fun() ->  io:format("11111~p~n",[snmp_ex2_manager:start_link()]) end).



read_device_type() ->
    {ok,IP_list} = file:consult("log/IP_list.txt").
    
    
    
    
test2(C) ->
    if 
        C =:= 32 ; C=:=33 -> true;
        C =:= 44           -> false
    end.

test3() ->
    IM_list= [{[1,3,6,1,2,1,4,20,1,3,192,168,5,1],[255,255,255,0]},
   {[1,3,6,1,2,1,4,20,1,3,192,168,4,1],[255,255,255,0]},
   {[1,3,6,1,2,1,4,20,1,3,192,168,3,1],[255,255,255,0]},
   {[1,3,6,1,2,1,4,20,1,3,192,168,2,1],[255,255,255,0]},
   {[1,3,6,1,2,1,4,20,1,3,192,168,1,1],[255,255,255,0]},
   {[1,3,6,1,2,1,4,20,1,3,192,168,0,254],[255,255,255,0]}],
   lists:map(fun({I,M}) -> 
             {lists:subtract(I,[1,3,6,1,2,1,4,20,1,3]),M} end,
             IM_list).


test4() ->
    A = [1,2,3,4,0,5,6,0,3,3,3,333],
    lists:splitwith(fun(I) -> I>0 end , A).

test5() ->
    Oid = [[1,3,6,1,2,1,1]],
    snmp_ex2_manager:start_link(),
    snmp_ex2_manager:agent({192,168,0,254},[{community,"public1"}]),
    io:format("~n~n~n~n~n okkkkkkkkkkkkkkkk~n"),
    Return = snmp_ex2_manager:sync_get_next({192,168,0,254},161,Oid,2000),
    io:format("Return ~p~n",[Return]),
    snmp_ex2_manager:stop().
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

