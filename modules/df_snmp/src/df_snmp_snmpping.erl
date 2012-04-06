-module(df_snmp_snmpping).
 
-compile(export_all).


-include_lib("df_snmp_record.hrl").
-include_lib("snmp/include/snmp_types.hrl").





start() ->
    Server_list = df_util:for_up(1,100,fun(I) -> ready(I)end),
    Pid_log = df_log:openLog(),
    snmp_ex2_manager:start_link(),
    SP_list = main(Server_list,2000,Pid_log,512),
    io:format("SP_list is ~p~n",[SP_list]),
    df_log:closeLog(Pid_log),
    snmp_ex2_manager:stop().



ready(I) ->
    Server = {192,168,3,I rem 255},
    #snmpPara{
                    server    = Server,
                    port      = 161,
                    community = "public",
                    timeout   = 2000,
                    retry     = 2
              }.



main(SP_list, Timeout, Pid_log, Ping_Len) ->
    getValidSP_list(SP_list, Timeout, Pid_log, Ping_Len).
    
    
getValidSP_list(SP_list, Timeout, Pid_log, Len) ->
    io:format("~n~none time~n"),
    df_log:writeLog(Pid_log,"the list length is ~p ~p~n",[length(SP_list),SP_list]),
    case length(SP_list)>Len of
        true  -> 
            SP_list_sub = lists:sublist(SP_list, Len),
            SP_list_new = lists:subtract(SP_list,SP_list_sub),
            lists:umerge
            (
                getValidSP_list(SP_list_sub, Timeout,Pid_log),
                getValidSP_list(SP_list_new, Timeout,Pid_log, Len)
            );
        false ->
            getValidSP_list(SP_list,Timeout,Pid_log)
    end.
    
getValidSP_list([],_,_) ->[];

getValidSP_list(SP_list,Timeout,Pid_log) ->
    snmpping(SP_list,self()),
    %Ip_list = getIps([],Timeout,length(SP_list),Pid_log),
    %ValSP_list = getValidSP(SP_list,Ip_list),
    ValSP_list = getSP_list([],Timeout,{length(SP_list),0},Pid_log),
    df_log:writeLog(Pid_log,"test here valid SP"),
    lists:usort(ValSP_list).

getSP_list(SP_list,_,{All,Len},_) when All<Len+1 ->
    SP_list;
    
getSP_list(SP_list,Timeout,{All,Len},Pid_log) ->
    receive
        {ok,SP} -> 
            
            getSP_list(lists:umerge([SP],SP_list),Timeout,{All,Len+1},Pid_log);
        R       ->
            io:format("test here ~p~n",[R]),
            getSP_list(SP_list,Timeout,{All,Len+1},Pid_log)
    after Timeout -> SP_list
    end.

%getValidSP(_,[]) -> [];

%getValidSP(SP_list,[Ip|Ip_list]) ->
    %case getValidSP(SP_list,Ip) of
     %   false -> getValidSP(SP_list,Ip_list);
    %    R     -> [R|getValidSP(SP_list,Ip_list)]
    %end;

%getValidSP([],_) -> false;

%getValidSP([SP|SP_list],Ip) ->
    %case SP of
    %    {_,Ip,_,_,_,_} -> SP;
     %   _               -> getValidSP(SP_list,Ip)
    %end.

snmpping(SP_list,Pid_main) ->
    lists:foreach(fun(SP) -> createPro({SP,Pid_main}) end, SP_list).

createPro({SP,Pid_main}) ->
    spawn_link(fun() -> process({SP,Pid_main}) end).
    
process({SP,Pid_main}) ->
    io:format("Sp    ~p~n",[SP]),
    %%df_log:writeLog(Pid_log,"snmpping ~p~n",[Ip]),
    snmp_ex2_manager:agent(SP#snmpPara.server,[{community,SP#snmpPara.community}]),
    case snmp_ex2_manager:sync_get_next(SP#snmpPara.server,SP#snmpPara.port,[[1,3,6,1,2,1,1]],SP#snmpPara.timeout) of
        {ok,_,_} -> 
            io:format("the ~p is valid~n",[SP]),
            Pid_main!{ok,SP};
        R      -> 
            io:format("~p test here here ~p~n",[SP,R]),
            Pid_main!timeout
    end.


getIps(IP_list,_,0,Pid_log) ->
    IP_list_u = lists:usort(IP_list),
    df_log:writeLog(Pid_log,"receive all snmp device~n~n~n~n~n~n~n~n~n~n~n~n"),
    IP_list_u;
    
getIps(IP_list, Timeout,Len,Pid_log) ->
    receive
        {ok,{Ips,_}} -> 
            df_log:writeLog(Pid_log,"~n**********get Ips = ~p~n",[Ips]),
            getIps([Ips|IP_list], Timeout,Len-1,Pid_log);
        _R         -> 
            getIps(IP_list, Timeout, Len,Pid_log)
    after Timeout -> 
        IP_list_u = lists:usort(IP_list),
        df_log:writeLog(Pid_log,"the snmpping receive ip timeout~n~n~n~n~n~n~n~n~n~n~n~n"),
        IP_list_u
    end.


















