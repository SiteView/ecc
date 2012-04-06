-module(test_agn).

-compile(export_all).

-include_lib("df_snmp_record.hrl").
-include_lib("snmp/include/snmp_types.hrl").

start() ->
    df_snmp_base:start_link(),
    Begin = now(),
    %%S = main(ready(1),[1,3,6,1,2,1,4,22,1,1]),
    Oid = [1,3,6,1,2,1,17,4,3,1,2],
    %%Oid = [1,0],
    %SP_list = [ready(251),ready(252),ready(253),ready(254)],
    SP_list = [ready(252)],
    S = main(SP_list,Oid,2000),
    io:format("the answer is~p~n",[lists:usort(S)]),
    End   = now(),
    timeSub(Begin,End).
    %%df_snmp_base:stop().

timeSub({A1,A2,A3},{B1,B2,B3}) ->
    {B1-A1,B2-A2,B3-A3}.
    
ready(I) ->
    Server = {192,168,0,I},
    #snmpPara{
                    server    = Server,
                    port      = 161,
                    community = "public1",
                    timeout   = 2000,
                    retry     = 2
              }.

    
    

main(SP_list,Oids,Timeout) ->
    Pid_main = self(),
    Pid_dtd = getPid_dtd(Pid_main,SP_list,Oids,dict:new()),
    recV(Pid_dtd,[],Timeout).

getPid_dtd(_,[],_,Dtd) ->Dtd;
getPid_dtd(Pid_main,[SP|SP_list],Oids,Dtd) ->
    {snmpPara,Ip,_,_,_,_} = SP,
    Pid = spawn(fun() -> getAgn(Pid_main,SP,Oids) end),
    getPid_dtd(Pid_main,SP_list,Oids,dict:store(Ip,Pid,Dtd)).
    
    
    

recV(Pid_dtd,IV_list,Timeout) ->
    receive
        {ok,{Ip,VBs}} -> 
            {ok,Pid_ip} = dict:find(Ip,Pid_dtd),
            io:format("1~p  ~p  ~p~n",[Ip,VBs,Pid_ip]),
            Pid_ip!VBs,
            recV(Pid_dtd,IV_list,Timeout);
        {agn,{SP,V_list}} -> 
            recV(Pid_dtd,[{SP,V_list}|IV_list],Timeout)
        after Timeout -> 
            io:format("3~n"),
            IV_list
    end.
    

    
getAgn(Pid_main,SP,Oids) ->
    V_list = agn_list(SP,[],Oids,Oids),
    Pid_main!{agn,V_list}.
    
agn_list(SP,V_list,Prefix,Oids) ->
    agn(SP,[Oids]),
    receive
        [{_,Oid2,_,Value,_}|_] ->
            case lists:prefix(Prefix,Oid2) of
                true ->
                    %%io:format("agn_list get one next oid ~p~n~n~n",[Oid2]),
                    agn_list(SP,[{Oid2,Value}|V_list],Prefix,Oid2);
                _    -> 
                    %%io:format("it's ok~n"),
                {SP,V_list}
            end;
        R -> io:format("wrong receive ~p~n",[R])           
    after 2000 -> 
        io:format("agn_list timeout~n~n~n"),
        {SP,V_list}
    end.


agn(SP,Oids) ->
    {snmpPara,Ip,_,Community,Timeout,_Retry} = SP,
    case df_snmp_base:agent(Ip,[{community,Community}]) of
        {error,_} -> updateCom(SP);
        _         -> true
    end,
    df_snmp_base:async_get_next(Ip, Oids, Timeout).

updateCom(SP) ->
    df_snmp_base:update_agent(SP#snmpPara.server,community,SP#snmpPara.community).










