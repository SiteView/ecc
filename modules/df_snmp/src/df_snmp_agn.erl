-module(df_snmp_agn).

-compile(export_all).

-include_lib("df_snmp_record.hrl").
-include_lib("snmp/include/snmp_types.hrl").


start() ->
    snmp_ex2_manager:start_link(),
    Begin = now(),
    %%S = main(ready(1),[1,3,6,1,2,1,4,22,1,1]),
    Oid = [1,3,6,1,2,1,17,4,3,1,2],
    %%Oid = [1,3,6,1,2,1,1],
    SP_list = [ready(252)],
    S = main(SP_list,Oid,5000),
    io:format("the answer is~p~n",[S]),
    End   = now(),
    timeSub(Begin,End).
    %%snmp_ex2_manager:stop().

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
ready1(I) ->
    #snmpPara{
                    server    = {192,168,0,251},
                    port      = 161,
                    community = "public1@"++I,
                    timeout   = 2000,
                    retry     = 2
              }.




main(SP_list,Oids,Timeout) ->
    create_agn(self(),SP_list,Oids),
    recV([],Timeout,{length(SP_list),0}).
    
create_agn(_,[],_) -> true;
create_agn(Pid_main,[SP|SP_list],Oids) ->
    spawn_link(fun() -> getAgn(Pid_main,SP,Oids) end),
    create_agn(Pid_main,SP_list,Oids).
    

    

    
getAgn(Pid_main,SP,Oids) ->
    snmp_ex2_manager:agent(SP#snmpPara.server,[{community,SP#snmpPara.community}]),
    V_list = agn_list(SP,[],Pid_main,Oids,Oids),
    Pid_main!V_list.

%%getAgn(Pid_main,SP,Oids) ->
  %%  Obj = df_snmp_session_factory:session(v2,{SP#snmpPara.server,SP#snmpPara.community,SP#snmpPara.timeout}),
  %%  Pid_main!{SP,Obj:getnext_table(Oids)}.
    
agn_list(SP,V_list,Pid_main,Prefix,Oids) ->
    io:format("test for agn send~n"),
    case agn(SP,[Oids]) of
        {ok,{_,_,[{_,Oid2,_,Value,_}|_]},_} ->
            case lists:prefix(Prefix,Oid2) of
                true ->
                    Pid_main!wait,
                    agn_list(SP,[{Oid2,Value}|V_list],Pid_main,Prefix,Oid2);
                _    ->
                    io:format("test for not match ~p~n", [Oid2]),
                    {ok,{SP,V_list}}
            end;
         {error,timeout} -> 
            io:format("test for receive timeout ~p~n", [V_list]),
            {timeout,{SP,V_list}};
         R -> 
            io:format("test for receiveR ~p~n", [R]),
            R
    end.


agn(SP,Oids) ->
    snmp_ex2_manager:sync_get_next(SP#snmpPara.server,SP#snmpPara.port,Oids).


    

recV(IV_list,_,{All,Len}) when Len =:= All -> 
    IV_list;

recV(IV_list,Timeout,{All,Len}) ->
    receive
        {ok,{SP,V_list}} -> 
            recV([{SP,V_list}|IV_list],Timeout,{All,Len+1});
        {timeout,_} ->
            io:format("test here1 !!!!!~n~n~n~n"),
            recV(IV_list,Timeout,{All,Len+1});
        _ ->
            io:format("test here2 !!!!!~n~n~n~n"),
            recV(IV_list,Timeout,{All,Len})
    after Timeout ->
        io:format("test here3 !!!!!~n~n~n~n"),
        IV_list
    end.













