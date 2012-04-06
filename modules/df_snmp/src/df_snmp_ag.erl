-module(df_snmp_ag).

-compile(export_all).

-include_lib("df_snmp_record.hrl").
-include_lib("snmp/include/snmp_types.hrl").

start() ->
    snmp_ex2_manager:start_link(),
    Begin = now(),
    %%S = main(ready(1),[1,3,6,1,2,1,4,22,1,1]),
    %%Oid = [1,3,6,1,2,1,4,22,1,1],
    Oid_list = [[1,3,6,1,2,1,1,1,0],[1,3,6,1,2,1,1,2,0],[1,3,6,1,2,1,1,3,0],[1,3,6,1,2,1,1,4,0],
    [1,3,6,1,2,1,1,5,0],[1,3,6,1,2,1,1,6,0],[1,3,6,1,2,1,1,7,0],[1,3,6,1,2,1,17,1,1,0]],
    SP_list = [ready(251),ready(252),ready(253),ready(254)],
    S = main(SP_list,Oid_list,2000),
    io:format("the answer is~p~n",[S]),
    End   = now(),
    timeSub(Begin,End).

timeSub({A1,A2,A3},{B1,B2,B3}) ->
    {B1-A1,B2-A2,B3-A3},
    snmp_ex2_manager:stop().
    
ready(I) ->
    Server = {192,168,0,I},
    #snmpPara{
                    server    = Server,
                    port      = 161,
                    community = "public1",
                    timeout   = 2000,
                    retry     = 2
              }.



main(SP_list,Oid_list,Timeout) ->
    lists:foreach(fun(SP) -> ag(SP,Oid_list) end, SP_list),
    V_list = recV(SP_list,Timeout,[],{length(SP_list)*length(Oid_list),0}).
    


ag(SP,Oid_list) ->
    {snmpPara,Ip,_,Community,Timeout,_Retry} = SP,
    io:format("test for ag~p~n",[SP]),
    snmp_ex2_manager:agent(Ip,[{community,Community}]),
    lists:foreach(fun(Oid) -> snmp_ex2_manager:sync_get(Ip,[Oid],Timeout) end,
                    Oid_list).

recV(_,_,V_list,{All,Num}) when All<Num+1 ->
    V_list;

recV(SP_list,Timeout,V_list,{All,Num}) ->
    receive
        {ok,{IP,[{_,Oid,_,Value,_}|_]}} ->
            case lists:keysearch(IP,2,SP_list) of
                {value,SP} ->
                    case lists:keymember({SP,Oid},1,V_list) of
                        true -> 
                            recV(SP_list,Timeout,V_list,{All,Num});
                        _ ->
                            V_list_new = lists:umerge([{{SP,Oid},Value}],V_list),
                            recV(SP_list,Timeout,V_list_new,{All,Num+1})
                    end;
                 fasle ->
                    recV(SP_list,Timeout,V_list,{All,Num})
             end;
        _R ->
            %%io:format("test for value~p~n",[R]),
            recV(SP_list,Timeout,V_list,{All,Num})
    after Timeout ->
        V_list
    end.





















