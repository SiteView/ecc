-module(test_gn).

-compile(export_all).

-include_lib("df_snmp_record.hrl").
-include_lib("snmp/include/snmp_types.hrl").


start() ->
    snmp_ex2_manager:start_link(),
    Begin = now(),
    %%S = main(ready(1),[1,3,6,1,2,1,1]),
    %%io:format("the answer is ~p~n",[S]),
    %%Oid = [1,3,6,1,2,1,4,22,1,1],
    %%Oid_list = [[1,3,6,1,2,1,2,1],[1,3,6,1,2,1,2,2,1,3],[1,3,6,1,2,1,2,2,1,2],
      %%  [1,3,6,1,2,1,2,2,1,5],[1,3,6,1,2,1,2,2,1,6],[1,3,6,1,2,1,17,1,4,1,2]],
    Oid = [1,3,6,1,2,1,17,4,3,1,2],
    SP_list = [ready1("3"),ready1("5"),ready1("7"),ready1("1"),ready1("2")],
    %%S = lists:map(fun(Oid) -> getOid(SP_list,Oid) end,Oid_list),
    S = main(SP_list,Oid),
    io:format("s = ~p~n",[S]),
    End   = now(),
    timeSub(Begin,End),
    snmp_ex2_manager:stop().
    
getOid(SP_list,Oid) ->
    lists:map(fun(SP) -> main(SP,Oid) end, SP_list).

timeSub({A1,A2,A3},{B1,B2,B3}) ->
    {B1-A1,B2-A2,B3-A3}.

ready(I) ->
    Server = {192,168,0,I},
    #snmpPara{
                    server    = Server,
                    port      = 5000+I,
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
              
main(SP_list,Oids) ->
    %%io:format("~n~nPid_main is ~n Pid_ip is ~n~n"),
    S = lists:map(fun(SP) -> gn_list(SP,Oids) end, SP_list),
    %%io:format("test for gn ~p~n",[S]),
    S.
    

gn_list(SP,Oid) ->
    case gn_list(SP,[],Oid,Oid) of 
        wrong  -> 
            io:format("gn wrong ~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n"),
            {SP,[]};
        V_list -> {SP, lists:usort(V_list)}
    end.
    
gn_list(SP,V_list,Prefix,Oid) ->
    case gn(SP,[Oid]) of
        {ok,{noError,_,[{_,Oid2,_,Value,_}|_]},_} ->
            case lists:prefix(Prefix,Oid2) of
                true ->
                    %%io:format("11~n"),
                    gn_list(SP,[{Oid2,Value}|V_list],Prefix,Oid2);
                _    -> 
                    %%io:format("~n~ngn_list is ~p~n~n",[V_list]),
                    V_list
            end;
        _            -> 
            io:format("22 ~p~n",[SP]),
            wrong
    end.

gn(SP,Oids) ->
    {snmpPara,Ip,_,Community,Timeout,_Retry} = SP,
    case snmp_ex2_manager:agent(Ip,[{community,Community}]) of
        {error,_} -> updateCom(SP);
        _         -> true
    end,
    snmp_ex2_manager:sync_get_next(Ip, Oids, Timeout).

updateCom(SP) ->
    snmp_ex2_manager:update_agent(SP#snmpPara.server,community,SP#snmpPara.community).








