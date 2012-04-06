%% ---
%% snmp_session
%%
%%---
-module(snmp_session,[Server,Port,Ver,Community,AuthType,User,Passwd,PrivPasswd,ContextID,ContextName,Timeout, Options]).
-compile(export_all).
-include_lib("snmp/include/snmp_types.hrl").
-include_lib("snmp/src/manager/snmpm_usm.hrl").
-include("snmp_ecc.hrl").

new(Server,Port,Ver,Community,AuthType,User,Passwd,PrivPasswd,ContextID,ContextName,Timeout)-> 
	Obj = instance(Server,Port,Ver,Community,AuthType,User,Passwd,PrivPasswd,ContextID,ContextName,Timeout, []),
	Obj.
    
new(Server,Port,Ver,Community,AuthType,User,Passwd,PrivPasswd,ContextID,ContextName,Timeout, Options)-> 
	Obj = instance(Server,Port,Ver,Community,AuthType,User,Passwd,PrivPasswd,ContextID,ContextName,Timeout, Options),
	Obj.

%% 发送Trap消息的配置方法
config_trap(TrapObj) ->
    Trap = TrapObj#trap_obj.trap,
    NotifyName = erlang:atom_to_list(Trap#trap.trapname),
    snmp_ecc_agent:start_link(),
    case TrapObj#trap_obj.isgeneric of
        false ->
            (catch snmpa_symbolic_store:delete_notifications(Trap#trap.trapname)),
            Tk = snmpa_symbolic_store:set_notification(Trap,Trap#trap.trapname);
        _ ->
            []
    end,
    %%NotifyName = get_notifyname(),
    Veratom = 
    case Ver of
        "V1" ->
            v1;
        "V2" ->
            v2c
    end,
    (catch snmp_community_mib:delete_community(NotifyName)),
    SecurityName =
    case snmp_ecc_agent:add_community(NotifyName, Community, NotifyName, "", "") of
        {ok, SeName} ->
            SeName;
        _ ->
            []
    end,
    (catch snmp_view_based_acm_mib:delete_sec2group(SecurityName)),
    GroupName0 = 
    case snmp_ecc_agent:add_sec2group(Veratom, SecurityName, SecurityName) of
        {ok, VaName0} ->
            VaName0;
        _ ->
            []
    end,
    (catch snmp_view_based_acm_mib:delete_access(SecurityName)),
    GroupName = 
    case snmp_ecc_agent:add_access(SecurityName, "", Veratom, noAuthNoPriv, exact, "internet", "internet", "internet") of
        {ok, VaName} ->
            VaName;
        _ ->
            []
    end,
    (catch snmp_target_mib:delete_params(NotifyName)),
    Params =
    case snmp_ecc_agent:add_params(NotifyName, Veratom, Veratom, SecurityName, noAuthNoPriv) of
        {ok, Pa} ->
            Pa;
        _ ->
            []
    end,
    (catch snmp_notification_mib:delete_notify(NotifyName)),
    Notify = 
    case snmp_ecc_agent:add_notify(NotifyName, Params, trap) of
        {ok, Nofy} ->
            Nofy;
        _ ->
            []
    end,
    Ip = erlang:tuple_to_list(Server),
    (catch snmp_target_mib:delete_addr(Notify)),
    Addr = 
    case snmp_ecc_agent:add_addr(Notify, Ip, Port, Timeout, 3, Notify, Params, "ecc_snmp_engines", "", 1024000) of
        {ok, Ad} ->
            Ad;
        Other ->
            Other
    end,
    %%io:format("Notify = ~p~nAddr = ~p~nParams = ~p~nSecurityName = ~p~nGroupName = ~p~nGroupName0 = ~p~n", [Notify, Addr, Params, SecurityName, GroupName, GroupName0]),
    Invalid = snmp_notification_mib:invalidate_cache(),
    snmp_ecc_agent:start_link(),
    NotifyName.

get_notifyname() ->
    get_targetname_secname() ++ "_" ++ Ver ++ "_" ++ Community.

getLocalServer() ->
    case inet:gethostname() of
        {ok, Hostname} ->
            case inet:getaddr(Hostname, inet) of
                {ok, Address} ->
                    Address;
                _ ->
                    {127,0,0,1}
            end;
        Other ->
            {127,0,0,1}
    end.
    
getLocalServerStr() ->
    LocalServer = getLocalServer(),
    to_IpString(LocalServer).
    
send_trap1(Agent, Trap, Community, Varbinds) ->
    try snmp_ecc_agent:send_trap(snmp_master_agent, Trap, Community, Varbinds) of
        Suc ->
            {ok, trap_success}
    catch
        _:Reason ->
            {error, Reason}
    end.

%% 发送trap消息的接口
send_trap(Agent, Notification, Receiver,CtxName, Varbinds, TrapObj) ->
    %% 锁
    Trap = TrapObj#trap_obj.trap,
    LockName = erlang:atom_to_list(Trap#trap.trapname),
    %%io:format("LockName = ~p~n", [LockName]),
    is_lock(LockName),
    %%
    NotifyName = config_trap(TrapObj),
    Agt =
    case Agent of
        [] ->
            snmp_master_agent;
        Ag ->
            Ag
    end,
    Server_list = erlang:tuple_to_list(Server),
    Addresses = [{[1,3,6,1,6,1,1], Server_list++[Port,162]}],
    Rcr =
    case Receiver of
        [] ->
            no_receiver;
        Rv ->
            Rv
    end,
    Result =
    try snmp_ecc_agent:send_notification(snmp_master_agent, Notification, Rcr, NotifyName, CtxName, Varbinds) of
        {ok, _} ->
            {ok, trap_success};
        {error, Reason} ->
            {error, Reason};
        Suc ->
            {error, "other error"}
    catch
        _:Reason ->
            {error, Reason}
    end,
    release_lock(LockName),
    Result.

%% get(OIds)->
%%
%%
get(OIds)->
	lists:map(fun(X)->{X,THIS:g(X)} end,OIds).

%% OIds -> a list of oid,Index is oid's index
%% 
get(OIds,Index)->
    case Ver of
        "v3" ->
            lists:map(fun(X)->{X,THIS:g(X++[Index])} end, OIds);
        _ ->
            lists:map(fun(X)->{X,THIS:g(X++[Index])} end, OIds)
    end.
	
	
%%Oid=string()
%%
gn(Oid)->
    Sec_Name = get_targetname_secname(),
    is_lock(Sec_Name),
    start_config(),
    Value =
    case Ver of
        "v3" ->
            Extro =
            if
                ContextID =:= [] ->
                    [];
                true ->
                    [{engine_id, ContextID}]
            end,
            snmp_ex2_manager:sync_get_next_v3(?USER, Server, Port, ContextName, [Oid], Timeout, Extro,THIS);
        _ ->
            snmp_ex2_manager:sync_get_next(Server,Port,[Oid], Timeout)
    end,
    release_lock(Sec_Name),
    Value.

gb(NR, MR, Oids) ->
    Sec_Name = get_targetname_secname(),
    is_lock(Sec_Name),
    start_config(),
    Value =
    case Ver of
        "v3" ->
            Extro =
            if
                ContextID =:= [] ->
                    [];
                true ->
                    [{engine_id, ContextID}]
            end,
            %%snmp_ex2_manager:sync_get_next_v3(?USER, Server, Port, ContextName, Oids, Timeout, Extro,THIS);
            snmp_ex2_manager:sync_get_bulk_v3(?USER, Server, Port, ContextName, Oids, Timeout, Extro, NR, MR, THIS);
            %%snmp_ex2_manager:sync_get_bulk(Server, Port, NR, MR, Oids);
            %%snmp_ex2_manager:sync_get_bulk(?USER, Server, Port, NR, MR, ContextName, Oids, Timeout, Extro);
        _ ->
            snmp_ex2_manager:sync_get_bulk(Server, Port, NR, MR, Oids)
    end,
    release_lock(Sec_Name),
    Value.
    


start_config() ->
    snmp_ex2_manager:start_link(Options),
	case Ver of
		"v3"->
            case get_engineid() of
                [] ->
                    THIS:v3_config();
                EngineID ->
                    Sec_Name = get_targetname_secname(),
                    case is_usm_same(EngineID, Sec_Name) of
                        false ->
                            THIS:v3_config();
                        _ ->
                            ok
                    end
            end;
		_->
			THIS:v2_config()
	end.
    
is_usm_same(EngineID, SecName) ->
    case snmp_ex2_manager:get_usm_user_from_sec_name(EngineID, SecName) of
        {ok, UsmUser} ->
            {UIAuth, UIAuthKey} =
            case AuthType of
                "MD5"->
                    Md5Pwd = snmp_ex2_manager:passwd2localized_key(md5, Passwd, EngineID),
                    {usmHMACMD5AuthProtocol,Md5Pwd};
                "SHA" ->
                    SHAPwd = snmp_ex2_manager:passwd2localized_key(sha, Passwd, EngineID),
                    {usmHMACSHAAuthProtocol, SHAPwd};
                "None" ->
                    {usmNoAuthProtocol, Passwd};
                _ ->
                    Md5Pwd = snmp_ex2_manager:passwd2localized_key(md5, Passwd, EngineID),
                    {usmHMACMD5AuthProtocol, Md5Pwd}
            end,
            {UIPriv, UIPrivKey} =
            if
                PrivPasswd =:= [] ->
                    {usmNoPrivProtocol, PrivPasswd};
                true ->
                    PrivKey = snmp_ex2_manager:passwd2localized_key(md5, PrivPasswd, EngineID),
                    {usmDESPrivProtocol, PrivKey}
            end,
            if
                UsmUser#usm_user.name =/= User ->
                    false;
                UsmUser#usm_user.auth =/= UIAuth ->
                    false;
                UsmUser#usm_user.auth_key =/= UIAuthKey ->
                    false;
                UsmUser#usm_user.priv =/= UIPriv ->
                    false;
                UsmUser#usm_user.priv_key =/= UIPrivKey ->
                    false;
                true ->
                    true
            end;
        _ ->
            false
    end.

test_snmp() ->
    case g([1,3,6,1,2,1,1,1,0]) of
        {ok,{noError,_,[M|_]},_}->
            true;
        _ ->
            false
    end.

%%Oid=string()
%%
g(Oid) ->
    Sec_Name = get_targetname_secname(),
    is_lock(Sec_Name),
    start_config(),
    Extro =
            if
                ContextID =:= [] ->
                    [];
                true ->
                    [{engine_id, ContextID}]
            end,
    Va = 
    case Ver of
        "v3" ->
            snmp_ex2_manager:sync_get_v3(?USER, Server, Port, ContextName, [Oid], Timeout, Extro, THIS);
        _ ->
            snmp_ex2_manager:sync_get(Server,Port,[Oid])
    end,
    Value =
	case Va of
		{ok,{noSuchName,_,_},_}->
            case Ver of
                "v3" ->
                    snmp_ex2_manager:sync_get_v3(?USER, Server, Port, ContextName, [Oid++[0]], Timeout, Extro, THIS);
                _ ->
                    snmp_ex2_manager:sync_get(Server,Port,[Oid++[0]])
            end;
		R->
			case R of
				{ok,{noError,_,[Vb|_]},_}->
					case Vb#varbind.variabletype of
						'NULL'->
                            case Ver of
                                "v3" ->
                                    snmp_ex2_manager:sync_get_v3(?USER, Server, Port, ContextName, [Oid++[0]], Timeout, Extro, THIS);
                                _ ->
                                    snmp_ex2_manager:sync_get(Server,Port,[Oid++[0]])
                            end;
						_->
							R
					end;
				_->
					R
			end
	end,
    release_lock(Sec_Name),
    Value.
	


v2_config()->
	Agents = snmp_ex2_manager:which_agents(),
    Vs = erlang:list_to_atom(Ver),
	case lists:member({Server,Port},Agents) of
		true->
            snmp_ex2_manager:unagent(Server, Port),
            snmp_ex2_manager:agent(Server,Port,[{community,Community},{timeout,Timeout}]);
		_->
            snmp_ex2_manager:unagent(Server, Port),
			snmp_ex2_manager:agent(Server,Port,[{community,Community},{timeout,Timeout}])
	end.

get_engineid() ->
    Key = snmp_ex2_manager:get_Key(Server,Port, []),
    EgId =
        case snmp_ex2_manager:get_attribute(Key) of
            {ok, Value} ->
                Value;
            _ ->
                []
        end,
    EgId.

get_targetname_secname() ->
    to_IpString(Server) ++ ":" ++ erlang:integer_to_list(Port).

to_IpString(T) ->
    to_OidString(tuple_to_list(T)).

to_OidString(T) ->
    Temp = lists:map(fun(X) -> integer_to_list(X) end, T),
    string:join(Temp, ".").


is_lock(EngineID) ->
	global:set_lock({EngineID, atom_to_list(node()) ++ pid_to_list(self())}, [node()|nodes()]).	
					
release_lock(EngineID) ->	
	global:del_lock({EngineID, atom_to_list(node()) ++ pid_to_list(self())}, [node()|nodes()]).



%%执行v3config
exe_v3_config() ->
    %% enginid
    EngineID = get_engineid(),
    %%
    TargetName = get_targetname_secname(),
    Sec_Name = get_targetname_secname(),
    EngineId = 
    if
        EngineID =:= [] ->
            ?ENGINE;
        true ->
            EngineID
    end,
    AgentUser = 

            snmp_ex2_manager:unagent_v3(?USER,Server, Port),
            case lists:member({Server,Port},snmp_ex2_manager:which_agents_v3(?USER)) of
                true->
                    if
                        PrivPasswd =:= [] ->
                            snmp_ex2_manager:update_agent_v3(?USER,Server, Port, version, v3),
                            snmp_ex2_manager:update_agent_v3(?USER,Server, Port, sec_model, usm),
                            snmp_ex2_manager:update_agent_v3(?USER,Server, Port, sec_level, authNoPriv),
                            snmp_ex2_manager:update_agent_v3(?USER,Server, Port, engine_id, EngineId),
                            snmp_ex2_manager:update_agent_v3(?USER,Server, Port, sec_name, Sec_Name),
                            snmp_ex2_manager:update_agent_v3(?USER,Server, Port, target_name, TargetName);
                        true ->
                            snmp_ex2_manager:update_agent_v3(?USER,Server, Port, version, v3),
                            snmp_ex2_manager:update_agent_v3(?USER,Server, Port, sec_model, usm),
                            snmp_ex2_manager:update_agent_v3(?USER,Server, Port, sec_level, authPriv),
                            snmp_ex2_manager:update_agent_v3(?USER,Server, Port, engine_id, EngineId),
                            snmp_ex2_manager:update_agent_v3(?USER,Server, Port, sec_name, Sec_Name),
                            snmp_ex2_manager:update_agent_v3(?USER,Server, Port, target_name, TargetName)
                    end;
                _->
                    if
                        PrivPasswd =:= [] ->
                            snmp_ex2_manager:agent_v3(?USER, Server,Port,[{version,v3},{sec_model,usm},{sec_level,authNoPriv},{engine_id,EngineId},{sec_name,Sec_Name},{target_name,TargetName}]);
                        true ->
                            snmp_ex2_manager:agent_v3(?USER, Server,Port,[{version,v3},{sec_model,usm},{sec_level,authPriv},{engine_id,EngineId},{sec_name,Sec_Name},{target_name,TargetName}])
                    end
            end,
            case AuthType of
                "None" ->
                    snmp_ex2_manager:update_agent_v3(?USER,Server, Port, sec_level, noAuthNoPriv );
                _ ->
                    do_nothing
            end,
            case (catch snmpm_config:get_usm_user_from_sec_name(EngineID, Sec_Name)) of
                {ok, UsmUser = #usm_user{}} ->
                    snmp_ex2_manager:unregister_usm(EngineID, UsmUser#usm_user.name);
                _ ->
                    do_nothing
            end,
            St = snmp_ex2_manager:unregister_usm(?ENGINE, User),
            case EngineId of
                ""->
                    case snmp_ex2_manager:isExist_usm(EngineId, User, Sec_Name) of
                        true->
                            update_usm_by_authtype();
                        _->
                            register_usm_by_authtype()
                    
                    end;
                _->
                    Engine = EngineId,
                    case snmp_ex2_manager:isExist_usm(Engine, User, Sec_Name) of
                        true->
                            update_usm_by_authtype();
                        _->
                            register_usm_by_authtype()
                    
                    end
    end.
    
%%v3_config()->
%%    call({v3_config}).
v3_config()->
    EngineID = get_engineid(),
    exe_v3_config().
    
update_usm_by_authtype() ->
    %% enginid
    EngineID = get_engineid(),
    %%
    Sec_Name = get_targetname_secname(),
    case AuthType of
        "MD5"->
            Md5Pwd = snmp_ex2_manager:passwd2localized_key(md5, Passwd, EngineID),
            snmp_ex2_manager:update_usm_info(EngineID,User,sec_name,Sec_Name),
            snmp_ex2_manager:update_usm_info(EngineID,User,auth,usmHMACMD5AuthProtocol),
			snmp_ex2_manager:update_usm_info(EngineID,User,auth_key,Md5Pwd),
			snmp_ex2_manager:update_usm_info(EngineID,User,priv,usmNoPrivProtocol),
			snmp_ex2_manager:update_usm_info(EngineID,User,priv_key,PrivPasswd);
        "SHA" ->
            SHAPwd = snmp_ex2_manager:passwd2localized_key(sha, Passwd, EngineID),
            %%SHAPwd = snmp_ex2_manager:passwd2localized_key(sha, Passwd, EngineID),
            snmp_ex2_manager:update_usm_info(EngineID,User,sec_name,Sec_Name),
            snmp_ex2_manager:update_usm_info(EngineID,User,auth,usmHMACSHAAuthProtocol),
			snmp_ex2_manager:update_usm_info(EngineID,User,auth_key,SHAPwd),
			snmp_ex2_manager:update_usm_info(EngineID,User,priv,usmNoPrivProtocol),
			snmp_ex2_manager:update_usm_info(EngineID,User,priv_key,PrivPasswd);
        "None" ->
            snmp_ex2_manager:update_usm_info(EngineID,User,sec_name,Sec_Name),
			snmp_ex2_manager:update_usm_info(EngineID,User,auth,usmNoAuthProtocol),
			snmp_ex2_manager:update_usm_info(EngineID,User,auth_key,Passwd),
			snmp_ex2_manager:update_usm_info(EngineID,User,priv,usmNoPrivProtocol),
			snmp_ex2_manager:update_usm_info(EngineID,User,priv_key,PrivPasswd);
        _->
            Md5Pwd = snmp_ex2_manager:passwd2localized_key(md5, Passwd, EngineID),
            snmp_ex2_manager:update_usm_info(EngineID,User,sec_name,Sec_Name),
			snmp_ex2_manager:update_usm_info(EngineID,User,auth,usmHMACMD5AuthProtocol),
			snmp_ex2_manager:update_usm_info(EngineID,User,auth_key,Md5Pwd),
			snmp_ex2_manager:update_usm_info(EngineID,User,priv,usmNoPrivProtocol),
			snmp_ex2_manager:update_usm_info(EngineID,User,priv_key,PrivPasswd)
	end,
    if
        PrivPasswd =:= [] ->
            do_nothing;
        true ->
            PrivKey = snmp_ex2_manager:passwd2localized_key(md5, PrivPasswd, EngineID),
            snmp_ex2_manager:update_usm_info(EngineID,User,priv,usmDESPrivProtocol),
            snmp_ex2_manager:update_usm_info(EngineID,User,priv_key,PrivKey)
    end,
    ok.
    
register_usm_by_authtype() ->
    %% enginid
    EngineID = get_engineid(),
    %%
    TargetName = get_targetname_secname(),
    Sec_Name = get_targetname_secname(),
    case AuthType of
        "MD5" ->
            if
                PrivPasswd =:= [] ->
                    Md5Pwd = snmp_ex2_manager:passwd2localized_key(md5, Passwd, EngineID),
                    snmp_ex2_manager:unregister_usm(EngineID, User),
                    snmp_ex2_manager:register_usm(EngineID,User,[{sec_name,Sec_Name},{auth,usmHMACMD5AuthProtocol},
                                                                {auth_key,Md5Pwd},{priv,usmNoPrivProtocol},
                                                                {priv_key,PrivPasswd}]);
                true ->
                    PrivKey = snmp_ex2_manager:passwd2localized_key(md5, PrivPasswd, EngineID),
                    Md5Pwd = snmp_ex2_manager:passwd2localized_key(md5, Passwd, EngineID),
                    snmp_ex2_manager:unregister_usm(EngineID, User),
                    snmp_ex2_manager:register_usm(EngineID,User,[{sec_name,Sec_Name},{auth,usmHMACMD5AuthProtocol},
                                                                {auth_key,Md5Pwd},{priv,usmDESPrivProtocol},
                                                                {priv_key,PrivKey}])
            end;
        "SHA" ->
            if
                PrivPasswd =:= [] ->
                    SHAPwd = snmp_ex2_manager:passwd2localized_key(sha, Passwd, EngineID),
                    snmp_ex2_manager:unregister_usm(EngineID, User),
                    snmp_ex2_manager:register_usm(EngineID,User,[{sec_name,Sec_Name},{auth,usmHMACSHAAuthProtocol},
                                                         {auth_key,SHAPwd},{priv,usmNoPrivProtocol},
                                                         {priv_key,PrivPasswd}]);
                true ->
                    PrivKey = snmp_ex2_manager:passwd2localized_key(md5, PrivPasswd, EngineID),
                    SHAPwd = snmp_ex2_manager:passwd2localized_key(sha, Passwd, EngineID),
                    snmp_ex2_manager:unregister_usm(EngineID, User),
                    snmp_ex2_manager:register_usm(EngineID,User,[{sec_name,Sec_Name},{auth,usmHMACSHAAuthProtocol},
                                                         {auth_key,SHAPwd},{priv,usmDESPrivProtocol},
                                                         {priv_key,PrivKey}])
            end;
        "None" ->
            if
                PrivPasswd =:= [] ->
                    snmp_ex2_manager:unregister_usm(EngineID, User),
                    snmp_ex2_manager:register_usm(EngineID,User,[{sec_name,Sec_Name},{auth,usmNoAuthProtocol},
                                                         {auth_key,Passwd},{priv,usmNoPrivProtocol},
                                                         {priv_key,PrivPasswd}]);
                true ->
                    PrivKey = snmp_ex2_manager:passwd2localized_key(md5, PrivPasswd, EngineID),
                    snmp_ex2_manager:unregister_usm(EngineID, User),
                    snmp_ex2_manager:register_usm(EngineID,User,[{sec_name,Sec_Name},{auth,usmNoAuthProtocol},
                                                         {auth_key,Passwd},{priv,usmDESPrivProtocol},
                                                         {priv_key,PrivKey}])
            end;
        _->
            if
                PrivPasswd =:= [] ->
                    Md5Pwd = snmp_ex2_manager:passwd2localized_key(md5, Passwd, EngineID),
                    snmp_ex2_manager:unregister_usm(EngineID, User),
                    snmp_ex2_manager:register_usm(EngineID,User,[{sec_name,Sec_Name},{auth,usmHMACMD5AuthProtocol},
                                                         {auth_key,Md5Pwd},{priv,usmNoPrivProtocol},
                                                         {priv_key,PrivPasswd}]);
                true ->
                    PrivKey = snmp_ex2_manager:passwd2localized_key(md5, PrivPasswd, EngineID),
                    Md5Pwd = snmp_ex2_manager:passwd2localized_key(md5, Passwd, EngineID),
                    snmp_ex2_manager:unregister_usm(EngineID, User),
                    snmp_ex2_manager:register_usm(EngineID,User,[{sec_name,Sec_Name},{auth,usmHMACMD5AuthProtocol},
                                                         {auth_key,Md5Pwd},{priv,usmDESPrivProtocol},
                                                         {priv_key,PrivKey}])
            end
	end,
    ok.

get_table_col(OId)->
    erlang:put(now_snmp_time, sv_datetime:now()),
	get_table_col_item(OId,OId).
    
    

get_table_col_item(Prefix,Oid)->
    JudgeTimeout =
    case erlang:get(now_snmp_time) of
        undefined ->
            ok;
        [] ->
            ok;
        BeginTime ->
            Dispar = (sv_datetime:now() - BeginTime),
            if
                Dispar >= 30000 ->
                    stop;
                true ->
                    ok
            end
    end,
    if
        JudgeTimeout =:= ok ->
            case THIS:gn(Oid) of
                {ok,{noError,_,[Vb|_]},_}->
                    case Vb#varbind.variabletype of
                        'NULL'->    
                            [] ++
                            if
                                Oid =:= Vb#varbind.oid ->
                                    if
                                         Vb#varbind.oid =:= [1,0] ->
                                            THIS:get_table_col_item(Prefix,[1,1]);
                                         true ->
                                            []
                                    end;
                                true ->
                                    THIS:get_table_col_item(Prefix,Vb#varbind.oid)
                            end;
                        _->
                            case lists:prefix(Prefix,Vb#varbind.oid) of
                                true->
                                    [Vb] ++
                                    THIS:get_table_col_item(Prefix,Vb#varbind.oid);
                                _->
                                    []
                            end
                    end;
                _->
                    []
            end;
        true ->
            []
    end.
    
%%gb(NR, MR, Oids)
get_table_col_item_bulk(Oid, NRep, MaxRep)->
    JudgeTimeout =
    case erlang:get(now_snmp_time) of
        undefined ->
            ok;
        [] ->
            ok;
        BeginTime ->
            Dispar = (sv_datetime:now() - BeginTime),
            if
                Dispar >= 30000 ->
                    stop;
                true ->
                    ok
            end
    end,
    if
        JudgeTimeout =:= ok ->
            case THIS:gb(NRep, MaxRep, [Oid]) of
                {ok,{noError,_,Vbs},_}->
                    %%io:format("VbsLength: ~p~n", [string:len(Vbs)]),
                    case lists:last(Vbs) of
                        Vb=#varbind{} ->
                            %%io:format("Vb = ~p~n", [Vb]),
                            case Vb#varbind.value of
                                endOfMibView->  
                                    [];
                                _ ->
                                    %%platform:sleep(50),
                                    Vbs ++
                                    case Vb#varbind.variabletype of
                                        'NULL'->    
                                            if
                                                Oid =:= Vb#varbind.oid ->
                                                    if
                                                        Vb#varbind.oid =:= [1,0] ->
                                                            THIS:get_table_col_item_bulk([1,1], NRep, MaxRep);
                                                        true ->
                                                            []
                                                    end;
                                                true ->
                                                    THIS:get_table_col_item_bulk(Vb#varbind.oid, NRep, MaxRep)
                                            end;
                                        _->
                                            THIS:get_table_col_item_bulk(Vb#varbind.oid, NRep, MaxRep)
                                    end
                            end;
                        Other ->
                            io:format("Other = ~p~n", [Other]),
                            []
                    end;
                    
                    %%case Vb#varbind.variabletype of
                    %%    'NULL'->    
                    %%        [] ++
                    %%        if
                    %%            Oid =:= Vb#varbind.oid ->
                    %%                if
                    %%                     Vb#varbind.oid =:= [1,0] ->
                    %%                        THIS:get_table_col_item(Prefix,[1,1]);
                    %%                     true ->
                    %%                        []
                    %%                end;
                    %%            true ->
                    %%                THIS:get_table_col_item(Prefix,Vb#varbind.oid)
                    %%        end;
                    %%    _->
                    %%        case lists:prefix(Prefix,Vb#varbind.oid) of
                    %%            true->
                    %%                [Vb] ++
                    %%                THIS:get_table_col_item(Prefix,Vb#varbind.oid);
                    %%            _->
                    %%                []
                    %%        end
                    %%end;
                Other1->
                    io:format("Other1 = ~p~n", [Other1]),
                    []
            end;
        true ->
            []
    end.

get_table_col_filter_column(Column,Oid)->
	case THIS:gn(Oid) of
		{ok,{noError,_,[Vb|_]},_}->
			case Vb#varbind.variabletype of
				'NULL'-> [];
				_ ->
					case Vb#varbind.value of
						Column -> Vb;
						_ -> get_table_col_filter_column(Column,Vb#varbind.oid)
					end
			end;
		_->
			[]
	end.
    
gm(Oids)->
	Sec_Name = get_targetname_secname(),
    is_lock(Sec_Name),
    start_config(),
    Extro =
            if
                ContextID =:= [] ->
                    [];
                true ->
                    [{engine_id, ContextID}]
            end,
    Va = 
    case Ver of
        "v3" ->
            R1 = snmp_ex2_manager:sync_get_next_v3(?USER, Server, Port, ContextName, Oids, Timeout, Extro, THIS),
			case R1 of
				{ok,{noError,_,_},_}->
					R1;
				_->
					platform:sleep(100),
					snmp_ex2_manager:sync_get_next_v3(?USER, Server, Port, ContextName, Oids, Timeout, Extro, THIS)
			end;
        _ ->
            R1 = snmp_ex2_manager:sync_get(Server,Port,Oids),
			case R1 of
				{ok,{noError,_,_},_}->
					R1;
				_->
					platform:sleep(100),
					snmp_ex2_manager:sync_get_next(Server,Port,Oids)
			end
    end,
    release_lock(Sec_Name),
    Va.

gt(Oid) ->
    Sec_Name = get_targetname_secname(),
    is_lock(Sec_Name),
    start_config(),
    Extro =
            if
                ContextID =:= [] ->
                    [];
                true ->
                    [{engine_id, ContextID}]
            end,
    Va = 
    case Ver of
        "v3" ->
            snmp_ex2_manager:sync_get_v3(?USER, Server, Port, ContextName, [Oid], Timeout, Extro, THIS);
        _ ->
            snmp_ex2_manager:sync_get(Server,Port,[Oid],Timeout)
    end,
    Value =
	case Va of
		{ok,{noSuchName,_,_},_}->
            case Ver of
                "v3" ->
                    snmp_ex2_manager:sync_get_v3(?USER, Server, Port, ContextName, [Oid++[0]], Timeout, Extro, THIS);
                _ ->
                    snmp_ex2_manager:sync_get(Server,Port,[Oid++[0]],Timeout)
            end;
		R->
			case R of
				{ok,{noError,_,[Vb|_]},_}->
					case Vb#varbind.variabletype of
						'NULL'->
                            case Ver of
                                "v3" ->
                                    snmp_ex2_manager:sync_get_v3(?USER, Server, Port, ContextName, [Oid++[0]], Timeout, Extro, THIS);
                                _ ->
                                    snmp_ex2_manager:sync_get(Server,Port,[Oid++[0]],Timeout)
                            end;
						_->
							R
					end;
				_->
					R
			end
	end,
    release_lock(Sec_Name),
    Value.

s(VarsAndVals) ->
	Sec_Name = get_targetname_secname(),
    is_lock(Sec_Name),
    start_config(),
    Value = snmp_ex2_manager:sync_set(Server, Port, VarsAndVals),
    release_lock(Sec_Name),
    Value.
    