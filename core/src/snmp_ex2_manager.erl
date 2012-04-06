%%<copyright>
%% <year>2006-2007</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
%%
%%----------------------------------------------------------------------
%% This module examplifies how to write test suites for your SNMP agent.
%%----------------------------------------------------------------------

-module(snmp_ex2_manager).

-behaviour(gen_server).
-behaviour(snmpm_user).

-export([start_link/0, start_link/1, stop/0,
	 agent/2, agent/3, which_agents/0,update_agent/3,update_agent/4,
	 register_usm/3,which_usms/1,update_usm_info/4,
	 set_trap_handle/1,remove_trap_handle/1,do_trap/2,
         sync_get/2,      sync_get/3,	sync_get/4,
         sync_get_next/2, sync_get_next/3,sync_get_next/4,
         sync_get_bulk/4, sync_get_bulk/5,
         sync_set/2,      sync_set/3,

	 oid_to_name/1,unagent/1, unagent/2,
     isExist_usm/3, getOctetString/1,
     which_agents_v3/1,sync_get_v3/8,
     sync_get_next_v3/8,agent_v3/3,agent_v3/4,
     unagent_v3/2,unagent_v3/3,update_agent_v3/4,update_agent_v3/5,
     passwd2localized_key/3, get_engineid/2, get_engineId/1,get_bootsandtimes/1,
     get_attribute/1,set_attribute/2, unregister_usm/2,get_Key/3, get_usm_user_from_sec_name/2,
     any_to_list/1,oid2dots/1,is_string/1
     
	]).

%% Manager callback API:
-export([handle_error/3,
         handle_agent/4,
         handle_pdu/5,
         handle_trap/4,
         handle_inform/4,
         handle_report/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include_lib("snmp/include/snmp_types.hrl").
-include("snmp_ecc.hrl").

-define(SERVER,   'elecc_snmp_manager').
-define(USER_MOD, ?MODULE).
-record(state, {parent,trap=[],reqId2Pid=[]}).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link() ->
    start_link([]).

start_link(Opts) when is_list(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self(), Opts], []).

stop() ->
    cast(stop).
    

%% --- Instruct manager to handle an agent ---

agent(Addr, Conf) ->
    call({agent, Addr, Conf}).

agent(Addr, Port, Conf) ->
    call({agent, Addr, Port, Conf}).
    
unagent(Addr) ->
    call({unagent, Addr}).
    
unagent(Addr, Port) ->
    call({unagent, Addr, Port}).

update_agent(Addr,Item,Value)->
	call({update_agent,Addr,Item,Value}).

update_agent(Addr,Port,Item,Value)->
	call({update_agent,Addr,Port,Item,Value}).

which_agents()->
	call({which_agents,?USER}).

%% --- Misc utility functions ---

oid_to_name(Oid) ->
    call({oid_to_name, Oid}).

set_trap_handle(M)->
	call({set_trap_handle,M}).

remove_trap_handle(M)->
	call({remove_trap_handle,M}).
    

%%***************************************************************************************************
%% --- Version3 Operations ----
sync_get_v3(UserId, Addr, Port, ContextName, Oids, Expire, ExtraInfo, Session) ->
    NPort =
    if
        Port =:= [] ->
            161;
        true ->
            Port
    end,
    TimeOut =
    if 
        Expire =:= [] ->
            5000;
        true ->
            Expire
    end,
    case call({async_get_v3, UserId, Addr, NPort, ContextName, Oids, TimeOut, ExtraInfo,Session}) of
        {ok, ReqId} ->
            receive
                {error,ReqId,Reason} ->
                    %%io:format("error,ReqId,Reason~n"),
                    case Reason of
                        {usmStatsNotInTimeWindows, V3Hdr} ->
                            EId = get_engineId(V3Hdr),
                            {Boots, Time} = get_bootsandtimes(V3Hdr),
                            Key = get_Key(Addr,NPort, []),
                            snmpm_config:set_usm_eboots(EId, Boots),
                            Diff = snmp_misc:now(sec) - Time,
                            snmpm_config:set_usm_etime(EId, Diff),
                            sync_get_v3(UserId, Addr, Port, ContextName, Oids, Expire, ExtraInfo, Session);
                        {notInTimeWindow, Info} ->
                            case lists:keysearch(engine,1, Info) of
                                {value,{engine,Egid}} ->
                                    case lists:keysearch(boots,1, Info) of
                                        {value,{boots,Boots}} ->
                                            snmpm_config:set_usm_eboots(Egid, Boots);
                                        _ ->
                                            do_nothing
                                    end,
                                    case lists:keysearch(time,1, Info) of
                                        {value,{time,Time}} ->
                                            Diff = snmp_misc:now(sec) - Time,
                                            snmpm_config:set_usm_etime(Egid, Diff);
                                        _ ->
                                            do_nothing
                                    end,
                                    sync_get_v3(UserId, Addr, Port, ContextName, Oids, Expire, ExtraInfo, Session);
                                _ ->
                                    {error, timeout}
                            end;
                        {v3hdr, V3Hdr} ->
                            EId = get_engineId(V3Hdr),
                            Key = get_Key(Addr,NPort, []),
                            set_attribute(Key, EId),
                            Session:v3_config(),
                            %%snmp_ifc_lock:v3_config(Session),
                            sync_get_v3(UserId, Addr, Port, ContextName, Oids, Expire, ExtraInfo, Session);
                        {usmStatsUnknownEngineIDs, EgId} ->
                            Key = get_Key(Addr,NPort, []),
                            set_attribute(Key, EgId),
                            Session:v3_config(),
                            %%snmp_ifc_lock:v3_config(Session),
                            sync_get_v3(UserId, Addr, Port, ContextName, Oids, Expire, ExtraInfo, Session);
                        _ ->
                            gen_server:cast(?SERVER, {removeR2PState, ReqId}),
                            {error, timeout}
                    end;
                    
                {ReqId, Return} -> %%io:format("#####Receive#####~nReqId = ~p, Return = ~p~n", [ReqId,Return]), 
                    %%io:format("ReqId, Return~n"),
                    {ok,Return, TimeOut};
                Other ->
                    %%io:format("Other~n"),
                    {error, timeout}
            after TimeOut ->
                %%io:format("#####Timeout#####~nReqId = ~p~n", [ReqId]),
                gen_server:cast(?SERVER, {removeR2PState, ReqId}),              %%delete ReqId of genserver's state
                {error, timeout}
            end;
        {error, Reason} -> {error, Reason}
    end.
    
    
    
sync_get_next_v3(UserId, Addr, Port, ContextName, Oids, Expire, ExtraInfo, Session) ->
    NPort =
    if
        Port =:= [] ->
            161;
        true ->
            Port
    end,
    TimeOut =
    if 
        Expire =:= [] ->
            5000;
        true ->
            Expire
    end,
    %%io:format("get next ve~n"),
    case call({async_get_next_v3, UserId, Addr, NPort, ContextName, Oids, TimeOut, ExtraInfo}) of
        {ok, ReqId} ->
            receive
                {error,ReqId,Reason} ->
                    case Reason of
                        {usmStatsNotInTimeWindows, V3Hdr} ->
                            EId = get_engineId(V3Hdr),
                            {Boots, Time} = get_bootsandtimes(V3Hdr),
                            Key = get_Key(Addr,NPort, []),
                            snmpm_config:set_usm_eboots(EId, Boots),
                            Diff = snmp_misc:now(sec) - Time,
                            snmpm_config:set_usm_etime(EId, Diff),
                            sync_get_next_v3(UserId, Addr, Port, ContextName, Oids, Expire, ExtraInfo, Session);
                        {notInTimeWindow, Info} ->
                            case lists:keysearch(engine,1, Info) of
                                {value,{engine,Egid}} ->
                                    case lists:keysearch(boots,1, Info) of
                                        {value,{boots,Boots}} ->
                                            snmpm_config:set_usm_eboots(Egid, Boots);
                                        _ ->
                                            do_nothing
                                    end,
                                    case lists:keysearch(time,1, Info) of
                                        {value,{time,Time}} ->
                                            Diff = snmp_misc:now(sec) - Time,
                                            snmpm_config:set_usm_etime(Egid, Diff);
                                        _ ->
                                            do_nothing
                                    end,
                                    sync_get_next_v3(UserId, Addr, Port, ContextName, Oids, Expire, ExtraInfo, Session);
                                _ ->
                                    {error, timeout}
                            end;
                        {v3hdr, V3Hdr} ->
                            EId = get_engineId(V3Hdr),
                            Key = get_Key(Addr,NPort, []),
                            set_attribute(Key, EId),
                            Session:v3_config(),
                            sync_get_next_v3(UserId, Addr, Port, ContextName, Oids, Expire, ExtraInfo, Session);
                        {usmStatsUnknownEngineIDs, EgId} ->
                            Key = get_Key(Addr,NPort, []),
                            set_attribute(Key, EgId),
                            Session:v3_config(),
                            sync_get_next_v3(UserId, Addr, Port, ContextName, Oids, Expire, ExtraInfo, Session);
                        _ ->
                            gen_server:cast(?SERVER, {removeR2PState, ReqId}),
                            {error, timeout}
                    end;
                {ReqId, Return} -> %%io:format("#####Receive#####~nReqId = ~p, Return = ~p~n", [ReqId,Return]), 
                    {ok,Return, TimeOut}
            after TimeOut ->
                %%io:format("#####Timeout#####~nReqId = ~p~n", [ReqId]),
                gen_server:cast(?SERVER, {removeR2PState, ReqId}),              %%delete ReqId of genserver's state
                {error, timeout}
            end;
        {error, Reason} -> {error, Reason}
    end.
    
    
sync_get_bulk_v3(UserId, Addr, Port, ContextName, Oids, Expire, ExtraInfo, NR, MR, Session) ->
    NPort =
    if
        Port =:= [] ->
            161;
        true ->
            Port
    end,
    TimeOut =
    if 
        Expire =:= [] ->
            10000;
        true ->
            Expire
    end,
    %%io:format("get next ve~n"),
    case call({async_get_bulk_v3, UserId, Addr, Port, ContextName, Oids, TimeOut, ExtraInfo, NR, MR}) of
        {ok, ReqId} ->
            receive
                {error,ReqId,Reason} ->
                    case Reason of
                        {usmStatsNotInTimeWindows, V3Hdr} ->
                            EId = get_engineId(V3Hdr),
                            {Boots, Time} = get_bootsandtimes(V3Hdr),
                            Key = get_Key(Addr,NPort, []),
                            snmpm_config:set_usm_eboots(EId, Boots),
                            Diff = snmp_misc:now(sec) - Time,
                            snmpm_config:set_usm_etime(EId, Diff),
                            sync_get_next_v3(UserId, Addr, Port, ContextName, Oids, Expire, ExtraInfo, Session);
                        {notInTimeWindow, Info} ->
                            case lists:keysearch(engine,1, Info) of
                                {value,{engine,Egid}} ->
                                    case lists:keysearch(boots,1, Info) of
                                        {value,{boots,Boots}} ->
                                            snmpm_config:set_usm_eboots(Egid, Boots);
                                        _ ->
                                            do_nothing
                                    end,
                                    case lists:keysearch(time,1, Info) of
                                        {value,{time,Time}} ->
                                            Diff = snmp_misc:now(sec) - Time,
                                            snmpm_config:set_usm_etime(Egid, Diff);
                                        _ ->
                                            do_nothing
                                    end,
                                    sync_get_next_v3(UserId, Addr, Port, ContextName, Oids, Expire, ExtraInfo, Session);
                                _ ->
                                    {error, timeout}
                            end;
                        {v3hdr, V3Hdr} ->
                            EId = get_engineId(V3Hdr),
                            Key = get_Key(Addr,NPort, []),
                            set_attribute(Key, EId),
                            Session:v3_config(),
                            sync_get_next_v3(UserId, Addr, Port, ContextName, Oids, Expire, ExtraInfo, Session);
                        {usmStatsUnknownEngineIDs, EgId} ->
                            Key = get_Key(Addr,NPort, []),
                            set_attribute(Key, EgId),
                            Session:v3_config(),
                            sync_get_next_v3(UserId, Addr, Port, ContextName, Oids, Expire, ExtraInfo, Session);
                        _ ->
                            gen_server:cast(?SERVER, {removeR2PState, ReqId}),
                            {error, timeout}
                    end;
                {ReqId, Return} -> %%io:format("#####Receive#####~nReqId = ~p, Return = ~p~n", [ReqId,Return]), 
                    {ok,Return, TimeOut}
            after TimeOut ->
                %%io:format("#####Timeout#####~nReqId = ~p~n", [ReqId]),
                gen_server:cast(?SERVER, {removeR2PState, ReqId}),              %%delete ReqId of genserver's state
                {error, timeout}
            end;
        {error, Reason} -> {error, Reason}
    end.
    

agent_v3(UserId, Addr, Conf) ->
    %%Reply = (catch snmpm:register_agent(UserId, Addr, Conf)),
    %%Reply.
    call({agent_v3, UserId, Addr, Conf}).

agent_v3(UserId, Addr, Port, Conf) ->
    %%Reply = (catch snmpm:register_agent(UserId, Addr, Port, Conf)),
    %%Reply.
    call({agent_v3, UserId, Addr, Port, Conf}).
    
unagent_v3(UserId, Addr) ->
    %%Reply = (catch snmpm:unregister_agent(UserId, Addr)),
    %%Reply.
    call({unagent_v3, UserId, Addr}).
    
unagent_v3(UserId, Addr, Port) ->
    %%Reply = (catch snmpm:unregister_agent(UserId, Addr, Port)),
    %%Reply.
    call({unagent_v3, UserId, Addr, Port}).

update_agent_v3(UserId, Addr,Item, Value) ->
    %%Reply = (catch snmpm:update_agent_info(UserId, Addr, 161, Item,Value)),
    %%Reply.
    call({update_agent_v3, UserId, Addr,Item, Value}).

update_agent_v3(UserId, Addr, Port, Item, Value) ->
    %%Reply = (catch snmpm:update_agent_info(UserId, Addr, Port, Item,Value)),
    %%Reply.
    call({update_agent_v3, UserId, Addr, Port, Item, Value}).
    
register_user() ->
    case snmpm:register_user(?USER, ?USER_MOD, self()) of
	ok ->
	    ok;
	Error ->
	    error({failed_register_user, Error})
    end.

register_usm(Engine, UserId, Conf)->
    call({register_usm, Engine, UserId, Conf}).
    
unregister_usm(Engine, UserId)->
    call({unregister_usm, Engine, UserId}).

update_usm_info(Engine,UserId,Item,Val)->
    call({update_usm_info, Engine,UserId,Item,Val}).
    
which_usms(Engine)->
    call({which_usms, Engine}).
    
isExist_usm(Engine, UserId, ServerPortId) ->
    call({isExist_usm, Engine, UserId, ServerPortId}).
    
get_usm_user_from_sec_name(EngineID, SecName) ->
    call({get_usm_user_from_sec_name, EngineID, SecName}).

    
which_agents_v3(UserId) ->
    call({which_agents_v3, UserId}).
    
get_engineId(V3Hdr = #v3_hdr{}) ->
	case (catch snmp_pdus:dec_usm_security_parameters(V3Hdr#v3_hdr.msgSecurityParameters)) of
	    {'EXIT', _} ->
            [];
	    Res ->
            dec_usmSecurityParameters(Res)
	end;
get_engineId(_) ->
    [].

dec_usmSecurityParameters(Param = #usmSecurityParameters{}) ->
    Param#usmSecurityParameters.msgAuthoritativeEngineID;
dec_usmSecurityParameters(_) ->
    [].

get_reqId(ScopedPdu) ->
    (ScopedPdu#scopedPdu.data)#pdu.request_id.

get_bootsandtimes(V3Hdr = #v3_hdr{}) ->
    #v3_hdr{msgID                 = MsgID, 
	    msgMaxSize            = MMS, 
	    msgFlags              = MsgFlags,
	    msgSecurityModel      = MsgSecModel,
	    msgSecurityParameters = SecParams, 
	    hdr_size              = HdrSize} = V3Hdr,
    UsmSecParams =
	case (catch snmp_pdus:dec_usm_security_parameters(SecParams)) of
	    {'EXIT', Reason} ->
            [];
	    Res ->
            Res
	end,
    #usmSecurityParameters{msgAuthoritativeEngineBoots = MsgAuthEngineBoots,
			   msgAuthoritativeEngineTime = MsgAuthEngineTime} = UsmSecParams,
    {MsgAuthEngineBoots, MsgAuthEngineTime};
get_bootsandtimes(_) ->
    [].

%% 为AuthKey加密
passwd2localized_key(Alg, Passwd, EngineID) ->
    EngineId = 
    if
        EngineID =:= [] ->
            ?ENGINE;
        true ->
            EngineID
    end,
    Pwd =
    case Passwd of
        [] ->
            ?INIPWD;
        _ ->
            Passwd
    end,
    snmp:passwd2localized_key(Alg, Pwd, EngineId).
    
get_engineid(Obj, Ver) ->
    Reply =
    case Ver of
		"v3"->
			Obj:v3_config(),
            Val = Obj:g([1,3]),
            case Val of
                {v3hdr,V3Hdr}->
                    {v3hdr,V3Hdr};
                {engineid, EgId} ->
                    {engineid, EgId};
                Ret->
                    ?ENGINE
            end;
		_->
			?ENGINE
	end,
    Reply.
    
    
%%**********************************************************************************************

%% --- Various SNMP operations ----

sync_get(Addr, Oids) ->
    Timout = 5000,
    case call({async_get, Addr, Oids}) of
        {ok, ReqId} ->
            receive
                {error,ReqId,Reason} ->
                    %%io:format("v2 error~n"),
                    gen_server:cast(?SERVER, {removeR2PState, ReqId}),
                    {error, timeout};
                {ReqId, Return} -> %%io:format("#####Receive#####~nReqId = ~p, Return = ~p~n", [ReqId,Return]), 
                    {ok,Return, Timout}
            after Timout ->
                %%io:format("#####Timeout#####~nReqId = ~p~n", [ReqId]),
                gen_server:cast(?SERVER, {removeR2PState, ReqId}),              %%delete ReqId of genserver's state
                {error, timeout}
            end;
        {error, Reason} -> %%io:format("#####Error#####~nReason = ~p~n", [Reason]), 
            {error, Reason}
    end.
    
sync_get(Addr, Port, Oids) ->
    %%io:format("*********Timeout*********~nTimeout = ~p~n", [ReqId]),
    Timout = 5000,
    case call({async_get, Addr, Port, Oids}) of
        {ok, ReqId} ->
            receive
                {error,ReqId,Reason} ->
                    %%io:format("v2 error~n"),
                    gen_server:cast(?SERVER, {removeR2PState, ReqId}),
                    {error, timeout};
                {ReqId, Return} -> %%io:format("#####Receive#####~nReqId = ~p, Return = ~p~n", [ReqId,Return]), 
                    {ok,Return, Timout}
            after Timout ->
                %%io:format("#####Timeout#####~nReqId = ~p~n", [ReqId]),
                gen_server:cast(?SERVER, {removeR2PState, ReqId}),              %%delete ReqId of genserver's state
                {error, timeout}
            end;
        {error, Reason} -> {error, Reason}
    end.
     
sync_get(Addr, Port, Oids, Timout) ->
    %%io:format("*********Timeout*********~nTimeout = ~p~n", [ReqId]),
    %%Timout = 5000,
    case call({async_get, Addr, Port, Oids}) of
        {ok, ReqId} ->
            receive
                {error,ReqId,Reason} ->
                    %%io:format("v2 error~n"),
                    gen_server:cast(?SERVER, {removeR2PState, ReqId}),
                    {error, timeout};
                {ReqId, Return} -> %%io:format("#####Receive#####~nReqId = ~p, Return = ~p~n", [ReqId,Return]), 
                    {ok,Return, Timout}
            after Timout ->
                %%io:format("#####Timeout#####~nReqId = ~p~n", [ReqId]),
                gen_server:cast(?SERVER, {removeR2PState, ReqId}),              %%delete ReqId of genserver's state
                {error, timeout}
            end;
        {error, Reason} -> {error, Reason}
    end.    


sync_get_next(Addr, Oids) ->
    Timout = 5000,
    case call({async_get_next, Addr, Oids}) of
        {ok, ReqId} ->
            receive
                {error,ReqId,Reason} ->
                    %%io:format("v2 error~n"),
                    gen_server:cast(?SERVER, {removeR2PState, ReqId}),
                    {error, timeout};
                {ReqId, Return} -> %%io:format("#####get next Receive#####~nReqId = ~p, Return = ~p~n", [ReqId,Return]), 
                    {ok,Return, Timout}
            after Timout ->
                %%io:format("#####Timeout#####~nReqId = ~p~n", [ReqId]),
                gen_server:cast(?SERVER, {removeR2PState, ReqId}),              %%delete ReqId of genserver's state
                {error, timeout}
            end;
        {error, Reason} -> %%io:format("#####Error#####~nReason = ~p~n", [Reason]), 
            {error, Reason}
    end.

sync_get_next(Addr, Port, Oids) ->
Timout = 10000,
    case call({async_get_next, Addr, Port, Oids}) of
        {ok, ReqId} ->
            receive
                {error,ReqId,Reason} ->
                    %%io:format("v2 error~n"),
                    gen_server:cast(?SERVER, {removeR2PState, ReqId}),
                    {error, timeout};
                {ReqId, Return} -> %%io:format("#####get next Receive#####~nReqId = ~p, Return = ~p~n", [ReqId,Return]), 
                    {ok,Return, Timout}
            after Timout ->
                %%io:format("#####Timeout#####~nReqId = ~p~n", [ReqId]),
                gen_server:cast(?SERVER, {removeR2PState, ReqId}),              %%delete ReqId of genserver's state
                {error, timeout}
            end;
        {error, Reason} -> {error, Reason}
    end.

sync_get_next(Addr, Port, Oids,Timout) ->
    case call({async_get_next, Addr, Port, Oids}) of
        {ok, ReqId} ->
            receive
                {error,ReqId,Reason} ->
                    %%io:format("v2 error~n"),
                    gen_server:cast(?SERVER, {removeR2PState, ReqId}),
                    {error, timeout};
                {ReqId, Return} -> %%io:format("#####get next Receive#####~nReqId = ~p, Return = ~p~n", [ReqId,Return]), 
                    {ok,Return, Timout}
            after Timout ->
                %%io:format("#####Timeout#####~nReqId = ~p~n", [ReqId]),
                gen_server:cast(?SERVER, {removeR2PState, ReqId}),              %%delete ReqId of genserver's state
                {error, timeout}
            end;
        {error, Reason} -> {error, Reason}
    end.

sync_get_bulk(Addr, NR, MR, Oids) ->
    Timout = 10000,
    case call({async_get_bulk, Addr, NR, MR, Oids}) of
        {ok, ReqId} ->
            receive
                {error,ReqId,Reason} ->
                    %%io:format("v2 error~n"),
                    gen_server:cast(?SERVER, {removeR2PState, ReqId}),
                    {error, timeout};
                {ReqId, Return} -> %%io:format("#####get next Receive#####~nReqId = ~p, Return = ~p~n", [ReqId,Return]), 
                    {ok,Return, Timout}
            after Timout ->
                %%io:format("#####Timeout#####~nReqId = ~p~n", [ReqId]),
                gen_server:cast(?SERVER, {removeR2PState, ReqId}),              %%delete ReqId of genserver's state
                {error, timeout}
            end;
        {error, Reason} -> %%io:format("#####Error#####~nReason = ~p~n", [Reason]), 
            {error, Reason}
    end.

sync_get_bulk(Addr, Port, NR, MR, Oids) ->
    Timout = 10000,
    %%io:format("Action~n"),
    case call({async_get_bulk, Addr, Port, NR, MR, Oids}) of
        {ok, ReqId} ->
            %%io:format("ReqId: ~p~n", [ReqId]),
            receive
                {error,ReqId,Reason} ->
                    io:format("Reason: ~p~n", [Reason]),
                    %%io:format("v2 error~n"),
                    gen_server:cast(?SERVER, {removeR2PState, ReqId}),
                    {error, timeout};
                {ReqId, Return} -> %%io:format("#####get next Receive#####~nReqId = ~p, Return = ~p~n", [ReqId,Return]), 
                    {ok,Return, Timout}
            after Timout ->
                %%io:format("#####Timeout#####~nReqId = ~p~n", [ReqId]),
                gen_server:cast(?SERVER, {removeR2PState, ReqId}),              %%delete ReqId of genserver's state
                {error, timeout}
            end;
        {error, Reason} -> %%io:format("#####Error#####~nReason = ~p~n", [Reason]), 
            %%io:format("ErrReason: ~p~n", [Reason]),
            {error, Reason}
    end.
    
sync_get_bulk(UserId, Addr, Port, NR, MR, ContextName, Oids, Expire, ExtraInfo) ->
    Timout = 10000,
    case call({async_get_bulk, UserId, Addr, Port, NR, MR, ContextName, Oids, Expire, ExtraInfo}) of
        {ok, ReqId} ->
            receive
                {error,ReqId,Reason} ->
                    io:format("Reason: ~p~n", [Reason]),
                    gen_server:cast(?SERVER, {removeR2PState, ReqId}),
                    {error, timeout};
                {ReqId, Return} -> 
                    {ok,Return, Timout}
            after Timout ->
                gen_server:cast(?SERVER, {removeR2PState, ReqId}),              %%delete ReqId of genserver's state
                {error, timeout}
            end;
        {error, Reason} -> 
            {error, Reason}
    end.


sync_set(Addr, VarsAndVals) ->
    call({sync_set, Addr, VarsAndVals}).

sync_set(Addr, Port, VarsAndVals) ->
    call({sync_set, Addr, Port, VarsAndVals}).

%%--------------------------------------------------
%% ETS操作，主要是存储全局变量
%%--------------------------------------------------
make_a_set() ->
    Tab = ets:new(?SNMPENTITYETS,[public,named_table]).
    
    
get_attribute(Item) ->
    case ets:lookup(?SNMPENTITYETS, Item) of
        undefined ->
            {error, undefine_item};
        [{Item, Value}] ->
            {ok, Value};
        Other ->
            {error, Other}
    end.
    
set_attribute(Item, Value) ->
    ets:insert(?SNMPENTITYETS,{Item, Value}).
    
delete() ->
    ets:delete(?SNMPENTITYETS).


get_Key(Server, Port, Other) ->
    Key = {snmp_usm_entitys,Server, Port},
    Key.

%%%-------------------------------------------------------------------
%%% Callback functions from gen_server
%%%-------------------------------------------------------------------

init([Parent, Opts]) ->
    %%process_flag(trap_exit, true),
    io:format("start init~n"),
    (catch make_a_set()),
    case (catch do_init(Opts)) of
        {ok, State} ->
            {ok, State#state{parent = Parent}};
        {error, Reason} ->
            {stop, Reason};
	Crap ->
	    {stop, Crap}
    end.

do_init(Opts) ->
    {Dir, MgrConf, MgrOpts} = parse_opts(Opts),
    write_config(Dir, MgrConf),
    start_manager(MgrOpts),
    register_user(),
    {ok, #state{}}.

write_config(Dir, Conf) ->
    case snmp_config:write_manager_config(Dir, "", Conf) of
	ok ->
	    ok;
	Error ->
	    error({failed_writing_config, Error})
    end.

start_manager(Opts) ->
    case snmpm:start_link(Opts) of
	ok ->
	    ok; 
	Error ->
	    error({failed_starting_manager, Error})
    end.


    


parse_opts(Opts) ->
    Port     = get_opt(port,             Opts, 162),
    EngineId = get_opt(engine_id,        Opts, ?ENGINE),
    MMS      = get_opt(max_message_size, Opts, 4840000000),

    MgrConf = [{port,             Port},
               %%{address,          [127,0,0,1]},
               {engine_id,        EngineId},
               {max_message_size, MMS}],
    
    io:format("MgrConf = ~p~n", [MgrConf]),

    %% Manager options
    Mibs      = get_opt(mibs,     Opts, []),
    Vsns      = get_opt(versions, Opts, [v1, v2, v3]),
    {ok, Cwd} = file:get_cwd(),
    Dir       = get_opt(dir, Opts, Cwd++"/conf"),
    MgrOpts   = [{mibs,     Mibs},
		 {versions, Vsns}, 
		 {def_user_mod,?MODULE},
		 {def_user_data,self()},
		 %% {server,   [{verbosity, trace}]}, 
		 {config,   [% {verbosity, trace}, 
			     {dir, Dir}, {db_dir, Dir}]}],
    
    
    {Dir, MgrConf, MgrOpts}.


%%--------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({which_agents_v3, UserId}, _From, S) ->
    Reply = (catch snmpm:which_agents(UserId)),
    Reply,
    {reply, Reply, S};

handle_call({which_usms, Engine}, _From, S) ->
    EngineId = 
    if
        Engine =:= [] ->
            ?ENGINE;
        true ->
            Engine
    end,
    Reply =
	snmpm:which_usm_users(EngineId),
    {reply, Reply, S};

handle_call({update_usm_info, Engine,UserId,Item,Val}, _From, S) ->
    EngineId = 
    if
        Engine =:= [] ->
            ?ENGINE;
        true ->
            Engine
    end,
    User =
    if
        UserId =:= [] ->
            ?USER;
        true ->
            UserId
    end,
    Reply =
	snmpm:update_usm_user_info(EngineId,User,Item,Val),
    {reply, Reply, S};

handle_call({unregister_usm, Engine, UserId}, _From, S) ->
    EngineId = 
    if
        Engine =:= [] ->
            ?ENGINE;
        true ->
            Engine
    end,
    User = 
    if
        UserId =:= [] ->
            ?USER;
        true ->
            UserId
    end,
    Reply =
	case snmpm:unregister_usm_user(EngineId,User) of
        ok->
            ok;
        Error->
            %%error({failed_register_user, Error})
            error
	end,
    {reply, Reply, S};

handle_call({register_usm, Engine, UserId, Conf}, _From, S) ->
    EngineId = 
    if
        Engine =:= [] ->
            ?ENGINE;
        true ->
            Engine
    end,
    User =
    if
        UserId =:= [] ->
            ?USER;
        true ->
            UserId
    end,
    Reply =
	case snmpm:register_usm_user(EngineId,User, Conf) of
        ok->
            ok;
        Error->
            %%error({failed_register_user, Error})
            error
	end,
    {reply, Reply, S};

handle_call({isExist_usm, Engine, UserId, ServerPortId}, _From, S) ->
    EngineId = 
    if
        Engine =:= [] ->
            ?ENGINE;
        true ->
            Engine
    end,
    User = 
    if
        UserId =:= [] ->
            ?USER;
        true ->
            UserId
    end,
    Reply = 
    case snmpm:usm_user_info(EngineId, User, sec_name) of
        {ok, SPStr} ->
            if
                SPStr =:= ServerPortId ->
                    true;
                true ->
                    false
            end;
        _ ->
            false
    end,
    {reply, Reply, S};
    
handle_call({get_usm_user_from_sec_name, EngineID, SecName}, _From, S) ->
    Reply =
    case snmpm_config:get_usm_user_from_sec_name(EngineID,SecName) of
        {ok, User} ->
            {ok, User};
        _ ->
            {error, no_usm_user}
    end,
    {reply, Reply, S};

handle_call({agent, Addr, Conf}, _From, S) ->
    Reply = (catch snmpm:register_agent(?USER, Addr, Conf)),
    {reply, Reply, S};

handle_call({agent, Addr, Port, Conf}, _From, S) ->
    Reply = (catch snmpm:register_agent(?USER, Addr, Port, Conf)),
    {reply, Reply, S};
    
handle_call({unagent, Addr}, _From, S) ->
    Reply = (catch snmpm:unregister_agent(?USER, Addr)),
    {reply, Reply, S};
    
handle_call({unagent, Addr, Port}, _From, S) ->
    Reply = (catch snmpm:unregister_agent(?USER, Addr, Port)),
    {reply, Reply, S};


handle_call({update_agent, Addr,Item, Value}, _From, S) ->
    Reply = (catch snmpm:update_agent_info(?USER, Addr, 161, Item,Value)),
    {reply, Reply, S};

handle_call({update_agent, Addr, Port, Item, Value}, _From, S) ->
    Reply = (catch snmpm:update_agent_info(?USER, Addr, Port, Item,Value)),
    {reply, Reply, S};

handle_call({oid_to_name, Oid}, _From, S) ->
    Reply = (catch snmpm:oid_to_name(Oid)),
    {reply, Reply, S};

handle_call({which_agents, User}, _From, S) ->
    Reply = (catch snmpm:which_agents(User)),
    {reply, Reply, S};

handle_call({agent_v3, UserId, Addr, Conf}, _From, S) ->
    Reply = (catch snmpm:register_agent(UserId, Addr, Conf)),
    {reply, Reply, S};
    
handle_call({agent_v3, UserId, Addr, Port, Conf}, _From, S) ->
    Reply = (catch snmpm:register_agent(UserId, Addr, Port, Conf)),
    {reply, Reply, S};

handle_call({unagent_v3, UserId, Addr}, _From, S) ->
    Reply = (catch snmpm:unregister_agent(UserId, Addr)),
    {reply, Reply, S};
    
handle_call({unagent_v3, UserId, Addr, Port}, _From, S) ->
    Reply = (catch snmpm:unregister_agent(UserId, Addr, Port)),
    {reply, Reply, S};
    
handle_call({get_engineid, Obj, Ver}, _From, S) ->
    Reply =
    case Ver of
		"v3"->
			Obj:v3_config(),
            Val = Obj:g([1,3]),
            case Val of
                {v3hdr,V3Hdr}->
                    {v3hdr,V3Hdr};
                {engineid, EgId} ->
                    {engineid, EgId};
                Ret->
                    ?ENGINE
            end;
		_->
			?ENGINE
	end,
    {reply, Reply, S};
    
handle_call({update_agent_v3, UserId, Addr,Item, Value}, _From, S) ->
    Reply = (catch snmpm:update_agent_info(UserId, Addr, 161, Item,Value)),
    {reply, Reply, S};
    
handle_call({update_agent_v3, UserId, Addr, Port, Item, Value}, _From, S) ->
    Reply = (catch snmpm:update_agent_info(UserId, Addr, Port, Item,Value)),
    {reply, Reply, S};

handle_call({sync_get, Addr, Oids}, _From, S) ->
    Reply = (catch snmpm:g(?USER, Addr, Oids)),
    {reply, Reply, S};

handle_call({sync_get, Addr, Port, Oids}, _From, S) ->
    Reply = (catch snmpm:g(?USER, Addr, Port, Oids)),
    {reply, Reply, S};
    
handle_call({async_get, Addr, Oids}, _From, S) ->
    Expire = 5000,
    Reply = (catch snmpm:ag(?USER, Addr, Oids, Expire)),
    case Reply of
        {ok, ReqId} -> 
            NewReply = Reply,
            IsAlive = proplists:is_defined(ReqId, S#state.reqId2Pid),
            if 
                IsAlive == true ->
                    State = S#state{reqId2Pid = proplists:delete(ReqId, S#state.reqId2Pid) ++ [{ReqId, _From}]};
                    %%io:format("************ReqId = ~p~n", [ReqId]), io:format("true*************reqId2Pid = ~p", [State#state.reqId2Pid]);
                IsAlive == false ->
                    State = S#state{reqId2Pid = S#state.reqId2Pid ++ [{ReqId, _From}]};
                    %%io:format("************ReqId = ~p~n", [ReqId]), io:format("false*************reqId2Pid = ~p", [State#state.reqId2Pid]);
                true ->
                    State = S
            end;
        {error, Reason} -> 
            NewReply = {error, Reason}, %%io:format("************Async Request Error Reason= ~p~n", [Reason]),
            State = S;
        _All -> 
            NewReply = {error, "other error occured"},
            State = S
    end,
    {reply, NewReply, State};
    
handle_call({async_get, Addr, Port, Oids}, _From, S) ->
    Expire = 5000,
    Reply = (catch snmpm:ag(?USER, Addr, Port, Oids, Expire)),
    case Reply of
        {ok, ReqId} -> 
            NewReply = Reply,
            IsAlive = proplists:is_defined(ReqId, S#state.reqId2Pid),
            if 
                IsAlive == true ->
                    State = S#state{reqId2Pid = proplists:delete(ReqId, S#state.reqId2Pid) ++ [{ReqId, _From}]};
                    %%io:format("************ReqId = ~p~n", [ReqId]), io:format("true*************reqId2Pid = ~p", [State#state.reqId2Pid]);
                IsAlive == false ->
                    State = S#state{reqId2Pid = S#state.reqId2Pid ++ [{ReqId, _From}]};
                    %%io:format("************ReqId = ~p~n", [ReqId]), io:format("false*************reqId2Pid = ~p", [State#state.reqId2Pid]);
                true ->
                    State = S
            end;
        {error, Reason} -> 
            NewReply = {error, Reason}, %%io:format("************Async Request Error Reason= ~p~n", [Reason]),
            State = S;
        _All -> 
            NewReply = {error, "other error occured"},
            State = S
    end,
    {reply, NewReply, State};
    
handle_call({async_get_v3, UserId, Addr, NPort, ContextName, Oids, TimeOut, ExtraInfo,Session}, _From, S) ->
    Reply = (catch snmpm:ag(UserId, Addr, NPort, ContextName, Oids, TimeOut, ExtraInfo)),
    %%Reply = (catch snmp_ifc_lock:async_get_v3(UserId, Addr, NPort, ContextName, Oids, TimeOut, ExtraInfo, Session)),
    case Reply of
        {ok, ReqId} -> 
            NewReply = Reply,
            IsAlive = proplists:is_defined(ReqId, S#state.reqId2Pid),
            if 
                IsAlive == true ->
                    State = S#state{reqId2Pid = proplists:delete(ReqId, S#state.reqId2Pid) ++ [{ReqId, _From}]};
                    %%io:format("************ReqId = ~p~n", [ReqId]), io:format("true*************reqId2Pid = ~p", [State#state.reqId2Pid]);
                IsAlive == false ->
                    State = S#state{reqId2Pid = S#state.reqId2Pid ++ [{ReqId, _From}]};
                    %%io:format("************ReqId = ~p~n", [ReqId]), io:format("false*************reqId2Pid = ~p", [State#state.reqId2Pid]);
                true ->
                    State = S
            end;
        {error, Reason} -> 
            NewReply = {error, Reason}, %%io:format("************Async Request Error Reason= ~p~n", [Reason]),
            State = S;
        _All -> 
            NewReply = {error, "other error occured"},
            State = S
    end,
    {reply, NewReply, State};

handle_call({sync_get_next, Addr, Oids}, _From, S) ->
    Reply = (catch snmpm:gn(?USER, Addr, Oids)),
    {reply, Reply, S};

handle_call({sync_get_next, Addr, Port, Oids}, _From, S) ->
    Reply = (catch snmpm:gn(?USER, Addr, Port, Oids)),
    {reply, Reply, S};
    
handle_call({async_get_next, Addr, Oids}, _From, S) ->
    Reply = (catch snmpm:agn(?USER, Addr, Oids)),
    case Reply of
        {ok, ReqId} -> 
            NewReply = Reply,
            IsAlive = proplists:is_defined(ReqId, S#state.reqId2Pid),
            if 
                IsAlive == true ->
                    State = S#state{reqId2Pid = proplists:delete(ReqId, S#state.reqId2Pid) ++ [{ReqId, _From}]};
                    %%io:format("************ReqId = ~p~n", [ReqId]), io:format("true*************reqId2Pid = ~p", [State#state.reqId2Pid]);
                IsAlive == false ->
                    State = S#state{reqId2Pid = S#state.reqId2Pid ++ [{ReqId, _From}]};
                    %%io:format("************ReqId = ~p~n", [ReqId]), io:format("false*************reqId2Pid = ~p", [State#state.reqId2Pid]);
                true ->
                    State = S#state{reqId2Pid = S#state.reqId2Pid ++ [{ReqId, _From}]}
            end;
        {error, Reason} -> 
            NewReply = {error, Reason}, %%io:format("************Async Request Error Reason= ~p~n", [Reason]),
            State = S;
        _All -> 
            NewReply = {error, "other error occured"},
            State = S
    end,
    {reply, NewReply, State};

handle_call({async_get_next, Addr, Port, Oids}, _From, S) ->
    Reply = (catch snmpm:agn(?USER, Addr, Port, Oids)),
    case Reply of
        {ok, ReqId} -> 
            NewReply = Reply,
            IsAlive = proplists:is_defined(ReqId, S#state.reqId2Pid),
            if 
                IsAlive == true ->
                    State = S#state{reqId2Pid = proplists:delete(ReqId, S#state.reqId2Pid) ++ [{ReqId, _From}]};
                    %%io:format("************ReqId = ~p~n", [ReqId]), io:format("true*************reqId2Pid = ~p", [State#state.reqId2Pid]);
                IsAlive == false ->
                    State = S#state{reqId2Pid = S#state.reqId2Pid ++ [{ReqId, _From}]};
                    %%io:format("************ReqId = ~p~n", [ReqId]), io:format("false*************reqId2Pid = ~p", [State#state.reqId2Pid]);
                true ->
                    State = S#state{reqId2Pid = S#state.reqId2Pid ++ [{ReqId, _From}]}
            end;
        {error, Reason} -> 
            NewReply = {error, Reason}, %%io:format("************Async Request Error Reason= ~p~n", [Reason]),
            State = S;
        _All -> 
            NewReply = {error, "other error occured"},
            State = S
    end,
    {reply, NewReply, State};
    
handle_call({async_get_next_v3, UserId, Addr, NPort, ContextName, Oids, TimeOut, ExtraInfo}, _From, S) ->
    Reply = (catch snmpm:agn(UserId, Addr, NPort, ContextName, Oids, TimeOut, ExtraInfo)),
    case Reply of
        {ok, ReqId} -> 
            NewReply = Reply,
            IsAlive = proplists:is_defined(ReqId, S#state.reqId2Pid),
            if 
                IsAlive == true ->
                    State = S#state{reqId2Pid = proplists:delete(ReqId, S#state.reqId2Pid) ++ [{ReqId, _From}]};
                    %%io:format("************ReqId = ~p~n", [ReqId]), io:format("true*************reqId2Pid = ~p", [State#state.reqId2Pid]);
                IsAlive == false ->
                    State = S#state{reqId2Pid = S#state.reqId2Pid ++ [{ReqId, _From}]};
                    %%io:format("************ReqId = ~p~n", [ReqId]), io:format("false*************reqId2Pid = ~p", [State#state.reqId2Pid]);
                true ->
                    State = S
            end;
        {error, Reason} -> 
            NewReply = {error, Reason}, %%io:format("************Async Request Error Reason= ~p~n", [Reason]),
            State = S;
        _All -> 
            NewReply = {error, "other error occured"},
            State = S
    end,
    {reply, NewReply, State};
    
handle_call({async_get_bulk_v3, UserId, Addr, Port, ContextName, Oids, TimeOut, ExtraInfo, NR, MR}, _From, S) ->
    Reply = (catch snmpm:agb(UserId, Addr, Port, NR, MR, ContextName, Oids, TimeOut, ExtraInfo)),
    case Reply of
        {ok, ReqId} -> 
            NewReply = Reply,
            IsAlive = proplists:is_defined(ReqId, S#state.reqId2Pid),
            if 
                IsAlive == true ->
                    State = S#state{reqId2Pid = proplists:delete(ReqId, S#state.reqId2Pid) ++ [{ReqId, _From}]};
                IsAlive == false ->
                    State = S#state{reqId2Pid = S#state.reqId2Pid ++ [{ReqId, _From}]};
                true ->
                    State = S#state{reqId2Pid = S#state.reqId2Pid ++ [{ReqId, _From}]}
            end;
        {error, Reason} -> 
            NewReply = {error, Reason}, %%io:format("************Async Request Error Reason= ~p~n", [Reason]),
            State = S;
        _All -> 
            NewReply = {error, "other error occured"},
            State = S
    end,
    %%Reply = (catch snmpm:gb(?USER, Addr, NR, MR, Oids)),
    {reply, NewReply, State};
    

handle_call({async_get_bulk, Addr, NR, MR, Oids}, _From, S) ->
    io:format("****Addr = ~p~n", [Addr]),
    io:format("****NR = ~p~n", [NR]),
    io:format("****MR = ~p~n", [MR]),
    io:format("****Oids = ~p~n", [Oids]),
    Reply = (catch snmpm:agb(?USER, Addr, NR, MR, Oids)),
    case Reply of
        {ok, ReqId} -> 
            NewReply = Reply,
            IsAlive = proplists:is_defined(ReqId, S#state.reqId2Pid),
            if 
                IsAlive == true ->
                    State = S#state{reqId2Pid = proplists:delete(ReqId, S#state.reqId2Pid) ++ [{ReqId, _From}]};
                IsAlive == false ->
                    State = S#state{reqId2Pid = S#state.reqId2Pid ++ [{ReqId, _From}]};
                true ->
                    State = S#state{reqId2Pid = S#state.reqId2Pid ++ [{ReqId, _From}]}
            end;
        {error, Reason} -> 
            NewReply = {error, Reason}, %%io:format("************Async Request Error Reason= ~p~n", [Reason]),
            State = S;
        _All -> 
            NewReply = {error, "other error occured"},
            State = S
    end,
    %%Reply = (catch snmpm:gb(?USER, Addr, NR, MR, Oids)),
    {reply, NewReply, State};

handle_call({async_get_bulk, Addr, Port, NR, MR, Oids}, _From, S) ->
    %%io:format("****Addr = ~p~n", [Addr]),
    %%io:format("****Port = ~p~n", [Port]),
    %%io:format("****NR = ~p~n", [NR]),
    %%io:format("****MR = ~p~n", [MR]),
    %%io:format("****Oids = ~p~n", [Oids]),
    Reply = (catch snmpm:agb(?USER, Addr, Port, NR, MR, Oids)),
    %%io:format("Reply: ~p~n", [Reply]),
    case Reply of
        {ok, ReqId} -> 
            NewReply = Reply,
            IsAlive = proplists:is_defined(ReqId, S#state.reqId2Pid),
            if 
                IsAlive == true ->
                    State = S#state{reqId2Pid = proplists:delete(ReqId, S#state.reqId2Pid) ++ [{ReqId, _From}]};
                IsAlive == false ->
                    State = S#state{reqId2Pid = S#state.reqId2Pid ++ [{ReqId, _From}]};
                true ->
                    State = S#state{reqId2Pid = S#state.reqId2Pid ++ [{ReqId, _From}]}
            end;
        {error, Reason} -> 
            NewReply = {error, Reason}, %%io:format("************Async Request Error Reason= ~p~n", [Reason]),
            State = S;
        _All -> 
            NewReply = {error, "other error occured"},
            State = S
    end,
    %%Reply = (catch snmpm:gb(?USER, Addr, Port, NR, MR, Oids)),
    {reply, NewReply, State};
    
handle_call({async_get_bulk, UserId, Addr, Port, NR, MR, ContextName, Oids, Expire, ExtraInfo}, _From, S) ->
    Reply = (catch snmpm:agb(UserId, Addr, Port, NR, MR, ContextName, Oids, Expire, ExtraInfo)),
    case Reply of
        {ok, ReqId} -> 
            NewReply = Reply,
            IsAlive = proplists:is_defined(ReqId, S#state.reqId2Pid),
            if 
                IsAlive == true ->
                    State = S#state{reqId2Pid = proplists:delete(ReqId, S#state.reqId2Pid) ++ [{ReqId, _From}]};
                    %%io:format("************ReqId = ~p~n", [ReqId]), io:format("true*************reqId2Pid = ~p", [State#state.reqId2Pid]);
                IsAlive == false ->
                    State = S#state{reqId2Pid = S#state.reqId2Pid ++ [{ReqId, _From}]};
                    %%io:format("************ReqId = ~p~n", [ReqId]), io:format("false*************reqId2Pid = ~p", [State#state.reqId2Pid]);
                true ->
                    State = S
            end;
        {error, Reason} -> 
            NewReply = {error, Reason}, %%io:format("************Async Request Error Reason= ~p~n", [Reason]),
            State = S;
        _All -> 
            NewReply = {error, "other error occured"},
            State = S
    end,
    {reply, NewReply, State};

handle_call({sync_set, Addr, VarsAndVals}, _From, S) ->
    Reply = (catch snmpm:s(?USER, Addr, VarsAndVals)),
    {reply, Reply, S};

handle_call({sync_set, Addr, Port, VarsAndVals}, _From, S) ->
    Reply = (catch snmpm:s(?USER, Addr, Port, VarsAndVals)),
    {reply, Reply, S};

handle_call({set_trap_handle, M}, _From, S) ->
	case lists:member(M,S#state.trap) of
		true->
			{reply,{ok,already_add},S};
		false->
			{reply,{ok,add_ok},S#state{trap=S#state.trap ++ [M]}}
	end;

handle_call({remove_trap_handle, M}, _From, S) ->
	case lists:member(M,S#state.trap) of
		true->
			
			{reply,{ok,remove_ok},S#state{trap=lists:subtract(S#state.trap,[M])}};
		false->
			{reply,{ok,not_exist},S}
	end;

handle_call(Req, From, State) ->
    error_msg("received unknown request ~n~p~nFrom ~p", [Req, From]),
    {reply, {error, {unknown_request, Req}}, State}.

%% 如果为列表的话，如果结尾为0,那么就去掉0才正确。
getOctetString(Value)->
    case erlang:is_list(Value) of
        true ->
            if
                Value =:= [] ->
                    null;
                true ->
                    case lists:last(Value) of
                        0->
                            string:substr(Value,1,length(Value)-1);
                        _->
                            Value
                    end
            end;
        _ ->
            Value
    end.

%% 任何类型转化为字符串
any_to_list(AnyType) ->
    if
        is_tuple(AnyType) == true ->
            tuple_to_list(AnyType);
        is_integer(AnyType) == true ->
            integer_to_list(AnyType);
        is_float(AnyType) == true ->
            float_to_list(AnyType);
        is_list(AnyType) == true ->
            case is_string(AnyType) of
                true ->
                    AnyType;
                _ ->
                    oid2dots(AnyType)
            end;
        is_atom(AnyType) ->
            atom_to_list(AnyType);
        is_binary(AnyType) ->
            binary_to_list(AnyType);
        is_bitstring(AnyType) ->
            io:format("is bitstring~n"),
            bitstring_to_list(AnyType);
        is_boolean(AnyType) ->
            atom_to_list(AnyType);
        is_pid(AnyType) ->
            pid_to_list(AnyType);
        is_port(AnyType) ->
            erlang:port_to_list(AnyType);
        %%is_record(AnyType) ->
        %%    AnyType;
        %%is_reference(AnyType) ->
        %%    AnyType;
        true ->
            AnyType
    end.

%%--------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(stop, S) ->
    (catch snmpm:stop()),
    {stop, normal, S};
    
handle_cast({removeR2PState, ReqId}, S) ->
    IsAlive = proplists:is_defined(ReqId, S#state.reqId2Pid),
    if 
        IsAlive == true ->
            State = S#state{reqId2Pid = proplists:delete(ReqId, S#state.reqId2Pid)};
        true ->
            State = S
    end,
    {noreply, State};

handle_cast(Msg, State) ->
    error_msg("received unknown message ~n~p", [Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({snmp_callback, Tag, Info}, State) ->
	%%io:format("handle_info(snmp_callback):~p~n",[Info]),
    %%io:format("Tag:~p~n",[Tag]),
    case handle_snmp_callback(Tag, Info,State) of
		{ok,S}->
			{noreply, S};
		_->
			{noreply,State}
	end;

handle_info(Info, State) ->
    error_msg("received unknown info: "
              "~n   Info: ~p", [Info]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


code_change({down, _Vsn}, State, _Extra) ->
    {ok, State};

% upgrade
code_change(_Vsn, State, _Extra) ->
    {ok, State}.


do_trap([],Trap)->
    K = snmp_trap_base:new(),
    K:save_log(Trap),
    [];
do_trap([M|T],Trap)->
	try
		C = M:handle_trap(M,Trap),
		[M]
	catch
	_:Error->
		?MODULE:remove_trap_handle(M),
		[]
	end ++ do_trap(T,Trap).

%% ========================================================================
%% ========================================================================

handle_snmp_callback(handle_error, {ReqId, Reason},State) ->
    %%io:format("error ReqId = ~p~n", [ReqId]),
    P = proplists:get_value(ReqId, State#state.reqId2Pid),
    IsAlive = proplists:is_defined(ReqId, State#state.reqId2Pid),
    if
        IsAlive == true ->
            NewState = State#state{reqId2Pid = proplists:delete(ReqId, State#state.reqId2Pid)};
        true ->
            NewState = State
    end,
    case P of
        {Pid, _} ->
            if
                erlang:is_pid(Pid) ->
                    %%io:format("send to Pid~n"),
                    Pid ! {error,ReqId,Reason};
                true ->
                    %%io:format("not a Pid~n"),
                    do_nothing
            end;
        _ ->
            %%io:format("not Pid = ~p~n", [P]),
            do_nothing
    end,
    %%io:format("*** FAILURE ***"
	%%      "~n   Request Id: ~p"
	%%      "~n   Reason:     ~p"
	%%      "~n", [ReqId, Reason]),
    {ok, NewState};
handle_snmp_callback(handle_agent, {Addr, Port, SnmpInfo},_) ->
    {ES, EI, VBs} = SnmpInfo, 
    %%io:format("*** UNKNOWN AGENT ***"
	%%      "~n   Address:   ~p"
	%%      "~n   Port:      ~p"
	%%      "~n   SNMP Info: "
	%%      "~n     Error Status: ~w"
	%%      "~n     Error Index:  ~w"
	%%      "~n     Varbinds:     ~p"
	%%      "~n", [Addr, Port, ES, EI, VBs]),
    ok;
handle_snmp_callback(handle_pdu, {Addr, Port, ReqId, SnmpResponse},State) ->
    %%{ES, EI, VBs} = SnmpResponse, 
    P = proplists:get_value(ReqId, State#state.reqId2Pid),
    %%io:format("P = ~p~n", [P]),
    IsAlive = proplists:is_defined(ReqId, State#state.reqId2Pid),
    if
        IsAlive == true ->
            NewState = State#state{reqId2Pid = proplists:delete(ReqId, State#state.reqId2Pid)};
        true ->
            NewState = State
    end,
    %%io:format("*** Received PDU ***"
	%%      "~n   Address:       ~p"
	%%      "~n   Port:          ~p"
	%%      "~n   Request Id:    ~p"
	%%      "~n   SNMP response:"
	%%      "~n     Error Status: ~w"
	%%      "~n     Error Index:  ~w"
	%%      "~n     Varbinds:     ~p"
    %%      "~n     NewState:     ~p"
    %%      "~n     Pid:     ~p"
	%%      "~n", [Addr, Port, ReqId, ES, EI, VBs, NewState#state.reqId2Pid, Pid]),
    case P of
        {Pid, _} ->
            Pid ! {ReqId,SnmpResponse};
        _ ->
            io:format("ReqId: ~p~n", [ReqId]),
            io:format("State#state.reqId2Pid: ~p~n", [State#state.reqId2Pid]),
            ok
    end,
    {ok, NewState};
handle_snmp_callback(handle_trap, {Addr, Port, SnmpTrap},S) ->
	rpc:cast(node(),?MODULE,do_trap,[S#state.trap,{Addr,Port,SnmpTrap}]),
    {ok,S};

handle_snmp_callback(handle_inform, {Addr, Port, SnmpInform},_) ->
    {ES, EI, VBs} = SnmpInform, 
    %%io:format("*** Received INFORM ***"
	%%      "~n   Address:     ~p"
	%%      "~n   Port:        ~p"
	%%      "~n   SNMP inform: "
	%%      "~n     Error Status: ~w"
	%%      "~n     Error Index:  ~w"
	%%      "~n     Varbinds:     ~p"
	%%      "~n", [Addr, Port, ES, EI, VBs]),
    ok;
handle_snmp_callback(handle_report, {Addr, Port, SnmpReport},_) ->
    {ES, EI, VBs} = SnmpReport, 
    %%io:format("*** Received REPORT ***"
	%%      "~n   Address:   ~p"
	%%      "~n   Port:      ~p"
	%%      "~n   SNMP report: "
	%%      "~n     Error Status: ~w"
	%%      "~n     Error Index:  ~w"
	%%      "~n     Varbinds:     ~p"
	%%      "~n", [Addr, Port, ES, EI, VBs]),
    ok;
handle_snmp_callback(BadTag, Crap,_) ->
    %%io:format("*** Received crap ***"
	%%      "~n   ~p"
	%%      "~n   ~p"
	%%      "~n", [BadTag, Crap]),
    ok.
    


error(Reason) ->
    throw({error, Reason}).


error_msg(F, A) ->
    catch error_logger:error_msg("*** TEST-MANAGER: " ++ F ++ "~n", A).


call(Req) ->
    gen_server:call(?SERVER, Req, infinity).

cast(Msg) ->
    gen_server:cast(?SERVER, Msg).


%% ========================================================================
%% Misc internal utility functions
%% ========================================================================

get_opt(Key, Opts, Def) ->
    case lists:keysearch(Key, 1, Opts) of
        {value, {Key, Val}} ->
            Val;
        false ->
            Def
    end.
    
oid2dots([])->"";
oid2dots([H|T])->
	case T of
		[]->
			integer_to_list(H);
		_->
			integer_to_list(H) ++ "." ++ oid2dots(T)
	end.

is_string([]) ->
    true;
is_string([H|T]) ->
    if
        H>=1, H=<127 ->
            is_string(T);
        true ->
            false
    end.
    
%%is_string(Str) ->
%%%    UpStr = string:to_upper(Str),
%%    LoStr = string:to_lower(Str),
%%    Up_isstr = UpStr =:= Str,
%%    Lo_isstr = LoStr =:= Str,
%%    if
%%(Up_isstr and Lo_isstr) ->
%%            false;
%%        true ->
%%            true
%%    end.
    

%% ========================================================================
%% SNMPM user callback functions
%% ========================================================================

handle_error(ReqId, Reason, Server)-> %%when is_pid(Server) ->
	%%io:format("handle_error:~p,~p,~p~n",[ReqId, Reason, Server]),
    {RId, Rson} = 
    case Reason of
        {_,{_,usmStatsNotInTimeWindows,[{requestid,ReqqId}, {v3hdr, V3Hdr}]}} ->
            {ReqqId, {usmStatsNotInTimeWindows, V3Hdr}};
        {_,{_,usmStatsUnknownEngineIDs,[{requestid,ReqqId}, {v3hdr, V3Hdr}]}} ->
            {ReqqId, {v3hdr, V3Hdr}};
        {_,{_, notInTimeWindow, Info}}
            when is_list(Info) ->
                case lists:keysearch(requestid, 1, Info) of
                    {value, {requestid,RIds}} ->
                        {RIds, {notInTimeWindow, Info}};
                    _ ->
                        {ReqId, Reason}
                end;
        {_,[{sec_engine_id,_,EgId},_,_],_} ->
            {ReqId, {usmStatsUnknownEngineIDs, EgId}};
        {_,{_,_,{requestid,ReqqId}}} ->
            {ReqqId, Reason};
        {_,{_,R}} ->
            {ReqId, R};
        {invalid_sec_info, SecInfo, SnmpInfo} ->
            if
                erlang:is_list(SecInfo) ->
                    case lists:keysearch(sec_engine_id, 1, SecInfo) of
                        {value, {sec_engine_id,_,EgsId}} ->
                            {ReqId, {usmStatsUnknownEngineIDs, EgsId}};
                        _ ->
                            {ReqId, Reason}
                    end;
                true ->
                    {ReqId, Reason}
            end;
        _ ->
            {ReqId, Reason}
    end,
    report_callback(Server, handle_error, {RId, Rson}),
    ignore.


handle_agent(Addr, Port, SnmpInfo, Server) -> %% when is_pid(Server) ->
	%%io:format("handle_agent:~p,~p,~p~n",[Addr, SnmpInfo, Server]),
    report_callback(Server, handle_agent, {Addr, Port, SnmpInfo}),
    ignore.


handle_pdu(Addr, Port, ReqId, SnmpResponse, Server)-> %% when is_pid(Server) ->
	%%io:format("handle_pdu:~p,~p,~p~n",[Addr, ReqId, Server]),
    report_callback(Server, handle_pdu, {Addr, Port, ReqId, SnmpResponse}),
    ignore.


handle_trap(Addr, Port, SnmpTrap, Server) -> %% when is_pid(Server) ->
	%%io:format("handle_trap:~p,~p,~p~n",[Addr, SnmpTrap, Server]),
    report_callback(Server, handle_trap, {Addr, Port, SnmpTrap}),
    ok.

handle_inform(Addr, Port, SnmpInform, Server)-> %% when is_pid(Server) ->
	%%io:format("handle_inform:~p,~p,~p~n",[Addr, SnmpInform, Server]),
    report_callback(Server, handle_inform, {Addr, Port, SnmpInform}),
    ok.


handle_report(Addr, Port, SnmpReport, Server)-> %% when is_pid(Server) ->
	%%io:format("handle_report:~p,~p,~p~n",[Addr, SnmpReport, Server]),
    report_callback(Server, handle_inform, {Addr, Port, SnmpReport}),
    ok.

report_callback(Pid, Tag, Info) ->
    %%io:format("************Manager = ~p~n", [self()]),
    Pid ! {snmp_callback, Tag, Info}.
