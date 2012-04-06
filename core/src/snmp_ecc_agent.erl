-module(snmp_ecc_agent).
-behaviour(gen_server).
-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
-define(SERVER, ?MODULE).
-record(state, {parent, master_agent}).

init([Parent, Opts]) ->
    %%process_flag(trap_exit, true),
    case (catch do_init(Opts)) of
        {ok, State} ->
            {ok, State#state{parent = Parent}};
        {error, Reason} ->
            {stop, Reason};
	Crap ->
	    {stop, Crap}
    end.

do_init(Opts) ->
    (catch snmp:start()),
    {Dir, MgrOpts} = parse_opts(Opts),
    write_config(Dir),
    Master_ag = 
    case start_agent(MgrOpts) of
        {ok, Ag} ->
            Ag;
        _ ->
            self()
    end,
    {ok, #state{master_agent = Master_ag}}.

write_config(Dir) ->
    {ok, Hosts} = inet:gethostname(),
    {ok, Address} = inet:getaddr(Hosts, inet),
    case snmp_config:write_agent_snmp_conf(Dir, Address, 4000, "ecc_snmp_engines", 484) of
	ok ->
	    ok;
	Error ->
	    error
    end.

start_agent(Opts) ->
    case application:set_env(snmp, agent, Opts)  of
	ok ->
	    Ag = snmp:start_agent(),
        {ok, Ag};
	Error ->
	    error
    end.

parse_opts(Opts) ->
    Mibs      = get_opt(mibs,     Opts, []),
    Vsns      = get_opt(versions, Opts, [v1, v2, v3]),
    {ok, Cwd} = file:get_cwd(),
    Dir       = get_opt(dir, Opts, Cwd++"/conf/snmp_agent"),
    MgrOpts   = [{mibs,     Mibs},
		 {versions, Vsns}, 
		 {def_user_mod,?MODULE},
		 {def_user_data,self()},
		 {config,   [% {verbosity, trace}, 
			     {dir, Dir}, {db_dir, Dir}]},
         {db_dir, Dir},
         {dir, Dir}],
    {Dir, MgrOpts}.

get_opt(Key, Opts, Def) ->
    case lists:keysearch(Key, 1, Opts) of
        {value, {Key, Val}} ->
            Val;
        false ->
            Def
    end.
    
start_link() ->
    start_link([]).

start_link(Opts) when is_list(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self(), Opts], []).
    
stop() ->
    cast(stop).
    
%% ----------------------------------------------------------------
%% 其他接口
%% ----------------------------------------------------------------
add_addr(Name, Ip, Port, Timeout, Retry, TagList, Params, EngineId, TMask, MMS) ->
    snmp_target_mib:add_addr(Name, Ip, Port, Timeout, Retry, TagList, Params, EngineId, TMask, MMS).    

add_notify(Name, Tag, Type) ->
    snmp_notification_mib:add_notify(Name, Tag, Type).
    
add_params(Name, MPModel, SecModel, SecName, SecLevel) ->
    snmp_target_mib:add_params(Name, MPModel, SecModel, SecName, SecLevel).
    
add_community(Idx, CommName, SecName, CtxName, TransportTag) ->
    snmp_community_mib:add_community(Idx, CommName, SecName, CtxName, TransportTag).

add_sec2group(SecModel, SecName, GroupName) ->
    snmp_view_based_acm_mib:add_sec2group(SecModel, SecName, GroupName).
    
add_access(GroupName, Prefix, SecModel, SecLevel, Match, RV, WV, NV) ->
    snmp_view_based_acm_mib:add_access(GroupName, Prefix, SecModel, SecLevel, Match, RV, WV, NV).
    
    
%%-------------------------------------------------------------------

send_trap(Agent, Trap, Community, Varbinds) ->
    call({send_trap, Agent, Trap, Community, Varbinds}).

send_notification(Agent, Notification, Receiver, NotifyName, ContextName, Varbinds) ->
    call({send_notification,Agent, Notification, Receiver, NotifyName, ContextName, Varbinds}).
    

handle_call({send_trap, Agent, Trap, Community, Varbinds}, _From, S) ->
    Reply = snmp:send_trap(Agent, Trap, Community, Varbinds),
    {reply, Reply, S};

handle_call({send_notification,Agent, Notification, Receiver, NotifyName, ContextName, Varbinds}, _From, S) ->
    Reply = 
    try snmp:send_notification(Agent, Notification, Receiver, NotifyName, ContextName, Varbinds) of
        R ->
            {ok, success}
    catch
        _:Reason ->
            {error, Reason}
    end,
    {reply, Reply, S};

handle_call(Req, From, State) ->
    {reply, {error, {unknown_request, Req}}, State}.
    
handle_cast(Msg, State) ->
    {noreply, State}.
    
handle_info(Info, State) ->
    {noreply, State}.
    
terminate(_Reason, _State) ->
    ok.


code_change({down, _Vsn}, State, _Extra) ->
    {ok, State};

% upgrade
code_change(_Vsn, State, _Extra) ->
    {ok, State}.
    
call(Req) ->
    gen_server:call(?SERVER, Req, infinity).

cast(Msg) ->
    gen_server:cast(?SERVER, Msg).