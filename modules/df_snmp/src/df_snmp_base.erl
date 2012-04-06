-module(df_snmp_base).

-behaviour(gen_server).
-behaviour(snmpm_user).

-export([start_link/0, start_link/1, stop/0,astop/0,
	 agent/2, agent/3, which_agents/0,update_agent/3,update_agent/4,
	 register_usm/1,register_usm/2,which_usms/0,which_usms/1,update_usm_info/2,update_usm_info/3,
	 set_trap_handle/1,remove_trap_handle/1,do_trap/2,
         sync_get/2,      sync_get/3,      sync_get/4,
         async_get/2,      async_get/3,      async_get/4,
         sync_get_next/2, sync_get_next/3, sync_get_next/4,
         async_get_next/2, async_get_next/3, async_get_next/4,
         sync_get_bulk/4, sync_get_bulk/5,
         sync_set/2,      sync_set/3,

	 oid_to_name/1
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


-define(SERVER,   'elnnm_snmp_manager').
-define(USER,     'elnnm_snmp_user').
-define(USER_MOD, ?MODULE).
-define(ENGINE,"mgrEngine").
-record(state, {parent,trap=[]}).


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link() ->
    start_link([]).

start_link(Opts) when is_list(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self(), Opts], []).

stop() ->
    call(stop).
astop() ->
    cast(stop),
    wait(100).
wait(Time) ->
    receive
        after Time -> ok
    end.


%% --- Instruct manager to handle an agent ---

agent(Addr, Conf) ->
    call({agent, Addr, Conf}).

agent(Addr, Port, Conf) ->
    call({agent, Addr, Port, Conf}).

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

%% --- Various SNMP operations ----

sync_get(Addr, Oids) ->
    call({sync_get, Addr, Oids}).
    
sync_get(Addr, Oids, Timeout) ->
    call({sync_get, Addr, Oids, Timeout}).

sync_get(Addr, Port, Oids, Timeout) ->
    call({sync_get, Addr, Port, Oids, Timeout}).
    
async_get(Addr, Oids) ->
    call({async_get, Addr, Oids}).
    
async_get(Addr, Oids, Timeout) ->
    call({async_get, Addr, Oids, Timeout}).

async_get(Addr, Port, Oids, Timeout) ->
    call({async_get, Addr, Port, Oids, Timeout}).
    
sync_get_next(Addr, Oids) ->
    call({sync_get_next, Addr,Oids}).

sync_get_next(Addr, Oids, Timeout) ->
    call({sync_get_next, Addr, Oids, Timeout}).
    
sync_get_next(Addr, Port, Oids, Timeout) ->
    call({sync_get_next, Addr, Port, Oids, Timeout}).


async_get_next(Addr, Oids) ->
    call({async_get_next, Addr,Oids}).

async_get_next(Addr, Oids, Timeout) ->
    call({async_get_next, Addr, Oids, Timeout}).
    
async_get_next(Addr, Port, Oids, Timeout) ->
    call({async_get_next, Addr, Port, Oids, Timeout}).


sync_get_bulk(Addr, NR, MR, Oids) ->
    call({sync_get_bulk, Addr, NR, MR, Oids}).

sync_get_bulk(Addr, Port, NR, MR, Oids) ->
    call({sync_get_bulk, Addr, Port, NR, MR, Oids}).


sync_set(Addr, VarsAndVals) ->
    call({sync_set, Addr, VarsAndVals}).

sync_set(Addr, Port, VarsAndVals) ->
    call({sync_set, Addr, Port, VarsAndVals}).


%%%-------------------------------------------------------------------
%%% Callback functions from gen_server
%%%-------------------------------------------------------------------

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

register_user() ->
    case snmpm:register_user(?USER, ?USER_MOD, self()) of
	ok ->
	    ok;
	Error ->
	    error({failed_register_user, Error})
    end.

register_usm(Conf)->
	case snmpm:register_usm_user(?ENGINE,?USER, Conf) of
	ok->
		ok;
	Error->
		error({failed_register_user, Error})
	end.

register_usm(Engine,Conf)->
	case snmpm:register_usm_user(Engine,?USER, Conf) of
	ok->
		ok;
	Error->
		error({failed_register_user, Error})
	end.

update_usm_info(Item,Val)->
	snmpm:update_usm_user_info(?ENGINE,?USER,Item,Val).

update_usm_info(Engine,Item,Val)->
	snmpm:update_usm_user_info(Engine,?USER,Item,Val).

which_usms()->
	snmpm:which_usm_users(?ENGINE).

which_usms(Engine)->
	snmpm:which_usm_users(Engine).

parse_opts(Opts) ->
    Port     = get_opt(port,             Opts, 162),
    EngineId = get_opt(engine_id,        Opts, ?ENGINE),
    MMS      = get_opt(max_message_size, Opts, 484),

    MgrConf = [{port,             Port},
               {engine_id,        EngineId},
               {max_message_size, MMS}],

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

handle_call({agent, Addr, Conf}, _From, S) ->
    Reply = (catch snmpm:register_agent(?USER, Addr, Conf)),
    {reply, Reply, S};

handle_call({agent, Addr, Port, Conf}, _From, S) ->
    Reply = (catch snmpm:register_agent(?USER, Addr, Port, Conf)),
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

handle_call({sync_get, Addr, Oids}, _From, S) ->
    Reply = (catch snmpm:g(?USER, Addr, Oids)),
    {reply, Reply, S};
    
handle_call({sync_get, Addr, Oids, Timeout}, _From, S) ->
    Reply = (catch snmpm:g(?USER, Addr, Oids, Timeout)),
    {reply, Reply, S};

handle_call({sync_get, Addr, Port, Oids, Timeout}, _From, S) ->
    Reply = (catch snmpm:g(?USER, Addr, Port, Oids, Timeout)),
    {reply, Reply, S};
    
handle_call({async_get, Addr, Oids}, _From, S) ->
    Reply = (catch snmpm:ag(?USER, Addr, Oids)),
    {reply, Reply, S};
    
handle_call({async_get, Addr, Oids, Timeout}, _From, S) ->
    Reply = (catch snmpm:ag(?USER, Addr, Oids, Timeout)),
    {reply, Reply, S};

handle_call({async_get, Addr, Port, Oids, Timeout}, _From, S) ->
    Reply = (catch snmpm:ag(?USER, Addr, Port, Oids, Timeout)),
    {reply, Reply, S};

handle_call({sync_get_next, Addr, Oids}, _From, S) ->
    %%snmpm:agn(?USER, Addr, Oids);
    Reply = (catch snmpm:gn(?USER, Addr, Oids)),
    {reply, Reply, S};
    
%%gn(UserId, Addr, Oids, Timeout) 
handle_call({sync_get_next, Addr, Oids, Timeout}, _From, S) ->
    %%snmpm:agn(?USER, Addr, Oids, Timeout);
    Reply = (catch snmpm:gn(?USER, Addr, Oids, Timeout)),
    {reply, Reply, S};
    
%%gn(UserId, Addr, Port, Oids, Timeout)
handle_call({sync_get_next, Addr, Port, Oids, Timeout}, _From, S) ->
    %%snmpm:agn(?USER, Addr, Port, Oids, Timeout);
    Reply = (catch snmpm:gn(?USER, Addr, Port, Oids, Timeout)),
    {reply, Reply, S};

handle_call({async_get_next, Addr, Oids}, _From, S) ->
    %%snmpm:agn(?USER, Addr, Oids);
    Reply = (catch snmpm:agn(?USER, Addr, Oids)),
    io:format("99999~p~n",[_From]),
    io:format("99999~p~n",[Reply]),
    {reply, {Reply,_From}, S};
    
%%gn(UserId, Addr, Oids, Timeout) 
handle_call({async_get_next, Addr, Oids, Timeout}, _From, S) ->
    %%snmpm:agn(?USER, Addr, Oids, Timeout);
    %%io:format("99999~p~n",[_From]),
    Reply = (catch snmpm:agn(?USER, Addr, Oids, Timeout)),
    io:format("99999~p~n",[_From]),
    io:format("99999~p~n",[Reply]),
    {reply, {Reply,_From}, S};
    
%%gn(UserId, Addr, Port, Oids, Timeout)
handle_call({async_get_next, Addr, Port, Oids, Timeout}, _From, S) ->
    %%snmpm:agn(?USER, Addr, Port, Oids, Timeout);
    %%io:format("99999~p~n",[_From]),
    Reply = (catch snmpm:agn(?USER, Addr, Port, Oids, Timeout)),
    io:format("99999~p~n",[_From]),
    io:format("99999~p~n",[Reply]),
    {reply, {Reply,_From}, S};

handle_call({sync_get_bulk, Addr, NR, MR, Oids}, _From, S) ->
    Reply = (catch snmpm:gb(?USER, Addr, NR, MR, Oids)),
    {reply, Reply, S};

handle_call({sync_get_bulk, Addr, Port, NR, MR, Oids}, _From, S) ->
    Reply = (catch snmpm:gb(?USER, Addr, Port, NR, MR, Oids)),
    {reply, Reply, S};

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


%%--------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(stop, S) ->
    (catch snmpm:stop()),
    {stop, normal, S};

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
    case handle_snmp_callback(Tag, Info,State) of
		{ok,S}->
			{noreply, State};
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


do_trap([],_)->[];
do_trap([M|T],Trap)->
	try
		M:handle_trap(M,Trap),
		[M]
	catch
	_:_->
		?MODULE:remove_trap_handle(M),
		[]
	end ++ do_trap(T,Trap).

%% ========================================================================
%% ========================================================================

handle_snmp_callback(handle_error, {ReqId, Reason},_) ->
    io:format("*** FAILURE ***"
	      "~n   Request Id: ~p"
	      "~n   Reason:     ~p"
	      "~n", [ReqId, Reason]),
    ok;
handle_snmp_callback(handle_agent, {Addr, Port, SnmpInfo},_) ->
    {ES, EI, VBs} = SnmpInfo, 
    io:format("*** UNKNOWN AGENT ***"
	      "~n   Address:   ~p"
	      "~n   Port:      ~p"
	      "~n   SNMP Info: "
	      "~n     Error Status: ~w"
	      "~n     Error Index:  ~w"
	      "~n     Varbinds:     ~p"
	      "~n", [Addr, Port, ES, EI, VBs]),
    ok;
handle_snmp_callback(handle_pdu, {Addr, Port, ReqId, SnmpResponse},State) ->
    {ES, EI, VBs} = SnmpResponse, 
    {_,Pid,_} = State,
    io:format("*************state is ~p~n",[State]),
    io:format("*** Received PDU ***"
            "~n   Address:       ~p"
            "~n   Port:          ~p"
            "~n   Request Id:    ~p"
            "~n   SNMP response:"
            "~n     Error Status: ~w"
            "~n     Error Index:  ~w"
            "~n     Varbinds:     ~p"
            "~n", [Addr, Port, ReqId, ES, EI, VBs]),
    Pid!{ok,{Addr,VBs}},
    ok;
handle_snmp_callback(handle_trap, {Addr, Port, SnmpTrap},S) ->
	rpc:cast(node(),?MODULE,do_trap,[S#state.trap,{Addr,Port,SnmpTrap}]),
    {ok,S};

handle_snmp_callback(handle_inform, {Addr, Port, SnmpInform},_) ->
    {ES, EI, VBs} = SnmpInform, 
    io:format("*** Received INFORM ***"
	      "~n   Address:     ~p"
	      "~n   Port:        ~p"
	      "~n   SNMP inform: "
	      "~n     Error Status: ~w"
	      "~n     Error Index:  ~w"
	      "~n     Varbinds:     ~p"
	      "~n", [Addr, Port, ES, EI, VBs]),
    ok;
handle_snmp_callback(handle_report, {Addr, Port, SnmpReport},_) ->
    {ES, EI, VBs} = SnmpReport, 
    io:format("*** Received REPORT ***"
	      "~n   Address:   ~p"
	      "~n   Port:      ~p"
	      "~n   SNMP report: "
	      "~n     Error Status: ~w"
	      "~n     Error Index:  ~w"
	      "~n     Varbinds:     ~p"
	      "~n", [Addr, Port, ES, EI, VBs]),
    ok;
handle_snmp_callback(BadTag, Crap,_) ->
    io:format("*** Received crap ***"
	      "~n   ~p"
	      "~n   ~p"
	      "~n", [BadTag, Crap]),
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

%% get_opt(Key, Opts) ->
%%     case lists:keysearch(Key, 1, Opts) of
%%         {value, {Key, Val}} ->
%%             Val;
%%         false ->
%%             throw({error, {missing_mandatory, Key}})
%%     end.

get_opt(Key, Opts, Def) ->
    case lists:keysearch(Key, 1, Opts) of
        {value, {Key, Val}} ->
            Val;
        false ->
            Def
    end.


%% ========================================================================
%% SNMPM user callback functions
%% ========================================================================

handle_error(ReqId, Reason, Server)-> %%when is_pid(Server) ->
	io:format("handle_error:~p,~p,~p~n",[ReqId, Reason, Server]),
    report_callback(Server, handle_error, {ReqId, Reason}),
    ignore.


handle_agent(Addr, Port, SnmpInfo, Server) -> %% when is_pid(Server) ->
	io:format("handle_agent:~p,~p,~p~n",[Addr, SnmpInfo, Server]),
    report_callback(Server, handle_agent, {Addr, Port, SnmpInfo}),
    ignore.


handle_pdu(Addr, Port, ReqId, SnmpResponse, Server)-> %% when is_pid(Server) ->
	%%io:format("handle_pdu:~p,~p,~p~n",[Addr, ReqId, Server]),
    report_callback(Server, handle_pdu, {Addr, Port, ReqId, SnmpResponse}),
    ignore.


handle_trap(Addr, Port, SnmpTrap, Server) -> %% when is_pid(Server) ->
	io:format("handle_trap:~p,~p,~p~n",[Addr, SnmpTrap, Server]),
    report_callback(Server, handle_trap, {Addr, Port, SnmpTrap}),
    ok.

handle_inform(Addr, Port, SnmpInform, Server)-> %% when is_pid(Server) ->
	io:format("handle_inform:~p,~p,~p~n",[Addr, SnmpInform, Server]),
    report_callback(Server, handle_inform, {Addr, Port, SnmpInform}),
    ok.


handle_report(Addr, Port, SnmpReport, Server)-> %% when is_pid(Server) ->
	io:format("handle_report:~p,~p,~p~n",[Addr, SnmpReport, Server]),
    report_callback(Server, handle_inform, {Addr, Port, SnmpReport}),
    ok.

report_callback(Pid, Tag, Info) ->
    Pid ! {snmp_callback, Tag, Info}.