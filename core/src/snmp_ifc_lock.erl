-module(snmp_ifc_lock).
-behaviour(gen_server).
-record(state, {pid}).
-define(SERVER,'ecc_snmp_session_lock').
-export([v3_config/1, static/0, async_get_v3/8, g/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%---------------------------------
%% genserver
%%--------------------------------

init([]) ->
    {ok, #state{}}.
%% snmpv3 config
handle_call({v3_config, Session}, _From, State) ->
    Reply = Session:v3_config(),
    {reply, Reply, State};
handle_call({g, Oid, Session}, _From, State) ->
    Reply = Session:g_lock(Oid),
    {reply, Reply, State};
handle_call({async_get_v3, UserId, Addr, NPort, ContextName, Oids, TimeOut, ExtraInfo,Session}, _From, State) ->
    Session:v3_config(),
    Reply = (catch snmpm:ag(UserId, Addr, NPort, ContextName, Oids, TimeOut, ExtraInfo)),
    {reply, Reply, State}.

    
handle_cast(stop, State) ->
    {stop,close_file, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) -> {ok, State}.


start_link() ->
    start_link([]).

start_link(Opts) when is_list(Opts) ->
    case gen_server:start_link({local, ?SERVER}, ?MODULE, [], []) of
        {ok, State} ->
            {ok, State};
        {stop, Reason} ->
            {error, Reason};
        {error,{already_started,_}} ->
            {ok, already_started};
        Other ->
            {error, Other}
    end.
    
call(Req) ->
    gen_server:call(?SERVER, Req, infinity).
    
cast(Req) ->
    gen_server:cast(?SERVER, Req).

%%---------------------------------------

%%--------------------------------------
%% interface
%%--------------------------------------
static() ->
    Reply = (catch start_link()),
    io:format("Reply = ~p~n", [Reply]).

v3_config(Session) ->
    call({v3_config, Session}).
    
g(Oid,Session) ->
    call({g, Oid, Session}).

%% snmpv3 send request
async_get_v3(UserId, Addr, NPort, ContextName, Oids, TimeOut, ExtraInfo,Session) ->
    call({async_get_v3, UserId, Addr, NPort, ContextName, Oids, TimeOut, ExtraInfo,Session}).
    
