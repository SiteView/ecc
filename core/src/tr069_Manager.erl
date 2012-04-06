-module(tr069_Manager).

-behaviour(gen_server).
-compile(export_all).
-define(SERVER,'sv_tr069_Manager').

-record(state, {child=[]}).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
         

%% gen_server callbacks

init([{dbname,DBName}]) ->
    tr069_AlarmProcQueue:start_link(),
    upgrade_server:start_link(),
    api_tr069:set_point(),
	catch(keepalive_server:start(DBName)),
    catch(tr069_cpestatus_server:start_link([{dbnode,DBName}])),
    Tr069_devicecache_server = (catch(tr069_CpeCache:start_link([{dbnode,DBName}]))),
	{ok,#state{}}.
    
handle_call(Other, _From, State) ->
    {reply, {error, notsupport},State}.
    
handle_cast(stop, State) ->
    {stop,close_file, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% API

start_link() ->
    start_link([]).

start_link(Opts) when is_list(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).
    
call(Req) ->
    gen_server:call(?SERVER, Req, infinity).
    
cast(Req) ->
    gen_server:cast(?SERVER, Req).

 
%% api 


stop() ->
    cast(stop).
    

    
    
    
