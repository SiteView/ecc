-module(tr069_AlarmProcQueue).

-behaviour(gen_server).
-compile(export_all).
-define(SERVER,'sv_tr069_AlarmProcQueue').

-define(SLEEPTIME,100).

-record(state, {child=[]}).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
         

%% gen_server callbacks

init([]) ->
	{ok,#state{}}.

handle_call({process_event,ManufacturerT,OUI,ProductClass,SerialNumber,Host,EventType}, _From, State) ->
    platform:sleep(?SLEEPTIME),
    api_tr069:process_event(ManufacturerT,OUI,ProductClass,SerialNumber,Host,EventType),
    {reply, {ok, SerialNumber, EventType},State};
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
    
    
process_event(ManufacturerT,OUI,ProductClass,SerialNumber,Host,EventType) ->
    call({process_event, ManufacturerT,OUI,ProductClass,SerialNumber,Host,EventType}).
    

    
    
    
