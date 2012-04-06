-module(tr069_cpestatus_server).
-define(Table,"tr069_device").
-define(Author,<<"ning.cai@dragonflow.com">>).

-include("monitor.hrl").
-define(TimeMultiples,3).
-behaviour(gen_server).
-record(state, {
                    update_cep=dict:new()       %% Be modified cpe, if not set value, initialize a hash table
                    }).
-define(SERVER,'ecc_tr069_cpestatus_server').
-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
         

%% gen_server callbacks

init([]) ->
    {ok, #state{}}.
    
handle_call(_Info, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({update_mul_cpestatus, App, Devs},State) ->
    %%io:format("update_mul_cpestatus~n"),
    (catch update_mul_cpestatus({update_mul_cpestatus, App, Devs})),
    {noreply, State};
handle_cast({update_cpestatus, App, A}, State) ->
    %%io:format("A#tr069_device.state = ~p~n", [A#tr069_device.state]),
    %%io:format("A#tr069_device.keepalive = ~p~n", [A#tr069_device.keepalive]),
    FlagKeeplive = 
        if 
            A#tr069_device.state =:= "alive", A#tr069_device.keepalive =:= 0 ->
                true;
            true ->
                false
        end,
    Flag = (calendar:datetime_to_gregorian_seconds(erlang:localtime()) > (A#tr069_device.timestamp + A#tr069_device.keepalivetime * ?TimeMultiples)),
    %%io:format("FlagKeeplive = ~p~nFlag = ~p~n", [FlagKeeplive, Flag]),
    UpdateStatus = 
    if 
        FlagKeeplive,Flag ->
            if 
                A#tr069_device.description == undefined ->
                    New = A#tr069_device{timestamp = integer_to_list(A#tr069_device.timestamp),state = "dead",keepalive = integer_to_list(A#tr069_device.keepalive),keepalivetime = integer_to_list(A#tr069_device.keepalivetime),description=""},        
                    dbcs_tr069:update_device(New,App);
                true ->
                    New = A#tr069_device{timestamp = integer_to_list(A#tr069_device.timestamp),state = "dead",keepalive = integer_to_list(A#tr069_device.keepalive),keepalivetime = integer_to_list(A#tr069_device.keepalivetime)},        
                    dbcs_tr069:update_device(New,App)
            end; 
        true ->
            {error, "empty"}
    end,
    %%io:format("UpdateStatus = ~p~n", [UpdateStatus]),
    {noreply, State};
handle_cast(stop, State) ->
    {stop,close_file, State};
handle_cast(_Info, State) ->
    {noreply, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% API

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
    

%% Update status of a single cpe
update_cpestatus({update_cpestatus, App, A}) ->
    cast({update_cpestatus, App, A}).

update_mul_cpestatus(App, Devs) ->
    cast({update_mul_cpestatus, App, Devs}).




%% -------------------------------------------
%% Status update multiple cpe
update_mul_cpestatus({update_mul_cpestatus, App, []}) ->
    ok;
update_mul_cpestatus({update_mul_cpestatus, App, [A=#tr069_device{}|B]}) ->
    FlagKeeplive = 
        if 
            A#tr069_device.state =:= "alive", A#tr069_device.keepalive =:= 0 ->
                true;
            true ->
                false
        end,
    Flag = (calendar:datetime_to_gregorian_seconds(erlang:localtime()) > (A#tr069_device.timestamp + A#tr069_device.keepalivetime * ?TimeMultiples)),
    %%io:format("FlagKeeplive = ~p~nFlag = ~p~n", [FlagKeeplive, Flag]),
    UpdateStatus = 
    if 
        FlagKeeplive,Flag ->
            if 
                A#tr069_device.description == undefined ->
                    New = A#tr069_device{timestamp = integer_to_list(A#tr069_device.timestamp),state = "dead",keepalive = integer_to_list(A#tr069_device.keepalive),keepalivetime = integer_to_list(A#tr069_device.keepalivetime),description=""},        
                    dbcs_tr069:update_device(New,App);
                true ->
                    New = A#tr069_device{timestamp = integer_to_list(A#tr069_device.timestamp),state = "dead",keepalive = integer_to_list(A#tr069_device.keepalive),keepalivetime = integer_to_list(A#tr069_device.keepalivetime)},        
                    dbcs_tr069:update_device(New,App)
            end,
            platform:sleep(200);
            ok;
        true ->
            {error, "empty"}
    end,
    update_mul_cpestatus({update_mul_cpestatus, App, B});
update_mul_cpestatus({update_mul_cpestatus, App, [A|B]}) ->
    update_mul_cpestatus({update_mul_cpestatus, App, B}).
    
    

            
    
    
    
