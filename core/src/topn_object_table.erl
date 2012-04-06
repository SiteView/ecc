%% ---
%%TopN object table
%%
%%---
-module(topn_object_table).
-behaviour(gen_server).

-compile(export_all).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2,terminate/2]).
-export([write/1, read/1, read/0, delete/0, delete/1, delete_all_objects/0, delete_object/1, getRegName/0]).

-record(state, {}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
init(_)->
    ets:new(?MODULE, [set, public, named_table]),    %% save topn  process object
    {ok,#state{}}.

    
handle_call(_, _From, Sv) ->
	{reply,ok,Sv}.
	
handle_cast(stop, S) ->
    {stop, normal, S};	
handle_cast(_, Sv) ->
	{noreply,  Sv}.

	
terminate(_Reason, _State) ->
    ok.
    
call(Req) ->
    gen_server:call(?MODULE, Req, infinity).
%% api

write(Object) ->
    ets:insert(?MODULE, Object).
    
read(Key) ->
    ets:lookup(?MODULE, Key).
    
read() ->
    case ets:match(topn_object_table, '$1') of
        Val when erlang:is_list(Val) ->
            lists:append(Val);
        _ ->
            []
    end.
    
delete(Key) ->
    ets:delete(?MODULE, Key).
    
delete() ->
    ets:delete(?MODULE).
    
delete_all_objects() ->
    ets:delete_all_objects(?MODULE).
    
delete_object(Object) ->
    ets:delete_object(?MODULE, Object).

getRegName() ->
    ?MODULE.
