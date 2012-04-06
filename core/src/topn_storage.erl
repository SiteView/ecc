%% ---
%%TopN´æ´¢Æ÷
%%
%%---
-module(topn_storage).
-behaviour(gen_server).

-compile(export_all).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2,terminate/2]).
-export([getRegName/0]).

-record(state, {}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
init(_)->
    {ok,#state{}}.
    
handle_call(_, _From, Sv) ->
	{reply,ok,Sv}.
	
handle_cast(stop, S) ->
    {stop, normal, S};	
handle_cast(_, Sv) ->
	{noreply,  Sv}.

	
terminate(_Reason, _State) ->
    ok.
    
getRegName() ->
    ?MODULE.