%% ---
%% erlang cache
%%
%%---
-module(erlcache).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2,terminate/2, handle_info/2,code_change/3]).

-export([start_link/0,stop/0,set/3,get/1,remove/1]).

-include("config.hrl").

-define(TIMEOUT,10000).

-record(sv,{tab}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop()->
    gen_server:cast(?MODULE, stop).
	

init(_)->
	Tab = ets:new(?MODULE, [set,public]),
	{ok,#sv{tab=Tab}}.
	
set(Key,Val,ExpireTime)->
	gen_server:call(?MODULE,{set,Key,Val,ExpireTime}).
	
get(Key)->
	gen_server:call(?MODULE,{get,Key}).
	
remove(Key)->
	gen_server:call(?MODULE,{remove,Key}).
	
handle_call({remove,Key}, _From, Sv) ->
	ets:delete(Sv#sv.tab,Key),
	{reply,ok,Sv};
handle_call({set,Key,Val,ExpireTime}, _From, Sv) ->
	ets:insert(Sv#sv.tab,{Key,Val,ExpireTime*1000 + sv_datetime:now()}),
	{reply,ok,Sv};
handle_call({get,Key}, _From, Sv) ->
	case ets:lookup(Sv#sv.tab,Key) of
		undefined->
			{reply,{error,undefined},Sv};
		[]->
			{reply,{error,undefined},Sv};
		[{_,Val,Time}|_]->
			Now = sv_datetime:now(),
			if
				Now > Time->
					{reply,{error,expired},Sv};
				true->
					{reply,{ok,Val},Sv}
			end
	end;
				
handle_call(_, _From, Sv) ->
	{reply,ok,Sv}.
	
handle_cast(stop, S) ->
	ets:delete(S#sv.tab),
    {stop, normal, S};	
handle_cast(_, Sv) ->
	{noreply,  Sv}.

	
%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ets:delete(_State#sv.tab),
	stop().

handle_info(_, State) ->
    {noreply, State}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.