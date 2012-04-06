%% ---
%% siteview_object_table
%%
%%---
-module(propertied_object_server).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2,terminate/2, handle_info/2,code_change/3]).

-export([start_link/0,stop/0,new_object/0]).

-define(TIMEOUT,10000).

-define(ETS_COUNT,50).

-record(sv,{ptab=[],atab=[]}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop()->
    gen_server:cast(?MODULE, stop).
	

init(_)->
	Ids = lists:seq(1,?ETS_COUNT),
	Tab1  = lists:map(fun(_)-> ets:new(?MODULE,[set,public]) end,Ids),
	Tab2  = lists:map(fun(_)-> ets:new(?MODULE,[set,public]) end,Ids),
	{ok,#sv{ptab=Tab1,atab=Tab2}}.
	new_object()->
	start_link(),
	gen_server:call(?MODULE, {new_object}).
	handle_call({new_object}, _From, Sv) ->
	Ra = random:uniform(?ETS_COUNT),
	Tp = lists:nth(Ra,Sv#sv.ptab),
	Ta = lists:nth(Ra,Sv#sv.atab),
	{reply,{Tp,Ta},Sv};
	
handle_call(_, _From, Sv) ->
	{reply,ok,Sv}.
	
handle_cast(stop, Sv) ->
    {stop, normal, Sv};
	
handle_cast(_, Sv) ->
	{noreply,  Sv}.

	
%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
   stop().
handle_info(_, State) ->
    {noreply, State}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.
