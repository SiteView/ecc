%% ---
%% siteview_global
%%
%%---
-module(siteview_global).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2,terminate/2, handle_info/2,code_change/3]).

-export([start_link/0,stop/0,set_siteview/1,get_current_siteview/0,totalPointsUsed/0,
		addPoints/1,reducePoints/1,get_started_time/0,addPoints/2,reducePoints/2]).

-include("config.hrl").

-define(TIMEOUT,10000).

-record(sv,{tab}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop()->
    gen_server:cast(?MODULE, stop).
	

init(_)->
	Tab = ets:new(siteview_elecc, [set,public]),
	ets:insert(Tab,{start_time,sv_datetime:now()}),
	{ok,#sv{tab=Tab}}.
	set_siteview(SiteView)->
	gen_server:call(?MODULE, {set_siteview,SiteView}).
	
get_current_siteview()->
	gen_server:call(?MODULE, {get_current_siteview}).
	
totalPointsUsed()->
	App = dbcs_base:get_app(),
	gen_server:call(?MODULE, {totalPointsUsed,App}).
	addPoints(Count)->
	App = dbcs_base:get_app(),
	gen_server:call(?MODULE, {addPoints,Count,App}).
	
addPoints(App,Count)->
	gen_server:call(?MODULE, {addPoints,Count,App}).
	reducePoints(Count)->
	App = dbcs_base:get_app(),
	gen_server:call(?MODULE, {reducePoints,Count,App}).
	
reducePoints(App,Count)->
	gen_server:call(?MODULE, {reducePoints,Count,App}).
	get_started_time()->
	gen_server:call(?MODULE, {get_started_time}).
handle_call({set_siteview,SiteView}, _From, Sv) ->
	ets:insert(Sv#sv.tab,{siteview,SiteView}),
	{reply,ok,Sv};
handle_call({get_current_siteview}, _From, Sv) ->
	try
	[{siteview,SV}|_] = ets:lookup(Sv#sv.tab,siteview),
	{reply,SV,Sv}
	catch
		_:_->
			{reply,undefined,Sv}
	end;

handle_call({totalPointsUsed,App}, _From, Sv) ->
	Point = 
	case ets:lookup(Sv#sv.tab,{App,totalPointsUsed}) of
		[]->
			0;
		[{_,V}|_]->
			V
	end,
	{reply,Point,Sv};
handle_call({addPoints,Count,App}, _From, Sv) -> 
	case ets:lookup(Sv#sv.tab,{App,totalPointsUsed}) of
		[]->
			ets:insert(Sv#sv.tab,{{App,totalPointsUsed},Count});
		_->
			ets:update_counter(Sv#sv.tab,{App,totalPointsUsed},Count)
	end,
	{reply,ok,Sv};
	
handle_call({reducePoints,Count,App}, _From, Sv) -> 
	ets:update_counter(Sv#sv.tab,{App,totalPointsUsed},-Count),
	{reply,ok,Sv};
	handle_call({get_started_time}, _From, Sv) -> 
	StatedTime = 
	case ets:lookup(Sv#sv.tab,start_time) of
		[]->
			ets:insert(Sv#sv.tab,{start_time,sv_datetime:now()}),
			0;
		[{_,Time}|_]->
			Now = sv_datetime:now(),
			Now -Time
	end,
	{reply,StatedTime,Sv};
	
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