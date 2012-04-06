%% ---
%% siteview_object_table
%%
%%---
-module(siteview_object_table).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2,terminate/2, handle_info/2,code_change/3]).

-export([start_link/0,stop/0,set_object/3,
		set_object/4,get_object/1,get_object_by_class/1,
		get_object_by_class/2,get_object_by_type/1,
		remove_object/1,remove_object/2]).

-include("config.hrl").

-define(TIMEOUT,10000).

-record(sv,{tab}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop()->
    gen_server:cast(?MODULE, stop).
	

init(_)->
	Tab = ets:new(elecc_object_table, [set,public]),
	{ok,#sv{tab=Tab}}.
	set_object(Id,Class,Object)->
	App = dbcs_base:get_app(),
	gen_server:call(?MODULE, {set_object,Id,Class,Object,App}).
	
%% @doc insert a object to table
%%
%%
set_object(Id,Type,Class,Object)->
	App = dbcs_base:get_app(),
	gen_server:call(?MODULE, {set_object,Id,Type,Class,Object,App}).
	
	
%% @doc get a object from table
%%
get_object(Id)->
	App = dbcs_base:get_app(),
	gen_server:call(?MODULE, {get_object,Id,App}).
	
get_object(Id,Type)->
	App = dbcs_base:get_app(),
	gen_server:call(?MODULE, {get_object,Id,App,Type}).

%% @doc get objects from table
%%
%%
get_object_by_class(Class)->
	App = dbcs_base:get_app(),
	gen_server:call(?MODULE, {get_object_by_class,Class,App}).
	
%% @doc get objects from table
%%
%%
get_object_by_class(Type,Class)->
	App = dbcs_base:get_app(),
	gen_server:call(?MODULE, {get_object_by_class,Type,Class,App}).

get_object_by_type(Type)->
	App = dbcs_base:get_app(),
	gen_server:call(?MODULE, {get_object_by_type,Type,App}).

remove_object(Id)->
	App = dbcs_base:get_app(),
	gen_server:call(?MODULE, {remove_object,Id,App}).
	

remove_object(App,Id)->
	gen_server:call(?MODULE, {remove_object,Id, App}).
	handle_call({set_object,Id,Class,Object,App}, _From, Sv) ->
	ets:insert(Sv#sv.tab,{{App,Id},Class,Class,Object}),
	C = try
		Object:getCostInLicensePoints()
		catch
			_:_->
				0
		end,
	siteview_global:addPoints(App,C),
	{reply,ok,Sv};
	handle_call({set_object,Id,Type,Class,Object,App}, _From, Sv) ->
	ets:insert(Sv#sv.tab,{{App,Id},Type,Class,Object}),
	C = 
	try
		Object:getCostInLicensePoints()
		catch
		_:_->
			0
		end,
	siteview_global:addPoints(App,C),
	{reply,ok,Sv};
	handle_call({get_object,Id,App}, _From, Sv) ->
	Ret = 
	case ets:lookup(Sv#sv.tab,{App,Id}) of
		[]->
			[];
		R->
			[Y||{_,_,_,Y}<-R]
	end,
	{reply,Ret,Sv};
	
handle_call({get_object,Id,App,Type}, _From, Sv) ->
	Ret = 
	case ets:match_object(Sv#sv.tab,{{App,Id},Type,'_','_'}) of
		[]->
			[];
		R->
			[Y||{_,_,_,Y}<-R]
	end,
	{reply,Ret,Sv};
	handle_call({get_object_by_class,Class,App}, _From, Sv) ->
	Ret = 
	case ets:match_object(Sv#sv.tab,{{App,'_'},'_',Class,'_'}) of
		[]->
			[];
		R->
			[Y||{_,_,_,Y}<-R]
	end,
	{reply,Ret,Sv};
	handle_call({get_object_by_class,Type,Class,App}, _From, Sv) ->
	Ret = 
	case ets:match_object(Sv#sv.tab,{{App,'_'},Type,Class,'_'}) of
		[]->
			[];
		R->
			[Y||{_,_,_,Y}<-R]
	end,
	{reply,Ret,Sv};
	handle_call({get_object_by_type,Type,App}, _From, Sv) ->
	Ret = 
	case ets:match_object(Sv#sv.tab,{{App,'_'},Type,'_','_'}) of
		[]->
			[];
		R->
			[Y||{_,_,_,Y}<-R]
	end,
	{reply,Ret,Sv};
	handle_call({remove_object,Id,App}, _From, Sv) ->
	Ret = 
	case ets:lookup(Sv#sv.tab,{App,Id}) of
		[]->
			[];
		R->
			[Y||{_,_,_,Y}<-R]
	end,
	Result = 
	case Ret of
		[]->
			{error,object_not_found};
		[Object|_]->
			C = try
				Object:getCostInLicensePoints()
				catch
					_:_->0
				end,
			siteview_global:reducePoints(App,C),
			ets:delete(Sv#sv.tab,{App,Id})
	end,
	{reply,Result,Sv};
	
handle_call(_, _From, Sv) ->
	{reply,ok,Sv}.
	
handle_cast(stop, Sv) ->
	ets:delete(Sv#sv.tab),
    {stop, normal, Sv};
	
handle_cast(_, Sv) ->
	{noreply,  Sv}.

	
%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->	ets:delete(_State#sv.tab),
   stop().
handle_info(_, State) ->
    {noreply, State}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.
