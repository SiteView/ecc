%% 
%% @doc proxy_mapping module,save monitor proxy mapping
%% @version{1.0}
%% @copyright 2010 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>
%%
-module(proxy_mapping).
-compile(export_all).

-behaviour(gen_server).

%% gen_server callbacks
 -export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-record(state, {parent,files=[]}).

-define(PREFS_DIR,"logs/").

-define(SERVER,'elecc_proxy_mapping').

%% @spec start_link() ->(ok | {error,Reason})
%% where
%%	Reason = atom()
%% @doc start proxy_mapping services
%%
start_link() ->
    start_link(['proxy_mapping']).

start_link(Opts) when is_list(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Opts], []).

init([Opts])->
	[dets:open_file(X,[{file,?PREFS_DIR ++ atom_to_list(X)},{type,bag}])||X<-Opts],
	{ok,#state{files=Opts}}.

stop() ->
    gen_server:cast(?SERVER, stop).

get(App,Key)->
	gen_server:call(?SERVER, {get,App,Key}).
	
get_by_proxy(App,Proxy)->
	gen_server:call(?SERVER, {get_by_proxy,App,Proxy}).

remove({App,Key,Value})->
	gen_server:call(?SERVER, {remove,{App,Key,Value}}).

add(App,Key,Val)->
	gen_server:call(?SERVER,{add,App,Key,Val}).

all(App)->
	gen_server:call(?SERVER,{all,App}).

handle_call({get,App,Key}, _, State)->
	case dets:match_object(proxy_mapping,{App,Key,'$1'}) of
		{error,Err}->
			{reply,{error,Err},State};
		Ids->
			{reply,{ok,[{X,Y} || {_,X,Y}<-Ids]},State}
	end;
	
handle_call({get_by_proxy,App,Proxy}, _, State)->
	case dets:match_object(proxy_mapping,{App,'$1',Proxy}) of
		{error,Err}->
			{reply,{error,Err},State};
		Ids->
			{reply,{ok,[{X,Y} || {_,X,Y}<-Ids]},State}
	end;
	
handle_call({add,App,Key,Val}, _, State)->
	dets:insert(proxy_mapping,{App,Key,Val}),
	dets:sync(proxy_mapping),
	{reply,{ok,proxy_mapping},State};
	
handle_call({remove,{App,Key,Val}}, _, State)->
	dets:delete_object(proxy_mapping,{App,Key,Val}),
	dets:sync(proxy_mapping),
	{reply,{ok,deleted},State};

handle_call({all,App}, _, State)->
	Ret = dets:match(proxy_mapping,{App,'$1','$2'}),
	{reply,{ok,[list_to_tuple(X)||X<-Ret]},State};

handle_call(Req, _, State) ->
    {reply, {error,unknown_request}, State}.

handle_cast(stop, S) ->
    [dets:close(X)||X<-S#state.files],
    {stop, normal, S};

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

test()->
	start_link(),
	add(localhost,mailServer,"mail.dragonflow.com"),
	add(localhost,autoEmail,"xianfang.shi@dragonflow.com"),
	io:format("~p~n",[?MODULE:get(localhost,mailServer)]),
	io:format("~p~n",[?MODULE:get(localhost,autoEmail)]),
	stop(),
	ok.