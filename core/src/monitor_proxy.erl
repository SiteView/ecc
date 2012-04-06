%% ---
%%your comment
%%
%%---
-module(monitor_proxy).
-behaviour(gen_server).

-compile(export_all).

-include("monitor.hrl").

-export([start_link/0,stop/0,require_register/3]).

-record(state, {}).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE,stop).


init([]) ->
	process_flag(trap_exit, true),
	{ok,#state{}}.


% Server initiative to increase agent, call this function to complete the registration
require_register(Master,DbNode,Force)->
	gen_server:call(?MODULE, {require_register,Master,DbNode,Force}).
	
unregister()->
	gen_server:call(?MODULE, {unregister}).

handle_call({require_register, Master,DbNode,Force}, _, State) ->
	Started = lists:member(monitor_proxy_client,erlang:registered()),
	if
		Started == true andalso Force==false->
			{reply,{error,registered},State};
		true->
			monitor_proxy_client:stop(),
			server_conf:setServerConf(master_node,Master),
			server_conf:setServerConf(dbNode,DbNode),
			Ret = monitor_proxy_client:start_link(),
			{reply,Ret,State}
	end;
handle_call({unregister}, _, State) ->
	Ret = monitor_proxy_client:stop(),
	{reply,Ret,State};
handle_call({q, Date}, _, State) ->
	{reply,Date,State};
handle_call(Req, _, State) ->
    {reply, {error, {unknown_request, Req}}, State}.

handle_cast(stop, State) ->	
	{stop,normal,State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

code_change({down, _Vsn}, State, _Extra) ->
    {ok, State};

% upgrade
code_change(_Vsn, State, _Extra) ->
    {ok, State}.


