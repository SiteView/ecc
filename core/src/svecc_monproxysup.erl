%% ---
%% svecc supervisor
%%
%%---
-module(svecc_monproxysup).
-behaviour(supervisor).

-export([start_link/1,init/1,start/0]).

start() ->
	spawn(fun() ->
		supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = [])
	end).

start_link(Args) ->
    supervisor:start_link({local,?MODULE},?MODULE, Args).

init(_Args) ->
    {ok, {{one_for_one, 3, 10},
          [
%% 			{monitor_logger,{monitor_logger,start_link,[]},transient, brutal_kill, worker, [monitor_logger]}
			{server_conf,{server_conf,start_link,[]},transient, brutal_kill, worker, [server_conf]},
			{preferences,{preferences,start_link,[]},transient, brutal_kill, worker, [preferences]},
			{commandLine, {commandLine, start, []},transient, brutal_kill, worker, [commandLine]},
			{iconv, {iconv, start, []},transient, brutal_kill, worker, [iconv]},
			{monitor_proxy_client,{monitor_proxy_client,start_link,[]},transient, brutal_kill, worker, [monitor_proxy_client]},
%%******************************proxy must start in this node	  
%%		  {remoteMachineManager,{remoteMachineManager,start_link,[]},transient, brutal_kill, worker, [remoteMachineManager]},
%%******************************
		  {monitor_proxy,{monitor_proxy,start_link,[]},transient, brutal_kill, worker, [monitor_proxy]},
		  	{report_proxy,{report_proxy,start_link,[]},transient, brutal_kill, worker, [report_proxy]}
%% 		  	{queue_client,{queue_client,start_link,[]},transient, brutal_kill, worker, [queue_client]},
%% 			{queue_server,{queue_server,start_link,[]},transient, brutal_kill, worker, [queue_server]}
		  ]}}.

