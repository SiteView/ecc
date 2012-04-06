%% ---
%% svecc supervisor
%%
%%---
-module(svecc_logsup).
-behaviour(supervisor).

-export([start_link/1,init/1,start/0]).

start() ->
	spawn(fun() ->
		supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = [])
	end).

start_link(Args) ->
    supervisor:start_link({local,?MODULE},?MODULE, Args).

init(_Args) ->
    {ok, {{one_for_one, 2000, 10},
          [%% 			{reloader, {reloader, start, []},transient, brutal_kill, worker, [reloader]},
 		    {iconv, {iconv, start, []},transient, brutal_kill, worker, [iconv]},
 			{server_conf,{server_conf,start_link,[]},transient, brutal_kill, worker, [server_conf]},
 			{alert_logger_server,{alert_logger_server,start_link,[]},transient, brutal_kill, worker, [alert_logger_server]},
%%  			{jdbc_Logger,{jdbc_Logger,start, []},transient, brutal_kill, worker, [jdbc_Logger]},
  			{monitor_logger_server,{monitor_logger_server,start_link,[]},transient, brutal_kill, worker, [monitor_logger_server]}
			%{preferences,{preferences,start_link,[]},transient, brutal_kill, worker, [preferences]},
			%{commandLine, {commandLine, start, []},transient, brutal_kill, worker, [commandLine]},
			% {monitor_proxy_client,{monitor_proxy_client,start_link,[]},transient, brutal_kill, worker, [monitor_proxy_client]},
		  	% {monitor_proxy,{monitor_proxy,start_link,[]},transient, brutal_kill, worker, [monitor_proxy]},
		  	%{report_proxy,{report_proxy,start_link,[]},transient, brutal_kill, worker, [report_proxy]}
		  ]}}.
	    

