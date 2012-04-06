%% ---
%% svecc supervisor
%%
%%---
-module(testecc).
-behaviour(supervisor).

-export([start_link/1,init/1,start/0]).

start() ->
	spawn(fun() ->
		supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = [])
	end).

start_link(Args) ->
    supervisor:start_link({local,?MODULE},?MODULE, Args).

init(_Args) ->
    {ok, {{one_for_one, 20000, 10},
          [
%%		    {reloader,{reloader,start_link,[]},transient, brutal_kill, worker, [reloader]},
			{server_conf,{server_conf,start_link,[]},transient, brutal_kill, worker, [server_conf]},
			{lc_server,{lc_server,start_link,[]},transient, brutal_kill, worker, [lc_server]},
			{preferences,{preferences,start_link,[]},transient, brutal_kill, worker, [preferences]},
            {index_store,{index_store,start_link,[]},transient, brutal_kill, worker, [index_store]},
	    	{monitor_status_store, {monitor_status_store, start_link, []},transient, brutal_kill, worker, [monitor_status_store]},
            {commandLine, {commandLine, start, []},transient, brutal_kill, worker, [commandLine]},
			{siteview, {siteview, start_link, []},transient, brutal_kill, worker, [siteview]},
%% 			{monitor_manager,{monitor_manager, start_link, [[]]},transient, brutal_kill, supervisor, [monitor_manager]},
			{topn_siteview, {topn_siteview, start_link, []},transient, brutal_kill, worker, [topn_siteview]},
%%             {topn_manager,{topn_manager,start_link, [[]]},transient, brutal_kill, worker, [topn_manager]},
		   	{monitor_logger_server,{monitor_logger_server,start_link,[]},transient, brutal_kill, worker, [monitor_logger_server]},
%% 			{alert_logger,{alert_logger,start_link,[]},transient, brutal_kill, worker, [alert_logger]},
			{iconv, {iconv, start, []},transient, brutal_kill, worker, [iconv]},
			{gettext_server,{gettext_server,start_link,[gettext_server]},transient,brutal_kill,worker,[gettext_server]},
			{extension, {extension, start_link, []},transient, brutal_kill, worker, [extension]}
		  ]}}.

	
	
init1(_Args) ->
    {ok, {{one_for_one, 20000, 10},
          [
		    {reloader,{reloader,start_link,[]},transient, brutal_kill, worker, [reloader]},
			{server_conf,{server_conf,start_link,[]},transient, brutal_kill, worker, [server_conf]},
%%          {lc_server,{lc_server,start_link,[]},transient, brutal_kill, worker, [lc_server]},
			{preferences,{preferences,start_link,[]},transient, brutal_kill, worker, [preferences]}, 
            {index_store,{index_store,start_link,[]},transient, brutal_kill, worker, [index_store]},
	    	{monitor_status_store, {monitor_status_store, start_link, []},transient, brutal_kill, worker, [monitor_status_store]},
            {commandLine, {commandLine, start, []},transient, brutal_kill, worker, [commandLine]},
%% 	    	{user_operation_log,{user_operation_log,start_link, []},transient, brutal_kill, worker, [user_operation_log]},
%% 	    	{proxy_mapping,{proxy_mapping,start_link, []},transient, brutal_kill, worker, [proxy_mapping]},	
%%  	    {monitor_proxy_server,{monitor_proxy_server,start_link, []},transient, brutal_kill, worker, [monitor_proxy_server]},
%% 	    	{monitor_local_stat,{monitor_local_stat,start_link, []},transient, brutal_kill, worker, [monitor_local_stat]},
	    	{monitor_manager,{monitor_manager, start_link, [[]]},transient, brutal_kill, supervisor, [monitor_manager]},
            {topn_manager,{topn_manager,start_link, [[]]},transient, brutal_kill, worker, [topn_manager]},
		   	{monitor_logger_server,{monitor_logger_server,start_link,[]},transient, brutal_kill, worker, [monitor_logger_server]},
			{alert_logger,{alert_logger,start_link,[]},transient, brutal_kill, worker, [alert_logger]},
			{iconv, {iconv, start, []},transient, brutal_kill, worker, [iconv]},
			{extension, {extension, start, []},transient, brutal_kill, worker, [extension]}
%%           {jdbc_Logger,{jdbc_Logger,start, []},transient, brutal_kill, worker, [jdbc_Logger]},
%% 		  	{report_proxy,{report_proxy,start_link,[]},transient, brutal_kill, worker, [report_proxy]}
		  ]}}.