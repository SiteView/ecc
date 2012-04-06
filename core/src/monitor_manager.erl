%% ---
%%监测器管理模块
%%
%%---
-module(monitor_manager).
-behaviour(supervisor).

-export([start_link/1,init/1,start/0]).

start() ->
	spawn(fun() ->
		supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = [])
	end).

start_link(Args) ->
    supervisor:start_link({local,?MODULE},?MODULE, Args).

init(_Args) ->
    {ok, {{one_for_all, 60, 20},
          [
			{siteview, {siteview, start_link, []},transient, brutal_kill, worker, [siteview]}
 			%{nnm_sup, {nnm_sup, start_link, [[]]},transient, brutal_kill, supervisor, [nnm_sup]}
		  ]}}.

