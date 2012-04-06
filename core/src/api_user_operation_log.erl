%% 
%% @doc api of user operation log
%% @version{1.0}
%% @copyright 2009 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>

-module(api_user_operation_log).
-compile(export_all).
-include("monitor.hrl").
-export([log/1,query_log/1]).

%% @spec log(Log)->(ok | {error,Reasn})
%% where
%%	Log = #operation_log{}
%%	Reasn = atom()
%% @doc log user operation.
%% <br>Log is a record of #operation_log{},which defined in <a href='monitor.hrl'>monitor.hrl</a></br>
log(Log=#operation_log{})->
	user_operation_log:log(Log);
log(_)->{error,error_parameter}.

%% @spec query_log(Params)->({ok,Ret} | {error,Reasn})
%% where
%%	Params = [{Key,Op,Val}]
%%	Key = term()
%%	Op = '>' | '<' | '=' | '>=' | '=<'
%%	Val = term()
%%	Ret = [#operation_log{}]
%%	Reasn = atom()
%% @doc query log of user operation.
%% <br>Ret is a list of #operation_log{},which defined in <a href='monitor.hrl'>monitor.hrl</a></br>	
query_log(Params)when is_list(Params)->
	user_operation_log:q(Params);
query_log(_)->{error,error_parameter}.