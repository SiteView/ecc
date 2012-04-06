-module(kvs_main).

-export([start/0, stop/0]).

%%-export([start/2, stop/1]).

%-behaviour(application).

-include("kvs_define.hrl").

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}      |
%%          {ok, Pid, State} |
%%          {error, Reason}   
%%--------------------------------------------------------------------
%start(Type, StartArgs) ->
%        case 'TopSupervisor':start_link(StartArgs) of
%    	{ok, Pid} -> 
%    	    {ok, Pid};
%    	Error ->
%    	    Error
%        end.

start() ->
    kvs_config:init(),
    kvs_index:init(),
    kvs_cache:init(),
    kvs_search:init(),
    kvs_rollup:init(),
    kvs_storage:init(),
    io:format("*******************Key-Value Store OK*************************~n").

%%--------------------------------------------------------------------
%% Func: stop/1
%% Returns: any 
%%--------------------------------------------------------------------
%stop(State) ->
%    mnesia:stop(),
%    ok.
stop() ->
    kvs_storage:stop(),
    kvs_rollup:stop(),
    kvs_cache:stop(),
    kvs_search:stop(),
    kvs_index:stop(),
    kvs_config:stop(),
    ok.