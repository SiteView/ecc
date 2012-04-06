-module(topn_siteview).
-behaviour(gen_server).

-compile(export_all).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2,terminate/2]).

-record(state, {}).



start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
init(_)->
    initialize(),
    {ok,#state{}}.
    
handle_call(_, _From, Sv) ->
	{reply,ok,Sv}.
	
handle_cast(stop, S) ->
    {stop, normal, S};	
handle_cast(_, Sv) ->
	{noreply,  Sv}.
    
handle_info(_Info, State) ->
    case _Info of
        {'EXIT', From, normal} ->
            ok;
        {'EXIT', From, Reason} ->
            ok;
        {'DOWN', MonitorRef, Type, {RegName, Node}, Info} ->
            case lists:member(RegName, [topn_object_table:getRegName(), topn_storage:getRegName(), topn_siteview:getRegName()]) of
                true ->
                    Restart = RegName:start_link();
                _ ->
                    ok
            end;
        _ ->    
            ok
    end,
    {noreply, State}.

	
terminate(_Reason, _State) ->
    ok.

initialize() ->
    erlang:process_flag(trap_exit, true),
    %% These two processes must start a store topn work process object, a data storage topn
    {ok, TablePid} = topn_object_table:start_link(),
    {ok, StoragePid} = topn_storage:start_link(),
    {ok, SchedulerPid} = topn_scheduler:start_link(),
    erlang:monitor(process, topn_object_table:getRegName()),
    erlang:monitor(process, topn_storage:getRegName()),
    erlang:monitor(process, topn_storage:getRegName()),
    ok.