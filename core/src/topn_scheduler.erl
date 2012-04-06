%% ---
%%TopN调度器模块
%%
%%---
-module(topn_scheduler).
-behaviour(gen_server).

-compile(export_all).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2,terminate/2]).
-export([getRegName/0]).

-record(state, {schedule}).

-define(TEMPLATENAME, "templates.topn/TopNTemplate.topn").

-include("topn.hrl").


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
init(_)->
    State = initialize(),
    {ok,State}.
    
handle_call(_, _From, Sv) ->
	{reply,ok,Sv}.
	
handle_cast(stop, S) ->
    {stop, normal, S};	
handle_cast(_, Sv) ->
	{noreply,  Sv}.
    
handle_info(_Info, State) ->
    NState =
    case _Info of
        {'EXIT', From, normal} ->
            State;
        {'EXIT', From, Reason} ->
            State;
        {timeout, TimerRef, {atomic_topn, M}} ->
            handle_schedule(M, State);
        _ ->    
            State
    end,
    {noreply, NState}.

	
terminate(_Reason, _State) ->
    ok.

initialize() ->
    erlang:process_flag(trap_exit, true),
    %% 加载topn工作进程
    load(),
    Schedules = load_schedule(),
    #state{schedule=Schedules}.
    
handle_schedule(M, State) ->
    schedule(M),
    case load_schedule(M) of
        {Id, TimerRef, M} ->
            Schedules = State#state.schedule,
            Sches = lists:keystore(Id, 1, Schedules, {Id, TimerRef, M}),
            State#state{schedule=Sches};
        _ ->
            State
    end.

%% 加载topn进程到schedule里面去
load_schedule(M) ->
    case M:get_property(?TOPN_FREQUENCY) of
        {ok, {?TOPN_FREQUENCY,Frequency}} ->
            {ok, {?TOPN_ID, Id}} = M:get_property(?TOPN_ID),
            {Id, erlang:start_timer(Frequency, ?MODULE, {atomic_topn, M}), M};
        _ ->
            []
    end.

%% 加载所有topn到schedule里面去
load_schedule() ->
    TopNObjs = topn_object_table:read(),
    [load_schedule(X)||{Id, X}<-TopNObjs, erlang:is_process_alive(X:getTid())].
    

%% 执行调度
schedule(M) ->
    erlang:spawn(fun()-> update(M) end).

update(M) ->
    M:update(),
    ok.

load() ->
    Result =
    case file:consult(?TEMPLATENAME) of
        {ok, Conf} ->
            load_simple(Conf, []);
        _ ->
            {error, "load topn error"}
    end,
    case Result of
        {ok, TopNs} ->
            Fun = 
                fun(X) ->
                    case X:get_property(?TOPN_ID) of
                        {ok, {?TOPN_ID, Id}} ->
                            topn_object_table:write({Id, X});
                        _ ->
                            ok
                    end
                end,
            lists:foreach(Fun, TopNs),
            ok;
        _ ->
            ok
    end.
   
    
load_simple([], Re) ->
    {ok, Re};
load_simple([A|B], Re) when erlang:is_list(A) ->
    case lists:keysearch(?TOPN_CLASS, 1, A) of
        {value, {?TOPN_CLASS, Class}} ->
            TopN = Class:new(),
            [TopN:set_property(XKey, XValue)||{XKey, XValue}<-A],
            load_simple(B, [TopN|Re]);
        _ ->
            load_simple(B, Re)
    end;
load_simple([A|B], Re) ->
    load_simple(B, Re).
    
    
%% api

getRegName() ->
    ?MODULE.