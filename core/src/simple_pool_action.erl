%% @author Cheng kaiyang <kaiyang.cheng@dragonflow.com>
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc simple process pool action.
-module(simple_pool_action).
-compile(export_all).
-record(state, {request, process=[], result=[], total=0, finish=0, options=[{pool_timeout, 3600000}, {worker_timeout, 60000}, {process, 5}, {loop, fun(X)->X end}, {save_result, true}], start}).
-record(workstate, {idle=true, used=0}).

%main process init
start_pool() ->
    process_flag(trap_exit, true),
    %timer:start(),
    %timer:kill_after(proplists:get_value(pool_timeout, Opt)),
    {T,_} = statistics(wall_clock),
    wait_for_request(#state{start=T}).

%main process
wait_for_request(State) ->
    receive
        %parameters settings
        {From, {set_option, Options}} ->
            NewState = State#state{options=Options},
            From ! {self(), ok},
            wait_for_request(NewState);
        %stop request process
        {From, cancel_request} ->
            ReqPid = State#state.request,
            Reply = case State#state.request of
                undefined ->
                    {error, request_not_found};
                ReqPid ->
                    ReqPid ! {self(), stop},
                    {ok, stopped}
            end,
            From ! {self(), Reply},
            wait_for_request(State);
        %record request process and task 
        {From, {request, ReqId, Total}} ->
            From ! {self(), {ok, start}},
            {T,_} = statistics(wall_clock),
            wait_for_request(State#state{request=ReqId, total=Total, start=T, finish=0, result=[]});
        %finish request
        {_From, request_finish} ->
            io:format("request finish!~n"),
            wait_for_request(State);
        %get  use process
        {From, get_idle_process} ->
            Process = State#state.process,
            IdleProcess = [{K, V#workstate.used}||{K,V}<- Process, V#workstate.idle==true],
            List = lists:sort(fun(X, Y)->if element(2, X)<element(2, Y)-> true;true->false end end, IdleProcess),
            %io:format("IdleProcess:~p~n", [IdleProcess]),
            {Reply, NewState} = if
                length(List)>0 ->
                    IP = hd(List),
                    io:format("chose:~p~n", [IP]),
                    {element(1, IP), State};
                true ->
                    TotalProcess = proplists:get_value(process, State#state.options),
                    if
                        length(Process)<TotalProcess ->
                            NewWorker = spawn_link(fun()->worker_loop() end),
                            {NewWorker, State#state{process=[{NewWorker, #workstate{idle=false, used=0}}|Process]}};
                        true ->
                            {{error, not_found}, State}
                    end
            end,
            From ! {self(), Reply},
            wait_for_request(NewState);
        %customer request
        {_From, {client, IdlePid, Mod, Fun, Args}} ->
            IdlePid ! {self(), {assign_work, Mod, Fun, Args}},
            ProcessState = State#state.process,
            NewState = case lists:keysearch(IdlePid, 1, ProcessState) of
                {value, {_, WorkState}} ->
                    WorkerUsed = WorkState#workstate.used + 1,
                    State#state{process = lists:keyreplace(IdlePid, 1, ProcessState, {IdlePid, WorkState#workstate{idle=false, used=WorkerUsed}})};
                _ ->
                    io:format("~p not found in process list~n",[IdlePid]),
                    State
            end,
            wait_for_request(NewState);
        %Report on the work process, and push the process to request to receive the next request
        {From, {worker, Response}} ->
            ProcessState = State#state.process,
            Result = State#state.result,
            NewState = case lists:keysearch(From, 1, ProcessState) of
                false ->
                    io:format("unkown worker:~p~n",[From]),
                    State;
                {value, {_, WorkState}} ->
                    NewWorkState = WorkState#workstate{idle=true},
                    Finished = State#state.finish,
                    Save = proplists:get_value(save_result, State#state.options),
                    if
                        Save ->
                            DealWith = proplists:get_value(loop, State#state.options),
                            NR = try DealWith(Response) of
                                Value ->
                                    Value
                            catch
                            _:X ->
                            io:format("handle with result:~p, error occour:~p~n", [Response, X]),
                            Response
                            end,
                            State#state{process=lists:keyreplace(From, 1, ProcessState, {From, NewWorkState}), result=[NR|Result], finish = Finished+1};
                        true ->
                            State#state{process=lists:keyreplace(From, 1, ProcessState, {From, NewWorkState}), finish = Finished+1}
                    end
            end,
            case NewState#state.request of
                undefined ->
                    nothing;
                ReqId ->
                    ReqId ! {self(), {wait_for_request, From}}
            end,
            wait_for_request(NewState);
        %stop process pool
        {From, stop} ->
            case State#state.request of
                undefined ->
                    nothing;
                ReqId ->
                    ReqId ! {self(), stop}
            end,
            lists:map(fun({P, _S}) ->P ! {self(), stop} end, State#state.process),
            From ! {self(),{ok,stopped}};
        % clear pool
        {From, clear} ->
            case State#state.request of
                undefined ->
                    nothing;
                ReqId ->
                    ReqId ! {self(), stop}
            end,
            lists:map(fun({P, _S}) ->P ! {self(), stop} end, State#state.process),
            From ! {self(),{ok,cleared}},
            {T,_} = statistics(wall_clock),
            wait_for_request(#state{start=T});
        %work status
        {From, work_state} ->
            {Now,_} = statistics(wall_clock),
            From ! {self(), {State#state.finish, State#state.total, round((Now - State#state.start)/1000)}},
            wait_for_request(State);
        %process pool status
        {From, pool_state} ->
            From ! {self(), State#state.process},
            wait_for_request(State);
        %result
        {From, result} ->
            From ! {self(), State#state.result},
            wait_for_request(State);
        {_From, {ok, stopped}} ->
            wait_for_request(State);
        {'EXIT',Worker,_} ->
            ProcessList = State#state.process,
            NewState = case proplists:get_value(Worker, ProcessList) of
                undefined ->
                    State;
                _V ->
                    State#state{process = lists:keydelete(Worker, 1, ProcessList)}
            end,
            wait_for_request(NewState);
        Other ->
            io:format("simple pool receive unkown request: ~p~n", [Other]),
            wait_for_request(State)
    end.
    
%work process
worker_loop() ->
    receive
        %process pool command to  worker
        {From, {assign_work, Mod, Fun, Args}} ->
            Response = 
            try erlang:apply(Mod, Fun, Args) of
                Result ->
                    Result
            catch
            _:X ->X
            end,
            From ! {self(), {worker, Response}},
            worker_loop();
        {From, stop} ->
            From ! {self(), {ok, stopped}};
        Other ->
            io:format(lists:flatten(io_lib:format("~p: unkown request:~p~n",[self(), Other]))),
            worker_loop()
    end.
    
sleep(Time)->
	receive
	after Time ->
	ok
	end.