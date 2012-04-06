-module(pool_manage).
-behaviour(gen_server).
-compile(export_all).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,code_change/3, terminate/2]).
-define(TIMEOUT,5000).
-define(TimeOut_T,6000).
-define(TimeOut,"10000").

-record(state, {pidaction,pidsave,n=1}).
%%%%%%%%%%%%%%%%%%    user api %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_action(IPString)->
    do_action(IPString," -T3 -sS  -sU  -O -F ").
do_action(IPString,N,NmapStr) when is_integer(N)->
    IPAddress = analyse_ip_string:analy_IPString(IPString),
    do_scan(IPAddress,N,NmapStr,length(IPAddress)),
    length(IPAddress);
do_action(IPString,NmapStr,User)->
    process_pool:clear_all_data(),
    IPAddress = analyse_ip_string:analy_IPString(IPString),
    do_scan2(IPAddress,NmapStr,User,length(IPAddress)),
    length(IPAddress).
do_action(IPString,N,NmapStr,User)->
    process_pool:clear_all_data(),
    IPAddress = analyse_ip_string:analy_IPString(IPString),
    do_scan2(IPAddress,N,NmapStr,User,length(IPAddress)),
    length(IPAddress).
do_action(IPString,NmapStr)->
    process_pool:clear_all_data(),
    IPAddress = analyse_ip_string:analy_IPString(IPString),
    do_scan(IPAddress,NmapStr,length(IPAddress)),
    length(IPAddress).

    
do_scan([],N,_NmapStr,_IPNumber)when is_integer(N)->ok;
do_scan(IPAddress,N,NmapStr,IPNumber)when is_integer(N)->
    case whereis(?MODULE) of
        undefined   ->  start_link(IPNumber);
        _   ->  ok
    end,
    IPs = lists:sublist(IPAddress,N), 
    call_action({nmap,scan,IPs++[NmapStr,"-host_timeout  " ++ integer_to_list(?TimeOut_T*N)],atom_to_list(db_ecc:domain(get(hostname)))}),
    case length(IPAddress)>N of 
    true->  do_scan(lists:nthtail(N,IPAddress),N,NmapStr,IPNumber);
    _->  do_scan([],N,NmapStr,IPNumber)
    end.

do_scan([],_NmapStr,_IPNumber)->ok;
do_scan([IP|NEXT],NmapStr,IPNumber)->
    case whereis(?MODULE) of
        undefined   ->  start_link(IPNumber);
        _   ->  ok
    end,
    call_action({nmap,scan,[IP]++[NmapStr,"-host_timeout  " ++ ?TimeOut],atom_to_list(db_ecc:domain(get(hostname)))}),
    do_scan(NEXT,NmapStr,IPNumber).
    
do_scan2([],_NmapStr,_User,_IPNumber)->ok;
do_scan2([IP|NEXT],NmapStr,User,IPNumber)->
    case whereis(?MODULE) of
        undefined   ->  start_link(IPNumber);
        _   ->  ok
    end,
    call_action({nmap,scan,[IP]++[NmapStr,"-host_timeout  " ++ ?TimeOut],User,1}),
    do_scan2(NEXT,NmapStr,User,IPNumber).
    
do_scan2([],N,_NmapStr,_User,_IPNumber)when is_integer(N)->ok;
do_scan2(IPAddress,N,NmapStr,User,IPNumber)when is_integer(N)->
    case whereis(?MODULE) of
        undefined   ->  start_link(IPNumber);
        _   ->  ok
    end,
    IPs = lists:sublist(IPAddress,N), 
    call_action({nmap,scan,IPs++[NmapStr,"-host_timeout  " ++ integer_to_list(?TimeOut_T*N)],User,N}),
    case length(IPAddress)>N of 
    true->  do_scan2(lists:nthtail(N,IPAddress),N,NmapStr,User,IPNumber);
    _->  do_scan2([],N,NmapStr,User,IPNumber)
    end.
%%获得进程池中“free”进程的数量。

stop_pid(Pid)->
    Pid ! {?MODULE,stop}.

get_state()->
    case process_pool:call_process() of
    {"free",N}->
        case N=:=process_pool:get_process_number() of
        true-> "pool free";
        _->"pool busy"
        end;
    _Other->
    "pool busy"
    end.
    
%%读出全部的扫描结果。
all_result()->
    process_pool:get_all().
getByIP(IP)->
    process_pool:get("match_ip",IP).
clear_all()->
    process_pool:clear_all_data().
%%stop_all()->
%%    {State} = call({state}),
%%    Pid = State#state.pid,
%%    erlang:exit(Pid,"stoped").
%%%%%%%%%%%%%%%%%%%%  gen_server api  %%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(N)->
    process_pool:start(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [N], []).

init([N])->
    PidSave = proc_lib:spawn_link(fun()->process_events:loop_store([]) end),
    Pidaction = proc_lib:spawn_link(fun()->loop_S(PidSave,[]) end),
    rpc(Pidaction,start),%%启动轮询
    {ok,#state{pidaction=Pidaction,pidsave=PidSave,n=N}}.

handle_cast(stop, State)->
   {stop,close_file, State}.
    
handle_info(Info, State) ->
    NewState = case Info of
    {_Pid,{ok,loop_store_stopped}} -> State#state{pidsave=[]};
    {_Pid,{ok,loop_S_stopped}} -> State#state{pidaction=[]};
    _->State
    end,
    case NewState#state.pidsave =:= [] andalso NewState#state.pidaction =:= [] of
    true-> {stop,normal,NewState};
    _->    {noreply, NewState}
    end.

terminate(_Reason, _State) ->
    ok.

stop_all()->
     {State} =call_state(),
     ActionPid = State#state.pidaction,
    SavePid = State#state.pidsave,
    stop_pid(SavePid) ,
    stop_pid(ActionPid).

stop() ->
    gen_server:cast(?MODULE, stop).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_call({state},_From,State)->
    {reply,{State},State};
handle_call({all},_From,State)->
     PidSave = State#state.pidsave,
     case PidSave of
     []->{reply,{ok,stopped},State};
     _->
     Result = action_store(PidSave,get_all),
     {reply,{ok,Result},State}
     end;
handle_call({action,{Mod,Fun,Args,User,N}},_From,State)->
    PidSave = State#state.pidsave,
    case PidSave of
        []->{reply,{ok,"ok stopped"},State};
        _->
        Eventid = nmap_util:random_id(),
        Result = action_store(PidSave,{set_one,{Eventid,{Mod,Fun,Args,User,N}}}),
        {reply,{Result},State}
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

call(Info) ->
    case whereis(?MODULE) of
        undefined   ->  {error,"gen_server stoped"};
        _   ->   gen_server:call(?MODULE, Info)
    end.
call_action(Info)->
    call({action,Info}).
call_state()->
    call({state}).
call_n()->
    {State} = call_state(),
    State#state.n.
get_all()->
    call({all}).
cast(Msg) ->
    gen_server:cast(?MODULE, Msg).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action_store(Tid,Action)->
    case erlang:is_process_alive(Tid) of
        true->
        Tid!{self(),Action},
        receive
            {Tid,Ret}->
                Ret
            after ?TIMEOUT->
                {error,timeout}
            end;
        _->{error,"pid is died"}
    end.

rpc(Pid,Message)->
    Pid ! {?MODULE,Message}.
    
%%调度进程
loop_S(Pid,LoopPid)->
    receive
    {From,{'EXIT',"ok,stoped"}}->
        From ! {self(),{ok,loop_S_stopped}}; 
    {From,stop}->
        case is_pid(LoopPid) of
            true-> erlang:exit(LoopPid,"ok,stoped");
            _-> ok
        end,
        From ! {self(),{ok,loop_S_stopped}};
    {From,start}->
        From ! {self(),{ok,started}},
        Loop = proc_lib:spawn_link(fun()->loop_A(Pid) end),  %%把轮询进程调起来
        loop_S(Pid,Loop)
    end.

%% 轮询方法
loop_A(Pid)->
    case process_pool:call_process() of
     {ok,"ControlPid stopped"}-> "stopped";
     {ok,"is error"}->
        timer:sleep(50),
        loop_A(Pid);
     {ok,_}  ->
        case action_store(Pid,{get_one}) of  %%读出一个消息
        {error,_}-> timer:sleep(50);
        {ok,{Eventid,Event}} ->
            process_pool:call_event(Event),
            action_store(Pid,{remove_one,Eventid});
        _->  timer:sleep(50)
        end,
        loop_A(Pid);         
    _->
        timer:sleep(100),
        loop_A(Pid)
    end.
    