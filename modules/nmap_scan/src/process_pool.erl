-module(process_pool).
-behaviour(gen_server).
-compile(export_all).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,code_change/3, terminate/2]).
-define(TIMEOUT,5000).
%%适当设置进程数量，如果CPU或者内存不够容易崩溃。
-define(PN,5).
-define(DIR,get_etsDir()).
-define(Dets_store,process_pool_store).
-include_lib("stdlib/include/qlc.hrl").
-record(state,{processes,n=1}).

%%打开dets
open_dets()->
    case whereis(?Dets_store) of
    undefined->dets:open_file(?Dets_store,[{file,?DIR ++atom_to_list(?Dets_store)},{keypos,1},{auto_save,1000}]);
    _->ok
    end.
%%关闭dets
close_dets()->
    dets:close(?Dets_store). 
%%返回进程池中的进程数
get_process_number()->
    ?PN.
%%启动gen_server
start(N) when is_integer(N)->
    start_link(N,[]).
    
%%启动N个进程
start_link(N,Proecsses)when is_integer(N)->
    case N =:= 0 of
    true->
        start_link(Proecsses);
    _->
        Pid = create_process(),
        start_link(N-1,Proecsses++[Pid])
    end.
    
start_link(Opts) when is_list(Opts)->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

%%停gen_server
stop() ->
    gen_server:cast(?MODULE, stop).
    
stop_all()->
    {State} = call_state(),
    ControlPid  = State#state.processes,
    stop_pid(ControlPid).

%% 进程执行   
loop_S()->
    receive
        {From,stop}->
            From ! {self(),{ok,stopped}};
        {From,{Mod,Fun,Args,User,N}} ->
            Result = Mod:Fun(Args),
            From ! {self(),{Result,User,N}},
            loop_S();
        {From,Other}->
        From ! {self(),Other},
        loop_S()
    end.

%%控制进程状态
pool_state_call(Tid,Action)->
    case erlang:is_process_alive(Tid) of
    true->
        Tid!{self(),Action},
        receive
            {Tid,Ret}->
                Ret
        after ?TIMEOUT->
            {error,timeout}
        end;
   _->
   {error,"pid is died"}
   end.

%%进程调用    
rpc(Pid,Request)->
 %%   P = proc_lib:spawn_link(fun()->loop_R(Pid) end).
    Pid ! {?MODULE,Request}.

stop_pid(Pid)->
    Pid ! {?MODULE,stop}.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% gen_server 接口 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%初始化进程状态
init([Opts])-> 
    Pid = proc_lib:spawn(fun()->process_events:loop_pool([]) end),
    [pool_state_call(Pid,{set,X}) || X<-Opts],
    {ok,#state{processes = Pid}}.

handle_cast(stop, _State) ->
    {stop,close_file, #state{processes = []}}.

handle_info(Info, State) ->
    case Info of
    {_From,{ok,loop_pool_stopped}}->
        {stop,normal,State#state{processes = []}};
    {From,{Result,User,N}} ->
        ControlPid = State#state.processes,
        pool_state_call(ControlPid,{setFree,From}),
        AddN = N-length(Result),
        add_n(AddN,User),
        case Result of
        []->
        {noreply,State};
        _->
            Fun = 
            fun(IPService)->
                open_dets(),
                case length(IPService)>0 of
                true->
                    IP = proplists:get_value(ip,IPService),
                    Ports = proplists:get_value(ports,IPService),
                    Services = [[{port,Port},{protocol,Protocol},{service,Service},{product,Product},{version,VerSion}] || {port,[{portid,Port}, {protocol,Protocol},{service,Service},{product,Product},{version,VerSion}]} <- Ports],
                    OS = proplists:get_value(osclass,proplists:get_value(os,IPService)),
                    dets:insert(?Dets_store,{{id,nmap_util:random_id()},{ip,IP},{user,User},{service,Services},{os,OS}});
                _->
                    dets:insert(?Dets_store,{{id,nmap_util:random_id()},{ip,[]},{user,User},{service,[]},{os,[]}})
                 end,
                close_dets()
            end,
            lists:foreach(Fun,Result),
            {noreply,State}
        end;
    _Other->
        {noreply, State}
     end.

add_n(0,_User)->[];
add_n(N,User)->
    open_dets(),
    dets:insert(?Dets_store,{{id,nmap_util:random_id()},{ip,[]},{user,User},{service,[]},{os,[]}}),
    close_dets(),
    add_n(N-1,User).

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_call({process},_From, State)->
    ControlPid = State#state.processes,
    case ControlPid of
    []->   {reply,{ok,"ControlPid stopped"},State};
    _->
        case pool_state_call(ControlPid,{has_a_freeOne}) of
        {ok,Pid}->
            {reply,{ok,Pid},State};
        _Err->
            {reply,{ok,"is error"},State}
        end
    end;
handle_call({all},_From,State)->
    ControlPid =State#state.processes,
    case ControlPid of 
    []->{reply,{ok,"ControlPid stopped"},State};
    _->Result = pool_state_call(ControlPid,get_all),
        {reply,{Result},State}
    end;
handle_call({state},_From,State)->
     {reply,{State},State};
handle_call({event,{Mod,Fun,Args,User,N}},_From, State)->
    ControlPid =State#state.processes,
    case ControlPid of
    []->  {reply,{ok,"ControlPid stopped"},State};
    _->
        case pool_state_call(ControlPid,{get_a_freeOne}) of
        {ok,Pid}->
            Pid1 = case erlang:process_info(Pid) of
                        undefined-> 
                        %%进程如果死了，就重新启动一个进程替代它。
                            NPid = create_process(),
                            pool_state_call(ControlPid,{replace,Pid,{NPid,"busy"}});
                        _->  Pid
                        end,
            rpc(Pid1,{Mod,Fun,Args,User,N}),
            {reply,{ok,Pid1},State};
       {error,_Err}-> 
            {reply,{ok,"Bad Argument1"},State};
        _->    
            {reply,{ok,"Bad Argument2"},State}
        end
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

start()->   start(?PN).

call(Info)->
    case whereis(?MODULE) of
        undefined   ->  {error,"gen_server stoped"};
        _   ->  gen_server:call(?MODULE,Info)
    end.
call_all()->
    call({all}). 
call_event(Info) ->
    call({event,Info}).
call_process() ->
    call({process}).
call_state() ->
    call({state}).
cast(Msg) -> 
    gen_server:cast(?MODULE, Msg).
    
%% stop_all()->
%%    {State} = call({state}), 
%%    Process = State#state.processes,
%%    [rpc(Pid,{})||{Pid,_State} <- Process].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%读取dets数据
get(Type,Condition)->
    open_dets(),
    QH =
    case Type of
    "match_ip"->
        qlc:q([[{ip,IP},{service,Services},{os,OS},{user,User}] || {{id,_ID},{ip,IP},{user,User},{service,Services},{os,OS}} <- dets:table(?Dets_store),(IP=:=Condition)],cache);
    "match_service"->
        qlc:q([[{ip,IP},{service,Services},{os,OS},{user,User}] || {{id,_ID},{ip,IP},{user,User},{service,Services},{os,OS}} <- dets:table(?Dets_store),(Services=:=Condition)],cache);
    "match_user"->
        qlc:q([[{ip,IP},{service,Services},{os,OS},{user,User}] || {{id,_ID},{ip,IP},{user,User},{service,Services},{os,OS}} <- dets:table(?Dets_store),(User=:=Condition)],cache);
    "match_user_result"->
        qlc:q([[{ip,IP},{service,Services},{os,OS},{user,User}] || {{id,_ID},{ip,IP},{user,User},{service,Services},{os,OS}} <- dets:table(?Dets_store),(User=:=Condition andalso IP=/=[])],cache);
    _->
        qlc:q([[{ip,IP},{service,Services},{os,OS},{user,User}] || {{id,_ID},{ip,IP},{user,User},{service,Services},{os,OS}} <- dets:table(?Dets_store)],cache)
    end,  
  RE = qlc:e(QH),
  close_dets(),
  RE.

%%返回全部
get_all()->
    get("","").
%%清除全部数据
clear_all_data()->
    open_dets(),
    dets:delete_all_objects(?Dets_store),
    close_dets().
 
clear_all_user_data(User)->
    open_dets(),
    dets:match_delete(?Dets_store,{'_','_',{user,User},'_','_'}),
    close_dets().

%% dets目录位置
 get_etsDir()->
	case os:type() of
		{win32,_}->
			"modules\\nmap_scan\\db\\";
		_->
			"modules/nmap_scan/db/"
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%创建一个进程
create_process()->
    proc_lib:spawn(fun()->loop_S() end).
    
create_process_N(N)->
        case  N<0 orelse N=:=0 of 
        true -> [];
        _->[{create_process(),"free"}] ++ create_process_N(N-1)
        end.



