-module(monitor_local_stat).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	code_change/3, terminate/2]).
		 
-export([start_link/0,start_link/1,stop/0]).

-export([get_nodename/0,add_ref/1,release/1,get_stat/0]).


-behaviour(gen_server).

-record(state, {start_time=0,wp,app_stat=[],cpu=0,mem=0}).
-record(stat,{total=0,cur_count=0,cur=[],his=[],cur_min=0,cur_min_count=0,max_min=0,max_min_count=0}).

-define(MAX_HIS,10).

start_link() ->
    start_link([]).
	
start_link(Opts) when is_list(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

init([Opts])->
	Parent = self(),
	P = spawn(fun()->watch_master(Parent,node(),pang) end),
	on_watch_exit(P,node(),Parent),
	{ok,#state{start_time=sv_datetime:now(),wp=P}}.

	
stop() ->
    gen_server:cast(?MODULE, stop).
	
get_nodename()->
	gen_server:call(?MODULE, get_nodename).
	
get_stat()->
	gen_server:call(?MODULE, get_stat).

add_ref({App,Id})->
	gen_server:call(?MODULE, {add_ref,{App,Id}}).
	
release({App,Id})->
	gen_server:call(?MODULE, {release,{App,Id}}).
	

	

handle_call({release,{App,Id}}, _, State) ->
	AppStat = proplists:get_value(App,State#state.app_stat, #stat{}),
	Cur = lists:delete(Id,AppStat#stat.cur),
	HisLen = length(AppStat#stat.his),
	His = 
	if
		HisLen > ?MAX_HIS ->
			[Id] ++ lists:sublist(AppStat#stat.his,?MAX_HIS);
		true->
			[Id] ++ AppStat#stat.his
	end,
	NewAppStat = AppStat#stat{cur_count=AppStat#stat.cur_count-1,total=AppStat#stat.total+1,cur=Cur,his=His},
	AllStat = lists:keyreplace(App,1,State#state.app_stat,{App,NewAppStat}),
	{reply,ok,State#state{app_stat=AllStat}};
handle_call({add_ref,{App,Id}}, _, State) ->
	AppStat = proplists:get_value(App,State#state.app_stat,#stat{cur_min=sv_datetime:now() div 60000,max_min=sv_datetime:now()}),
	Cv = sv_datetime:now() div 60000,
	NewAppStat = 
	if
		Cv == AppStat#stat.cur_min ->
			AppStat#stat{cur_min_count=AppStat#stat.cur_min_count+1,cur = [Id] ++ AppStat#stat.cur,cur_count = AppStat#stat.cur_count+1};
		true ->
			if
				AppStat#stat.cur_min_count >= AppStat#stat.max_min_count ->
					AppStat#stat{cur_min=Cv, cur_min_count=1,cur = [Id] ++ AppStat#stat.cur,cur_count = AppStat#stat.cur_count+1,
								max_min=AppStat#stat.cur_min*60000,max_min_count=AppStat#stat.cur_min_count};
				true ->
					AppStat#stat{cur_min=Cv, cur_min_count=1,cur = [Id] ++ AppStat#stat.cur,cur_count = AppStat#stat.cur_count+1}
			end
	end,
	AllStat = 
	case lists:keymember(App,1,State#state.app_stat) of
		true->
			lists:keyreplace(App,1,State#state.app_stat,{App,NewAppStat});
		_->
			State#state.app_stat ++ [{App,NewAppStat}]
	end,
	

	{reply,ok,State#state{app_stat=AllStat}};
handle_call(get_stat, _, State) ->

	
	AppStat = proplists:get_value(localhost,State#state.app_stat,#stat{}),

	[CpuUsed,MemUsed] = [State#state.cpu,State#state.mem],
	
    {reply, {ok, {node(),sv_datetime:now2str(State#state.start_time),AppStat#stat.total,AppStat#stat.cur_count,
					AppStat#stat.cur,AppStat#stat.his,AppStat#stat.cur_min_count,
					AppStat#stat.max_min_count,sv_datetime:now2str(AppStat#stat.max_min),round(CpuUsed*100)/100,round(MemUsed*100)/100}}, State};
handle_call(get_nodename, _, State) ->
    {reply, {ok, node()}, State};


handle_call({new_watch,P}, _, State) ->
    {reply, ok, State#state{wp = P}};
	
handle_call(Req, _, State) ->
    {reply, {error, {unknown_request, Req}}, State}.
	
handle_cast({cpu_stat,Cpu,Mem}, S) ->
	{noreply, S#state{cpu=Cpu,mem=Mem}};
	
handle_cast(stop, S) ->
	S#state.wp ! {self(),stop},

    {stop, normal, S};

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.	
	
on_watch_exit(Pid,Node,M)->
	spawn(fun()->process_flag(trap_exit,true),
				link(Pid),
				receive
					{'EXIT',Pid,normal}->
						io:format("watch Pid:~p,Exit:~p~n",[Pid,normal]);
					{'EXIT',Pid,Why}->
						% M ! {Pid,event_done,E,Why},
						io:format("watch Pid:~p,Exit:~p~n",[Pid,Why]),
						P = spawn(fun()->erlang:monitor_node(Node,true),watch_master(M,Node,pang) end),
						case gen_server:call(M,{new_watch,P}) of
							ok->
								on_watch_exit(P,Node,M);
							_->
								P ! {self(),stop}
						end,
						ok
				end
		end).

watch_master(Parent,Node,_)->
	receive
		{_,stop}->
			io:format("watch stopped!~n"),
			ok;
		Else->
			io:format("watch_master error msg:~p~n",[Else]),
			watch_master(Parent,Node,pong)
	after 10000->
		%%try
			cpu_stat(Parent),
		%%catch
			%%_:Err->io:format("----aaqc----error:~p~n",[Err])
		%%end,
		watch_master(Parent,Node,pong)
	end.
	
	
cpu_stat(Parent)->
	LastMt = case get(last_get) of undefined->0;Vm1->Vm1 end,
	Now = sv_datetime:now(),
	if
		Now - LastMt > 10000->
			put(last_get,Now),
			Cu = 
			case platform:cpuUsed("",0,0,[0],test,[]) of
				[V5,_,_,_]->
					V5;
				_->
					0
			end,
			Mu = 
			case platform:getMemoryFull("",0,0) of
				{ok,[V6|_]}->
					V6;
				_->
					0
			end,
			gen_server:cast(Parent,{cpu_stat,Cu,Mu});
		true->
			pass
	end.