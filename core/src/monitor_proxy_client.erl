-module(monitor_proxy_client).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	code_change/3, terminate/2]).
		 
-export([start_link/0,start_link/1,stop/0]).

-export([get_nodename/0,add_ref/1,release/1,get_stat/0,sync/1]).


-behaviour(gen_server).

-record(state, {start_time=0,wp,master,app_stat=[],cpu=0,mem=0}).
-record(stat,{total=0,cur_count=0,cur=[],his=[],cur_min=0,cur_min_count=0,max_min=0,max_min_count=0}).

-record(proxy,{name,type,app=localhost}).

-define(MAX_HIS,10).

start_link() ->
    start_link([]).
	
start_link(Opts) when is_list(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

init([Opts])->
	MasterNode=
	case server_conf:getServerConf(master_node) of
		undefined->
			{ok, Host} = inet:gethostname(),
			list_to_atom("master@" ++ Host);
		V->
			V
	end,
	Parent = self(),
	P = spawn(fun()->erlang:monitor_node(MasterNode,true),watch_master(Parent,MasterNode,pang) end),
	on_watch_exit(P,MasterNode,Parent),
	{ok,#state{start_time=sv_datetime:now(),wp=P,master=MasterNode}}.
	
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
	
	% case get(cur_min) of
		% undefined->
			% put(cur_min,Cv),
			% put(cur_min_count,1);
		% V->
			% if
				% V == Cv->
					% Mc = get(cur_min_count),
					% put(cur_min_count,Mc +1);
				% true->
					% Mc = get(cur_min_count),
					% put(cur_min,Cv),
					% put(cur_min_count,1),
					% Mmc = case get(max_min_count) of undefined->0;V2->V2 end,
					% if
						% Mc >= Mmc ->
							% put(max_time,V*60000),
							% put(max_min_count,Mc);
						% true->
							% pass
					% end
			% end
	% end,
	% Cur = [{App,Id}] ++ State#state.cur,
	{reply,ok,State#state{app_stat=AllStat}};
handle_call(get_stat, _, State) ->
	% Mc = case get(cur_min_count) of undefined->0;V1->V1 end,
	% Mmc = case get(max_min_count) of undefined->0;V2->V2 end,
	% Mt = case get(max_time) of undefined->sv_datetime:now();V3->V3 end,
	% LastMt = case get(last_get) of undefined->0;Vm1->Vm1 end,
	App = case server_conf:getServerConf(proxy_app) of undefined->localhost;V->V end,
	
	AppStat = proplists:get_value(App,State#state.app_stat,#stat{}),
	
	% Now = sv_datetime:now(),
	[CpuUsed,MemUsed] = [State#state.cpu,State#state.mem],
	
    {reply, {ok, {node(),sv_datetime:now2str(State#state.start_time),AppStat#stat.total,AppStat#stat.cur_count,
					AppStat#stat.cur,AppStat#stat.his,AppStat#stat.cur_min_count,
					AppStat#stat.max_min_count,sv_datetime:now2str(AppStat#stat.max_min),round(CpuUsed*100)/100,round(MemUsed*100)/100}}, State};
handle_call(get_nodename, _, State) ->
    {reply, {ok, node()}, State};
handle_call(connect_master, _, State) ->
	io:format("connect_master~n"),
	try
	{Type,_}=os:type(),
	App = case server_conf:getServerConf(proxy_app) of undefined->localhost;V->V end,
	case rpc:call(State#state.master,monitor_proxy_server,register_proxy,[#proxy{name=node(),type=Type,app=App}]) of
		ok->
			{reply, ok, State};
		_->
			{reply, error, State}
	end
	catch
		_:_->{reply, error, State}
	end;

handle_call({new_watch,P}, _, State) ->
    {reply, ok, State#state{wp = P}};
	
handle_call(Req, _, State) ->
    {reply, {error, {unknown_request, Req}}, State}.
	
handle_cast({cpu_stat,Cpu,Mem}, S) ->
	{noreply, S#state{cpu=Cpu,mem=Mem}};
	
handle_cast(stop, S) ->
	S#state.wp ! {self(),stop},
	{Type,_}=os:type(),
	catch(rpc:call(S#state.master,monitor_proxy_server,unregister_proxy,[#proxy{name=node(),type=Type}])),
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

watch_master(Parent,Node,pong)->
%% 	io:format("ping pong~n"),
	receive
		{nodedown,N}->
			timer:sleep(1000),
			erlang:monitor_node(Node,true),
			watch_master(Parent,Node,pang);
		{_,stop}->
			io:format("watch stopped!~n"),
			ok;
		Else->
			io:format("watch_master error msg:~p~n",[Else]),
			watch_master(Parent,Node,pong)
	after 10000->
		try
			cpu_stat(Parent),
			sync(Node)
		catch
			_:Err->io:format("error:~p~n",[Err])
		end,
		watch_master(Parent,Node,pong)
	end;
watch_master(Parent,Node,pang)->
%% 	io:format("ping pang~n"),
	receive
		{_,stop}->
			io:format("watch stopped!~n"),
			ok;
		{nodedown,N}->
			timer:sleep(1000),
			erlang:monitor_node(Node,true),
			watch_master(Parent,Node,pang);
		Else->
			io:format("watch_master error msg:~p~n",[Else]),
			watch_master(Parent,Node,pang)
	after 1000->
		case net_adm:ping(Node) of
			pong->
				case gen_server:call(?MODULE,connect_master) of
					ok->
						% erlang:monitor_node(Node,true),
						watch_master(Parent,Node,pong);
					_->
						watch_master(Parent,Node,pang)
				end;
			_->
				watch_master(Parent,Node,pang)
		end
	end.
	

sync(Node)-> 
	Now = sv_datetime:now(),
	case server_conf:getServerConf(proxy_sync) of
		undefined->
			error;
		Psync->
			F = fun(X)->
				Lt = case get(element(2,X)) of undefined->0;V1->V1 end,
				if
					Now - Lt > element(3,X) * 60000->
						case element(1,X) of
							dir->
								put(element(2,X),Now),
								file_sync:dir_sync(Node,element(2,X));
							file->
								put(element(2,X),Now),
								file_sync:file_sync(Node,element(2,X));
							_->
								error
						end;
					true->
						pass
				end
			end,
			lists:map(F,Psync)
	end.
	% Latp = case get(preferences) of undefined->0;V2->V2 end,
	% if
	%	Now - Latp > 600000->
	%		put(preferences,Now),
	%		preferences:sync(Node);
	%	true->
	%		pass
	% end.
	
cpu_stat(Parent)->
	LastMt = case get(last_get) of undefined->0;Vm1->Vm1 end,
	Now = sv_datetime:now(),
	if
		Now - LastMt > 10000->
			put(last_get,Now),
			Cu = 
			case platform:cpuUsed("",0,0,[],test,[]) of
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