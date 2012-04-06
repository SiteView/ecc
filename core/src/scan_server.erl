-module(scan_server).
-behaviour(gen_server).

-export([start_link/0,start_link/1,stop/0]).
-export([start_scan/3,stop_scan/0,get_scan_info/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-define(SERVER,   'elecc_scan_server').
-define(SCANER_COUNT,5).

-record(state, {state,scan_type,scan_target,scan_params=[],scan_time,done_time,scan_progress="",scan_pid,
				kill_scan=false,total_hosts=0,scaned_hosts=0,result=[],scaners=0}).
-record(scan,{type,target,params}).

start_link() ->
    start_link([]).
	
start_link(Opts) when is_list(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Opts], []).
	
init([Opts])->
	{ok,#state{state=idle}}.
	
stop() ->
    gen_server:cast(?SERVER, stop).
	
start_scan(Type,Target,Params)->
	start_link(),
	gen_server:call(?SERVER, {start_scan,Type,Target,Params}).
	
stop_scan()->
	gen_server:call(?SERVER, {stop_scan}).

get_scan_info()->
	gen_server:call(?SERVER, {get_scan_info}).



handle_call({start_scan,Type,Target,Params}, _, State) ->
	case State#state.state of
		busy->
			{error,busy};
		_->
			Pid = spawn(fun()->do_scan(#scan{type=Type,target=Target,params=Params}) end),
			on_exit(Pid),
			{reply,ok,#state{state=busy,scan_type=Type,
				scan_target=Target,scan_params=Params,
				scan_pid=Pid,scan_time=erlang:localtime()}}
	end;
handle_call({stop_scan}, _, State) ->
	{reply,ok,State#state{kill_scan=true}};
handle_call({get_scan_info}, _, State) ->
			{reply,
					{ok,{State#state.state,State#state.scan_type,State#state.scan_target,State#state.scan_params,
					State#state.scan_time,State#state.done_time,State#state.scan_progress,State#state.total_hosts,
					State#state.scaned_hosts,State#state.result,State#state.kill_scan
					}},
			State};
	
handle_call({set_progress,Ip,Str}, _, State) ->
	_OR = State#state.result,
	F = fun(X)->
		OIp=proplists:get_value(ip,X),
		case OIp of
			Ip->
				lists:keyreplace(scan_status,1,X,{scan_status,scanning});
			_->
				X
		end
	end,
	_NR=[F(X)||X<-_OR],
	{reply,ok,State#state{scan_progress=Str,result=_NR}};
handle_call({add_scan_host,Hs}, _, State) ->
	Ip = proplists:get_value(ip,Hs),
	_OR = State#state.result,
	F = fun(X)->
		OIp=proplists:get_value(ip,X),
		case OIp of
			Ip->
				Hs++[{scan_status,scan_done}];
			_->
				X
		end
	end,
	_NR=[F(X)||X<-_OR],
	{reply,ok,State#state{scaned_hosts=State#state.scaned_hosts+1,result=_NR}};
handle_call({set_host_status,Ip,Status}, _, State) ->
	_OR = State#state.result,
	F = fun(X)->
		OIp=proplists:get_value(ip,X),
		case OIp of
			Ip->
				lists:keyreplace(scan_status,1,X,{scan_status,Status});
			_->
				X
		end
	end,
	_NR=[F(X)||X<-_OR],
	{reply,ok,State#state{result=_NR}};
handle_call({total_hosts,Count,Ret}, _, State) ->
	NR = [X++[{scan_status,waiting}]||X<-Ret],
	{reply,ok,State#state{total_hosts=Count,result = NR}};
handle_call({scan_done}, _, State) ->
	{reply,ok,State#state{state=idle,done_time=erlang:localtime()}};
handle_call({get_stop_flag}, _, State) ->
	{reply,State#state.kill_scan,State};

handle_call(Req, _, State) ->
    {reply, {error, {unknown_request, Req}}, State}.


handle_cast(stop, S) ->
    {stop, normal, S};

handle_cast(_, State) ->
    {noreply, State}.

handle_info(Info, State) ->
	io:format("~p :receive info:~p~n",[?MODULE,Info]),
    {noreply, State}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
	
%% ---------------------------------------------------------------------------------

set_total_hosts(Count,Ret)->
	gen_server:call(?SERVER, {total_hosts,Count,Ret}).
	
add_scan_host(Hs)->
	gen_server:call(?SERVER, {add_scan_host,Hs}).
	
scan_done()->
	gen_server:call(?SERVER, {scan_done}).
	
set_progress_str(Ip,Str)->
	gen_server:call(?SERVER, {set_progress,Ip,Str}).
	
get_stop_flag()->
	gen_server:call(?SERVER, {get_stop_flag}).
	
set_host_status(Ip,Status)->
	gen_server:call(?SERVER, {set_host_status,Ip,Status}).
	
do_scan(P=#scan{})->
	set_progress_str("","discovery..."),
	% Ret = nmap_scan:fast_scan(P#scan.target),
	Ret = do_discovery(P),
	set_total_hosts(length(Ret),Ret),
	scaner_manager(Ret,0,P#scan.params).
	
	
do_discovery(P)->
	snmp_ex2_manager:start_link(),
	% Ret = nmap_scan:fast_scan(P#scan.target),
	Coms = case proplists:get_value(community,P#scan.params) of
		undefined->
			[];
		Cms->
			string:tokens(Cms,",")
	end,
	ArpDf = case proplists:get_value(arp_discovery,P#scan.params) of
				true->
					true;
				_->
					false
			end,
			
	io:format("Community:~p,Auto_discover:~p~n",[Coms,ArpDf]),
	filterResult(do_discovery_loop([P#scan.target],[],ArpDf,Coms),[]).
	
filterResult([],_)->[];
filterResult([H|T],V)->
	Ip = proplists:get_value(ip,H),
	case lists:member(Ip,V) of
		true->
			filterResult(T,V);
		_->
			[H] ++ filterResult(T,V++[Ip])
	end.

do_discovery_loop([],_,_,_)->[];
do_discovery_loop([Tg|T],V,Af,Communitys)->
	case get_stop_flag() of
		true->
			[];
		_->
			case (lists:prefix("127.0.0",Tg) or lists:member(Tg,V)) of
				true ->
					do_discovery_loop(T,V,Af,Communitys);
				_->
					set_progress_str("","discovery..." ++ Tg),
					% Ret = nmap_scan:fast_scan(Tg),
					Ret = nmap_scan:scan(Tg,["-n -T4 -sU -p U:161"]),
					case Af of
						true ->
							% Ips = [proplists:get_value(ip,X)||X<-Ret],
							Ips = get_snmp_hosts(Ret),
							Tgs = snmp_read_arp:get_ip_from_arp(Ips,Communitys),
							io:format("subnet:~p~n",[Tgs]),
							NV  = V ++ [Tg],
							Ret ++ do_discovery_loop(T ++ Tgs,NV,Af,Communitys);
						_->
							Ret
					end
			end
	end.
	
get_snmp_hosts([])->[];
get_snmp_hosts([S|T])->
	case proplists:get_value(ports,S) of
		undefined->
			get_snmp_hosts(T);
		[]->
			get_snmp_hosts(T);
		Ports->
			F = fun({_,X})->
				case proplists:get_value(portid,X) of
					"161"->
						true;
					_->
						false
				end
			end,
			case lists:any(F,Ports) of
				true->
					[proplists:get_value(ip,S)] ++ get_snmp_hosts(T);
				_->
					get_snmp_hosts(T)
			end
	end.
		
	

scaner_manager([],0,_)->[];
scaner_manager([],C,Params)->
	receive
		{scan_done,Ip,Rt}->
			case Rt of
				normal->
					set_host_status(Ip,scan_done);
				_->
					set_host_status(Ip,error)
			end,
			scaner_manager([],C-1,Params);
		_->
			scaner_manager([],C,Params)
	end;
scaner_manager([H|T],C,Params)->
	receive
		{scan_done,Ip,Rt}->
			case Rt of
				normal->
					set_host_status(Ip,scan_done);
				_->
					set_host_status(Ip,scan_error)
			end,
			start_scaner(H,Params),
			scaner_manager(T,C,Params);
		_->
			scaner_manager([H]++T,C,Params)
	after 10->
		Max = case proplists:get_value(scan_threads,Params) of
				undefined->
					?SCANER_COUNT;
				Scount when is_integer(Scount)->
					Scount;
				_->
					?SCANER_COUNT
			end,
		if
			C < Max -> % limit max scaner
				start_scaner(H,Params),
				scaner_manager(T,C+1,Params);
			true->
				scaner_manager([H]++T,C,Params)
		end
	end.
	
start_scaner(X,Params)->
	Pid = spawn(fun()->do_scan_host([X],Params) end),
	on_scaner_exit(Pid,self(),proplists:get_value(ip,X)).
	
do_scan_host([],_)->
	[];
do_scan_host([H|T],Params)->
	case get_stop_flag() of
		true->
			[];
		_->
			case proplists:get_value(ip,H) of
				undefined->
					pass;
				Ip->
					set_progress_str(Ip,"scan target:"++Ip ++ " ..."),
					Scanp = ["-n","-sU","-sT","-O","-F","-T4"] ++ 
							case proplists:get_value(top_ports,Params) of
								undefined->
									[];
								TopPorts->
									["--top-ports " ++ TopPorts]
							end,
					case nmap_scan:scan(Ip,Scanp) of
						[]->
							[];
						[S|_]->
							Ports = proplists:get_value(ports,S),
							F = fun(X,Y)->
								case X of
									"udp"->
										"U:" ++ Y;
									_->
										"T:" ++ Y
								end
							end,
							PortsParam = [F(proplists:get_value(protocol,X),proplists:get_value(portid,X))||{_,X}<-Ports],
							Cmds = ["-n -sV --version-light -sU -sT -T4 -p"] ++ [string:join(PortsParam,",")],
							NPorts = case nmap_scan:scan(Ip,Cmds) of
								[]->
									[];
								[NS|_]->
									proplists:get_value(ports,NS)
								end,
							case NPorts of
								[]->
									update_ci(S),
									add_scan_host(S);
								_->
									S2 = lists:keyreplace(ports,1,S,{ports,NPorts}),
									update_ci(S2),
									add_scan_host(S2)
							end
					end
			end,
			do_scan_host(T,Params)
	end.
	
on_exit(Pid)->
	spawn(fun()->process_flag(trap_exit,true),
				link(Pid),
				receive
					{'EXIT',Pid,Why}->
						io:format("Pid:~p,Exit:~p~n",[Pid,Why]),
						set_progress_str("","scan done"),
						scan_done()
				end
				end).
				
on_scaner_exit(Pid,ParentId,Ip)->
	spawn(fun()->process_flag(trap_exit,true),
				link(Pid),
				receive
					{'EXIT',Pid,Why}->
						io:format("scaner Pid:~p,Exit:~p~n",[Pid,Why]),
						ParentId ! {scan_done,Ip,Why}
				end
				end).
				
update_ci(X)->
	case proplists:get_value(ip,X) of
		undefined->
			pass;
		Ip->
			case erlcmdb:find_ci(["ip=" ++ Ip]) of
				[]->
					erlcmdb:create_ci("Server",X);
				[Ci|_]->
					Id = proplists:get_value(id,Ci),
					erlcmdb:update_ci("Server",Id,X)
			end
	end.