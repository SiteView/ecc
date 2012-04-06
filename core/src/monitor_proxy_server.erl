-module(monitor_proxy_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,code_change/3, terminate/2]).

-export([start_link/0,start_link/1,stop/0]).

-export([get_stat/0,register_proxy/1,get_node/3,get_proxy_list/0,add_proxy/1,unregister_proxy/1,remove_proxy/1]).

-behaviour(gen_server).

-define(TIMEOUT,5000).

-record(state, {proxy=[],wp}).
-record(proxy,{name,type,app}).

start_link() ->
    start_link([]).
	
start_link(Opts) when is_list(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []).
	

init([Opts])->
	Wp = spawn(fun()->watch_proxy(?MODULE) end),
	proxy_mapping:start_link(),
	{ok,#state{wp=Wp}}.
	
stop() ->
    gen_server:cast(?MODULE, stop).
	
get_node(App,Id,Platform)->
	gen_server:call(?MODULE,{get_node,App,Id,Platform}).
	
get_stat()->
	gen_server:call(?MODULE, {get_stat,dbcs_base:get_app()}).
	
get_proxy_list()->
	gen_server:call(?MODULE, {get_proxy_list,dbcs_base:get_app()}).
	
register_proxy(Proxy)->
	gen_server:call(?MODULE, {register_proxy,Proxy}).
	
unregister_proxy(Proxy)->
	gen_server:call(?MODULE, {unregister_proxy,Proxy}).

% ��Ӵ���ڵ�	
add_proxy(Node)->
	case net_adm:ping(Node) of
		pang->
			{error,"node down"};
		pong->
			io:format("auth:~p,realm:~p~n",[node(),server_conf:get_db_node()]),
			rpc:call(Node,monitor_proxy,require_register,[node(),server_conf:get_db_node(),false]);
		Else->
			{error,lists:flatten(io_lib:format("~p",[Else]))}
	end.
	
remove_proxy(Node)->
	case net_adm:ping(Node) of
		pang->
			{error,"node down"};
		pong->
			rpc:call(Node,monitor_proxy_client,stop,[]);
		Else->
			{error,lists:flatten(io_lib:format("~p",[Else]))}
	end.
	
handle_call({register_proxy,Proxy}, _, State) ->
	case lists:member(Proxy,State#state.proxy) of
		true->
			{reply,ok,State};
		_->
			State#state.wp ! {self(),{watch,Proxy#proxy.name}},
			receive
				{_,ok}->
					{reply,ok,State#state{proxy=State#state.proxy++[Proxy]}};
				_->
					{reply,error,State}
			after ?TIMEOUT->
				{reply,error,State}
			end
	end;
handle_call({unregister_proxy,Proxy}, _, State) ->
	case lists:member(Proxy,State#state.proxy) of
		true->
			Nodes = lists:filter(fun(X)->
							X#proxy.name=/=Proxy#proxy.name
						end,State#state.proxy),
			{reply,ok,State#state{proxy=Nodes}};
		_->
			{reply,ok,State}
	end;
handle_call({proxydown,N}, _, State) ->
	Nodes = lists:filter(fun(X)->
							X#proxy.name=/=N
						end,State#state.proxy),
	{reply,ok,State#state{proxy=Nodes}};
	
handle_call({get_stat,App}, _, State) ->
	F = fun(X,R)->
		case X#proxy.app of
			App->
				case rpc:call(X#proxy.name,monitor_proxy_client,get_stat,[]) of
					{ok,St}->
						R ++ [St];
					_->
						R
				end;
			_->
				R
		end
	end,
	Stats = lists:foldl(F,[],State#state.proxy),
	{reply, {ok, Stats}, State};

handle_call({get_node,App,Id,Platform}, _, State) ->
	% io:format("get node:~p,~p,~p~n",[App,Id,Platform]),
	case proxy_mapping:get(App,Id) of
		{ok,[]}->
			% Nodes = 
				% case Platform of
					% ""->

						% lists:foldl(fun(X,R)->
								% case X#proxy.app == App of
									% true->
										% R ++ [X];
									% _->
										% R
								% end end,[],State#state.proxy);
					% _->
						% lists:foldl(fun(X,R)->
								% case lists:member(X#proxy.type,Platform) andalso  X#proxy.app == App of
									% true->
										% R ++ [X];
									% _->
										% R
								% end end,[],State#state.proxy)
				% end,
			% Len = length(Nodes),
			% if
				% Len < 1 ->
					% {reply,{error,no_proxy},State};
				% true->
					% Rd = random:uniform(Len),
					% N = lists:nth(Rd,Nodes),
					% {reply,{ok,N#proxy.name},State}
			% end;
			
			{reply,{error,no_proxy},State};
			
		{ok,Ret}->
			MNodes = lists:foldl(fun(X,R)->R ++ [element(2,X)] end,[],Ret),
			Nodes = lists:foldl(fun(X,R)->
									case lists:member(X#proxy.name,MNodes) andalso X#proxy.app == App of
										true->
											R ++ [X];
										_->
											R
									end
								end,[],State#state.proxy),
			Len = length(Nodes),
			if
				Len < 1 ->
					{reply,{error,no_proxy},State};
				true->
					Rd = random:uniform(Len),
					N = lists:nth(Rd,Nodes),
					{reply,{ok,N#proxy.name},State}
			end;
		_->
			{reply,{error,no_proxy},State}
			% Nodes = State#state.proxy,
			% Len = length(Nodes),
			% if
				% Len < 1 ->
					% {reply,{error,no_proxy},State};
				% true->
					% Rd = random:uniform(Len),
					% N = lists:nth(Rd,Nodes),
					% {reply,{ok,N#proxy.name},State}
			% end
	end;
	
handle_call({get_proxy_list,App}, _, State) ->
	F = fun(X,R)->
		% io:format("App:~p,~p~n",[App,X#proxy.app]),
		case X#proxy.app of
			App->
				R ++ [{X#proxy.name,X#proxy.type}];
			_->
				R
		end
	end,
	Proxys = lists:foldl(F,[],State#state.proxy),
	{reply, {ok, Proxys}, State};
	
handle_call(Req, _, State) ->
    {reply, {error, {unknown_request, Req}}, State}.
	
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
	_State#state.wp ! {self(),stop},
    ok.	
	
watch_proxy(Parent)->
	receive
		{From,{watch,Proxy}}->
			io:format("watch proxy:~p~n",[Proxy]),
			case erlang:monitor_node(Proxy,true) of
				true->
					From ! {self(),ok};
				_->
					From ! {self(),error}
			end,
			watch_proxy(Parent);
		{nodedown,N}->
			gen_server:call(?MODULE,{proxydown,N}),
			watch_proxy(Parent);
		{From,stop}->
			ok
	end.
	