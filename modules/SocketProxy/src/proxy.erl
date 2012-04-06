%%%Author:huanghao
%%%CreateDate:2011-03-08
%%%LastModify:2011-03-17
-module(proxy).
-compile(export_all).
-record(proxy,{ip,port}).

start() ->
	io:format("Starting proxy server...~n"),
	ets:new(?MODULE, [set,named_table,public]),
	proxy_server:start(),
	case file:consult(proxy.conf) of
		{ok,Data} ->
			[{port,Port}] = Data,
			{_,Host} = inet:gethostname(),
			{_,Ipaddress} = inet:getaddr(Host, inet),
			Proxy_ip = integer_to_list(element(1,Ipaddress))++"."++integer_to_list(element(2,Ipaddress))++"."++integer_to_list(element(3,Ipaddress))++"."++integer_to_list(element(4,Ipaddress)),
			Proxy = #proxy{ip=Proxy_ip,port=Port},
			ets:insert(?MODULE,{proxy,Proxy}),
			start_proxy(Port);
		_Error ->
			io:format("An error happen on starting proxy server...~n")
	end.
	
get_proxyinfo() ->
	case ets:lookup(?MODULE,proxy) of
		[{_,Data}] -> Data;
	     _ -> []
	end.

start_proxy(Port) ->
	case gen_tcp:listen(Port,[binary,{packet,0}]) of
		{ok,ListenSocket} ->
			io:format("Start proxy server and listen port:~p ok~n",[Port]),
			spawn(fun() -> listen_connect(ListenSocket) end);
		{error,_} ->
			io:format("Listen port:~p error~n",[Port])
	end.

listen_connect(ListenSocket) ->
	{ok,Socket} = gen_tcp:accept(ListenSocket),
	io:format("An client(~p) connect to the server~n",[Socket]),
	spawn(fun() -> listen_connect(ListenSocket) end),
	loop(Socket).

loop(Socket) ->
	receive
		{tcp,Socket,Bin} ->
			try binary_to_list(Bin) of
				Receive ->
					io:format("Proxy receive data:~p~n",[string:tokens(Receive,",")]),
					case string:tokens(Receive,",") of
						["login",Host,User,Password,Port,Type] ->
							Reply = proxy_server:open(Socket,Host,list_to_integer(Port),User,Password,list_to_atom(Type));
						["command",Host,Key] ->
							Reply = proxy_server:getdatabykey(Socket,Host,Key);
						_ -> Reply = 'error format'
					end,
					io:format("Proxy reply data:~p~n",[Reply]),
					io:format("Proxy reply data[binary]:~p~n",[term_to_binary(Reply)]),
					gen_tcp:send(Socket,term_to_binary(Reply)),
					loop(Socket)
			catch
				_ -> error
			end;
		{tcp_closed,Socket} ->
			io:format("An client(~p) connect closed~n",[Socket])
	end.
	