%%%Author:huanghao
%%%CreateDate:2011-03-08
%%%LastModify:2011-03-15
-module(proxy_client).
-compile(export_all).
-record(info,{socket,host}).

start() ->
	Tables = ets:all(),
	case lists:member(?MODULE,Tables) of
		false -> ets:new(?MODULE, [set,named_table,public]);
		true -> ok
	end,
	%~ connect_proxy("192.168.9.179",2323).
	connect_proxy("192.168.9.88",2323).

test() ->	
	Tables = ets:all(),
	case lists:member(?MODULE,Tables) of
		false -> ets:new(?MODULE, [set,named_table,public]);
		true -> ok
	end,
	connect_proxy("192.168.9.179",2323),
	%~ connect_server("192.168.0.225","root","siteview800903",22,ssh).
	connect_server("192.168.9.88","root","huanghao",22,ssh).
	
test2() ->	
	Tables = ets:all(),
	case lists:member(?MODULE,Tables) of
		false -> ets:new(?MODULE, [set,named_table,public]);
		true -> ok
	end,
	connect_proxy("192.168.9.88",2323),
	connect_server("192.168.9.88","root","huanghao",22,ssh).

test3() ->	
	Tables = ets:all(),
	case lists:member(?MODULE,Tables) of
		false -> ets:new(?MODULE, [set,named_table,public]);
		true -> ok
	end,
	connect_proxy("192.168.9.179",2323),
	connect_server("192.168.0.118","root","siteview123",23,telnet).
	
connect_proxy(Host,Port) ->
	case gen_tcp:connect(Host,Port,[binary,{packet,0}]) of
		{ok,Socket} ->
			io:format("Proxy server ~p:~p conncet success~n",[Host,Port]),
			ets:insert(?MODULE,[{socket,Socket}]);
		{error,_} ->
			io:format("Proxy server ~p:~p conncet error~n",[Host,Port])
	end.
	
connect_server(Host,User,Password,Port,Type) ->
	case ets:lookup(?MODULE,socket) of
		[{socket,Socket}] ->
			LoginCom = lists:flatten(["login,",Host,",",User,",",Password,",",integer_to_list(Port),",",atom_to_list(Type)]),
			gen_tcp:send(Socket,list_to_binary(LoginCom)),
			%~ gen_tcp:send(Socket,{login,Host,User,Password,Port,Type}),
			case loop() of
				ok -> 
					Info = #info{socket = Socket,host = Host},
					ets:insert(?MODULE,{info,Info}),
					io:format("Connect to server[~p:~p] success~n",[Host,Port]);
				_ -> io:format("Connect to server[~p:~p] error~n",[Host,Port])
			end;
		_ ->
			io:format("Socket connect error~n")
	end.
	
send_command(Command) ->
	case ets:lookup(?MODULE,info) of
		[{info,Info}] ->
			FormatedCom = lists:flatten(["command,",Info#info.host,",",Command]),
			gen_tcp:send(Info#info.socket,list_to_binary(FormatedCom)),
			loop();
		_ ->
			io:format("Remot Server connect error~n")
	end.
	
loop() ->
	receive
		{tcp,_Socket,Bin} ->
			Receive = binary_to_term(Bin),
			Receive
	end.
	

	