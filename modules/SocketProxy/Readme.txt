SSH调度
	{ok,Pid} = mm_ssh:start()
	mm_ssh:connect(Pid,Host,Port,User,Password)
	mm_ssh:send(Pid,Command)
	
Telnet调度
	{ok,Pid} = telnet_client:start()
	telnet_client:connect(Pid,Host,Port,User,Password)
	telnet_client:cmd(Pid,Command)
	
Proxy端
	proxy:start()监听一个指定的端口信息
	{ok,Socket} = gen_tcp:accept(ListenSocket)接收端口请求
	loop(Socket)分析请求的数据并返回相应的数据