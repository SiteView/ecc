-module(st_test).



-compile(export_all).

start() -> test().  
stop() -> ok.
test()->

    {ok,Pid} = st_telnet:connect("192.168.1.20",23,"hxibswh","hxibs_kjb"),
    io:format("Pid:~p~n~n~n",[Pid]),
     io:format("telentResult:~p~n~n~n",[st_telnet:cmd(Pid,"/bin/ps -e -o vsz=MEMSIZE -o args=COMMAND")]),
     
      io:format("telentResult:~p~n~n~n",[st_telnet:cmd(Pid,"export COLUMNS=200; setenv COLUMNS 200; /usr/bin/ps -e -o \"%a\"")]),
     
     %~ Result = ssh_command:exec("192.168.0.118", 22, "root","", "/usr/bin/top n 1 d 2 b -p 1"),
     %~ io:format("   sshResult:~p~n~n~n",[Result]),
    ok.

	