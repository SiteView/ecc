-module(test).
-compile(export_all).

start() -> test().  
stop() -> ok.
test()->
    regex:start(),
    
    io:format("~p~n",[{regex:regex_str("^sshd[\\d+]: Accepted \\S+ for (\\S+) from (\\S+) port ","sshd[21405]: Accepted password for root from 192.1.1.1 port 6023"),"^sshd[\\d+]: Accepted \\S+ for (\\S+) from (\\S+) port ","sshd[21405]: Accepted password for root from 192.1.1.1 port 6023"}]),
    
    io:format("~p~n",[{regex:prematch("^sshd[\\d+]: Accepted \\S+ for (\\S+) from (\\S+) port ","sshd[21405]: Accepted password for root from 192.1.1.1 port 6023"),"^sshd[\\d+]: Accepted \\S+ for (\\S+) from (\\S+) port ","sshd[21405]: Accepted password for root from 192.1.1.1 port 6023"}]),
    
    io:format("~p~n",[{regex:regex("\\w+\\s+\\w+\\d+\\s\\$","a aa11  "),"\\w+\\s+\\w+\\d+\\s\\$","a aa11  "}]),   

    
	io:format("~p~n",[{regex:match("^abc ","abc   "),"^abc ","abc   "}]),
    io:format("~p~n",[{regex:match("^abc ","11111111abc   "),"^abc ","11111111abc   "}]).

	