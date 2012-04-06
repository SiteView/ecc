-module(erlnode).
-author('author <author@example.com>').
-export([start/0, stop/0,stop/1]).





start() -> ok.  
stop() -> ok.
    
    
stop([Node]) ->
    io:format("Stop:~p~n",[Node]),
    
    case net_adm:ping(Node) of
	pong -> ok;
	pang ->
	    io:format("There is no node with this name~n")
    end,
    rpc:cast(Node, init, stop, []),
    init:stop();

stop(_) ->
    io:format("no nodename ~n").