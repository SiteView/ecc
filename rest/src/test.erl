%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(test).
-author('author <author@example.com>').
-export([start/0, stop/0,stop/1]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the test server.

start() ->    
    test_deps:ensure(),
	crypto:start(),
    ensure_started(crypto),
    application:start(test).
    

%% @spec stop() -> ok
%% @doc Stop the test server.
stop() ->
    Res = application:stop(test),
    application:stop(crypto),
    Res.
	
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
