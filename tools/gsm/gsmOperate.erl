-module(gsmOperate).
-compile(export_all).

start() ->
    start("gsmoperate").

start(SharedLib) ->
    case erl_ddll:load_driver(".", SharedLib) of
	ok -> ok;
	{error, already_loaded} -> ok;
	Other -> 
    io:format("Other:~p~n",[Other]),
    exit({error, could_not_load_driver})
    end,
    spawn(fun() -> init(SharedLib) end).

init(SharedLib) ->
    register(gsm, self()),
    Port = open_port({spawn, SharedLib}, []),
    loop(Port).

stop() ->
    gsm ! stop.

twice(X) -> call_port({twice, X}).
sum(X,Y) -> call_port({sum, X, Y}).

call_port(Msg) ->
    gsm ! {call, self(), Msg},
    receive
	{gsm, Result} ->
	    Result
    end.

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, encode(Msg)}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {gsm, decode(Data)}
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    io:format("~p ~n", [Reason]),
	    exit(port_terminated)
    end.

encode({twice, X})  -> [1, X];
encode({sum, X, Y}) -> [2, X, Y].

decode([Int]) -> Int.
