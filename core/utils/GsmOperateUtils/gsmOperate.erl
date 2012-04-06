-module(gsmOperate).
-compile(export_all).

start() ->
    start("gsmoperate").

start(SharedLib) ->
    case os:type() of
    {_,nt} ->    
        case erl_ddll:load_driver(".", SharedLib) of
	    ok -> ok;
	        {error, already_loaded} -> ok;
	    _ -> 
           exit({error, could_not_load_driver})
        end;
    _ ->
        case erl_ddll:load_driver(".", SharedLib) of
	    ok -> ok;
	        {error, already_loaded} -> ok;
	    _ -> 
            exit({error, could_not_load_driver})
        end
    end,
    spawn(fun() -> init(SharedLib) end).

init(SharedLib) ->
    register(gsm, self()),
    Port = open_port({spawn, SharedLib}, []),
    loop(Port).

stop() ->
    gsm ! stop.
	
is_string(Str)->
	case is_list(Str) of
		true->
			[F|_]=Str,
			L = lists:last(Str),
			if
				is_number(F) andalso is_number(L) ->
					true;
				true->
					false
			end;
		_->
			false
	end.

sendMessage(Port, RecvPhone,Message)->
	case is_string(Port) andalso is_string(RecvPhone) andalso is_string(Message) of
		true->
			call_port({send,Port, RecvPhone,Message});
		_->
			2
	end.

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

encode({send, Port, RecvPhone,Message}) -> [ Port++"&"++RecvPhone++"&"++Message].

decode([RecStatu]) -> RecStatu.
