
-module(telnet_client).

-behaviour(gen_fsm).

-define(TELNET_PORT, 23).
-define(OPEN_TIMEOUT,10000).
-define(IDLE_TIMEOUT,10000).

%% telnet control characters
-define(SE,	240).
-define(NOP,	241).
-define(DM,	242).
-define(BRK,	243).
-define(IP,	244).
-define(AO,	245).
-define(AYT,	246).
-define(EC,	247).
-define(EL,	248).
-define(GA,	249).
-define(SB,	250).
-define(WILL,	251).
-define(WONT,	252).
-define(DO,	253).
-define(DONT,	254).
-define(IAC,	255).

%% telnet options
-define(BINARY,            0).
-define(ECHO,	           1).
-define(SUPPRESS_GO_AHEAD, 3).
-define(TERMINAL_TYPE,     24).  
-define(WINDOW_SIZE,       31).

-define(TELNET_TIMEOUT, 20000).
-define(TIMEOUT,10000).

-define(PRX,"login: |Password: |\\\$|# |>|> |$|$ ").
-define(ErrorPRX,"Login incorrect|login incorrect|You entered an invalid login name or password").
-compile(export_all). 

-export([start/0, connect/5, cmd/2, close/1]).

%% gen_fsm callbacks
-export([init/1, 
         handle_event/3, 
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(telnetstate, {keepalive,replyto,socket,host,port,user, password,received,command}).


start() -> gen_fsm:start(?MODULE, [], []).

connect(FsmRef, Host, Port,User, Password) ->
	try gen_fsm:sync_send_event(FsmRef, {connect, Host, Port,User, Password}, ?TELNET_TIMEOUT) of
	     Result -> Result
	catch  error:_Error -> {error,timeout};
		_:_ -> {error,timeout}
	end.

cmd(FsmRef, Command) ->
        try gen_fsm:sync_send_event(FsmRef, {send, Command}, ?TELNET_TIMEOUT) of
	     Result -> Result
	catch  error:_Error -> {error,timeout};
		_:_ -> {error,timeout}
	end.
	 
	
close(FsmRef) ->
	gen_fsm:send_all_state_event(FsmRef, close).



init([]) ->       
    {ok, starting, #telnetstate{received=[], host=[]}}.



handle_event(close, _StateName, StateData)  ->  {stop, normal, StateData};
    
handle_event(_Event, StateName, StateData) -> {next_state, StateName, StateData}.


handle_sync_event(_Event, _From, StateName, StateData) -> {reply, ok, StateName, StateData}.

handle_info(Info, login, StateData) ->  
    {tcp, _ConnectionRef, Data} = Info,
    dbg("Data: ~p~n", [Data]),
    Msg = check_msg(StateData,Data,[]),    
    dbg("Msg: ~p~n", [Msg]),
    NewData = StateData#telnetstate.received ++ Msg,
    case check_prx(NewData) of
          true -> 
	      send(StateData,StateData#telnetstate.user++"\n"),
	      {next_state, password, StateData#telnetstate{received=[]}};
	  _ -> 	    
	    {next_state, login, StateData#telnetstate{received=NewData}}
    end;
    
handle_info(Info, password, StateData) ->  
    {tcp, _ConnectionRef, Data} = Info, 
    dbg("passwordData: ~p~n", [Data]),
    Msg = check_msg(StateData,Data,[]),
    dbg("passwordMsg: ~p~n", [Msg]),
    NewData = StateData#telnetstate.received ++ Msg,
    case check_prx(NewData) of
          true ->
	     send(StateData,StateData#telnetstate.password++"\n"), 
   	     {next_state, readying,StateData#telnetstate{received=[]}};
	  _ -> 	     
	     {next_state, password, StateData#telnetstate{received=NewData}}
    end;  
    
handle_info({tcp, _ConnectionRef, Data}, readying, StateData) -> 
    NewData = StateData#telnetstate.received ++ Data,   
    dbg("readying: ~p~n", [Data]),   
    case check_error_prx(NewData) of    
           true -> 
	        gen_fsm:reply(StateData#telnetstate.replyto, {error, "Error password!"}),
	        {stop, normal, StateData};
           _ ->
	    case check_prx(NewData) of
		  true ->
		     dbg("readying: ~p~n", [Data]),
		     gen_fsm:reply(StateData#telnetstate.replyto, {ok,connected}),
		     Ref = gen_fsm:start_timer(5000, "KeepAlive"), 
		     {next_state, readying,StateData#telnetstate{received=[],keepalive=Ref}};
		  _ -> 	     
		     {next_state, readying, StateData#telnetstate{received=NewData}}
	    end
    end;
    
  
  
handle_info( {tcp, _ConnectionRef, Data}, cmding, StateData) ->  
    NewData = StateData#telnetstate.received ++ Data,  
    dbg("cmding: ~p~n", [NewData]),   
    case checkeof(StateData,NewData) of
          {ok,CmdData} ->
	     dbg("cmding: ~p~n", [CmdData]),
	     gen_fsm:reply(StateData#telnetstate.replyto, {ok,lists:flatten(list_to_string(CmdData,"\r\n"))}),
	     Ref = gen_fsm:start_timer(5000, "KeepAlive"), 
	     put(status,stoped),
   	     {next_state, readying,StateData#telnetstate{received=[],keepalive=Ref}};
	  _ -> 	     
	     {next_state, cmding, StateData#telnetstate{received=NewData}}
    end;
      
handle_info(_Info, StateName, StateData) ->
    dbg("unknown info: ~p~n", [{_Info, StateName, StateData}]),
    {next_state, StateName, StateData}.
    
terminate(_Reason, _StateName, StateData) when StateData#telnetstate.host =/= [] ->
      dbg("terminate: ~p~n", [StateData#telnetstate.host]),
      gen_tcp:close(StateData#telnetstate.socket),
      ok;
terminate(_Reason, _StateName, _StateData) -> ok.


code_change(_OldVsn, StateName, StateData, _Extra) ->  {ok, StateName, StateData}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_to_string([H], Space,Acc) when is_binary(H) -> list_to_string([], Space,[binary_to_list(H)|Acc]);
list_to_string([H|R], Space,Acc) when is_binary(H) ->  list_to_string(R, Space,[Space,binary_to_list(H)|Acc]);
list_to_string([H], Space,Acc) when is_list(H)  -> list_to_string([], Space,[H|Acc]);
list_to_string([H|R], Space,Acc) when is_list(H) ->  list_to_string(R, Space,[Space,H|Acc]);
list_to_string([H], Space,Acc) when is_atom(H) -> list_to_string([], Space,[atom_to_list(H)|Acc]);
list_to_string([H|R], Space,Acc) when is_atom(H) ->  list_to_string(R, Space,[Space,atom_to_list(H)|Acc]);
list_to_string([H], Space,Acc) -> list_to_string([], Space,[H|Acc]);
list_to_string([H|R], Space,Acc) ->  list_to_string(R, Space,[Space,H|Acc]);
list_to_string([], _,Acc) -> lists:reverse(Acc).
list_to_string(List,Space) ->
  lists:flatten(list_to_string(List,Space,[])).

checkeof(StateData,Data) ->
   DataList = string:tokens(Data,"\r\n"), 
   checkeof(StateData,DataList,[]).
   
checkeof(_,[L|_],Acc) when L =:= "Siteview_Ready" ->  {ok, lists:reverse(Acc)};
checkeof(StateData,[L|R],Acc) when L =:= StateData#telnetstate.command ->  checkeof(StateData,R,Acc);
checkeof(StateData,[L|R],Acc) ->
     case check_prx(L) of
          true ->  {ok, lists:reverse(Acc)};
	  _ -> checkeof(StateData,R,[L|Acc])
     end;
    
checkeof(_,[],Acc) -> lists:reverse(Acc). 

starting({connect, Host,Port,User, Password}, From, StateData) ->
    dbg("connect: ~p~n", [{Host,Port,User, Password}]),
    case gen_tcp:connect(Host, Port, [list,{packet,0}], ?OPEN_TIMEOUT) of
	{ok,Sock} ->
	    dbg("Connected to: ~p (port: ~w)\n", [Host,Port]),
	    NewState = StateData#telnetstate{replyto=From,socket=Sock,host=Host,port=Port,user=User,password=Password},
	    send(NewState, [?IAC,?DO,?SUPPRESS_GO_AHEAD]),
	    {next_state, login, NewState};
        ErrorReason ->
	    {reply, {error, ErrorReason},starting, StateData}
    end.
starting(timeout, StateData) ->
    dbg("Connected \n", []),
    {stop, timeout, StateData}.


cmdstrip([13|Rest], Acc) ->  cmdstrip(Rest, Acc);
cmdstrip([10|Rest], Acc) ->  cmdstrip(Rest, Acc);
cmdstrip([C|Rest], Acc) ->  cmdstrip(Rest, [C|Acc]);
cmdstrip([], Acc) ->    lists:reverse(Acc).

readying({send, Command}, From, StateData) ->
    gen_fsm:cancel_timer(StateData#telnetstate.keepalive),
    put(status,starting),
    %~ dbg("Command: ~p~n", [Command]),
    NewCommand = Command++";echo Siteview_Ready\n",
    %~ send(StateData,"\n"),  
    send(StateData,NewCommand),  
    %~ dbg("Command: ~p~n", [cmdstrip(NewCommand,[])]),
    {next_state, cmding, StateData#telnetstate{replyto=From, received=[],command=cmdstrip(NewCommand,[])}}.
   
readying({timeout,_,"KeepAlive"}, StateData) ->  
    %~ dbg("KeepAlive: ~p~n", [KeepAlive]),  
    send(StateData,[?IAC,?NOP]),
    Ref = gen_fsm:start_timer(5000, "KeepAlive"),   
    {next_state, readying, StateData#telnetstate{keepalive=Ref}}.
    
 
    
cmding({timeout,_,"KeepAlive"}, StateData) ->  
    %~ dbg("KeepAlive: ~p~n", [KeepAlive]),  
    send(StateData,[?IAC,?NOP]),
    Ref = gen_fsm:start_timer(5000, "KeepAlive"),   
    {next_state, cmding, StateData#telnetstate{keepalive=Ref}}.
    
check_prx(Msg) ->
   PrxList = string:tokens(?PRX,"|"),  
   Fun = fun(X) ->  string:str(Msg,X) > 0 end,
   lists:any(Fun,PrxList).
   
check_error_prx(Msg) ->
   PrxList = string:tokens(?ErrorPRX,"|"),  
   Fun = fun(X) ->  string:str(Msg,X) > 0 end,
   lists:any(Fun,PrxList).   

check_msg(StateData, [?IAC,?IAC | T], Acc) ->
    check_msg(StateData, T,[?IAC|Acc]);



check_msg(StateData, [?IAC | Cs], Acc) ->
    case get_cmd(Cs) of
	{Cmd,Cs1} ->
	    dbg("Got ~n", []), 
	    cmd_dbg("recv:",Cmd),	   
	    respond_cmd(Cmd,Cs1, StateData),
	    check_msg(StateData, Cs1, Acc); 
	error ->  lists:reverse(Acc)
    end;


check_msg(Sock, [H|T], Acc) ->
    check_msg(Sock, T, [H|Acc]);
    

check_msg(_Sock, [], Acc) -> lists:reverse(Acc). 

strip([255|_], Acc) ->  lists:reverse(Acc);
strip([0|_], Acc) ->  lists:reverse(Acc);
strip([C|Rest], Acc) ->  strip(Rest, [C|Acc]);
strip([], Acc) ->    lists:reverse(Acc).

%~ %% Positive responses (WILL and DO).
respond_cmd([?DO,?WINDOW_SIZE],_Rest,StateData) -> 
   R = [?IAC,?WILL,?WINDOW_SIZE],
   cmd_dbg("send:",R),
   gen_tcp:send(StateData#telnetstate.socket, R),   
   send(StateData,[?IAC,?SB,?WINDOW_SIZE,0,200,0,200,?IAC,?SE]);

respond_cmd([?WILL,?ECHO],_Rest,StateData) ->   
    R = [?IAC,?DO,?ECHO],
    cmd_dbg("send:",R),
    gen_tcp:send(StateData#telnetstate.socket, R);

respond_cmd([?DO,?ECHO],_Rest,StateData) ->
    %~ io:format("~s",[strip(_Rest,[])]),
    R = [?IAC,?WILL,?ECHO],
    cmd_dbg("send:",R),
    gen_tcp:send(StateData#telnetstate.socket, R);

%% Answers from server

respond_cmd([?WILL,?SUPPRESS_GO_AHEAD],_Rest,_StateData) ->
    dbg("Server will suppress-go-ahead\n", []);

respond_cmd([?WONT,?SUPPRESS_GO_AHEAD],_Rest,_StateData) ->
    dbg("Warning! Server won't suppress-go-ahead\n", []);

respond_cmd([?DONT | _Opt],_Rest,_StateData) ->		% server ack?
    ok;						
respond_cmd([?WONT | _Opt],_Rest, _StateData) ->		% server ack?
    ok;						

%% Negative responses (WON'T and DON'T). These are default!


respond_cmd([?WILL,Opt],_Rest, StateData) ->
    R = [?IAC,?DONT,Opt],    
    cmd_dbg("send:",R),
    gen_tcp:send(StateData#telnetstate.socket, R);

respond_cmd([?DO | Opt],_Rest, StateData) ->
    R = [?IAC,?WONT | Opt],
    cmd_dbg("send:",R),
    gen_tcp:send(StateData#telnetstate.socket, R);

%% Commands without options (which we ignore)

respond_cmd(?NOP,_Rest, _StateData) ->
    ok;

%% Unexpected messages.

respond_cmd([Cmd | Opt],_Rest, _StateData) when Cmd >= 240, Cmd =< 255 ->
    dbg("Received cmd: ~w. Ignored!\n", [[Cmd | Opt]]);

respond_cmd([Cmd | Opt],_Rest, _StateData)  ->
    dbg("WARNING: Received unknown cmd: ~w. Ignored!\n", [[Cmd | Opt]]).


get_cmd([Cmd | Rest]) when Cmd == ?SB -> get_subcmd(Rest, []);

get_cmd([Cmd | Rest]) when Cmd >= 240, Cmd =< 249 ->  {?NOP, Rest};

get_cmd([Cmd,Opt | Rest]) when Cmd >= 251, Cmd =< 254 ->  {[Cmd,Opt], Rest};

get_cmd(_Other) ->  error.

get_subcmd([?SE | Rest], Acc) -> {[?SE | lists:reverse(Acc)], Rest};

get_subcmd([Opt | Rest], Acc) -> get_subcmd(Rest, [Opt | Acc]).

   
send(StateData, Data) ->  
    case Data of 
	[?IAC|_] = Cmd ->
	    cmd_dbg("send:",Cmd);
	_ ->
	    dbg("Sending: ~p\n", [Data])
    end,   
    gen_tcp:send(StateData#telnetstate.socket,Data).


    
    
    
    
    
    
    
    
    
-ifdef(debug).

dbg(_Str,_Args) ->
    io:format(_Str,_Args).

cmd_dbg(Tip,_Cmd) ->
    case _Cmd of
	[?IAC|Cmd1] ->
	    cmd_dbg(Tip,Cmd1);
	[Ctrl|Opts] ->
	    CtrlStr =
		case Ctrl of
		    ?DO ->   "DO";
		    ?DONT -> "DONT";
		    ?WILL -> "WILL";
		    ?WONT -> "WONT";
		    ?NOP ->  "NOP";
		    _ ->     "CMD"
		end,
	    Opts1 =
		case Opts of
		    [Opt] -> Opt;
		    _ -> Opts
		end,
	    io:format("~p~s(~w): ~w\n", [Tip,CtrlStr,Ctrl,Opts1]);
	Any  ->
	    io:format("Unexpected in cmd_dbg:~n~w~n",[Any])
    end.
    
    
-else.
dbg(_Str,_Args) ->
    ok.

cmd_dbg(_Tip,_Cmd) ->
    ok.
-endif.    