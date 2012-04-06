%%% -------------------------------------------------------------------
%%% -------------------------------------------------------------------
-module(mm_ssh).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(BANNER_TIMEOUT, 2000).
-define(PROMPT_TIMEOUT, 1000).
-define(COMMAND_TIMEOUT, 5000).
-define(SSH_TIMEOUT, 60000).
-define(CONNECT_TIMEOUT, 15000).
-define(DEFAULT_PACKET_SIZE, 32768).
-define(DEFAULT_WINDOW_SIZE, 8*?DEFAULT_PACKET_SIZE).

-define(PRX,"login: |Password: |\\\$|# |> ").

%% ------------------------------------------------------mm--------------
%% External exports
-export([start/0, connect/5, send/2, close/1]).

%% gen_fsm callbacks
-export([init/1, starting/3, reading_banner/2, guess_prompt/2,
         ready/3, waiting/2,
         handle_event/3, 
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {host,connection, prompt, channel, replyto, received, command}).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_fsm:start(?MODULE, [], []).

connect(FsmRef, Host, Port,User, Password) ->
	gen_fsm:sync_send_event(FsmRef, {connect, Host, Port,User, Password}, ?SSH_TIMEOUT). 

send(FsmRef, Command) ->
         gen_fsm:sync_send_event(FsmRef, {send, Command}, ?SSH_TIMEOUT).
	 
	
close(FsmRef) ->
	gen_fsm:send_all_state_event(FsmRef, close).


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, starting, #state{received=[], command=[],host=[]}}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% -------------------------------------------------------------------- 
reading_banner(timeout, StateData) ->
    send_cr(StateData),    
    {next_state, guess_prompt, StateData, ?PROMPT_TIMEOUT}.

guess_prompt(timeout, StateData) ->    
    case StateData#state.prompt of
        ok ->               
		gen_fsm:reply(StateData#state.replyto, {ok,connected}),
	        {next_state, ready, StateData};
	_ ->
	        gen_fsm:reply(StateData#state.replyto, {error, failed_to_guess_prompt}),
	        {stop, normal, StateData}
     end.		
	    

waiting(timeout, StateData) ->
    {stop, timeout, StateData}.
      

%% --------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
starting({connect, Host,Port,User, Password}, From, StateData) ->
    dbg("connect: ~p~n", [{Host,Port,User, Password}]),
    case ssh:connect(Host,Port, [{silently_accept_hosts, true}, 
                                 {user_interaction, false}, 
                                 {user_dir, "."}, 
                                 {user, User}, 
                                 {password, Password},
                                 {connect_timeout, ?CONNECT_TIMEOUT}]) of
		{ok, Connection} ->
		         dbg("Command: ~p~n", [Connection]),
    		        {ok, Channel} = ssh_connection:session_channel(Connection, ?DEFAULT_WINDOW_SIZE,?DEFAULT_PACKET_SIZE,?SSH_TIMEOUT),
			success = ssh_connection:open_pty(Connection, Channel, "dumb", 800, 96, [], ?SSH_TIMEOUT),
    		        ok = ssh_connection:shell(Connection, Channel),    		        
			{next_state, reading_banner, StateData#state{host=Host,connection=Connection, channel=Channel, replyto=From}};
		{error, Reason} ->
			dbg("Reason: ~p~n", [Reason]),
			{reply, {error, Reason},starting, StateData}
		
     end.
    
    
ready({send, Command}, From, StateData) ->
    put(status,starting),
    send_cmd(StateData, cmdcr(Command)),
    {next_state, waiting, StateData#state{replyto=From, received=[],command=cmd(Command)}}.
    
cmdcr(Cmd) ->  lists:flatten([string:strip(Cmd,right,$;), ";echo Siteview_Ready\n"]).
cmd(Cmd) ->  lists:flatten([string:strip(Cmd,right,$;), ";echo Siteview_Ready"]).
%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------

handle_event(close, _StateName, StateData) when StateData#state.host =:= []  ->   
    {stop, normal, StateData};

handle_event(close, _StateName, StateData)  ->  
    ssh:close(StateData#state.connection),
    {stop, normal, StateData};
    
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info(_Info, starting, StateData) ->
    dbg("starting info: ~p~n", [_Info]),
    {next_state, starting, StateData};

handle_info({ssh_cm, _ConnectionRef, {data, _ChannelId, _Type, _Data}} = Info, reading_banner, StateData) ->
    dbg("banner: ~p ~n", [Info]),
    {next_state, reading_banner, StateData, ?BANNER_TIMEOUT};
  
handle_info({ssh_cm, _ConnectionRef, {data, _ChannelId, _Type, _Data}} = Info, guess_prompt, StateData) ->
    dbg("guess prompt: ~p~n", [Info]),
    {next_state, guess_prompt, StateData#state{prompt=ok}, ?PROMPT_TIMEOUT};

handle_info({ssh_cm, _ConnectionRef, {data, _ChannelId, _Type, _Data}} = Info, ready, StateData) ->   
    dbg("ready: ~p~n", [Info]),
    {next_state, ready, StateData#state{received=[], command=[]}};

handle_info({ssh_cm, _ConnectionRef, {data, _ChannelId, _Type, Data}}, waiting, StateData) ->       
    NewReceived = StateData#state.received ++ binary_to_list(Data),
    %~ dbg("data: ~p~n", [{NewReceived,Data,StateData#state.command,lists:prefix(StateData#state.command,NewReceived)}]),
    case checkeof(StateData,NewReceived) of
          {ok,CmdData} ->
	     dbg("cmding: ~p~n", [CmdData]),
	     gen_fsm:reply(StateData#state.replyto, {ok,lists:flatten(list_to_string(CmdData,"\r\n"))}),
	     put(status,stoped),
   	     {next_state, ready,StateData#state{received=[], command=[]}};
	  _ -> 	     
	     {next_state, waiting, StateData#state{received=NewReceived}, ?COMMAND_TIMEOUT}
    end;
   
handle_info(_Info, StateName, StateData) ->
    dbg("unknown info: ~p~n", [_Info]),
    {next_state, StateName, StateData}.
    
    
  

  
%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(_Reason, _StateName, StateData)  when StateData#state.host =:= []  ->      
    ok;
terminate(_Reason, _StateName, StateData) ->   
    %%io:format("terminate: ~p~n", [{Hashkey,StateData#state.host}]),    
    ssh:close(StateData#state.connection),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

send_cmd(StateData, Cmd) ->   
	ssh_connection:send(StateData#state.connection,  StateData#state.channel, Cmd).

send_cr(StateData) ->
	ssh_connection:send(StateData#state.connection,  StateData#state.channel, "\n").
	
	
	
	
	
	
	
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



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
checkeof(StateData,[L|R],Acc) when L =:= StateData#state.command ->  checkeof(StateData,R,Acc);
checkeof(StateData,[L|R],Acc) ->
     %~ dbg("unknown info: ~p~n", [{L , StateData#state.command}]),
     case check_prx(L) of
          true ->  {ok, lists:reverse(Acc)};
	  _ -> checkeof(StateData,R,[L|Acc])
     end;
    
checkeof(_,[],Acc) -> lists:reverse(Acc).


check_prx(Msg) ->
   PrxList = string:tokens(?PRX,"|"),  
   Fun = fun(X) ->  string:str(Msg,X) > 0 end,
   lists:any(Fun,PrxList).
   
   
    
-ifdef(debug).

dbg(_Str,_Args) ->
    io:format(_Str,_Args).
-else.
dbg(_Str,_Args) ->
    ok.
-endif.    