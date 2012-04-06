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


%% ------------------------------------------------------mm--------------
%% External exports
-export([start/0, connect/5, send/2, close/1]).

%% gen_fsm callbacks
-export([init/1, starting/3, reading_banner/2, guess_prompt/2,
         ready/3, waiting/2,
         handle_event/3, 
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {host,connection, prompt, channel, replyto, received, command,eof}).

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
    {ok, starting, #state{received=[], command=[],host=[],eof=ok}}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% -------------------------------------------------------------------- 
reading_banner(timeout, StateData) ->
    send_cr(StateData),
    
    %%io:format("______~p_________~n", [{reading_banner,StateData}]),
    {next_state, guess_prompt, StateData, ?PROMPT_TIMEOUT}.

guess_prompt(timeout, StateData) ->
    %%io:format("______~p_________~n", [{guess_prompt,StateData#state.prompt}]),
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
    %%io:format("connect: ~p~n", [{Host,Port,User, Password}]),
    case ssh:connect(Host,Port, [{silently_accept_hosts, true}, 
                                 {user_interaction, false}, 
                                 {user_dir, "."}, 
                                 {user, User}, 
                                 {password, Password},
                                 {connect_timeout, ?CONNECT_TIMEOUT}]) of
		{ok, Connection} ->
		         %%io:format("Command: ~p~n", [Connection]),
    		        {ok, Channel} = ssh_connection:session_channel(Connection, ?DEFAULT_WINDOW_SIZE,?DEFAULT_PACKET_SIZE,?SSH_TIMEOUT),
			success = ssh_connection:open_pty(Connection, Channel, "dumb", 800, 96, [], ?SSH_TIMEOUT),
    		        ok = ssh_connection:shell(Connection, Channel),    		        
			{next_state, reading_banner, StateData#state{host=Host,connection=Connection, channel=Channel, replyto=From}};
		{error, Reason} ->
			%%io:format("Reason: ~p~n", [Reason]),
			{reply, {error, Reason},starting, StateData#state{host=Host}}
		
     end.
    
    
ready({send, Command}, From, StateData) ->
    Cmd = cmd(Command),
   %% io:format("Command: ~p~n", [{Cmd}]),
    send_cmd(StateData, Cmd),
    put(status,starting),
    {next_state, waiting, StateData#state{replyto=From, received=[],command=Cmd,eof=[]}}.
    
   
    
cmd(Cmd) ->  lists:flatten([string:strip(Cmd,right,$;), ";echo SiteviewSshEnd\r\n"]).

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
    %%io:format("starting info: ~p~n", [_Info]),
    {next_state, starting, StateData};

handle_info(Info, reading_banner, StateData) ->
    {ssh_cm, _ConnectionRef, {data, _ChannelId, _Type, _Data}} = Info,
    %%io:format("banner: ~p ~n", [Info]),
    {next_state, reading_banner, StateData, ?BANNER_TIMEOUT};
  
handle_info(Info, guess_prompt, StateData) ->
    {ssh_cm, _ConnectionRef, {data, _ChannelId, _Type, _Data}} = Info,
    %%io:format("guess prompt: ~p~n", [Info]),
    {next_state, guess_prompt, StateData#state{prompt=ok}, ?PROMPT_TIMEOUT};

handle_info(Info, ready, StateData) ->
    {ssh_cm, _ConnectionRef, {data, _ChannelId, _Type, _Data}} = Info,
    %%io:format("ready: ~p~n", [Info]),
    case StateData#state.eof of
          ok ->  {next_state, ready, StateData#state{received=[], command=[]}};
          _ ->  {next_state, guess_prompt, StateData#state{prompt=ok}, ?COMMAND_TIMEOUT}
    end;
    

         

handle_info(Info, waiting, StateData) ->   
    {ssh_cm, _ConnectionRef, {data, _ChannelId, _Type, Data}} = Info, 
    NewReceived = StateData#state.received ++ binary_to_list(Data),
   %% io:format("data: ~p~n", [{NewReceived,StateData#state.command,lists:suffix(StateData#state.command,NewReceived),lists:suffix("SiteviewSshEnd\r\n",NewReceived)}]),
    %%io:format("data: ~p~n", [{NewReceived,Data,StateData#state.command,lists:prefix(StateData#state.command,NewReceived)}]),
    sleep(25),
    case lists:prefix(StateData#state.command,NewReceived) of
          true ->
	      Result = string:substr(NewReceived, length(StateData#state.command)+1),
	      %%io:format("data: ~p~n", [Result]),
	      case string:str(Result,"SiteviewSshEnd\r\n") of
	            0 -> {next_state, waiting, StateData#state{received=NewReceived}, ?COMMAND_TIMEOUT};
		    Pos -> 
		      SshResult = string:substr(Result, 1,Pos-1),
		     %% io:format("data: ~p~n", [{SshResult,StateData#state.command}]),
		      gen_fsm:reply(StateData#state.replyto, {ok,SshResult}),
		      put(status,stoped),		      
		      {next_state, ready, StateData#state{received=[], command=[],eof=ok}}	
	      end;	            
	  _ ->
	   %%io:format("data: ~p~n", [NewReceived]),
	   {next_state, waiting, StateData#state{received=NewReceived}, ?COMMAND_TIMEOUT}
    end;
    
    
    
    %~ case {lists:suffix(StateData#state.command,NewReceived),lists:suffix("SiteviewSshEnd\r\n",NewReceived)} of
          %~ {false,true} ->
	      %~ Result = string:substr(StateData#state.received, length( StateData#state.command)+1),
	      %~ %io:format("data: ~p~n", [{StateData#state.received,StateData#state.command}]),
	      %~ gen_fsm:reply(StateData#state.replyto, {ok,Result}),	   
	      %~ {next_state, ready, StateData#state{received=[], command=[],eof=ok}};	      
	  %~ _ ->
	   %~ {next_state, waiting, StateData#state{received=NewReceived}, ?COMMAND_TIMEOUT}
    %~ end;
handle_info(_Info, StateName, StateData) ->
    %%io:format("unknown info: ~p~n", [Info]),
    {next_state, StateName, StateData}.
    
    
sleep(Time)->
    receive 
		after Time ->
            ok
    end.    

hash(Data) ->
    <<I:160/integer>> = crypto:sha(Data),
    string:to_lower(lists:flatten(io_lib:fwrite("~31..0s", [erlang:integer_to_list(I, 36)]))).
    
%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(_Reason, _StateName, StateData)  when StateData#state.host =:= []  ->      
    ok;
terminate(_Reason, _StateName, StateData) ->
    Hashkey = list_to_atom(hash(StateData#state.host)),
    %%io:format("terminate: ~p~n", [{Hashkey,StateData#state.host}]),
    ets:delete(ssh_pool, Hashkey),
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
