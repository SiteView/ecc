%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.0, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.erlang.org/license/EPL1_0.txt

%%%-------------------------------------------------------------------
%%% File	: monitor_server.erl
%%% Author	: Erik Reitsma <elnerei@tina29.etm.ericsson.se>
%%% Description : SOAP monitor server
%%%
%%% Created : 23 Dec 2003 by Erik Reitsma <elnerei@tina29.etm.ericsson.se>
%%%-------------------------------------------------------------------
-module(monitor_server).
-vsn("0.1").

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-define(DEFAULT_PORT,8999).

-export([start_link/0,start/0,start_listener/1]).

-export([add_socket/1,remove_socket/1,send_data/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {sockets=[]}).

-define(SERVER,?MODULE).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: start/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
	gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% Server functions
%%====================================================================
add_socket(Socket) ->
	gen_server:call(?SERVER,{add_socket,Socket}).

remove_socket(Socket) ->
	gen_server:call(?SERVER,{remove_socket,Socket}).

send_data(Data) ->
	gen_server:cast(?SERVER,{send_data,Data}).

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State} 		 |
%%			{ok, State, Timeout} |
%%			ignore				 |
%%			{stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
	Port =
	%case config:get_value(monitor_server) of
	%	 {ok,P} ->
	%	 P;
	%	 _ ->
		?DEFAULT_PORT
	%end
	,
	tcp_server:start_raw_server(
	  Port,
	  fun(S) ->
		  monitor_server:start_listener(S)
	  end,
	  100,
	  0),
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}		   |
%%			{reply, Reply, State, Timeout} |
%%			{noreply, State}			   |
%%			{noreply, State, Timeout}	   |
%%			{stop, Reason, Reply, State}   | (terminate/2 is called)
%%			{stop, Reason, State}			 (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({add_socket,Socket}, _From, State) ->
	{reply,ok,State#state{sockets=[Socket|State#state.sockets]}};
handle_call({remove_socket,Socket}, _From, State) ->
	{reply,ok,State#state{sockets=lists:delete(Socket,State#state.sockets)}};
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}		  |
%%			{noreply, State, Timeout} |
%%			{stop, Reason, State}			 (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({send_data,Data}, State) ->
	SendFun =
	fun(S) ->
		case catch gen_tcp:send(S,Data) of
			{'EXIT',_Reason} -> false;
			{error,_Reason} -> false;
			ok -> true
		end
	end,
	NewSockets =
	lists:filter(SendFun,State#state.sockets),
	{noreply,State#state{sockets=NewSockets}};
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}		  |
%%			{noreply, State, Timeout} |
%%			{stop, Reason, State}			 (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
start_listener(Socket) ->
	add_socket(Socket),
	socket_loop(Socket).

socket_loop(Socket) ->
	receive
	{tcp_closed,Socket} ->
		remove_socket(Socket);
	{tcp_error,Socket} ->
		remove_socket(Socket);
	{tcp,Socket,_Data} ->
		%% ignore incoming data
		socket_loop(Socket)
	end.
