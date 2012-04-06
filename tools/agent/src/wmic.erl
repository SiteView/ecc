-module(wmic).

-behaviour(gen_server).

-export([start/0, wmic/4]).

%% Internal exports, call-back functions.
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2]).

-define(DRIVER, wmic_drv).

	
	
receive_port_data(Port) ->
    receive
        {Port, {data, Data}} ->  Data
	after 60000 ->  {error,timeout}
    end.

%%===============================================


start() ->
 start_link().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {_, Path} = file:get_cwd(),  
    case erl_ddll:load_driver(Path++"/priv/lib", ?DRIVER) of
	ok -> ok;
	{error, already_loaded} -> ok;
	{error,ErrorDesc} -> io:format("===>~p~n", [erl_ddll:format_error(ErrorDesc)]),exit({error, could_not_load_driver});
	_ ->  exit({error, could_not_load_driver})
    end,   
    {ok, []}.


%%% --------------------------------------------------------
%%% The call-back functions.
%%% --------------------------------------------------------



handle_call(_, _, State) -> {noreply, State}.

handle_cast(stop, State) ->  {stop, normal, State};
handle_cast(_, State) -> {noreply, State}.

handle_info({'EXIT', Port, Reason}, Port) ->	
    {stop, {port_died, Reason}, Port};
handle_info({'EXIT', _Pid, _Reason}, Port) ->	
    {noreply, Port};
handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _) ->    ok.
    

wmic(Server,UserName,PassWord,Psql) ->
	Port = open_port({spawn, ?DRIVER}, [binary]),
	Bin = term_to_binary({0,Server,UserName,PassWord,Psql}),
	port_command(Port,Bin),
	Result = receive_port_data(Port),
	case Result of
	    {error,timeout} -> {error,timeout};
	    _ ->	    
		port_close(Port),	
		binary_to_term(Result)
	end.	
	
	
	