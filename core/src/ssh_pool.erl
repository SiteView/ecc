%%% -------------------------------------------------------------------
%%% Author  : oldhand
%%% Description :
%%%
%%% Created : 2010 6-28
%%% -------------------------------------------------------------------
-module(ssh_pool).

-behaviour(gen_server).

-export([start/0]).  
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,  
     terminate/2, code_change/3]).  
-compile(export_all).  
  
  
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).  
stop()  -> gen_server:call(?MODULE, stop).  
  
%~ exec(Host, Port,User, Password,Cmd)      -> gen_server:call(?MODULE, {exec, Host, Port,User, Password,Cmd},infinity).  
%~ reexec(Host, Port,User, Password,Cmd)      -> gen_server:call(?MODULE, {reexec, Host, Port,User, Password,Cmd},infinity).  
  
pool(Host) ->  gen_server:call(?MODULE, {pool, Host},infinity).  
  
init()      -> gen_server:call(?MODULE, init,infinity).    


sleep(Time)->
    receive 
		after Time ->
            ok
    end.
    
getstatus(Pid,Host,Count) ->
	ProcessInfo = process_info(Pid),
	case lists:keysearch(dictionary,1,ProcessInfo) of
		{value,{dictionary,Dictionary}} ->
			%%io:format("Result:~p~n",[{Host,Pid,lists:keysearch(status,1,Dictionary)}]),
			case  lists:keysearch(status,1,Dictionary) of
			     false -> ok;
			     {value,{status,stoped}} -> ok;
			     _ when Count > 120 -> error;
			     _ -> 
				  sleep(500),
				  getstatus(Pid,Host,Count+1)
			end;
		_ -> error
	end.
	

exec(Host, Port,User, Password,Cmd)  -> 
  case pool(Host) of
        {ok,EtsPid} -> 
		  case getstatus(EtsPid,Host,0) of
		        ok ->
			  try mm_ssh:send(EtsPid,Cmd) of
			       Result -> Result		       
			  catch  error:_Error ->  mm_ssh:close(EtsPid), {error,crash};
				_:_ ->  mm_ssh:close(EtsPid),{error,crash}
			  end;	
			_ -> mm_ssh:close(EtsPid),{error,crash}
		  end;		  	  
	  _ ->	  		   
		{ok,Pid}=mm_ssh:start(),
		try mm_ssh:connect(Pid,Host,Port,User,Password) of
		    {ok,connected} -> 	
			Hashkey = list_to_atom(hash(Host)),
			ets:insert(?MODULE, {Hashkey,{ok,Pid}}),
			Result = mm_ssh:send(Pid,Cmd),
			Result;
		    Reason ->
			mm_ssh:close(Pid), 
			Reason
		catch  error:_Error -> mm_ssh:close(Pid),{error,crash};
		        _:_ ->  mm_ssh:close(Pid),{error,crash}
		end
  end.
  
reexec(Host, Port,User, Password,Cmd)  ->          
	{ok,Pid}=mm_ssh:start(),
	try mm_ssh:connect(Pid,Host,Port,User,Password) of
	    {ok,connected} -> 	
		Hashkey = list_to_atom(hash(Host)),	    
		ets:insert(?MODULE, {Hashkey,{ok,Pid}}),
		Result = mm_ssh:send(Pid,Cmd),
		%io:format("Result:~p~n",[Result]),
		Result;
	    Reason ->
		mm_ssh:close(Pid), 
		Reason
	catch  error:_Error -> mm_ssh:close(Pid),{error,crash};
		_:_ ->  mm_ssh:close(Pid),{error,crash}
	end.

  
init([]) -> {ok, []}.  

getkey(Key) ->
    case ets:lookup(?MODULE,Key) of
	     [{_,Data}] -> Data;
	     _ -> []
	end.	
	
hash(Data) ->
    <<I:160/integer>> = crypto:sha(Data),
    string:to_lower(lists:flatten(io_lib:fwrite("~31..0s", [erlang:integer_to_list(I, 36)]))).
    
   
handle_call(init, _From, Tab) -> 
    ets:new(?MODULE, [set,named_table,public]),
    {reply, ok, Tab}; 

handle_call({pool, Host}, _From, Tab) -> 
    Hashkey = list_to_atom(hash(Host)),
    Reply = getkey(Hashkey),
    {reply, Reply, Tab}; 
    
    
%~ handle_call({exec, Host, Port,User, Password,Cmd}, _From, Tab) -> 
    %~ %%io:format("~p~n",[{Host, User, Password}]),
    %~ Hashkey = list_to_atom(hash(Host)),
    %~ Reply = case getkey(Hashkey) of
          %~ {ok,EtsPid} -> 
	          %~ io:format("Hashkey:~p~n",[{Host,Cmd}]),                	  
		  %~ %%Result = mm_ssh:send(EtsPid,Cmd),
		  %~ try mm_ssh:send(EtsPid,Cmd) of
		       %~ Result -> Result		       
		  %~ catch  error:Error ->  {error,crash};
		        %~ _:_ ->  {error,crash}
		  %~ end;		  
	  %~ _ ->	  		   
		%~ {ok,Pid}=mm_ssh:start(),
		%~ try mm_ssh:connect(Pid,Host,Port,User,Password) of
		    %~ {ok,connected} -> 			       
			%~ ets:insert(?MODULE, {Hashkey,{ok,Pid}}),
			%~ Result = mm_ssh:send(Pid,Cmd),
			%~ %io:format("Result:~p~n",[Result]),
			%~ Result;
		    %~ Reason ->
			%~ mm_ssh:close(Pid), 
			%~ Reason
		 %~ catch  error:Error -> {error,crash};
		        %~ _:_ ->  {error,crash}
		%~ end
		     
    %~ end,  
    %~ {reply, Reply, Tab}; 
%~ handle_call({reexec, Host, Port,User, Password,Cmd}, _From, Tab) -> 
    %~ %%io:format("~p~n",[{Host, User, Password}]),
    %~ Hashkey = list_to_atom(hash(Host)),
     		   
	%~ {ok,Pid}=mm_ssh:start(),
	%~ Reply = try mm_ssh:connect(Pid,Host,Port,User,Password) of
		    %~ {ok,connected} -> 			       
			%~ ets:insert(?MODULE, {Hashkey,{ok,Pid}}),
			%~ Result = mm_ssh:send(Pid,Cmd),
			%~ %io:format("Result:~p~n",[Result]),
			%~ Result;
		    %~ Reason ->
			%~ mm_ssh:close(Pid), 
			%~ Reason
		 %~ catch  error:Error ->  {error,crash};
		        %~ _:_ ->  {error,crash}
		%~ end, 
    %~ {reply, Reply, Tab};     
handle_call(stop, _From, Tab) ->  
    {stop, normal, stopped, Tab}.  
  
handle_cast(_Msg, State) -> {noreply, State}.  
handle_info(_Info, State) -> {noreply, State}.  
terminate(_Reason, _State) -> ok.  
code_change(_OldVsn, State, _Extra) -> {ok, State}.

