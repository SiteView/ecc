%%% -------------------------------------------------------------------
%%% Author  : oldhand
%%% Description :
%%%
%%% Created : 2010 6-28
%%% -------------------------------------------------------------------
-module(agent).


-include("config.hrl").

-behaviour(gen_server).

-export([start/0]).  
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,  
     terminate/2, code_change/3]).  
-compile(export_all).  
  
  
-record(agent, {pid,host,type,port,user, password,os,discovery}).  
  
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).  
stop()  -> gen_server:call(?MODULE, stop).  

pool(Host) ->  gen_server:call(?MODULE, {pool, Host},infinity).  
  

sleep(Time)->
    receive 
	after Time ->   ok
    end.
    

  
init([]) ->
      crypto:start(),
      ssh:start(),
      regex:start(),
      ets:new(?MODULE, [set,named_table,public]),
      case file:consult("agent.conf") of
		{ok,Data} ->
		   %%io:format("~p~n",[Data]),
		   F = fun({Key,Value}) ->
			    case Key of
				     system ->									  
					     lists:foreach(fun({SytemKey,SytemValue}) ->  ets:insert(?MODULE, {SytemKey,SytemValue}) end,Value);
					 _ -> ets:insert(?MODULE, {Key,Value})
				end
					
			end,
		   lists:foreach(F,Data);
		ErrorReason -> ?LOG("Error:~p~n",[ErrorReason])
     end,
     {ok, []}.  
    

     
%~ open(Host, Port,User, Password,ssh) ->
     %~ Hashkey = list_to_atom(hash(Host)),
     %~ case getkey(Hashkey) of
          %~ [] ->
	     %~ Discovery = getkey(discovery),
	     %~ io:format("Result:~p~n",[Discovery]),
	     %~ {ok,Pid}=mm_ssh:start(),
	     %~ try mm_ssh:connect(Pid,Host,Port,User,Password) of
		    %~ {ok,connected} when is_binary(Discovery) -> 		       
			%~ Cmd =  binary_to_list(Discovery),
			%~ io:format("Cmd:~p~n",[Cmd]),
			%~ Result = mm_ssh:send(Pid,Cmd),
			%~ io:format("Result:~p~n",[Result]),
			%~ Result;
		   %~ {ok,connected} when is_list(Discovery) -> 	
                        %~ Fun = fun(X) -> 
			   %~ io:format("______~p~n",[X]),
			   %~ R = mm_ssh:send(Pid,binary_to_list(X)),
			   %~ io:format("________________R:~p~n",[R])
			   %~ end,
			%~ Result = lists:map(Fun,Discovery),
			%~ io:format("Result:~p~n",[Result]),
			%~ Result;
		    %~ Reason ->
		        %~ io:format("Result:~p~n",[Reason]),
			%~ mm_ssh:close(Pid), 
			%~ Reason
	     %~ catch  error:_Error -> mm_ssh:close(Pid),{error,crash};
		        %~ _:_ ->  mm_ssh:close(Pid),{error,crash}
	     %~ end;
	  %~ AgentInfo -> ok     
     %~ end;
     
open(Host) ->
     Hashkey = list_to_atom(hash(Host)),
     case getkey(Hashkey) of
          [] -> [];
	  'opening'  -> 'opening'; 
	  AgentInfo -> AgentInfo#agent.pid    
     end. 


open(Host, Port,User, Password,ssh) ->
     Hashkey = list_to_atom(hash(Host)),
     case getkey(Hashkey) of
          [] ->  ssh_open(Hashkey,Host, Port,User, Password);
	  'opening'  -> {error,"opening"}; 
	  AgentInfo ->
		case {erlang:is_process_alive(AgentInfo#agent.pid),process_info(AgentInfo#agent.pid)} of
		    {true,Process_info} when Process_info =/= undefined -> ok;  
		    _ -> ssh_open(Hashkey,Host, Port,User, Password)
		end,
		{ok,AgentInfo#agent.discovery}     
     end;     
open(Host, Port,User, Password,telnet) ->
     io:format("open:~p~n",[{Host, Port,User, Password,telnet}]),
     Hashkey = list_to_atom(hash(Host)),
     case getkey(Hashkey) of
          [] ->  telnet_open(Hashkey,Host, Port,User, Password);
	  'opening'  -> {error,"opening"}; 
	  AgentInfo ->
		case {erlang:is_process_alive(AgentInfo#agent.pid),process_info(AgentInfo#agent.pid)} of
		    {true,Process_info} when Process_info =/= undefined -> ok;  
		    _ -> telnet_open(Hashkey,Host, Port,User, Password)
		end,
		{ok,AgentInfo#agent.discovery}     
     end;

open(_, _,_,_,_) -> {error,"protol error!"}.

ssh_open(Hashkey,Host, Port,User, Password) ->
     ets:insert(?MODULE, {Hashkey,'opening'}),
     Discovery = getkey(discovery),
     {ok,Pid} = mm_ssh:start(),	     
     try mm_ssh:connect(Pid,Host,Port,User,Password) of
	    {ok,connected} when is_list(Discovery) -> 
		case run_ssh_cmd(Pid,Discovery,[]) of
		     {ok,Data} -> 
			   %~ io:format("Result:~p~n",[Data]),
			 Discoverydata = discovery(Data),
			 
			 case lists:keysearch('OS',1,Discoverydata) of
				  {value,{'OS',Value}} -> 
				   AgentInfo = #agent{pid = Pid,
					host = Host,
					port = Port,
					type = 'ssh',
					user = User,
					os = Value,
					password = Password,
					discovery = Discoverydata},
			           %~ ets:delete(?MODULE, Hashkey),
				   ets:insert(?MODULE, {Hashkey,AgentInfo}),
				   {ok,Discoverydata};
				  _ ->  
				     mm_ssh:close(Pid),
				     ets:insert(?MODULE, {Hashkey,[]}),
				     {error,"no found OS type!"}
			 end;
		     ErrorReason -> mm_ssh:close(Pid),ets:insert(?MODULE, {Hashkey,[]}),ErrorReason
		end;		  
	    Reason -> Reason
     catch  error:_Error ->  mm_ssh:close(Pid),ets:insert(?MODULE, {Hashkey,[]}),{error,crash};
		_:_ ->  mm_ssh:close(Pid),ets:insert(?MODULE, {Hashkey,[]}),{error,crash}
     end.
     
telnet_open(Hashkey,Host, Port,User, Password) ->
     ets:insert(?MODULE, {Hashkey,'opening'}),
     Discovery = getkey(discovery),
     {ok,Pid} = telnet_client:start(),
     %~ io:format("Pid:~p~n",[Pid]),     
     try telnet_client:connect(Pid,Host,Port,User,Password) of
	    {ok,connected} when is_list(Discovery) -> 
		case run_telnet_cmd(Pid,Discovery,[]) of
		     {ok,Data} -> 
			   %~ io:format("Result:~p~n",[Data]),
			 Discoverydata = discovery(Data),
			 
			 case lists:keysearch('OS',1,Discoverydata) of
				  {value,{'OS',Value}} -> 
				   AgentInfo = #agent{pid = Pid,
					host = Host,
					port = Port,
					type = 'telnet',
					user = User,
					os = Value,
					password = Password,
					discovery = Discoverydata},
			           %~ ets:delete(?MODULE, Hashkey),    
				   ets:insert(?MODULE, {Hashkey,AgentInfo}),
				    %~ io:format("AgentInfo:~p~n",[AgentInfo]), 
				     %~ io:format("AgentInfo:~p~n",[open(Host)]),
				   {ok,Discoverydata};
				  _ ->  
				     telnet_client:close(Pid),
				     ets:insert(?MODULE, {Hashkey,[]}),
				     {error,"no found OS type!"}
			 end;
		     ErrorReason -> telnet_client:close(Pid), ets:insert(?MODULE, {Hashkey,[]}),ErrorReason
		end;		  
	    Reason -> Reason
     catch  error:_Error ->  telnet_client:close(Pid),ets:insert(?MODULE, {Hashkey,[]}),{error,crash};
		_:_ ->  telnet_client:close(Pid),ets:insert(?MODULE, {Hashkey,[]}),{error,crash}
     end.
getstatus(Pid,Host,Count) ->        	
	case process_info(Pid) of
	     undefined -> {error,"gen_fsm crash!"};
	     ProcessInfo ->
		case lists:keysearch(dictionary,1,ProcessInfo) of
			{value,{dictionary,Dictionary}} ->
				 %~ io:format("Result:~p~n",[{Host,Pid,lists:keysearch(status,1,Dictionary)}]),
				case  lists:keysearch(status,1,Dictionary) of
				     false -> ok;
				     {value,{status,stoped}} -> ok;
				     _ when Count > 120 -> {error,"Wait Timeout!"};
				     _ -> 
					  sleep(500),
					  getstatus(Pid,Host,Count+1)
				end;
			_ ->  {error,"dictionary error!"}
		end
	end.
    
    
     
getdatabykey(Host,Key) ->  
     Hashkey = list_to_atom(hash(Host)),
     case getkey(Hashkey) of
          [] -> {error,"Please open host."};
	 'opening'  -> {error,"opening"}; 
	 AgentInfo ->  	   
	    case lists:keysearch(Key,1,AgentInfo#agent.discovery) of
		  {value,{Key,Value}} ->  {ok,Value};
	          _ ->  
		    case getkey(list_to_atom(AgentInfo#agent.os)) of
		       [] ->  {error,"agent.conf no "++AgentInfo#agent.os++" data!"};
		       ConfigData ->  
		            %~ io:format("agent.pid:~p~n",[AgentInfo#agent.pid]), 
		            case erlang:is_process_alive(AgentInfo#agent.pid) of
				    true -> 
					    case getstatus(AgentInfo#agent.pid,Host,0) of
						ok -> getdatabyprotocol(AgentInfo,ConfigData,Key);
						ErrorReason -> ErrorReason
					    end;  
				    _ -> {error,"Process no alive!"}
			    end                            
		    end
		  
	    end
     end.
     
getdatabyprotocol(AgentInfo,ConfigData,Key) when AgentInfo#agent.type =:= 'telnet' ->
     case lists:keysearch(Key,1,ConfigData) of
       {value,{Key,{table,Pattern,Cmd}}} ->
	      case telnet_client:cmd(AgentInfo#agent.pid,binary_to_list(Cmd)) of
			 {ok,Result} -> 
			    %~ io:format("Result:~p~n",[Result]),			    
			    case analyse(Result,binary_to_list(Pattern)) of
                                 [] -> {error,Result};
				 Analysedata -> {ok,{table,Analysedata}}
			     end;
			 _ -> 							    
			    {error,"exec " ++ binary_to_list(Cmd) ++ " error."}
	      end;
       {value,{Key,{simple_string,Pattern,Cmd}}} ->
	      case telnet_client:cmd(AgentInfo#agent.pid,binary_to_list(Cmd)) of
			 {ok,Result} -> 
			    %%io:format("Result:~p~n",[Result]),
			    case analyse_simple_string(Result,binary_to_list(Pattern)) of
			         [] -> {error,Result};
			         Analysedata -> {ok,{simple_string,Analysedata}}
			    end;
			 _ ->							    
			    {error,"exec " ++ binary_to_list(Cmd) ++ " error."}
	      end;
       {value,{Key,{simple_tuple,Pattern,Cmd}}} ->
	      case telnet_client:cmd(AgentInfo#agent.pid,binary_to_list(Cmd)) of
			 {ok,Result} -> 
			     %~ io:format("Result:~p~n",[Result]),
			    case analyse_simple_tuple(Result,Pattern) of
			         [] -> {error,Result};
			         Analysedata -> {ok,{simple_tuple,Analysedata}}
			    end;
			 _ -> 
			    {error,"exec " ++ binary_to_list(Cmd) ++ " error."}
	      end;
       {value,{Key,{simple_integer,Pattern,Cmd}}} ->
	      case telnet_client:cmd(AgentInfo#agent.pid,binary_to_list(Cmd)) of
			 {ok,Result} -> 
			    %~ io:format("Result:~p~n",[Result]),			    
			    case analyse_simple_integer(Result,binary_to_list(Pattern)) of
			         [] -> {error,Result};
			         Analysedata -> {ok,{simple_integer,Analysedata}}
			    end;
			 _ ->
			    {error,"exec " ++ binary_to_list(Cmd) ++ " error."}
	      end;					      
       _ ->  
	  {error,"no "++atom_to_list(Key)++" key"}
   end;
   
getdatabyprotocol(AgentInfo,ConfigData,Key) when AgentInfo#agent.type =:= 'ssh' ->
     case lists:keysearch(Key,1,ConfigData) of
       {value,{Key,{table,Pattern,Cmd}}} ->
	      case mm_ssh:send(AgentInfo#agent.pid,binary_to_list(Cmd)) of
			 {ok,Result} -> 
			      %~ io:format("Result:~p~n",[Result]),
			     case analyse(Result,binary_to_list(Pattern)) of
                                 [] -> {error,Result};
				 Analysedata -> {ok,{table,Analysedata}}
			     end;	 
			 _ -> 							    
			    {error,"exec " ++ binary_to_list(Cmd) ++ " error."}
	      end;
       {value,{Key,{simple_string,Pattern,Cmd}}} ->
	      case mm_ssh:send(AgentInfo#agent.pid,binary_to_list(Cmd)) of
			 {ok,Result} -> 
			    %%io:format("Result:~p~n",[Result]),
			    case analyse_simple_string(Result,binary_to_list(Pattern)) of
			         [] -> {error,Result};
			         Analysedata -> {ok,{simple_string,Analysedata}}
			    end;
			 _ ->							    
			    {error,"exec " ++ binary_to_list(Cmd) ++ " error."}
	      end;
       {value,{Key,{simple_tuple,Pattern,Cmd}}} ->
	      case mm_ssh:send(AgentInfo#agent.pid,binary_to_list(Cmd)) of
			 {ok,Result} -> 
			     %~ io:format("Result:~p~n",[Result]),			    
			    case analyse_simple_tuple(Result,Pattern) of
			         [] -> {error,Result};
			         Analysedata -> {ok,{simple_tuple,Analysedata}}
			    end;
			 _ -> 
			    {error,"exec " ++ binary_to_list(Cmd) ++ " error."}
	      end;
       {value,{Key,{simple_integer,Pattern,Cmd}}} ->
	      case mm_ssh:send(AgentInfo#agent.pid,binary_to_list(Cmd)) of
			 {ok,Result} -> 
			    %~ io:format("Result:~p~n",[Result]),
			    case analyse_simple_integer(Result,binary_to_list(Pattern)) of
			         [] -> {error,Result};
			         Analysedata -> {ok,{simple_integer,Analysedata}}
			    end;
			 _ ->
			    {error,"exec " ++ binary_to_list(Cmd) ++ " error."}
	      end;					      
       _ ->  
	  {error,"no "++atom_to_list(Key)++" key"}
   end;     
getdatabyprotocol(_,_,_) -> {error,"Error Protocol!"}.

run_ssh_cmd(Pid,[L|R],Acc) ->
    case mm_ssh:send(Pid,binary_to_list(L)) of
	 {ok,Result} -> sleep(100), run_ssh_cmd(Pid,R,[Result++"\r\n"|Acc]);
	 _ -> {error,"cmd error."}
    end;
    
run_ssh_cmd(_,[],Acc) -> {ok,lists:flatten(lists:reverse(Acc))}.

run_telnet_cmd(Pid,[L|R],Acc) ->
    case telnet_client:cmd(Pid,binary_to_list(L)) of
	 {ok,Result} -> sleep(100), run_telnet_cmd(Pid,R,[Result++"\r\n"|Acc]);
	 _ -> {error,"cmd error."}
    end;
    
    
run_telnet_cmd(_,[],Acc) -> {ok,lists:flatten(lists:reverse(Acc))}.

discovery_regex([L|R],Acc) ->
    case regex:regex_str("^[\\S+]:(\\S+):(\\S+)=",L) of
           {ok,[Value,_Type,Key]} ->  discovery_regex(R,[{list_to_atom(Key),Value}|Acc]);
	   _ -> discovery_regex(R,Acc)
    end;
discovery_regex([],Acc) -> lists:reverse(Acc).

discovery(DiscoveryData) ->
   %%regex:regex_str("^[\\S+]:LOG:(\\S+)=",A)
   Tokens = string:tokens(DiscoveryData,"\r\n"),
   discovery_regex(Tokens,[]).

analyse_simple_integer(Data,Pattern) ->
   %~ io:format("analyse_simple_integer:~p~n",[regex:regex_str(Pattern,Data)]), 
   case regex:regex_str(Pattern,Data) of
           {ok,[_|RegexData]} ->                 	   
		 case string:to_integer(lists:flatten(RegexData)) of
		      {error,_} -> 0;
		      {Value,_} -> Value;
		      _ -> 0
		 end;
	   _ -> Data
    end.

analyse_simple_tuple(Data,Pattern) when is_binary(Pattern) ->   
   case regex:regex_str(binary_to_list(Pattern),Data) of
           %~ {ok,[_|RegexData]} when length(RegexData) =:= 1->  hd(RegexData) ; 
	   {ok,[_|RegexData]} -> list_to_tuple(RegexData); 
	   _ -> Data
    end;
analyse_simple_tuple(Data,Pattern) when is_list(Pattern) ->   analyse_simple_tuple_list(Data,Pattern);
analyse_simple_tuple(_,_) -> {error,"Error Pattern"}.  

analyse_simple_tuple_list(Data,[L|R]) ->
      %~ io:format("Result:~p~n",[{analyse_simple_tuple(Data,L),L}]),
      case analyse_simple_tuple(Data,L) of
           Data -> analyse_simple_tuple_list(Data,R);
	   Result -> Result
      end;
analyse_simple_tuple_list(_,[]) -> {error,"no match"}.

%~ analyse_simple_tuple(Data,Pattern)  ->   
   %~ case regex:regex_str(Pattern,Data) of
           %~ {ok,[_|RegexData]} ->		
		 %~ list_to_tuple(RegexData);
	   %~ _ -> Data
    %~ end. 
 
analyse_simple_string(Data,Pattern) ->  
   case regex:regex_str(Pattern,Data) of
           {ok,[_|RegexData]} ->		
		 lists:flatten(RegexData);
	   _ -> Data
    end.   
    
analyse(Data,Pattern) ->
    Tokens = string:tokens(Data,"\r\n"),
    %~ io:format("Result:~p~n",[Tokens]),
    analyse(Tokens,Pattern,[]). 
    
analyse([L|R],Pattern,Acc) ->
   case regex:regex_str(Pattern,L) of
           {ok,[_|RegexData]} ->		
		analyse(R,Pattern,[list_to_tuple(RegexData)|Acc]);
	   _ -> analyse(R,Pattern,Acc)
    end;
analyse([],_,Acc) -> lists:reverse(Acc).

getkey(Key) ->
    case ets:lookup(?MODULE,Key) of
	     [{_,Data}] -> Data;
	     _ -> []
	end.	
	
hash(Data) ->
    <<I:160/integer>> = crypto:sha(Data),
    string:to_lower(lists:flatten(io_lib:fwrite("~31..0s", [erlang:integer_to_list(I, 36)]))).
    
   
handle_call(init, _From, Tab) ->
    {reply, ok, Tab}; 

handle_call({pool, Host}, _From, Tab) -> 
    Hashkey = list_to_atom(hash(Host)),
    Reply = getkey(Hashkey),
    {reply, Reply, Tab}; 
    
      
handle_call(stop, _From, Tab) ->  
    {stop, normal, stopped, Tab}.  
  
handle_cast(_Msg, State) -> {noreply, State}.  
handle_info(_Info, State) -> {noreply, State}.  
terminate(_Reason, _State) -> ok.  
code_change(_OldVsn, State, _Extra) -> {ok, State}.

