-module(agent_win).
-include("config.hrl").
-behaviour(gen_server).
-export([start/0,init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-compile(export_all).
-record(agent_win, {host, port,user, password,os,discovery}).  

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop()  -> gen_server:call(?MODULE, stop).

pool(Host) ->  gen_server:call(?MODULE, {pool, Host},infinity).  

init([]) ->
	crypto:start(),
	ssh:start(),
	regex:start(),
	ets:new(?MODULE, [set,named_table,public]),
	case file:consult("agent_win.conf") of
		{ok,Data} ->
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

open(Host, Port,User, Password,win) ->
	Hashkey = list_to_atom(hash(Host)),
	case getkey(Hashkey) of
		[] ->
			Os = "Windows",
			Discovery = [{"OS","Windows"}],
			AgentInfo  = #agent_win{host = Host,
				port = Port,
				user = User,
				password = Password,
				os = Os,
				discovery = Discovery
			},
			ets:insert(?MODULE, {Hashkey,AgentInfo}),
			{ok,[{"OS","Windows"}]};
		AgentInfo -> {ok,AgentInfo#agent_win.discovery}     
	end;

open(_, _,_,_,_) -> {error,"protol error!"}.

getdatabykey(Host,Key) ->  
	Hashkey = list_to_atom(hash(Host)),
	case getkey(Hashkey) of
		[] -> 	     
			{error,"Please open host."};
		AgentInfo ->  	   
			case lists:keysearch(Key,1,AgentInfo#agent_win.discovery) of
				{value,{Key,Value}} ->  {ok,Value};
				_ ->  
					case getkey(list_to_atom(AgentInfo#agent_win.os)) of
						[] -> 		           
							{error,"agent_win.conf no "++AgentInfo#agent_win.os++" data!"};
						ConfigData ->  
							wmic:start(),
							User = AgentInfo#agent_win.user,
							Password = AgentInfo#agent_win.password,
							case lists:keysearch(Key,1,ConfigData) of
								{value,{Key,{table,Pattern,Cmd}}} ->
									case wmic:wmic(Host,User,Password,binary_to_list(Cmd)) of
										{ok,[{_,{ok,[Result|_]}}|_]} -> 
											Analysedata = [analyse_binary(Value) || Value <- tuple_to_list(Result)],
											{ok,{table,Analysedata}};
										_ ->
											{error,"exec " ++ binary_to_list(Cmd) ++ " error."}
									end;
								{value,{Key,{simple_list,Pattern,Cmd}}} ->
									case wmic:wmic(Host,User,Password,binary_to_list(Cmd)) of
										{ok,[{_,{ok,Result}}]} ->
											F = fun(Column) ->
												%%lists:map(fun({Name,Value})->{Name,analyse_binary(Value)} end,tuple_to_list(Column))
												[analyse_binary(Value) || Value <- tuple_to_list(Column)]
											end,
											Analysedata = [{'Num',length(Result)}|lists:map(F,Result)],
											{ok,{simple_list,Analysedata}};
										_ ->
											{error,"exec " ++ binary_to_list(Cmd) ++ " error."}
									end;
								_ ->
									{error,"no "++atom_to_list(Key)++" key"}
							end
					end
			end
	end.
	
analyse_binary({Key,Value}) ->
	case is_binary(Value) of
		true -> {Key,binary_to_list(Value)};
		_ -> {Key,Value}
		end.

analyse_simple_string(Data,Pattern) ->  
	case regex:regex_str(Pattern,Data) of
		{ok,[_|RegexData]} ->		
			lists:flatten(RegexData);
		_ -> Data
	end. 

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