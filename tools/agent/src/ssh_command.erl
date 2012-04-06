-module(ssh_command).
-include("config.hrl").
-export([exec/5]).
-define(SSH_TIMEOUT, 60000).

-define(DEFAULT_PACKET_SIZE, 32768).
-define(DEFAULT_WINDOW_SIZE, 8*?DEFAULT_PACKET_SIZE).

-include("ssh.hrl").

-export([test/0]).

test() ->
  exec("192.168.0.223", 22, "root", "dragonflow123", "ll").

exec(Host, Port, User, Passwd, Cmd) ->
	crypto:start(),
	ssh:start(),
	case ssh_pool:start() of
	      {ok,_} -> ssh_pool:init();
	      ErrorResult -> 
		%%?LOG("ssh_pool:~p~n",[ErrorResult]),
		ok
	end,

	
        case  ssh_pool:exec(Host, Port, User, Passwd, Cmd) of
	     {ok,Result} -> 
		   %%?LOG("ssh_pool:~p~n",[{Host,Cmd,Result}]),
		   {ok,Result};
	     {error,crash} ->
		    case ssh_pool:reexec(Host, Port, User, Passwd, Cmd) of
		     {ok,Result} -> 
			   %%?LOG("ssh_pool:~p~n",[{Host,Cmd,Result}]),
			   {ok,Result};
		     %~ {error,crash} -> exec1(Host, Port, User, Passwd, Cmd);
		     %~ {error,crash} -> exec1(Host, Port, User, Passwd, Cmd);			     
		     Reason ->	    
			   %%?LOG("~p =====> ~p~n",[{Host,Cmd},Reason]),
			   Reason
		    end;
	     Reason ->	    
		   %%?LOG("~p =====> ~p~n",[{Host,Cmd},Reason]),
		   Reason
        end.    
    
%~ exec1(Host, Port, User, Passwd, Cmd) ->
	%~ ?LOG("ssh_command:~p~n",[{Host, Port, User, Passwd, Cmd}]),
	%~ case ssh:connect(Host, Port, [{user_dir, "."}, {connect_timeout, ?SSH_TIMEOUT}, {silently_accept_hosts, true}, {user_interaction, false}, {user, User}, {password, Passwd}]) of
		%~ {ok, ConId} ->
			%~ Result = execute_command(ConId, Cmd),
			%~ ssh:close(ConId),				
			%~ Result;
		%~ {error, Reason} ->				
			%~ {error, Reason}
	%~ end.
	
		
%~ execute_command(ConId, Cmd) ->
	%~ case ssh_connection:session_channel(ConId,?DEFAULT_WINDOW_SIZE,?DEFAULT_PACKET_SIZE,?SSH_TIMEOUT) of
		%~ {ok, ChaId} ->
			%~ case ssh_connection:exec(ConId, ChaId, Cmd, ?SSH_TIMEOUT) of
				%~ success -> 
				    %~ recv_result(ConId, ChaId, <<>>);
				%~ failure ->
					%~ ssh_connection:close(ConId, ChaId), 
                    %~ {error, excute_error}
			%~ end;
		%~ {error, Reason} ->
			%~ {error, Reason}
	%~ end.
	
%~ recv_result(ConId, ChaId, Res) ->
	%~ receive
		%~ {'ssh_cm', ConId, {'data', ChaId, _Type, Data}} ->
			    %~ %io:format("1:~n"), 
			    %~ ssh_connection:adjust_window(ConId,ChaId,size(Data)),            
			    %~ recv_result(ConId, ChaId,  list_to_binary([Res ,Data]));
	        %~ {'ssh_cm', ConId, {'eof', ChaId}} ->
			    %~ %io:format("2:~n"),
			    %~ ssh_connection:close(ConId, ChaId),   
			    %~ recv_result(ConId, ChaId, Res); 
		%~ {'ssh_cm', ConId, {'closed', ChaId}} ->
			%~ %io:format("3:~n"),
			%~ %sh_connection:close(ConId, ChaId),   
			%~ {ok,binary_to_list(Res)};
		%~ {ssh_cm,ConId,{exit_status,ChaId,_Status}} ->
			%~ %io:format("4:~n"), 
			%~ %ssh_connection:close(ConId, ChaId),   
			%~ recv_result(ConId, ChaId, Res); 
		%~ _Other ->
			%~ %io:format("___________________________:~p~n",[_Other]),  
			%~ ssh_connection:close(ConId, ChaId),   
			%~ recv_result(ConId, ChaId, Res)
	%~ end.
    

    
%my_split_binary(Bin) ->
%   my_split_binary_t(Bin,size(Bin),1,[]).
%my_split_binary_t({<<10>>,B},0,_N,List) ->     
%    List;
%my_split_binary_t(Binary,Size,N,L) ->
%    {Head,End} = split_binary(Binary,1),
            


    
	