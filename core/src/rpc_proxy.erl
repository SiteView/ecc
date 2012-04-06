
%% @copyright 2010 siteview
%% @version 1.0
%% @doc manage the nodes with a central out bound call, enable easy configuration of local call or remote call
%%   server_conf only manage the localhost or remote machine call
-module(rpc_proxy).
-compile(export_all).

%% @doc  three cases: 
%% 		local call: M/F/A, 
%% 		remote call on localhost: M/F/A, 
%% 		remote call: M/F/A, 
call(Node,Module,Fun,Args) ->
	io:format("*************rpc:proxy****************~p~n~p~n~p~n*****",[Module,Fun,Args]),
	Return = erlang:apply(Module,Fun,Args),
	io:format("Return************:~p~n",[Return]),
	%%TODO: list of all possible nodes: db, nnm, wmi,layout,master,monitorproxy
	Return.
