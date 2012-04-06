%%
%% java_node
%%
%%
-module(java_node).
-compile(export_all).

-define(PROCESS,'java_mail_box').
-define(OFBIZ_MBOX,'eccadmin').

send(Class,Action,Msg)->
	Node = siteview:get_java_node(),
	{?PROCESS,Node} ! {Class,Action,node(),self(),Msg},
	receive
		Ret ->
			Ret
	after 60000 ->
		{error,lists:flatten(io_lib:format("receive message from java monitor node(~p) timeout.",[Node]))}
	end.
	
send(Class,Action,Msg,Timeout)->
	Node = siteview:get_java_node(),
	{?PROCESS,Node} ! {Class,Action,node(),self(),Msg},
	receive
		Ret ->
			Ret
	after Timeout ->
		{error,lists:flatten(io_lib:format("receive message from java monitor node(~p) timeout.",[Node]))}
	end.

ofbizcall(ServiceName,Param,Timeout) ->
	Node = server_conf:get_ofbiz_node(),
	{?OFBIZ_MBOX,Node} ! {self(),"OfbizService",ServiceName,Param},
	receive
		Ret ->
			Ret
	after Timeout ->
		{error,lists:flatten(io_lib:format("receive message from javanode(~p) timeout.",[Node]))}
	end.

ofbizcall(ServiceName,Param) ->
	ofbizcall(ServiceName,Param,60000).