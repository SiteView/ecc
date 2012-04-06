%% ---
%%your comment
%%
%%---
-module(monitor_runner).
-export([run/1,run_rpc/2,getBrowseData/3,getrpcMonitorCon/3]).
-include("monitor.hrl").
run(M)->
	M:run(M,hh).
	
run_rpc(Data,Params)->
	{P,A} = binary_to_term(zlib:unzip(Data)),
	% io:format("==RPC CALL run_rpc:~n~p~n~p~p~n",[P,A,Params]),
	Class = proplists:get_value(class,P),
	Id = proplists:get_value(id,P),
	M = Class:new(),
	M:add_properties(P),
	M:add_attributes(A),
	App = M:get_app(),
	
	try
		monitor_proxy_client:add_ref({App,Id}),
		dbcs_base:set_app(App), %% set app 
		
		case M:update() of
			{error,Error}->
				% M:get_tid()!{self(),stop},
				monitor_proxy_client:release({App,Id}),
				M:delete(),
				{error,Error};
			_->
				R = zlib:zip(term_to_binary({M:get_properties(),M:get_attributes()})),
				% M:get_tid()!{self(),stop},
				monitor_proxy_client:release({App,Id}),
				M:delete(),
				{ok,R}
		end
	catch
		EE:Err->
			monitor_proxy_client:release({App,Id}),
			M:delete(),
			{error,{EE,Err}}
	end.

getBrowseData(Oper,Params,MonitorData)->
%% 	io:format("==RPC CALL getBrowseData:~p",[Oper]),
	case Oper of
		"add"->
			Pa = binary_to_term(zlib:unzip(Params)),
%% 			io:format("==RPC CALL getBrowseData:Pa~p~n~n",[Pa]),
			Class = proplists:get_value(class,Pa),
			Key = list_to_atom(Class),
			M = Key:new(),
			M:set_property(?PAGE_PARAMS,Pa),
			Ret = M:getBrowseData(Pa),
			% io:format("Ret:~p~n",[Ret]),
			M:delete(),
			zlib:zip(term_to_binary(Ret));
		"edit"->
			{P,A} = binary_to_term(zlib:unzip(MonitorData)),
			Pa = binary_to_term(zlib:unzip(Params)),
			Class = proplists:get_value(class,Pa),
			Key = list_to_atom(Class),
			M = Key:new(),
			M:add_properties(P),
			M:add_attributes(A),
			Ret = M:getBrowseData(Pa),
			M:delete(),
			zlib:zip(term_to_binary(Ret))
	end.

getrpcMonitorCon(Oper,Node,Params) ->
		case Oper of
				"add"->
					io:format("*******************************getBrowseData:~p~n",[Params]),
					Class = proplists:get_value(class,Params),
					Key = list_to_atom(Class),
					M = Key:new(),
					try
					M:set_property(?PAGE_PARAMS,Params),
					Ret = M:getBrowseData(Params),
					io:format("+===================Ret~p~n", [Ret]),
					io:format("*******************************1getBrowseData:~p~n",[Params]),
					Ret
					catch
						_:Err->
							io:format("*******************************error:~p~n",[Err]),
							{error,Err}
					after
						M:delete()
					end;
				"edit"->
					Id = proplists:get_value(id,Params),
					case api_siteview:find_object(list_to_atom(Id)) of
						[]->
							[];
						[M2|_]->
							M2:getBrowseData(Params)
					end;
				_->
					[]
			end.