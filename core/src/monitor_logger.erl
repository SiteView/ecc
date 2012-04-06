-module(monitor_logger).
-compile(export_all).

get_node()->
	case server_conf:getServerConf(logNode) of
		undefined->
			{ok,Node}=inet:gethostname(),
			%list_to_atom("logger@"++Node);
			list_to_atom("master@"++Node);
		LogNode->
			LogNode
	end.
	%% @spec log(Terms)->{ok,Log} | {error,Reason}
%% where
%%	Terms = record()
%%	Log = string()
%% Reason = string()
%% @doc call monitor_logger_server:log, called by monitor:save_result/2
log(Terms)->
	App = atom_to_list(dbcs_base:get_app()),
	rpc:call(get_node(),monitor_logger_server,call,[{log,Terms,App}]).
	
q(Date)->
	App = atom_to_list(dbcs_base:get_app()),	rpc:call(get_node(),monitor_logger_server,call,[{q,Date,App}]).
	q(Date,Id)->
	App = atom_to_list(dbcs_base:get_app()),
	rpc:call(get_node(),monitor_logger_server,call,[{q,Date,Id,App}]).
	q(StartDate,EndDate,Ids)->
	App = atom_to_list(dbcs_base:get_app()),
	rpc:call(get_node(),monitor_logger_server,call,[{q,StartDate,EndDate,Ids,App}]).q(Id,StartDate,StartTime,EndDate,EndTime)->
	App = atom_to_list(dbcs_base:get_app()),
	rpc:call(get_node(),monitor_logger_server,call,[{q,Id,StartDate,StartTime,EndDate,EndTime,App}]).
	ql(StartDate,StartTime, EndDate,EndTime, Params)->
	App = atom_to_list(dbcs_base:get_app()),
	rpc:call(get_node(),monitor_logger_server,call,[{ql,StartDate,StartTime,EndDate,EndTime,Params,App}]).
	qc(Ids, Count, DaysLimit)->	
	App = atom_to_list(dbcs_base:get_app()),
%% 	io:format("monitor_logger qceeeeeeeeeeee ~p~n", [App]),
%% 	io:format("monitor_logger qc1 ~p~n", [get_node()]),
	rpc:call(get_node(),monitor_logger_server,call,[{qc,Ids,Count,DaysLimit,App}]).
	
qc(Ids,Count)->
	qc(Ids,Count,1).