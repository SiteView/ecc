-module(alert_logger).
-compile(export_all).

get_node()->
	case server_conf:getServerConf(logNode) of
		undefined->
			{ok,Node}=inet:gethostname(),
			list_to_atom("master@"++Node);
		LogNode->
			LogNode
	end.
	log(Terms)->
	App = atom_to_list(dbcs_base:get_app()),
	rpc:call(get_node(),alert_logger_server,call,[{log,Terms,App}]).
	
q(Date)->
	App = atom_to_list(dbcs_base:get_app()),	rpc:call(get_node(),alert_logger_server,call,[{q,Date,App}]).

q(Date,Id)->
	App = atom_to_list(dbcs_base:get_app()),
	rpc:call(get_node(),alert_logger_server,call,[{q,Date,Id,App}]).
	
q(StartDate,EndDate,Type)->
	App = atom_to_list(dbcs_base:get_app()),
	Params = 
	case Type of
		"all"->
			[];
		_->
			[{type,'=',Type}]
	end,
	rpc:call(get_node(),alert_logger_server,query_data,[App,StartDate,0,EndDate,0,Params]).
	% rpc:call(get_node(),alert_logger_server,call,[{q,StartDate,EndDate,Type,App}]).
	
q(StartDate,StartTime, EndDate,EndTime, Params)->
	App = atom_to_list(dbcs_base:get_app()),
	rpc:call(get_node(),alert_logger_server,query_data,[App,StartDate,StartTime,EndDate,EndTime,Params]).

q(StartDate,StartTime, EndDate,EndTime, Params,From,Count)->
	App = atom_to_list(dbcs_base:get_app()),
	rpc:call(get_node(),alert_logger_server,query_data,[App,StartDate,StartTime,EndDate,EndTime,Params,From,Count]).	