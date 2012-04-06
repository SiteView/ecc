%% @copyright 2008-2009 Dragonflow
%% @author shaohua.wang, kaiyang.cheng
%% @version 1.0
%%
%% Description: Monitor the server performance statistics from Oracle Database 
%% %% Versions supported: Oracle Database 8i, 9i, 10g and 11g servers
%% Platform: All
%%
-module(oracle_jdbc_monitor, [BASE]).
-extends(browsable_base).
-compile(export_all).
-define(RECEIVE_TIME_OUT, 60*1000).
-define(DEBUG_INFO, debug_info).
-define(CURRENT_STATE, current_state).
-define(REG_NAME, java_mail_box).


-include("monitor.hrl").
-include("monitor_template.hrl").


%% @spec new() -> Obj
%% @type Obj = term()
%% @doc create a new instance for oracle monitor
new() ->
	Base = browsable_base:new(),
	Base:set_attribute(?DEBUG_INFO, ""),
	Base:set_attribute(?CURRENT_STATE, ""),
	Base:set_attribute(countersInError, 0),
	Base:set_attribute(counter_names, []),
	
	{?MODULE,Base}.


%% @spec get_classifier(error) -> List
%% @type List = [Tuple]
%% @type Tule = {status, Logic, Value}
%% @type Logic = '!=' | '==' | '>' | '<' | 'contain
%% @type Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error) ->
	Count = THIS:getMaxCounters(),
	Cls = case THIS:get_property(error_classifier) of
			  {ok, {error_classifier, Classifier}} ->
				  Classifier;
			  _ ->
				  [{countersInError, '>', 0}]
		  end,
	if 
		length(Cls) < Count ->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,Count - length(Cls)));
		true ->
			Cls
	end;
get_classifier(warning) ->
	Count = THIS:getMaxCounters() ,
	Cls = case THIS:get_property(warning_classifier) of
			  {ok, {warning_classifier, Classifier}} ->
				  Classifier;
			  _ ->
				  [{countersInError, '>', 0}]
		  end,
	if 
		length(Cls) < Count ->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,Count - length(Cls)));
		true ->
			Cls
	end;
get_classifier(good) -> 
	Count = THIS:getMaxCounters(),
	Cls = case THIS:get_property(good_classifier) of
			  {ok, {good_classifier, Classifier}} ->
				  Classifier;
			  _ ->
				  [{countersInError, '==', 0}]
		  end,
	if 
		length(Cls) < Count ->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,Count - length(Cls)));
		true ->
			Cls
	end.


%% @spec getBrowseData(Params) -> List
%% @type Params =[term()]
%% @type List = [term()]
%% @doc getBroweData return the counters this monitor contained.
getBrowseData(Params) ->
	THIS:set_attribute(?DEBUG_INFO, "get browse data ..."),
	Conn_Url = proplists:get_value(connection_url, Params),
	User = proplists:get_value(user, Params),
	Driver =proplists:get_value(driver, Params), 		
	Password = proplists:get_value(password, Params),
	Conn_Timeout = proplists:get_value(connection_timeout, Params),
	Query_Timeout = proplists:get_value(query_timeout, Params),	
	Request = [
			{connection_url, Conn_Url},
			{user, User},
			{driver, Driver},			
			{password, Password},
			{connection_timeout, Conn_Timeout},
			{query_timeout, Query_Timeout},
			{max_counter, THIS:getMaxCounters()}
		   ],
	Java_Node = siteview:get_java_node(),
	Response = rpc(?REG_NAME, Java_Node, {"com.dragonflow.erlangecc.monitor.OracleJDBCMonitor", "getBrowseData", Request}),

	case Response of
		[{error, Reason}] ->
			{error, Reason};
		List ->
			qsort(List)
	end.

getHostname()->
	case THIS:get_property(connection_url) of
		{ok,{_,V}}->
			V;
		_->
			BASE:getHostname()
	end.

%% @spec get_template_property() -> List
%% @type List = [term()]
%% @doc get_template_property is the function called by schedule to show user the GUI interface to input data.
get_template_property()->
	BASE:get_template_property() ++ 
		[
		 #property{name=connection_url, title="Database Connection URL", type=text, order=1, default="", description="JDBC URL to the database connection. URL Format: hostname:port/database-name"},
		 #property{name=user, title="Database User Name", type=text, order=2,  description="User name used to connect to the database."},
		 #property{name=password, title="Database Password", type=password, order=3,  description="Password used to connect to the database."},
		 #property{name=driver, title="Database Driver", type=text, order=4,  default="oracle.jdbc.driver.OracleDriver", description="Driver used to connect to the database."},
		 #property{name=connection_timeout, title="Connection Timeout", type=numeric, order=5,  default=60, description="Time in seconds to wait for a database connection.",baselinable=true},
		 #property{name=query_timeout, title="Query Timeout", type=numeric, order=6,  default=60, description="Time in seconds to wait for a database query to complete.",baselinable=true}
		
		].
		
qsort([]) -> [];
qsort([Pivot|T]) ->
	qsort([X || X <- T, element(2,X) < element(2, Pivot)])
	++ [Pivot] ++
	qsort([X || X <- T, element(2,X) >= element(2,Pivot)]).
	


%% @spec update() -> Result
%% @type Result = term()
%% @doc update is the run function called by schedule to monitor oracle database performance.
update() ->
	THIS:set_attribute(?DEBUG_INFO, "monitor update begin..."),
	initCounters(),
	Request = get_args(),
	Java_Node = siteview:get_java_node(),
	Response = rpc(?REG_NAME, Java_Node, {"com.dragonflow.erlangecc.monitor.OracleJDBCMonitor", "update", Request}),
	set_attribute_loop(Response, []),	
	true.

initCounters()->
	THIS:set_attribute(countersInError, 0),
	lists:foreach(fun(X)->THIS:set_attribute(element(1,X),"n/a") end, THIS:get_counters()).
%% @spec get_args() -> Args
%% @type Args = [Tuples]
%% @type Tuples = {Key, Value}
%% @type Key = atom()
%% @type Value = term()
%% @doc get arguments needed to be sent to the java node .
get_args() ->
	THIS:set_attribute(?DEBUG_INFO, "get arguments ..."),
	{ok,  {_, Conn_Url}} = THIS:get_property(connection_url),
	{ok, {_, User}} = THIS:get_property(user),
	{ok, {_, Driver}} = THIS:get_property(driver),
	{ok, {_, Password}} = THIS:get_property(password),
	{ok, {_, Conn_Timeout}} = THIS:get_property(connection_timeout),
	{ok, {_, Query_Timeout}} = THIS:get_property(query_timeout),
	Counters = get_counters(),	
	[
	 {connection_url, Conn_Url},
	 {user, User},
	 {driver, Driver},
	 {password, Password},
	 {connection_timeout, Conn_Timeout},
	 {query_timeout, Query_Timeout},
	 {max_counter, THIS:getMaxCounters()},
	 {counters, Counters}	   
	].

	
%% @spec defaultTitle(Params) -> List
%% @type Params = [term()]
%% @type List = string()
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	BASE:defaultTitle(Params) ++ ": " ++ case proplists:get_value(connection_url,Params) of undefined->"";V->V end.

%%@spec rpc(RegName, Node, Msg) -> Response
%%@type RegName = atom()
%%@type Node = atom()
%%@type Msg = [tuple()]
%%@doc remote process calling for the java node with messages.
rpc(RegName, Node, Msg) ->
	THIS:set_attribute(?DEBUG_INFO, "remote process call java node ..."),
	Ping = net_adm:ping(Node),
	{RegName, Node} ! Msg,	
	receive
		{ok, _, Ret} ->	
			qsort(Ret);
		{error, _From, Ret} ->
			Ret;
		{error, Reason} ->
			[{error, Reason}]
	after ?RECEIVE_TIME_OUT ->
			case Ping of
				pong ->
					[{error, "time is out. "}];
				pang ->					
					[{error, "Connect Java Node Error! "}]
			end
	end.


%% @spec set_attribute_loop(Response) -> ok | stop
%% @type Response = [term()]
%% @doc set the response value to the corresponding attribute, stop if message contians error.
set_attribute_loop([], Result) ->
	THIS:set_attribute(?STATE_STRING, Result);	
set_attribute_loop([H|T], Result) ->
	case H of
		{error, Error} ->
			THIS:set_attribute(countersInError, THIS:get_counterssize()),
			THIS:set_attribute(?STATE_STRING, Error);
		{"counters_in_error", Counter_Errors} ->
			THIS:set_attribute(countersInError, Counter_Errors),
			set_attribute_loop(T, Result);
		Counters_Value ->
			Result1 = Result ++ set_counter_value(Counters_Value, get_counters()),
			set_attribute_loop(T, Result1)	
	
	end.


%% @spec set_counter_value(Values) -> ok
%% @type Values = [{CounterId, Value}]
%% @type CounterId = string()
%% @type Value = term()
%% @doc set each counter value to this monitor.
set_counter_value(H, Counter) ->
	case H of
		{Atom, VVV} ->
			Value = number_util:get_value(VVV),
			VV = 
			if
				is_float(Value) ->
					erlang:round(Value*100)/100;
				true ->
					Value
			end,			
			THIS:set_attribute(Atom, VV),
			case lists:keysearch(element(1,H),1,Counter) of
				{value, {_K, V}} ->
					lists:flatten(io_lib:format("~p = ~p <br>", [V, VV]));
				_ ->
					[]
			end;
		_ ->
			[]
	end.



%% @spec get_counters() -> CounterList
%% @type CounterList = [Counter]
%% @type Counter = [string(), string()]
%% @doc get counters that user has selected.
get_counters() ->
	{ok, {_, Browse}} = THIS:get_property(browse),
	Len = length(Browse),
	if
		(Len > 0) ->
			Browse;
		true ->
			[]
	end.

get_counterssize() ->
	{ok, {_, Browse}} = THIS:get_property(browse),
	Len = length(Browse).
				
%% @spec getMaxCounters() -> Number
%% @type Number = integer()
%% @doc get the max counters this monitor is.
getMaxCounters() ->
	THIS:getMaxCounter().


%% @spec verify(Params) -> {ok, []} | {error, Reason}
%% @type Params = [term()]
%% @type Reason = string()
%% @doc verifying data input by user is correct or not.
verify(Params) ->
	Errs = 
		case proplists:get_value(connection_url, Params) of
			"" ->
				[{connection_url, "Database Connection URL can not be empty. "}];
			Database ->
				case str_utils:contain_spaces(Database) of
					false ->
						[];
					true ->
						[{connection_url, "Database Connection URL no spaces are allowed"}]
				end
		end ++
			case proplists:get_value(connection_timeout, Params) of
				"" ->
					[{connection_timeout, "timeout missing."}];
				V->
					if
						not is_number(V) ->
							[{connection_timeout, "timeout must be a number."}];
						true->
							[]
					end
		end ++ 
			case proplists:get_value(query_timeout, Params) of
				"" ->
					[{query_timeout, "timeout missing."}];
				V->
					if
						not is_number(V) ->
							[{query_timeout, "timeout must be a number."}];
						true->
							[]
					end
		end ++ 
			case BASE:verify(Params) of
				{error, E} ->
					E;
				_ ->
					[]
		end,
	if
		(length(Errs) >0 ) ->
			{error, Errs};
		true ->
			{ok, ""}
	end.

					
			
	
	
	
	
	
	
	
	
	
	
	
	
	
	

