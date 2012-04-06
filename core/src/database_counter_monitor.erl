%% @copyright 2008-2009 Dragonflow
%% @author shaohua.wang, jian.huang, kaiyang.cheng
%% @version 1.0

%% @doc DateBase Counter monitor
%% Description: Monitor DateBase performance with counters selected
%% %% Versions supported: Any database with a valid JDBC driver that supports SQL queries
%% Platform: All
%%
-module(database_counter_monitor, [BASE]).
-extends(browsable_base).
-compile(export_all).

-define(PROPERTY_NAME_COUNTER_VALUE, "counter_id_").
-define(UNDEFINED, "undefined").
-define(REG_NAME, java_mail_box).
-define(EMPTY, empty).
-define(FULL, full).
-define(RECEIVE_TIME_OUT, 60*1000).
-define(DEBUG_INFO, debug_info).
-define(CURRENT_STATE, current_state).
-define(LAST_MESUREMENTS, last_measurements).
-define(CURRENT_VALUE, "current_value_").

-include("monitor.hrl").
-include("monitor_template.hrl").


%% @spec new() -> Obj
%% @type Obj = term()
%% @doc create a new instance for oracle monitor
new() ->
	Base = browsable_base:new(),
	Base:set_attribute(?DEBUG_INFO, "new"),
	Base:set_attribute(?CURRENT_STATE, ""),
	Base:set_attribute(?LAST_MESUREMENTS, []),
	Base:set_attribute(countersInError, 0),
	Base:set_attribute(error_string, ""),
	Base:set_attribute(browse_data, []),
	
	{?MODULE,Base}.


%% @spec get_classifier(error) -> List
%% @type List = [Tuple]
%% @type Tule = {status, Logic, Value}
%% @type Logic = '!=' | '==' | '>' | '<' | 'contain
%% @type Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error.
get_classifier(error) ->
	%%get max counters the system support
	Count = THIS:getMaxCounters(),
	Cls = case THIS:get_property(error_classifier) of
			  {ok, {error_classifier, Classifier}} ->
				  Classifier;
			  _ ->
				  [{countersInError, '>', 0}]
		  end,
	%%show max counters  
	if 
		length(Cls) < Count ->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,Count - length(Cls)));
		true ->
			Cls
	end;
get_classifier(warning) ->
	%%get max counters the system support
	Count = THIS:getMaxCounters() ,
	Cls = case THIS:get_property(warning_classifier) of
			  {ok, {warning_classifier, Classifier}} ->
				  Classifier;
			  _ ->
				  [{countersInError, '<', 0}]
		  end,
	%%show max counters  
	if 
		length(Cls) < Count ->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,Count - length(Cls)));
		true ->
			Cls
	end;
get_classifier(good) -> 
	%%get max counters the system support
	Count = THIS:getMaxCounters(),
	Cls = case THIS:get_property(good_classifier) of
			  {ok, {good_classifier, Classifier}} ->
				  Classifier;
			  _ ->
				  [{countersInError, '==', 0}]
		  end,
	%%show max counters  
	if 
		length(Cls) < Count ->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,Count - length(Cls)));
		true ->
			Cls
	end.



%% @spec update() -> Result
%% @type Result = term()
%% @doc update is the run function called by schedule to monitor performance with counters.
update() ->
	initCounters(),
	THIS:set_attribute(?DEBUG_INFO, "monitor update begin..."),

	%%get argument of the update argument send to java node 
	Request = get_args(),
	%%get java node 
	Java_Node = siteview:get_java_node(),

	%%send request to java
	Response = rpc(?REG_NAME, Java_Node, {"com.dragonflow.erlangecc.monitor.DatabaseCounterMonitor", "update", Request}),	
	case Response of
		%%process result
		Response when is_list(Response) ->
			set_attribute_loop(Response);		
		_ ->			
			THIS:set_attribute(countersInError, THIS:getMaxCounters()),
			THIS:set_attribute(?STATE_STRING, "Unknow error")
	end,
	THIS:set_attribute(?DEBUG_INFO, "update ok..."),
	true.
initCounters()->
	THIS:set_attribute(countersInError, 0),
	lists:foreach(fun(X)->THIS:set_attribute(element(1,X),"n/a") end, THIS:get_counters()).

%% @spec set_attribute_loop(Response) -> ok | stop
%% @type Response = [term()]
%% @doc set the response value to the corresponding attribute, stop if message contians error.
set_attribute_loop([]) ->
	ok;
set_attribute_loop([H|T]) ->
	case H of		
		%%java send back error
		{error, Error} ->
			THIS:set_attribute(?STATE_STRING, Error),
			THIS:set_attribute(countersInError, THIS:get_counterssize()),
			stop;
		%%state string show to UI	
		{state_string, State_String} ->
			THIS:set_attribute(?STATE_STRING, State_String),
			set_attribute_loop(T);
		%%counter number that retrieve data error	
		{counters_in_error, Errors} ->
			THIS:set_attribute(countersInError, Errors),
			set_attribute_loop(T);
		%%get counter's value	
		{counter_values, Counter_Values} when is_list(Counter_Values) ->
			set_counter_value(Counter_Values),
%%			{ok, {_, {_, _Vals}}} = THIS:get_attribute(?LAST_MESUREMENTS),
%%			THIS:set_attribute(?LAST_MESUREMENTS, {?FULL, Counter_Values}),
			set_attribute_loop(T);
		{current_measurements, Current_Meas} ->
%%			{ok, {_, {_, _Vals}}} = THIS:get_attribute(?LAST_MESUREMENTS),
			THIS:set_attribute(?LAST_MESUREMENTS, Current_Meas),
			set_attribute_loop(T);	
		%%counters list	
		{browse_data, Browse_Data} ->
			THIS:set_attribute(browse_data, Browse_Data),
			set_attribute_loop(T);
		_ ->
			set_attribute_loop(T)
	end.


%% @spec set_counter_value(List) - ok
%% @type List = [term()]
%% @doc set_counter_value is the function called by set_attribute_loop to save counter values in this monitor.
set_counter_value([]) ->
	THIS:set_attribute(?DEBUG_INFO, "set counter value ok. "),
	ok;
set_counter_value([H|T]) ->
	case H of
		{K,VVV} ->
			V = number_util:get_value(VVV),
			VV = 
			if
				is_float(V) ->
					erlang:round(V*100)/100;
				true ->
					V
			end,
			THIS:set_attribute(atom_to_list(K),VV),
			set_counter_value(T);
		_ ->
			set_counter_value(T)
	end.
	

%% @spec getBrowseData(Params) -> List
%% @type Params =[term()]
%% @type List = [term()]
%% @doc getBroweData return the counters this monitor contained.
getBrowseData(Params) ->
	THIS:set_attribute(browse_data, []),
	THIS:set_attribute(?DEBUG_INFO, "get browse data ..."),

	%%get values required for getting counters
	Conn_Url = proplists:get_value(connection_url, Params),
	Driver_name = proplists:get_value(driver, Params),
	Db_user = proplists:get_value(user, Params),
	Db_password = proplists:get_value(password, Params),
	Query_cmd = proplists:get_value(pquery, Params),
	Connection_timeout = proplists:get_value(connection_timeout, Params),
	Query_timeout = proplists:get_value(query_timeout, Params),
	No_Divisor = proplists:get_value(no_divisor, Params),
	Query_div = proplists:get_value(divisor_query, Params),
	
	%%format the request
	Request = [
			{connection_url, Conn_Url},
			{driver, Driver_name},
			{user, Db_user},
			{password, Db_password},
			{query_cmd, Query_cmd},
			{divisor_query, Query_div},
			{no_divisor, No_Divisor},
			{connection_timeout, Connection_timeout},
			{query_timeout, Query_timeout}
		   ],
		   
	%%get java node	   
	Java_Node = siteview:get_java_node(),
	
	%%send request to java
	Response = rpc(?REG_NAME, Java_Node, {"com.dragonflow.erlangecc.monitor.DatabaseCounterMonitor", "getBrowseData", Request}),
	
	case Response of
		[{error, Reason}] ->
			{error, Reason};
		_ ->
			Response
	end.



	
%% @spec get_args() -> Args
%% @type Args = [Tuples]
%% @type Tuples = {Key, Value}
%% @type Key = atom()
%% @type Value = term()
%% @doc get arguments needed to be sent to the java node .
get_args() ->
	THIS:set_attribute(?DEBUG_INFO, "get request arguments ..."),
	MaxCounter = THIS:getMaxCounters(),		%% value should be 10.
	
	%%get argument required for getting values
	{ok,{driver,Driver_name}} = THIS:get_property(driver),
	{ok, {connection_url,Conn_Url}} = THIS:get_property(connection_url),
	{ok, {pquery,Query_cmd}} = THIS:get_property(pquery),
	{ok, {user,Db_user}} = THIS:get_property(user),
	{ok, {connection_timeout,Connection_timeout}} = THIS:get_property(connection_timeout),
	{ok, {query_timeout,Query_timeout}} = THIS:get_property(query_timeout),
	{ok, {password, Db_password}} = THIS:get_property(password),
	{ok, {divisor_query, Divisor_query}} = THIS:get_property(divisor_query),
	{ok, {no_cumulative, No_Cummulative}} = THIS:get_property(no_cumulative),
	{ok, {no_divisor, No_Divisor}} = THIS:get_property(no_divisor),

	Counters = get_counters(),
	{ok, {last_measurements, Mesurements}} = THIS:get_attribute(last_measurements),	

	%%format argument
	Args = [
			{max_counter, MaxCounter},
			{driver, Driver_name},
			{connection_url, Conn_Url},
			{user, Db_user},
			{password, Db_password},
			{query_cmd, Query_cmd},
			{divisor_query_cmd, Divisor_query},
			{connection_timeout, Connection_timeout},
			{query_timeout, Query_timeout},
			{no_cummulative, No_Cummulative},
			{no_divisor, No_Divisor},
			{counters_size, length(Counters)},	%% 
			{counters, Counters},
			{last_measurements, Mesurements}		   
			],	
	Args.


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
		{ok,_,Ret}->
			qsort(Ret);
		{error,Err}->
			[{error,Err}];
		{Ret, _, _}->
			[{error, atom_to_list(Ret)}];
		_ ->
			{error,"java node return a unknow error."}
			
		after ?RECEIVE_TIME_OUT ->
			case Ping of
				pong ->
					[{error, "time is out. "}];
				pang ->					
					[{error, "Connect Java Node Error! "}]
			end
	end.


%% @spec defaultTitle(Params) -> List
%% @type Params = [term()]
%% @type List = string()
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	BASE:defaultTitle(Params) ++ ": " ++ case proplists:get_value(connection_url,Params) of undefined->"";V->V end.
	
%% @spec get_template_property() -> List
%% @type List = [term()]
%% @doc get_template_property is the function called by schedule to show user the GUI interface to input data.
get_template_property()->
	BASE:get_template_property() ++ 
		[ 
		 #property{name=connection_url, title="Database Connection URL", type=text, order=1, default="localhost:1433/ecc", description="Enter the URL to the database connection. URL Format: hostname:port/database-name Informix:hostname:port/database-name:INFORMIXSERVER=myserver"},
		 #property{name=user, title="Database User Name", type=text, order=2, editable=false, default="sa", description="User name used to connect to the database."},
		 #property{name=password, title="Database Password", type=password, order=3, editable=false, default="123", description="Password used to connect to the database."},
		 #property{name=driver, title="Database Driver", type=scalar, order=4, editable=false, description="Driver used to connect to the database."},
		 #property{name=pquery, title="Query", type=text, order=5, editable=false, default="", description="SQL query to be performed on the database.  Needs to return \"Counter Name , Value1, Value2...\" format.  For example: SELECT name,gets,misses FROM counter;"},
		 #property{name=connection_timeout, title="Connection Timeout", type=numeric, order=6, editable=false, default=60, description="Time in seconds to wait for a database connection.",baselinable=true},
		 #property{name=query_timeout, title="Query Timeout", type=numeric, order=7, editable=false, default=60, description="Time in seconds to wait for a database query to complete.",baselinable=true},
		 #property{name=divisor_query, title="Divisor Query", type=text, order=9, advance=true,  description="The default is to divide by the time elapsed between monitor runs."},
		 #property{name=no_cumulative, title="Cumulative Counters", type=bool, order=11, advance=true, description="When selected, the counters will be treated as cumulative counters. The value will be determined directly from the query of the database."},
		 #property{name=no_divisor, title="Divide Counters", type=bool, advance=true, order=12, description="When selected, the counters will be divided by the divisor."}
		].



%% @spec set_current_value(Tuple) -> ok
%% @type Tuple = tuple()
%% @doc set_current_value is the function to set current counter value.
set_current_value(Tuple_Value) when(is_tuple(Tuple_Value))->
	{ok, {_, List}} = THIS:get_attribute(last_measurements),	
	THIS:set_attribute(last_measurements, List ++ [Tuple_Value]);
set_current_value(_) ->
	ok.

%% @spec is_current_value_key(Atom) -> String
%% @type Atom = atom()
%% @type String = string()
%% @doc this function is to test whether it is current value key.
is_current_value_key(Atom) ->
	Len = length(?CURRENT_VALUE),
	String = string:left(atom_to_list(Atom), Len),
	String == ?CURRENT_VALUE.

%% @spec is_counter_value_key(Atom) -> String
%% @type Atom = atom()
%% @type String = string()
%% @doc this function is to test whether it is current counter value key.
is_counter_value_key(Atom) ->
	Len = length(?PROPERTY_NAME_COUNTER_VALUE),
	String = string:left(atom_to_list(Atom), Len),
	String == ?PROPERTY_NAME_COUNTER_VALUE.


%% @spec get_counters() -> List
%% @type List = [term()]
%% @doc get_counters is the function called by get_args to list counters.
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
%% @doc getMaxCounters is the function return max counters configurated by this monitro.
getMaxCounters() ->
	THIS:getMaxCounter().


%% @spec getScalarValues(Prop, Params) -> List
%% @type Prop = atom()
%% @type Params = [term()]
%% @type List = [Opt]
%% @type Opt = {ShowContent, Value}
%% @type ShowContent = string()
%% @type Value = string()
%% @doc getScalarValues is the function called by schedule to show a dropdown list box and its value.
getScalarValues(Prop,Params)->
	case Prop of	
		%%supported database
		driver ->
			[
			 {"JDBC Driver for Oracle", "oracle.jdbc.driver.OracleDriver"},
			 {"JDBC Driver for MS SQL Server 2000", "com.microsoft.jdbc.sqlserver.SQLServerDriver"},
			 {"JDBC Driver for DB2-V8-V9", "com.ibm.db2.jcc.DB2Driver"},
			 {"JDBC Driver for MS SQL Server 2005", "com.microsoft.sqlserver.jdbc.SQLServerDriver"},
			 {"JDBC Driver for Informix", "com.informix.jdbc.IfxDriver"},
			 {"JDBC Driver for Sybase-11", "com.sybase.jdbc2.jdbc.SybDriver"},
			 {"JDBC Driver for MySQL", "com.mysql.jdbc.Driver"}
			];
		_ ->
			BASE:getScalarValues(Prop, Params)
	end.


%% @spec verify(Params) -> {ok, []} | {error, Reason}
%% @type Params = [term()]
%% @type Reason = string()
%% @doc verifying data input by user is correct or not.
verify(Params) ->
	Errs = 
		%%url can not empty or contain space
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
			%%query can not empty
			case proplists:get_value(pquery, Params) of
				"" ->
					[{pquery, "Query can not be empty. "}];
				_ ->
					[]
		end ++
			%%time out can not empty and must be a number
			case proplists:get_value(connection_timeout, Params) of
				""->
					[{connection_timeout, "timeout missing."}];
				V->
					if
						not is_number(V) ->
							[{connection_timeout, "timeout must be a number."}];
						true->
							[]
					end
		end ++ 
			%%query time can not and must be a number
			case proplists:get_value(query_timeout, Params) of
				""->
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
		
	%%is there some errors?
	if
		(length(Errs) > 0) ->
			{error, Errs};
		true ->
			{ok, ""}
	end.
	

qsort([]) -> [];
qsort([Pivot|T]) ->
	qsort([X || X <- T, element(2,X) < element(2, Pivot)])
	++ [Pivot] ++
	qsort([X || X <- T, element(2,X) >= element(2,Pivot)]).
	