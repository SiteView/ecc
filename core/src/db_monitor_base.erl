%% @copyright 2008-2009 Dragonflow
%% @author shaohua.wang
%% @version 1.0

%% @doc db_monitor_base
%%
%% This module is a base monitor for database monitor.
-module(db_monitor_base, [BASE]).
-extends(atomic_monitor).
-compile(export_all).

-include("monitor.hrl").
-include("monitor_template.hrl").
-include("db_monitor_base.hrl").

%% @spec new() -> Obj
%% @type Obj = term()
%% @doc create a new instance for db_monitor_base.
new() ->
	Base = atomic_monitor:new(),
	
	%%internal used by monitor
	Base:set_attribute(current_state, ""),
	Base:set_attribute(database_max_columns, ?MAX_COLUMNS),
	Base:set_attribute(database_max_rows, ?MAX_ROWS),
	Base:set_attribute(database_max_summary, ?MAX_SUMMARY),
	Base:set_attribute(first_row, ?DEFAULT_FIRST_ROW),
	Base:set_attribute(caption, ""),

	{?MODULE,Base}.

%% @spec defaultTitle(Params) -> List
%% @type Params = [term()]
%% @type List = string()
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	BASE:defaultTitle(Params) ++ ": " ++ case proplists:get_value(database,Params) of undefined->"";V->V end.
	
%% @spec update() -> Result
%% @type Result = term()
%% @doc update is the run function called by schedule to monitor database.
update() ->	
	%%state attribute
	THIS:set_attribute(rows, 0),
	THIS:set_attribute(columns,0),
	THIS:set_attribute(content_match, "n/a"),
	THIS:set_attribute(column_1th,"n/a"),	
	THIS:set_attribute(error_string,""),
	THIS:set_attribute(status, "good"),
	THIS:set_attribute(round_trip_time, 0),
		
	%%remmber start time
	Time_Start = now(),	

	%%format the argument list send to java
	Args = get_args(),
	Request = {"com.dragonflow.erlangecc.monitor.DatabaseMonitor", "update", Args},

	%%get java node
	Java_Node = siteview:get_java_node(),
	
	%%send request to java and receive the result
	Response = rpc(?REG_NAME, Java_Node, Request),
	_Handle_result = case Response of
						%%in normal condition, result is the list
						 Response when is_list(Response) ->
							set_attribute_loop(Response);
						%%error return
						_ ->
							THIS:set_attribute(error_string, "Unkown error"),
							THIS:set_attribute(status, "error")							
					 end,
	
	%%calculate the roundtime
	Time_Used = timer:now_diff(now(), Time_Start)/1000,
	THIS:set_attribute(round_trip_time, Time_Used),

	%%set the state string attribute
	set_state(),
	true.

%% @spec set_state() -> ok
%% 
%% @doc set_state is the function that set state after upate function being run.
set_state() ->
	%%get error string state
	{ok, {_, Error_Str}} = THIS:get_attribute(error_string),
	Error_String = if
					   (length(Error_Str) > 0) -> Error_Str ++ "<br>";
					   true -> ""
				   end,		
	%%get roundtime stat				
	{ok, {_, Round_Trip_Time}} = THIS:get_attribute(round_trip_time),
	ROUND_TRIP_TIME = erlang:integer_to_list(round(Round_Trip_Time)) ++ " millsec, <br>",
	%%get row stat
	{ok, {_, Rows}} = THIS:get_attribute(rows),
	ROWS = erlang:integer_to_list(Rows) ++ " rows, <br>",
	%%get columns stat
	{ok, {_, Columns}} = THIS:get_attribute(columns),
	COLS = erlang:integer_to_list(Columns) ++ " columns, <br>",
	%%get the first line of the table
	{ok, {_, First_Row}} = THIS:get_attribute(first_row),
	if
		%%if there is error string, just print it
		length(Error_String) > 0 ->
			THIS:set_attribute(status, "error"),
			STATES = Error_String;
		%%no error, print the row number, column number and first row of table
		true ->
			STATES = ROUND_TRIP_TIME ++  ROWS ++ COLS ++ First_Row
	end,
	THIS:set_attribute(?STATE_STRING,STATES),
	ok.

%%@spec rpc(RegName, Node, Msg) -> Response
%%@type RegName = atom()
%%@type Node = atom()
%%@type Msg = [tuple()]
%%@doc remote process calling for the java node with messages.
rpc(RegName, Node, Msg) ->	
	%%calculate the timeout time
	{ok, {connect_timeout,Connection_timeout}} = THIS:get_property(connect_timeout),
	{ok, {query_timeout,Query_timeout}} = THIS:get_property(query_timeout),
	Timeout = (Connection_timeout + Query_timeout) * 1000,
	
	%%ping the java node
	Ping = net_adm:ping(Node),
	
	%%send request to java node
	{RegName, Node} ! Msg,	
	
	%%try to get result
	receive
		%%some thing error
		{error, _From, Ret} ->
			THIS:set_attribute(status, "error"),		
			Ret;
		%%get  result	
		{ok, _From, Ret} ->	
			Ret
		%%time out	
		after Timeout ->
			case Ping of
				pong ->
					[{error, "time is out. "}];
				pang ->
					[{error, "Connect Java Node Error! "}]
			end
	end.



%% @spec get_args() -> Args
%% @type Args = [Tuples]
%% @type Tuples = {Key, Value}
%% @type Key = atom()
%% @type Value = term()
%% @doc get arguments needed to be sent to the java node .
get_args() ->	
	{ok,{driver,Driver_string}} = THIS:get_property(driver),
	{ok,{database,Database_name}} = THIS:get_property(database),
	{ok,{query_property,Query_cmd}} = THIS:get_property(query_property),
	{ok, {db_user,Db_user_name}} = THIS:get_property(db_user),
	{ok, {query_file_path, Query_file_path}} = THIS:get_property(query_file_path),
	Query_file_cmd = case length(Query_file_path) of
						 L when (L > 0) -> file_utils:read_file(Query_file_path);
						  _ -> ""
					 end,
	{ok, {connect_timeout,Connection_timeout}} = THIS:get_property(connect_timeout),
	{ok, {query_timeout,Query_timeout}} = THIS:get_property(query_timeout),
	{ok, {db_password, Db_password}} = THIS:get_property(db_password),
	Max_columns = THIS:get_attribute_as_number(database_max_columns, ?MAX_COLUMNS),
	Max_rows = THIS:get_attribute_as_number(database_max_rows, ?MAX_ROWS),
	Max_summary = THIS:get_attribute_as_number(database_max_summary, ?MAX_SUMMARY),
	Match_Val = case THIS:get_property(content_match) of
					{ok, {_, ?UNDEFINED}} ->
						?UNDEFINED;
					{ok, {_, ""}} ->
						?UNDEFINED;
					{ok, {_, Content_Match}} ->
						Content_Match
				end,
	[{dbUrl, Database_name}, 						
	 {driverName, Driver_string},
	 {dbUserName, Db_user_name},
	 {userpass, Db_password},
	 {connectionTimeout, Connection_timeout},
	 {queryTimeout, Query_timeout},
	 {dbMaxColums, Max_columns},
	 {dbMaxRows, Max_rows},
	 {dbMaxSummary, Max_summary},
	 {query_cmd, Query_cmd},
	 {queryFileCmd, Query_file_cmd},
	 {content_match, Match_Val}										 
	].



%% @spec set_attribute_loop(Response) -> ok | stop
%% @type Response = [term()]
%% @doc set the response value to the corresponding attribute, stop if message contians error.
set_attribute_loop([]) ->
	ok;
set_attribute_loop([H|T]) ->
	case H of
		{error, Error} ->
			THIS:set_attribute(error_string, Error),
			THIS:set_attribute(status, "error");		
		{rows, Rows} ->
			ROWS = erlang:list_to_integer(Rows),
			THIS:set_attribute(rows, ROWS),
			set_attribute_loop(T);
		{columns, Columns} ->
			COLS = erlang:list_to_integer(Columns),
			THIS:set_attribute(columns, COLS),
			set_attribute_loop(T);		
		{caption, Caption} ->
			THIS:set_attribute(caption, Caption),
			set_attribute_loop(T);
		{row_1th, First_Row} ->
			THIS:set_attribute(first_row, First_Row),			
			set_attribute_loop(T);
		{column_1th, Column_1th} ->
			THIS:set_attribute(column_1th, Column_1th),
%%			Numeric = numeric_utils:parse_numeric(Column_1th),
%%			THIS:set_attribute(?MEASUREMENT, Numeric),
			set_attribute_loop(T);
		{content_match, Match_Value} ->
			case Match_Value of
				"false" ->
					THIS:set_attribute(status, "error"),
					THIS:set_attribute(content_match, "false"),			
					THIS:set_attribute(error_string, "content not match.");
				"true" ->
					THIS:set_attribute(content_match, "true");
				_ ->
					THIS:set_attribute(status, "error"),
					THIS:set_attribute(error_string, "Unknown error")
					
			end,
			set_attribute_loop(T);		
		{_Key, _Value} ->
			%% 			io:format("other key: ----- ~p, value: ---- ~p~n", [Key, Value]),
			set_attribute_loop(T)		
	end.


%% @spec verify(Params) -> {ok, []} | {error, Reason}
%% @type Params = [term()]
%% @type Reason = string()
%% @doc verifying data input by user is correct or not.
verify(Params) ->
	Database = proplists:get_value(database, Params),
	Conn_Timeout = proplists:get_value(connect_timeout, Params),
	Query_Timeout = proplists:get_value(query_timeout, Params),	
	
	%%verify database url
	Errs =	case Database of
				"" ->
					[{database, "Database Connection URL can not be empty. "}];
				Database ->
					case str_utils:contain_spaces(Database) of
						false ->
							[];
						true ->
							[{database, "Database Connection URL no spaces are allowed"}]
					end
			end ++ 
	%%verify connect timeout		
				case Conn_Timeout of
					""->
						[{connect_timeout, "timeout missing."}];
					V->
						if
							not is_number(V) ->
								[{connect_timeout, "timeout must be a number."}];
							true->
								[]
						end
			end ++ 
	%%verify qury time
				case Query_Timeout of
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
				{error,E}->
					E;
				_->
					[]
			end,			
			
	if 
		(length(Errs) > 0) ->
			{error, Errs};
		true ->
			{ok, ""}
	end.



%% @spec get_template_property() -> List
%% @type List = [term()]
%% @doc get_template_property is the function called by schedule to show user the GUI interface to input data.
get_template_property()->
	BASE:get_template_property() ++ 
		[
		 #property{name=database,title="Database Connection URL", type=text, order=1, default="", description="Enter the URL to the database connection. URL Format: hostname:port/database-name"},
		 #property{name=query_property, title="Query", type=text,order=2, default="", description="the SQL statement to be performed on the database.  (for example, select * from sysobjects)"},
		 #property{name=db_user, title="Database User Name", type=text, order=4,  default="", description="optional, user name used to connect to the database"},
		 #property{name=db_password, title="Database Password", type=password, order=5, default="", description="optional,  password used to connect to the database"},
		 #property{name=driver, title="Database Driver", type=scalar, order=6, description="the driver used to connect to the database"},
		 #property{name=connect_timeout, title="Connection Timeout", type=numeric, order=7,  default=60, description="the time out, in seconds, to wait for a database connection",baselinable=true},
		 #property{name=content_match, title="Match Content", type=text, order=9, advance=true, default="", description="optional, match against the first record of querying result,using a string or a regular expression between the slashes //"},
		 #property{name=query_file_path, title="File Path", type=text, order=12, advance=true, default="", description="Excute a query stored in a file"},
		 #property{name=query_timeout, title="Query Timeout", type=numeric, order=14, advance=true, default=60, description="the time out, in seconds, to wait for a database query",baselinable=true},
		 #property{name=status, type=text, title="Status", configurable=false, state=true},
		 #property{name=rows, title="rows", type=numeric, default=0, configurable=false, state=true},
		 #property{name=columns, title="columns", type=numeric, default=0, configurable=false, state=true},
		 #property{name=column_1th, title="result column 1", type=text, configurable=false, state=true},
		 #property{name=round_trip_time, title="round trip time(in millsecond)", type=numeric, configurable=false, state=true,baselinable=true},
		 #property{name=content_match, title="content match", type=text, configurable=false, state=true}
		].


%% @spec getScalarValues(Prop, Params) -> List
%% @type Prop = atom()
%% @type Params = [term()]
%% @type List = [Opt]
%% @type Opt = {ShowContent, Value}
%% @type ShowContent = string()
%% @type Value = string()
%% @doc getScalarValues is the function called by schedule to show a dropdown list box and its value.
getScalarValues(Prop,Params)->
	BASE:getScalarValues(Prop, Params).





