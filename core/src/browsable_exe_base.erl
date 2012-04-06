
%% @copyright 2008-2009 Dragonflow
%% @author shaohua.wang
%% @version 1.0

%% @doc oracle_jdbc monitor
%%
%%This module is a base monitor that used by db2_monitor.
-module(browsable_exe_base, [BASE]).
-extends(browsable_base).
-compile(export_all).

-include("monitor.hrl").
-include("monitor_template.hrl").

-define(REG_NAME, java_mail_box).
-define(DEBUG_INFO, debug_info).
-define(RECEIVE_TIME_OUT, 10*1000).


%% @spec new() -> Obj
%% @type Obj = term()
%% @doc create a new instance for browsable_exe_base monitor.
new() ->
	Base = browsable_base:new(),
	Base:set_attribute(?DEBUG_INFO, "new"),
	Base:set_attribute(counters, []),
	
	{?MODULE,Base}.

%% @spec defaultTitle(Params) -> List
%% @type Params = [term()]
%% @type List = string()
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	BASE:defaultTitle(Params) ++ ": " ++ case proplists:get_value(server,Params) of undefined->"";V->V end.

%% @spec get_classifier(error) -> List
%% @type List = [Tuple]
%% @type Tule = {status, Logic, Value}
%% @type Logic = '!=' | '==' | '>' | '<' | 'contain
%% @type Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error)->
	
	Cls = case THIS:get_property(error_classifier) of
				{ok,{error_classifier,Classifier}}->
					Classifier;
				_->
					[{countersInError,'>',0}]
			end,
	if 
		length(Cls) < 10 ->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end;
get_classifier(warning)->
	Cls =case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'>',0}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end;
get_classifier(good)->
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'==',0}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end.


%% @spec update() -> Result
%% @type Result = term()
%% @doc update is the run function called by schedule to monitor performance.
update() ->	
	initCounters(),
	Request = get_args(),
	{ok, {_, Timeout}} = THIS:get_property(timeout),
	Java_Node = siteview:get_java_node(),
	Response = rpc(?REG_NAME, Java_Node, {"com.dragonflow.erlangecc.monitor.DB2Monitor", "update", Request}, Timeout*1000),
	io:format("Response:~p ", [Response]),
	set_update_data(Response),
	
	true.
initCounters()->
	THIS:set_attribute(countersInError, 0),
	lists:foreach(fun(X)->THIS:set_attribute(element(1,X),"n/a") end, THIS:get_counters()).
%% @spec set_update_data(Response) -> ok |stop
%% @type Response = [term()]
%% @doc set_update_data is the function to save data from response, return ok if response data contains 
%% no error message, return stop otherwise.
set_update_data([]) ->
	ok;
set_update_data([H|T]) ->
	case H of
		{error, Error} ->
			THIS:set_attribute(countersInError, THIS:get_counterssize()),
			THIS:set_attribute(?STATE_STRING, Error),
			stop;
		{state_string, State_String} ->
			THIS:set_attribute(?STATE_STRING, State_String),
			set_update_data(T);
		{counters_in_error, Counter_Errors} ->
			THIS:set_attribute(countersInError, Counter_Errors),
			set_update_data(T);
		{counters_value, Counters_Value} ->
			set_counter_value(Counters_Value),
			set_update_data(T);
		_ ->
			set_update_data(T)		
	
	end.


%% @spec set_counter_value(Values) - ok
%% @type Values = [Tuple]
%% @type Tuple = {Key, Value}
%% @type Key = atom()
%% @type Value = term()
%% @doc set_counter_value is the function to save each counter value into this monitor.
set_counter_value([]) ->
	ok;
set_counter_value([H|T]) ->
	case H of
		{Atom, VVV} ->
			V = number_util:get_value(VVV),
			VV = 
			if
				is_float(V) ->
					erlang:round(V*100)/100;
				true ->
					V
			end,	
			THIS:set_attribute(atom_to_list(Atom), VV),
			set_counter_value(T);
		_ ->
			io:format("unexpected value: ~p~n", [H]),
			set_counter_value(T)
	end.

%% @spec get_args() -> List
%% @type List = [term()]
%% @doc get_args is the function to get arguments to be sent to java node needed.
get_args() ->
	THIS:set_attribute(?DEBUG_INFO, "get args..."),
	{ok, {_, Server}} = THIS:get_property(server),
	{ok, {_, User}} = THIS:get_property(user),
	{ok, {_, Port}} = THIS:get_property(port),
	{ok, {_, Partition}} = THIS:get_property(partition),
	{ok, {_, Password}} = THIS:get_property(password),
	{ok, {_, Instance}} = THIS:get_property(instance),
	Counters = get_counters(),
	[
	 {server, Server},
 	 {port, Port},
	 {user, User},
	 {password, Password},
	 {instance, Instance},
 	 {partition, Partition},
	 {counters, Counters}
	].
	
	
%% @spec verify(Params) -> {ok, []} | {error, Reason}
%% @type Params = [term()]
%% @type Reason = string()
%% @doc verifying data input by user is correct or not.
verify(Params) ->
	MaxCounters = getMaxCounters(),	
	Errs = 
		case proplists:get_value(server, Params) of
			"" ->
				[{server, "Server can not be empty. "}];
			Server ->
				case str_utils:contain_spaces(Server) of
					false ->
						[];
					true ->
						[{server, "Server no spaces are allowed"}]
				end
		end ++ 
			case proplists:get_value(user, Params) of
				"" ->
					[{user, "User Name can not be empty. "}];
				UserName ->
					case str_utils:contain_spaces(UserName) of
						false ->
							[];
						true ->
							[{user, "User Name no spaces are allowed. "}]
					end
		end ++
			case proplists:get_value(instance, Params) of
				"" ->
					[{instance, "Node Name can not be empty. "}];
				Instance ->
					case str_utils:contain_spaces(Instance) of
						false ->
							[];
						true ->
							[{user, "Node Name no spaces are allowed. "}]
					end
		end ++
				case proplists:get_value(port, Params) of
					""->
						[{port, "port missing."}];
					V->
						if
							not is_number(V) ->
								[{port, "port must be a number."}];
							true->
								[]
						end
		end ++
				case proplists:get_value(timeout, Params) of
					""->
						[{timeout, "time out missing."}];
					V->
						if
							not is_number(V) ->
								[{timeout, "time out must be a number."}];
							true->
								[]
						end
		end ++		
				case proplists:get_value(browse, Params) of
				Browse when(erlang:length(Browse) > MaxCounters) ->
					[{browse, "Counters Number can not greater than MaxCounters Value. "}];
				_ ->
					[]
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


%% @spec getBrowseData(Params) -> List
%% @type Params =[term()]
%% @type List = [term()]
%% @doc getBroweData return the counters this monitor contained.
getBrowseData(Params) ->
	THIS:set_attribute(?DEBUG_INFO, "get browse data ..."),
	THIS:set_attribute(counters, []),
	Server = proplists:get_value(server, Params),
	Port = proplists:get_value(port, Params),
	User = proplists:get_value(user, Params),
	Partition = proplists:get_value(partition, Params),
	Password = proplists:get_value(password, Params),
	Instance = proplists:get_value(instance, Params),
	Timeout = proplists:get_value(timeout, Params),
	Request = [
			{server, Server},
			{port, Port},
			{user, User},
			{password, Password},
			{instance, Instance},
			{partition, Partition}
		   ],
	Java_Node = siteview:get_java_node(),
	Response = rpc(?REG_NAME, Java_Node, {"com.dragonflow.erlangecc.monitor.DB2Monitor", "getBrowseData", Request}, list_to_integer(Timeout)*1000),
	case Response of
		[{error, Reason}] ->
			{error, Reason};
		List ->
			qsort(List)
	end.

getHostname()->
	case THIS:get_property(server) of
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
		 #property{name=server, title="Server", type=text, order=1, description="Database server hostname or ip."},
		 #property{name=port, title="Port", type=numeric, order=2, default=50000, description="Database server port."},
		 #property{name=user, title="User Name", type=text, order=3, description="Username for Db2."},
		 #property{name=password, title="Password", type=password, order=4,  description="Password for Db2."},
		 #property{name=instance, title="Database Name", type=text, order=5, default="sample", description="Database Name for monitoring."},
		 #property{name=partition, title="Partition", type=scalar, order=6, default="-1", description="Database monitor scale.-1 is current partition, -2 is all partition."},

		 #property{name=timeout, type=numeric, order=1, advance=true, title="Timeout", description="the time out, seconds, to wait for the value to be retrieved", default=60,baselinable=true}

		].

%% @spec getScalarValues(Prop, Params) -> List
%% @type Prop = atom()
%% @type Params = [term()]
%% @type List = [Opt]
%% @type Opt = {ShowContent, Value}
%% @type ShowContent = string()
%% @type Value = string()
%% @doc getScalarValues is the function called by schedule to show a dropdown list box and its value
getScalarValues(Prop,Params)->
	case Prop of		
		partition ->
			[
			 {"-1", "-1"},
			 {"-2", "-2"}
			];
		_ ->
			BASE:getScalarValues(Prop, Params)
	end.

%%@spec rpc(RegName, Node, Msg) -> Response
%%@type RegName = atom()
%%@type Node = atom()
%%@type Msg = [tuple()]
%%@doc remote process calling for the java node with messages.
rpc(RegName, Node, Msg, Timeout) ->
	THIS:set_attribute(?DEBUG_INFO, "remote process call java node ..."),
	Ping = net_adm:ping(Node),
    if
        Ping==pang ->
            [{error,"Connect Java Node Error! "}];
        true ->
            {RegName, Node} ! Msg,	
            receive
                {ok, _, Ret} ->	
                    Ret;
                {error, _From, Ret} ->
                    Ret		
            after Timeout ->
                [{error, "time is out. "}]
            end
    end.


%% @spec getMaxCounters() -> Numeric
%% @type Numeric = numeric()
%% @doc getMaxCounters function returns the max value configurated in this monitor.
getMaxCounters() ->
	THIS:getMaxCounter().


%% @spec get_counters() -> List
%% @type List = [term()]
%% @doc get_counters is the function that return counters selected by user.
get_counters() ->
	THIS:set_attribute(?DEBUG_INFO, "get counters..."),
	{ok, {_, Browse}} = THIS:get_property(browse),
	Len = length(Browse),
	if
		(Len > 0) ->
			Browse;
		true ->
			[]
	end.
get_counterssize() ->
	THIS:set_attribute(?DEBUG_INFO, "get counters..."),
	{ok, {_, Browse}} = THIS:get_property(browse),
	Len = length(Browse).

qsort([]) -> [];
qsort([Pivot|T]) ->
	qsort([X || X <- T, element(2,X) < element(2, Pivot)])
	++ [Pivot] ++
	qsort([X || X <- T, element(2,X) >= element(2,Pivot)]).
	