%% @author lei.lin@dragonflow.com
%% @copyright 2009 siteview
%% @version 1.0
%% @doc monitor websphere performance 

-module(tuxedo_monitornew,[BASE]).
-compile(export_all).
-extends(dispatcher_monitor).
-include("monitor.hrl").
-include("monitor_template.hrl").

-define(MaxCounters,40).
-define(PROPERTY_NAME_COUNTER_VALUE,"browsableValue").
-define(PROPERTY_NAME_COUNTER_NAME,"_browseName").
-define(RECEIVE_TIME_OUT, 30*1000).
-define(DEBUG_INFO, debug_info).
-define(CURRENT_STATE, current_state).
-define(REG_NAME, java_mail_box).

%% @spec new() -> ok
%% @doc initialization browsable windows counter monitor.
new() ->
	Base = dispatcher_monitor:new(),
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
				  [{status, '==', 0}]
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
	Server = proplists:get_value(server, Params),
	Port = proplists:get_value(port, Params),
	Tuxclient = proplists:get_value(tuxclient, Params),
	Tuxdata =proplists:get_value(tuxdata, Params),
	Username = proplists:get_value(username, Params),
	Password = proplists:get_value(password, Params),
	
	T_MACHINE = platform:tuxedoGetParam(Server, Port, Username, Password, "T_MACHINE", "null"),
	T_SERVER = platform:tuxedoGetParam(Server, Port, Username, Password, "T_SERVER", "null"),
	T_MSG = platform:tuxedoGetParam(Server, Port, Username, Password, "T_MSG", "null"),
	T_CLIENT = platform:tuxedoGetParam(Server, Port, Username, Password, "T_CLIENT", "null"),
	T_SERVICE = platform:tuxedoGetParam(Server, Port, Username, Password, "T_SERVICE", "null"),
	T_WSH = platform:tuxedoGetParam(Server, Port, Username, Password, "T_WSH", "null"),
	T_QUEUE = platform:tuxedoGetParam(Server, Port, Username, Password, "T_QUEUE", "null"),
	ResponseOld = [{machine, "T_MACHINE"}, {server, "T_SERVER"}, {msg, "T_MSG"},
				{client, "T_CLIENT"},{service, "T_SERVICE"}, {wsh, "T_WSH"}, {queue, "T_QUEUE"}],
	
	Response = ResponseOld ++ [{X, "T_MACHINE/" ++ X}||X <- T_MACHINE] ++ [{X, "T_SERVER/" ++ X}||X <- T_SERVER] ++ [{X, "T_MSG/" ++ X}||X <- T_MSG]
	++ [{X, "T_CLIENT/" ++ X}||X <- T_CLIENT] ++ [{X, "T_SERVICE/" ++ X}||X <- T_SERVICE] ++ [{X, "T_WSH/" ++ X}||X <- T_WSH] ++ [{X, "T_QUEUE/" ++ X}||X <- T_QUEUE],
	
	case Response of
		[{error, Reason}] ->
			{error, Reason};
		List ->
			qsort(List)
	end.

%% getHostname()->
%% 	case THIS:get_property(connection_url) of
%% 		{ok,{_,V}}->
%% 			V;
%% 		_->
%% 			BASE:getHostname()
%% 	end.

	
qsort([]) -> [];
qsort([Pivot|T]) ->
	qsort([X || X <- T, element(2,X) < element(2, Pivot)])
	++ [Pivot] ++
	qsort([X || X <- T, element(2,X) >= element(2,Pivot)]).
	
%% @spec get_template_property() -> List
%% @doc overloading function.
get_template_property() ->
    BASE:get_template_property() ++ 
    [   
        #property{name=port,title="Port", description="Enter the port number for Tuxedo",type=text,editable=true,configurable=true,order=2},
		#property{name=tuxclient,title="Client Name", description="Enter the optional client name for Tuxedo",type=text,editable=true,configurable=true,order=3},
		#property{name=tuxdata,title="Connection Data", description="Enter the optional Connection Data for Tuxedo",type=text,editable=true,configurable=true,order=4},
		#property{name=username,title="Username", description="Enter the Username for Tuxedo",type=text,editable=true,configurable=true,order=5},
		#property{name=password,title="Password", description="Enter the Password for Tuxedo",type=text,editable=true,configurable=true,order=6}	
    ].

%% %% @spec get_template_property() -> List
%% %% @doc overloading function.
%% get_conprop() ->
%%     [   
%%         #property{name=pServerName,title="Server", description="the name of the server",type=text,editable=true,configurable=false,order=1},
%% 	 	#property{name=pPort,title="Port", description="Enter the port number for Tuxedo",type=text,editable=true,configurable=false,order=2},
%% 		#property{name=pTuxClient,title="Client Name", description="Enter the optional client name for Tuxedo",type=text,editable=true,configurable=false,order=3},
%% 		#property{name=pTuxData,title="Connection Data", description="Enter the optional Connection Data for Tuxedo",type=text,editable=true,configurable=false,order=4},
%% 		#property{name=pUsername,title="Username", description="Enter the Username for Tuxedo",type=text,editable=true,configurable=false,order=5},
%% 		#property{name=pPassword,title="Password", description="Enter the Password for Tuxedo",type=text,editable=true,configurable=false,order=6}	
%%     ].



%% @spec update() -> Result
%% @type Result = term()
%% @doc update is the run function called by schedule to monitor oracle database performance.
update() ->
	THIS:set_attribute(?DEBUG_INFO, "monitor update begin..."),
	Request = get_args(),
%% 	Java_Node = siteview:get_java_node(),
%% 	Response = rpc(?REG_NAME, Java_Node, {"com.dragonflow.erlangecc.monitor.TuxedoMonitor", "update", Request}),
%% 	set_attribute_loop(Response, []),	
	set_attribute_loop(Request, []),
	true.


%% @spec get_args() -> Args
%% @type Args = [Tuples]
%% @type Tuples = {Key, Value}
%% @type Key = atom()
%% @type Value = term()
%% @doc get arguments needed to be sent to the java node .
get_args() ->
	THIS:set_attribute(?DEBUG_INFO, "get arguments ..."),
	{ok,  {_, Server}} = THIS:get_property(server),
	{ok,  {_, Port}} = THIS:get_property(port),
	{ok,  {_, Tuxclient}} = THIS:get_property(tuxclient),
	{ok,  {_, Tuxdata}} =THIS:get_property(tuxdata),
	{ok,  {_, Username}} = THIS:get_property(username),
	{ok,  {_, Password}} = THIS:get_property(password),

%% 	T_MACHINE = platform:tuxedoGetParam(Server, Port, Username, Password, "T_MACHINE", "null"),
%% 	T_SERVER = platform:tuxedoGetParam(Server, Port, Username, Password, "T_SERVER", "null"),
%% 	T_MSG = platform:tuxedoGetParam(Server, Port, Username, Password, "T_MSG", "null"),
%% 	T_CLIENT = platform:tuxedoGetParam(Server, Port, Username, Password, "T_CLIENT", "null"),
%% 	T_SERVICE = platform:tuxedoGetParam(Server, Port, Username, Password, "T_SERVICE", "null"),
%% 	T_WSH = platform:tuxedoGetParam(Server, Port, Username, Password, "T_WSH", "null"),
%% 	T_QUEUE = platform:tuxedoGetParam(Server, Port, Username, Password, "T_QUEUE", "null"),
	Counters = get_counters(),	
	io:format("tuxedo_monitornew Counters:~p~n",[Counters]),
  	T_MACHINE = platform:tuxedoGetParam(Server, Port, Username, Password, "T_MACHINE", "CXY"),
	io:format("tuxedo_monitornew tuxedoGetParam:~p~n",[T_MACHINE]),
  	T_MSG1 = platform:tuxedoGetParam(Server, Port, Username, Password, "T_MSG", "1"),
	io:format("tuxedo_monitornew tuxedoGetParam:~p~n",[T_MSG1])	,		
  	T_MSG2 = platform:tuxedoGetParam(Server, Port, Username, Password, "T_MSG", "2"),
	io:format("tuxedo_monitornew tuxedoGetParam:~p~n",[T_MSG2]).
	
%% 	Ret = get_TuxedoData(Counters, Server, Port, Username, Password, "", "", []),
%% 	F = fun(X)->
%% 		{Key, Value} = X,
%% 		case string:rstr(Value, "T_MACHINE/") of
%% 			1 ->		
%% 			  T_MACHINE = platform:tuxedoGetParam(Server, Port, Username, Password, "T_MACHINE", Key),
%% 		  		io:format("tuxedo_monitornew tuxedoGetParam:~p~n",[T_MACHINE])		  	
%% 		end ,
%% 		
%% 		case string:rstr(Value, "T_SERVER/") of
%% 			1 ->		
%% 			  T_MACHINE = platform:tuxedoGetParam(Server, Port, Username, Password, "T_SERVER/", Key),
%% 		  		io:format("tuxedo_monitornew tuxedoGetParam:~p~n",[T_MACHINE])		  	
%% 		end	,	
%% 
%% 		case string:rstr(Value, "T_MSG/") of
%% 			1 ->		
%% 			  T_MACHINE = platform:tuxedoGetParam(Server, Port, Username, Password, "T_MSG", Key),
%% 		  		io:format("tuxedo_monitornew tuxedoGetParam:~p~n",[T_MACHINE])		  	
%% 		end	,	
%% 
%% 		case string:rstr(Value, "T_SERVICE/") of
%% 			1 ->		
%% 			  T_MACHINE = platform:tuxedoGetParam(Server, Port, Username, Password, "T_SERVICE", Key),
%% 		  		io:format("tuxedo_monitornew tuxedoGetParam:~p~n",[T_MACHINE])		  	
%% 		end
%% 
%% 	end,
%% 	lists:foreach(F, Counters).				
%% 	io:format("tuxedo_monitornew Rets:~p~n",[Ret]),
%% 	Ret.

%% @spec  
%% Obj = term()
%% @doc  
get_TuxedoData([], Server, Port, UserName, UserPwd, TaskType, TaskParam, Ret) ->
	Ret;
get_TuxedoData([H|T], Server, Port, UserName, UserPwd, TaskType, TaskParam, Ret) ->
  	{Key, Value} = H,
	io:format("tuxedo_monitornew get_TuxedoData Key:~p~n",[Key]),
	io:format("tuxedo_monitornew get_TuxedoData Value:~p~n",[Value]),
	case string:rstr(Value, "T_MACHINE/") of
		1 ->		
		  T_MACHINE = platform:tuxedoGetParam(Server, Port, UserName, UserPwd, "T_MACHINE", Key),
		  io:format("tuxedo_monitornew tuxedoGetParam:~p~n",[T_MACHINE]),
		  get_TuxedoData(T, Server, Port, UserName, UserPwd, "T_MACHINE", TaskParam, Ret ++ T_MACHINE)
	end,

	case string:rstr(Value, "T_SERVER/") of
		1 ->
		  T_MACHINE = platform:tuxedoGetParam(Server, Port, UserName, UserPwd, "T_SERVER", Key),
		  io:format("tuxedo_monitornew get_TuxedoData2:~p~n",[T_MACHINE]),
		  get_TuxedoData(T, Server, Port, UserName, UserPwd, "T_SERVER", TaskParam, Ret ++ T_MACHINE)
	end,

	case string:rstr(Value, "T_MSG/") of
		1 ->
		  T_MACHINE = platform:tuxedoGetParam(Server, Port, UserName, UserPwd, "T_MSG", Key),
		  io:format("tuxedo_monitornew tuxedoGetParam:~p~n",[T_MACHINE]),
		  get_TuxedoData(T, Server, Port, UserName, UserPwd, "T_MSG", TaskParam, Ret ++ T_MACHINE)
	end,
	
	case string:rstr(Value, "T_CLIENT/") of
		1 ->
		  T_MACHINE = platform:tuxedoGetParam(Server, Port, UserName, UserPwd, "T_CLIENT", Key),
		  io:format("tuxedo_monitornew tuxedoGetParam:~p~n",[T_MACHINE]),
		  get_TuxedoData(T, Server, Port, UserName, UserPwd, "T_CLIENT", TaskParam, Ret ++ T_MACHINE)
	end,
	
	case string:rstr(Value, "T_SERVICE/") of
		1 ->			
		  T_MACHINE = platform:tuxedoGetParam(Server, Port, UserName, UserPwd, "T_SERVICE", Key),
		  io:format("tuxedo_monitornew tuxedoGetParam:~p~n",[T_MACHINE]),
		  get_TuxedoData(T, Server, Port, UserName, UserPwd, "T_SERVICE", TaskParam, Ret ++ T_MACHINE)
	end,
	
	case string:rstr(Value, "T_WSH/") of
		1 ->
		  T_MACHINE = platform:tuxedoGetParam(Server, Port, UserName, UserPwd, "T_WSH", Key),
		  io:format("tuxedo_monitornew tuxedoGetParam:~p~n",[T_MACHINE]),
		  get_TuxedoData(T, Server, Port, UserName, UserPwd, "T_WSH", TaskParam, Ret ++ T_MACHINE)
	end,
	
	case string:rstr(Value, "T_QUEUE/") of
		1 ->
		  T_MACHINE = platform:tuxedoGetParam(Server, Port, UserName, UserPwd, "T_QUEUE", Key),
		  io:format("tuxedo_monitornew tuxedoGetParam:~p~n",[T_MACHINE]),
		  get_TuxedoData(T, Server, Port, UserName, UserPwd, "T_QUEUE", TaskParam, Ret ++ T_MACHINE)
	end,	
	get_TuxedoData(T, Server, Port, UserName, UserPwd, TaskType, TaskParam, Ret).

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
			THIS:set_attribute(countersInError, THIS:getMaxCounters()),
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

%% %% @spec getCostInLicensePoints() -> Result
%% %% Result = integer()
%% %% @doc get counters of selected for calculate license points
%% getCostInLicensePoints() ->
%%     I = THIS:getActiveCounters(THIS),
%%     1*I.

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
		case proplists:get_value(server, Params) of
			"" ->
				[{server, "server missing."}];
			Server ->
				case str_utils:contain_spaces(Server) of
					false ->
						[];
					true ->
						[{server, "Server no spaces are allowed."}]
				end
		end ++
			case proplists:get_value(port, Params) of
				"" ->
					[{port, "port missing."}];
				Port ->
				case str_utils:contain_spaces(Port) of
					false ->
						[];
					true ->
						[{port, "Port no spaces are allowed."}]
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