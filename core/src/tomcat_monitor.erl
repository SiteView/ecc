%% Author: Administrator
%% Created: 2010-10-21
%% Description: TODO: Add description to jmx_monitor
-module(tomcat_monitor,[BASE]).
-compile(export_all).
-extends(browsable_base).

-include("monitor.hrl").
-include("monitor_template.hrl").
-define(MAX_COUNTER,20).
-define(TIMEOUT,120*1000).
-define(REG_NAME, java_mail_box).
%% @spec new() -> Obj
%% @type Obj = term()
%% @r
new()->
	Base = browsable_base:new(),
	{?MODULE,Base}.

%% @spec defaultTitle(Params) -> List
%% @type Params = [term()]
%% @type List = string()
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	BASE:defaultTitle(Params) ++ ":" ++ proplists:get_value(server,Params).
%% @spec get_args() -> List
%% @type List = [term()]
%% @doc get_args is the function to get arguments to be sent to java node needed.
get_args() ->
	{ok, {_, HostName}} = THIS:get_property(server),
	{ok, {_, PortNumber}} = THIS:get_property(port),
	{ok, {_, Username}} = THIS:get_property(username),
	{ok, {_, Password}} = THIS:get_property(password),
	Counters=buildcounters(get_counters()),
	[
	 {server, HostName},
 	 {port, PortNumber},
	 {userName, Username},
	 {password, Password},
	 {counters,Counters}
	
	].
buildcounters([])->
	[];
buildcounters([H|E])->
	{K,V}=H,
	[{K,iconv:convert("utf-8","gbk",V)}]++buildcounters(E).
getCounterSize()->
	{ok, {_, Browse}} = THIS:get_property(browse),
	Len = length(Browse).
update()->	
	  case getCounterSize() > ?MAX_COUNTER of
		   true->
		    THIS:set_attribute(?NO_DATA,true),
		    THIS:set_attribute(?CATEGORY,?NO_DATA),
		    THIS:set_attribute(countersInError,?MAX_COUNTER),
		    THIS:set_attribute(?STATE_STRING,lists:flatten(io_lib:format("the counters > MAX_COUNTER:~p",[?MAX_COUNTER])));
		  _ ->
			  Request = get_args(),
			  Java_Node = siteview:get_java_node(),
   Response= try			  
      java_node:send("com.dragonflow.erlangecc.monitor.TomcatMonitor","update",Request,?TIMEOUT) of
		{ok,_,Ret}->
			%%get the counter value
			Ret;
		{error,_,[Err]}->
			%%some internal error hanppen in java node
			[{"error",Err}];
		{Ret, _, _}->
			%%get counter value error
			[{"error", atom_to_list(Ret)}];	
		_->
			[{"error","java node return a unknow error."}]
	catch _:X ->		 
			 [{"error","Connect Java Node Error! "}]
	      end,
      updateValues(Response)
	  end,
	
	ok.
updateValues([])->
	ok;
updateValues([H|T])->
	case H of
		{"error", Error} ->
			THIS:set_attribute(countersInError, THIS:getCounterSize()),
			THIS:set_attribute(?STATE_STRING, Error),
			stop;
		{"stateString", State_String} ->
			ConvertString=convert1(State_String),
			THIS:set_attribute(?STATE_STRING, util:replace(ConvertString,",","<br>") ),
			updateValues(T);
		{"countersInError", Counter_Errors} ->
			THIS:set_attribute(countersInError, Counter_Errors),
			updateValues(T);
%% 		{counters_value, Counters_Value} ->
%%   	set_counter_value(Counters_Value),
%% 			updateValues(T);
		{_,_}->
			THIS:setCounterValue(H),
			updateValues(T);
		_->
			updateValues(T)
	end.
setCounterValue({Key,Value})->
	THIS:set_attribute(convert1(Key),Value).
%% rpc(RegName, Node, Msg, Timeout) ->
%% 	Ping = net_adm:ping(Node),
%%     if
%%         Ping==pang ->
%%             {error,"Connect Java Node Error! "};
%%         true ->
%%             {RegName, Node} ! Msg,	
%%             receive
%%                 {ok, _, Ret} ->	
%%                     Ret;
%%                 {error, _From, Ret} ->
%%                     {error,Ret}		
%%             after Timeout ->
%%                 {error, "time is out. "}
%%             end
%%     end.
%% @spec getBrowseData(Params) -> Result
%% @type Param  = [term()]
%% @type Result = [{CounterValue, CounterName}]
%% @doc getBrowseData(Params) is the function called by schedule to get counters of Weblogic6x server
%%	CounterValue is the Counter's OID, which will be used to get value
%%	CounterName is the show name of the counter
getBrowseData(Params)->
	%%get argument	
     HostName = proplists:get_value(server, Params),
	 PortNumber = proplists:get_value(port, Params),
	 Username = proplists:get_value(username, Params),
	 Password = proplists:get_value(password, Params),
	Request = [
	 {server, HostName},
 	 {port, PortNumber},
	 {userName, Username},
	 {password, Password}
	],	
	%%send the request to java node
	case java_node:send("com.dragonflow.erlangecc.monitor.TomcatMonitor","getBrowseData",Request,?TIMEOUT) of
		{ok,_,Ret1}->
			%%get the counter value
			Ret= utftogbk(Ret1),
			qsort(Ret);
		{error,Err}->
			%%some internal error hanppen in java node
			{error,Err};
		{Ret, _, _}->
			%%get counter value error
			{error, atom_to_list(Ret)};	
		_->
			{error,"java node return a unknow error."}
	end.

utftogbk([])->
	[];
utftogbk([H|E])->
	{K,V}=H,
	[{K, convert(V)}]++utftogbk(E).



%%get the result, parse it now
process_result([],_)->"";
process_result([C|T],Result)->
	%%look for counters information
	case lists:keysearch(element(1,C),1,Result) of
		{value,{K,V}}->
			%%some special thing must to do for float to show correctly
			VV = 
			if
				is_float(V) ->
					erlang:round(V*100)/100;
				true ->
					V
			end,
			%%set the counter atrribute
			THIS:set_attribute(K,VV),
			%%set the show string
			lists:flatten(io_lib:format("~p=~p<br>",[element(2,C),VV])) ++ 
				process_result(T,Result);
		_->
			THIS:set_attribute(element(1,C),"n/a"),
			THIS:inc_attribute(countersInError),
			lists:flatten(io_lib:format("~p=n/a<br>",[element(2,C)])) ++ 
				process_result(T,Result)
	end.


%% @spec verify(Params) -> {ok, []} | {error, Reason}
%% @type Params = [term()]
%% @type Reason = string()
%% @doc verify is the function called by schedule to verify the parameter of monitor inputed by user
%%	ipmi monitor verify timeout, hostname and port
verify(Params)->
    Errs = 
	case proplists:get_value(port,Params) of
    ""->
	    [{port,"port missing."}];
    Port->
		if
			not is_number(Port) ->
				[{port,"Port must be a number."}];
			true->
				[]
		end
	end ++
	case proplists:get_value(server,Params) of
    ""->
		[{server,"Host Name missing"}];
    Host->
	    case string:rstr(Host," ") of
		    0->
			    [];
			_->
			    [{server, "no spaces are allowed"}]
	    end
	end ++
	case BASE:verify(Params) of
		{error,E}->
			E;
		_->
			[]
	end,	

	if 
		length(Errs)>0 ->
			{error,Errs};
		true ->
			{ok,""}
	end.
	
getMaxCounter()->
	?MAX_COUNTER.

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
convert1([H|R], Acc)->
	case H > 255 of
		true->
			<<H1:8, H2:8>> = <<H:16>>,
			List = iconv:convert("utf-32", "utf-8", [0, 0, H1, H2]),
			convert1(R, lists:reverse(List) ++ Acc);
		_ ->
			convert1(R, [H|Acc])
	end;
convert1([], Acc)->
  	lists:reverse(Acc).
%%  	iconv:convert("utf-8", "gbk", lists:reverse(Acc)).

convert1(Msg)->
	convert1(Msg, []).
convert([H|R], Acc)->
	case H > 255 of
		true->
			<<H1:8, H2:8>> = <<H:16>>,
			List = iconv:convert("utf-32", "utf-8", [0, 0, H1, H2]),
			convert(R, lists:reverse(List) ++ Acc);
		_ ->
			convert(R, [H|Acc])
	end;
convert([], Acc)->
	lists:reverse(Acc).
%% 	iconv:convert("utf-8", "gbk", lists:reverse(Acc)).

convert(Msg)->
	convert(Msg, []).
getHostname()->
	case THIS:get_property(server) of
		{ok,{_,V}}->
			V;
		_->
			BASE:getHostname()
	end.
qsort([]) -> [];
qsort([Pivot|T]) ->
	qsort([X || X <- T, element(2,X) < element(2, Pivot)])
	++ [Pivot] ++
	qsort([X || X <- T, element(2,X) >= element(2,Pivot)]).	
%% @spec get_template_property() -> List
%% @type List = [term()]
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
%% 1.elements show on gui interface
%%
get_template_property()->
	BASE:get_template_property() ++ 
	[
	 #property{name=server,title="Server",type=text,description="the name of the server",order=1},
	 #property{name=port,title="Port Number",type=numeric,description="the port number of Tomcat server",order=2},
	 #property{name=username,title="User Name",type=text,description="the username for login to Tomcat server",order=3},
	 #property{name=password,title="Password",type=password,description="the password for login to Tomcat server",order=4}
	
	].