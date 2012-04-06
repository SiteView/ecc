-module(filtereventlog_monitor,[BASE]).
-extends(server_monitor).
-compile(export_all).

-include("monitor.hrl").
-include("monitor_template.hrl").
-define(SNAME, "wmi").

new()->
	Base = server_monitor:new(),
	{?MODULE,Base}.

get_wmi_node()->
	%%io:format("---------------------- server_conf:~p",[server_conf:getWmiNode()]),
	case server_conf:getWmiNode() of
		undefined->
			%%io:format("---------------------- undefined"),
			{ok, Host} = inet:gethostname(),
			%%io:format("---------------------- Host: ~p",[Host]),
			list_to_atom( ?SNAME ++ "@" ++ Host);
		Node->
			%%io:format("----------------------not undefined ~p",[Node]),
			Node
	end.
	
getmhost(Host)->
	string:strip(Host, left, $\\).

update()->
	{ok,{_,Machine}}=THIS:get_property(machine),
	{ok,{_,EventType}}=THIS:get_property(eventtype),
	{ok,{_,EventId}}=THIS:get_property(eventid),
	TID =	case EventType of
				"error" -> 1;
				"warning" -> 2;
				"good" -> 3
			end,
	%%io:format("----------------------Machine:~p~n",[Machine]),
	case Machine of
		""->
			Host="127.0.0.1",
			User=" ",
			Passwd=" ",
	         THIS:getCounterValues(Host,User,Passwd);  
		_->
			case machine:getNTMachine(Machine) of
				[]->
					io:format("----------------------Machine is null"),
					THIS:set_attribute(?NO_DATA,true),
					THIS:set_attribute("status","error"),
					THIS:set_attribute(status,"error"),
				    THIS:set_attribute(?CATEGORY,?NO_DATA),
					THIS:set_attribute(?STATE_STRING,"connect target error!"),
					[];
				[M|_]->
					io:format("----------------------Machine is not null:~p~n",[M]),
                    THIS:getCounterValues(getmhost(M#machine.host),M#machine.login,M#machine.passwd, TID, EventId)
			end
  end.

filterCount(R) -> 
	case R of
		{_,[{_,{_,empty}}]} -> 0;
		{_,[{_,{_,RList}}]} -> length(RList);
		_ -> 0
	end.	

getCounterValues(Host,User,Passwd, EventType, EventID) ->
	io:format("----------------------Host:~p User:~p Passwd:~p EventType:~p EventID:~p",[Host,User,Passwd, EventType, EventID]),
	{YY, MM, DD} = date(),
	DATE = integer_to_list(YY) ++ integer_to_list(MM) ++ integer_to_list(DD) ++ "000000.000000+480",
	TWql = "SELECT Category FROM Win32_NTLogEvent where Logfile='System' and EventType=" ++ integer_to_list(EventType) ++ 
	" and TimeWritten>='"++ DATE ++ "'",
	IWql = "SELECT Category FROM Win32_NTLogEvent where Logfile='System' and EventType=" ++ integer_to_list(EventType) ++ 
	" and TimeWritten>='"++ DATE ++ "' and EventCode=" ++ integer_to_list(EventID),

	TT = filterCount(rpc:call(THIS:get_wmi_node(), wmic, wmic, [Host,User,Passwd,TWql])),
	II = filterCount(rpc:call(THIS:get_wmi_node(), wmic, wmic, [Host,User,Passwd,IWql])),
	%%io:format("----------------------TT: ~p  II: ~p ",[TT,II]),
	THIS:set_attribute(?STATE_STRING,"Total: " ++ integer_to_list(TT) ++ " Port: " ++ integer_to_list(II)).
	
	
get_classifier(error)->
  case THIS:get_property(error_classifier) of
	{ok,{error_classifier,Classifier}}->
		Classifier;
	_->
		[{total,'!=',0}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{total,'!=',0}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{total,'==',0}]
	end.

get_template_property()->
	BASE:get_template_property() ++
	[
		#property{name=eventtype,title="Event Type",type=scalar,default={"error","error"},order=1,description="The Event Type (e.g error,worning,info)"},
		#property{name=eventid,title="Event ID",type=numeric,default=1,order=2,description="The Event ID,please find it in System log"},
		#property{name=total,title="Total",type=numeric,state=true,configurable=false},
		#property{name=intotal,title="Filter total",type=numeric,state=true,configurable=false}
	].
	
getScalarValues(Prop,Params)->
	case Prop of		
		eventtype ->
			[
			 {"Error", "error"},
			 {"Warning", "warning"},
			 {"good", "good"}
			];
		_ ->
			BASE:getScalarValues(Prop, Params)
	end.
	
%% @spec defaultTitle(Params) ->string()
%% Params = [term()]
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	Host = proplists:get_value(machine,Params),
	if
		length(Host)>0->
			BASE:defaultTitle(Params) ++":" ++ Host;
		true ->
			BASE:defaultTitle(Params)
	end.