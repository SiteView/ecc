-module (base_monitor).
-compile(export_all).
-include("../../include/object.hrl").
-include("../../include/monitor.hrl").
-include("../../include/monitor_template.hrl").
-include("../../include/classifierstring.hrl").
-include_lib("../include/erlv8.hrl"). 
%
% monitor life cycle management:
%% 0. ping_monitor:start: initializing
%% 1. base_monitor:action(Self,waiting): triggerred by frequency base_monitor:request_resource_action()
%% 2. waiting-for-resource:
%% 3. running:
%% 4. base_monitor:logging
%% 5. waiting
%% waiting, disable, waiting
%% waiting, disable
%
%% TODO: add period based frequency
%% TODO: schedule optimization: grouping and seperating
%% 		grouping: using the same resource e.g. SSH coonection can be grouped togather to re-use the connection before close the connection 
%% 		seperating: seperating the monitors to reduce the parallel execution
 
extends () -> nil .

?PATTERN(resource_allocated_pattern)-> {?VALUE(name), get, {'_',resource_allocated}}; %%triggerred in resource_pool:do_request
?PATTERN(wakeup_pattern)-> {?VALUE(name), get, {wakeup}};
?PATTERN(logging_pattern)-> {?VALUE(name), get, {'_',logging}};
?PATTERN(refresh_pattern)-> {?VALUE(name), get, {refresh}};
?PATTERN(enable)-> [{?VALUE(name), get, {enable}},{?VALUE(name), get, {enable,fun(Time)-> Time >= 0 end}}];
?PATTERN(disable_pattern)-> {?VALUE(name), get, {disable,'_'}};
?PATTERN(frequency_pattern) -> ?VALUE(?FREQUENCY)*1000;
?PATTERN(disable_time) -> ?VALUE(disable_time)*1000.
%% ?PATTERN(disable_pattern)-> [{?VALUE(name), get, {disable}},{?VALUE(name), get, {disable,fun(Time)-> Time >= 0 end}}];

?EVENT(wakeup_event)-> {eresye,wakeup_pattern};
?EVENT(disable_event)-> {eresye,disable_pattern};
?EVENT(enable_event)-> {eresye,enable};
?EVENT(frequency_event) -> {timeout,frequency_pattern};
?EVENT(resource_allocated_event)-> {eresye,resource_allocated_pattern};
?EVENT(logging_event)-> {eresye,logging_pattern};
?EVENT(refresh_event)-> {eresye,refresh_pattern};
?EVENT(timed_enable_event) -> {timeout,disable_time}.

?ACTION(start) -> [{wakeup_event,init_action}];
?ACTION(disabled) -> [{timed_enable_event,enable_action},{enable_event, enable_action}];
?ACTION(logging) -> {logging_event, logging_action};
?ACTION(waiting) -> [
					{disable_event,disable_action},
					{refresh_event,request_refresh_resource_action},
					{frequency_event,request_resource_action}
					];
?ACTION(running) -> {frequency_event,request_resource_action};
?ACTION(waiting_for_resource) -> [{resource_allocated_event,update_action}].
init_action(Self,EventType,Pattern,State) ->
	io:format ( "[~w]:Type=~w,Action=init,State=~w,Event=~w,Pattern=~w\n",	[?VALUE(name),?MODULE,State,EventType,Pattern]),
	object:do(Self,waiting).

update_action(Self,EventType,Pattern,State) ->
	io:format ( "[~w]:Type=~w,Action=update,State=~w,Event=~w,Pattern=~w\n",	[?VALUE(name),?MODULE,State,EventType,Pattern]),
	object:do(Self,waiting).

%%@doc the constructor
base_monitor (Self,Name) ->
	?SETVALUE(?NAME,monitor),
	?SETVALUE(?FREQUENCY,5),
	?SETVALUE(?LASTUPDATE,0),
	?SETVALUE(disable_time,0),
	?SETVALUE(?MEASUREMENTTIME,0),
	?SETVALUE(wait_time,0),
	?SETVALUE(?DISABLED,false),
	?SETVALUE(?VERFIY_ERROR,true),
	?SETVALUE(?ERROR_FREQUENCY,60),
	?SETVALUE(?DEPENDS_ON,none),
	?SETVALUE(?DEPENDS_CONDITION,error),
    ?SETVALUE(?LAST_CATEGORY,nodata),
	?SETVALUE(?CATEGORY,nodata),
	?SETVALUE(?STATE_STRING,""),
	?SETVALUE(?RULES,[]),	
	?SETVALUE(name,Name),
	?SETVALUE(data_logger,"localhost"),  %%the data logger used in log_action
	eresye:start(Name). %%TODO: need evaluate whether start a rule engine for each monitor or one rule engine for all monitor ?

%%@doc the destructor
base_monitor_(Self)-> 
	eresye:stop(?VALUE(name)).

disable_action(Self,EventType,Pattern,State) ->
	io:format ( "[~w:~w]Action: disable, [State]:~w, [Event type]:~w, [Pattern]: ~w '\n",	[?MODULE,?LINE,State,EventType,Pattern]),
	object:do(Self,disabled).

%%@doc request resource from the resource pool, once got the resource, then run
%%    or put the resource request into a queue
request_resource_action(Self,EventType,Pattern,State) ->
	%%TODO: insert schedule processing here.
	
%% 	io:format ( "[~w:~w] ~w-1 Counter=~w, Action: request_resource_action, State=~w, Event type=~w, Pattern=~w '\n",	[?MODULE,?LINE,?VALUE(name),resource_pool:get_counter(?VALUE(name)),State,EventType,Pattern]),
	{Mega,Sec,MilliSec} = erlang:now(),
%% 	TODO: using PID as session, can be used to communicate with the update action
%% 	Session = self(),
	Session = Mega+Sec+MilliSec,
	resource_pool:request(?VALUE(name), Session,frequency_request),
	object:do(Self,waiting_for_resource).

request_refresh_resource_action(Self,EventType,Pattern,State) ->
%% 	io:format ( "Action: request_refresh_resource, [State]:~w, [Event type]:~w, [Pattern]: ~w '\n",	[State,EventType,Pattern]),
	resource_pool:request(?VALUE(name), refresh),
	object:do(Self,waiting_for_resource).

enable_action(Self,EventType,Pattern,State) ->
	io:format ( "Action: enable, [State]:~w, [Event type]:~w, [Pattern]: ~w '\n",	[State,EventType,Pattern]),
	object:do(Self,waiting).

%%@doc post run processing, e.g. releasing resources and changing the state 
post_run(Self) -> 
	resource_pool:release(?VALUE(name)), 	
	eresye:assert(?VALUE(name), {logging}),
	object:do(?VALUE(name),waiting).
%% 	object:do(?VALUE(name),logging).

%%@doc logging the measurement into database by inform the data_logger to pull the data from monitor object
logging_action(Self,EventType,Pattern,State) -> 
	{Session,_} = Pattern,
	resource_pool:release(?VALUE(name), Session),
	eresye:assert(?LOGNAME, {?VALUE(name),Session,erlang:now(),log}),	
	%TODO: logging data to database
	timer:sleep(random:uniform(2)*1000),  % simulate the logging time
%% 	io:format ( "[~w:~w] Action: logging, [State]:~w, [Event type]:~w, [Pattern]: ~w '\n",	[?MODULE,?LINE,State,EventType,Pattern]),
	object:do(Self,waiting).

allocate_resource(Self) ->
	eresye:assert(?VALUE(name),{resource_allocated}).

%%@doc get the measurement now, not by frequency
refresh(Self) ->
	eresye:assert(?VALUE(name), {refresh}).

on_starting(Self) ->
	io:format("This [~w] ~w object is starting \n",[?VALUE(name),?MODULE]).

on_stopping(Self) ->
	io:format("This [~w] ~w object is stopping \n",[?VALUE(name),?MODULE]).

%%@doc execute the js, set the ?CATEGORY value based on the result of executing the js
%% 		
%% 
%% 
%% 
runClassifiersjohn(Self, This)->
	{ok, VM} = erlv8_vm:start(),
	io:format("---------------runClassifiers VM------:~p~n", [VM]),
	Global = erlv8_vm:global(VM),
	Global:set_value("classifier",erlv8_object:new([{"error",?VALUE(error_classifier)}] )),
%% 	Global:set_value("classifier",erlv8_object:new([{"major",?VALUE(major_classifier)}] )),
	Global:set_value("classifier",erlv8_object:new([{"warning",?VALUE(warning_classifier)}] )),
%% 	Global:set_value("classifier",erlv8_object:new([{"minor",?VALUE(minor_classifier)}] )),
	Global:set_value("classifier",erlv8_object:new([{"ok",?VALUE(ok_classifier)}] )),
	{ok,Error} = erlv8_vm:run(VM,"classifier.error()"),
%% 	{ok,Major} = erlv8_vm:run(VM,"classifier.major()"),
	{ok,Warning} = erlv8_vm:run(VM,"classifier.warning()"),
%% 	{ok,Minor} = erlv8_vm:run(VM,"classifier.minor()"),
	{ok,OK} = erlv8_vm:run(VM,"classifier.ok()"),
	if Error -> ?SETVALUE(?CATEGORY,error);
%% 	   Major -> ?SETVALUE(?CATEGORY,major);
	   Warning -> ?SETVALUE(?CATEGORY,warning);
%% 	   Minor -> ?SETVALUE(?CATEGORY,minor);
	   OK -> ?SETVALUE(?CATEGORY,ok);
	   true -> un_classified
	end,
	ok.

set_classifier(Self, []) ->
	ok;
set_classifier(Self, [{Type,Classifier}|T]) -> 
	?SETVALUE(Type,Classifier),
	set_classifier(Self, T).

runClassifiersJs(Self, This)->
	{ok, VM} = erlv8_vm:start(),
	Global = erlv8_vm:global(VM),
	io:format("---------------runClassifiers erlv8_vm------:~p~n", [VM]),
	
%% 	Global:set_value("classifier", erlv8_object:new([{"obj_value", 
%% 	  fun (#erlv8_fun_invocation{}, [String]) -> ?VALUE(erlang:binary_to_list(String)) end}])),
%% 	{ok,Percent} = erlv8_vm:run(VM,"classifier.obj_value('percent')"),
	
	Global:set_value("Value", 
		 fun (#erlv8_fun_invocation{}, [String]) -> ?VALUE(erlang:binary_to_list(String)) end),
	{ok,Percent} = erlv8_vm:run(VM,"Value('percent')"),

	io:format("---------------runClassifiers Percent------:~p~n", [Percent]),
	
%% 	io:format("---------------runClassifiers error_classifier------:~p~n", [?VALUE("error_classifier")]),
%% 	Global:set_value("classifier",erlv8_object:new([{"error",?VALUE("error_classifier")}] )),	
%% 	Global:set_value("classifier",erlv8_object:new([{"warning",?VALUE("warning_classifier")}])),	
%% 	Global:set_value("classifier",erlv8_object:new([{"ok",?VALUE("ok_classifier")}] )),	
%% 	io:format("---------------runClassifiers classifier------:~p~n", [Global:proplist()]),
		
%% 	{ok,Error} = erlv8_vm:run(VM,"classifier.error"),
%% 	{ok,Warning} = erlv8_vm:run(VM,"classifier.warning"),
%% 	{ok,Ok} = erlv8_vm:run(VM,"classifier.ok"),

	{ok,Error} = erlv8_vm:run(VM,?VALUE(error_classifier)),
	{ok,Warning} = erlv8_vm:run(VM,?VALUE(warning_classifier)),
	{ok,Ok} = erlv8_vm:run(VM,?VALUE(ok_classifier)),

	io:format("---------------runClassifiers Ok: ~p , Warning ~p,  Error: ~p~n", [Ok, Warning, Error]),
	ok.

start(Name) ->
	case object:get_by_name(Name) of
		[] -> 
				X = object:new(?MODULE,[Name]),
				object:start(X),
				object:add_fact(Name,{wakeup}),
				X;
		_ -> atom_to_list(Name) ++ " not available, choose a new name"
	end.