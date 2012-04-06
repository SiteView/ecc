%% 
%% @doc ping agent module
%% to start the monitor schedule: eresye:assert(ping_agent_mind,{wakeup}).
%% @version{1.0}
%% @copyright 2012 dragonflow.com
%% 

-module (ping_agent).
-compile(export_all).
-include("../../include/monitor_template.hrl").
-include("../../include/monitor.hrl").
-include("../../include/object.hrl").

extends () -> atomic_agent.

?ACTION(disabled) -> [{timed_enable_event,enable},{enable_event, enable}];
?ACTION(running) -> {triggering_event, logging};
?ACTION(waiting) -> [{disable_event,disable},
						{frequency_event,run}].

?EVENT(temperature_event)-> {eresye,temperature};
?EVENT(wakeup_event)-> {eresye,wakeup};
?EVENT(disable_event)-> {eresye,disable};
?EVENT(frequency_event) -> {timeout,frequency};
?EVENT(timed_enable_event) -> {timeout,3000}.
%% ?EVENT(timed_enable_event) -> {timeout,disable_time}.


?PATTERN(wakeup)-> {?MINDVALUE, get, {wakeup}};
?PATTERN(disable)-> {?MINDVALUE, get, {disable,fun(Time)-> Time >= 0 end}};
?PATTERN(temperature) -> {?MINDVALUE, get, {temperature,fun(X)-> X>10 end,'C'} }.

alarm(Self,EventType,Pattern,State) ->
	io:format ( "Action: alarm, [State]:~w, [Event type]:~w, [Pattern]: ~w '\n",	[State,EventType,Pattern]),
	object:do(Self,waiting).

disable(Self,EventType,Pattern,State) ->
	io:format ( "Action: disable, [State]:~w, [Event type]:~w, [Pattern]: ~w '\n",	[State,EventType,Pattern]),
	object:do(Self,disabled).

enable(Self,EventType,Pattern,State) ->
	io:format ( "Action: enable, [State]:~w, [Event type]:~w, [Pattern]: ~w '\n",	[State,EventType,Pattern]),
	object:do(Self,waiting).

ping_agent(Self,Mind) ->
	object:set(Self,'hostname',object:new('property',[#property_table{name=hostname,value='localhost',title="Host Name",type=text,order=1,description="Monitoring of the host name"}])),
	object:set(Self,'timeout',object:new('property',[#property_table{name=timeout,title="Timeout",value=5000,type=numeric,advance=true,default= 3000,order=1,description="the time out, in seconds, to wait for the response",baselinable=true}])),
	object:set(Self,'size',object:new('property',[#property_table{name=size,title="Size",value=100,type=numeric,advance=true,order=2,default=32,description="Packet size"}])),
	object:set(Self,'packetsgood',object:new('property',[#property_table{name=packetsgood,title="% packets good%",type=numeric,order=4,configurable=false,state=true,upIsBad=false,baselinable=true}])),
	object:set(Self,'round_trip_time',object:new('property',[#property_table{name=round_trip_time,title="round trip time(milliseconds)",type=numeric,order=5,configurable=false,state=true,baselinable=true}])),
%% 	io:format("ping agent:~w,~w~n", [Self,Mind]),
	object:super(Self,[Mind]).  %adding [] (not Mind) is very important
	

ping_agent_(Self)->eresye:delete(?MINDVALUE).


logging(Self) -> 
	%TODO: logging data to database
	object:do(Self,waiting).

%% @spec run() -> Result
%% Result = term()
%% @doc update is the run function called by schedule to perform the measurement operation, taking the config properties as input and write to log properties
run(Self,EventType,Pattern,State) ->
%% 	io:format ( "[State]:~w, [Event type]:~w, [Pattern]: ~w '\n",	[State,EventType,Pattern]),
  	Start = erlang:now(),
	object:do(Self,running),
	io:format("[~w] Running: Hostname:~w, Timeout:~w, Size:~w\n", [?MINDVALUE,?VALUE(hostname),?VALUE(timeout),?VALUE(size)]),
%% 	Cmd = "ping -n 4 -l " ++ object:get(Size,'value') ++ " -w " ++ object:get(Timeout,'value') ++ "  " ++ object:get(Hostname,'value'),

%% 	simulated random data
	?SETVALUE(round_trip_time,100 * random:uniform(10)),
	?SETVALUE(packetsgood,random:uniform(4)),
	
	Diff = timer:now_diff(erlang:now(), Start)/1000,
	?SETVALUE(?MEASUREMENTTIME,Diff),
	?SETVALUE(?LASTUPDATE,erlang:now()),
	
%%  io:format("[~w] finish in ~w ms, return: RoundTripTime:~w, PacketsGood:~w\n", [?MODULE,Diff,object:get(RoundTripTime,'value'),object:get(PacketsGood,'value')]),
	object:do(Self,waiting).

%% @spec verify(Params) -> {ok, []} | {error, Reason}
%% Params = [term()]
%% Reason = string()
%% @doc verify is the function called by schedule to verify the parameter of monitor inputed by user
verify(Self,Params)->
	object:do(Self,waiting).

%% @spec defaultTitle(Params) ->string()
%%  Params = [term()]
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Self,Params)->
	object:do(Self,waiting).

%% @spec get_all_properties() ->list()
%% @doc get_all_properties return the properties for a monitor, including both config and log properties.
get_all_properties(Self) ->
	object:get(Self,'Properties').

%% @spec get_log_propertues() ->list()
%% @doc get_log_properties return the log properties for a monitor, including only log properties: both static state properties and dynamic(browseable) counter properties.
get_log_properties(Self) ->
	object:get(Self,'LogProperties').

%% @spec get_state_properties() ->list()
%% @doc get_state_properties return the static state properties for a monitor, including only static state properties, no dynamic(browseable) counter properties.
get_state_properties(Self) ->
	object:get(Self,'LogProperties').

%% @spec get_default_state_property() ->list()
%% @doc get_default_state_property return the static default state property for a monitor.
get_primary_state_property(Self) ->
	object:get(Self,'LogProperties').


set_template_property(Self,'Properties',PropList) ->
	object:set(Self,'Properties', PropList).

%% @spec start(Params) -> {Self}
%% Params = [term()], the name of the engine ("the mind")
%% Reason = string()
%% @doc start is the function called to create and wakeup the agent.
start(Mind) ->
	X = object:new(ping_agent,[Mind]),
	object:start(X),
%% 	eresye:add_rule(Mind,{?MODULE,check}),
	
	eresye:assert(Mind,{wakeup}),
	X.