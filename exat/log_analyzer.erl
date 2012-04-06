%% @doc  the analysing engine to process the logs
%%  it can be used to analyze the measurement logs and can also be used to analyze the 
%% 	internal state transition and monitoring health condition.  The log are asserted into the engine as facts
%% rules can be set to take action for the facts.
%%  fields: name, state, timestampe, sessions
%%  rules: if session complete, delete,
%%  actions: delete log, alert, corrective action
%% 
%%  monitor state machine:
%%  get the exago recognize exat's facts
%% logic to verify the counter: in running state, finished
%% 
%% TODO: 
%% 	1. checking for long running monitors,
%% 	2. failed monitor restarting
%%  3. check for the value update time with frequency
%% the name of new measurement will be save here until picked up and removed by the data base logger.

-module (log_analyzer).
-compile ([export_all]).

-include("object.hrl").
-include("../exago/exago_state_machine.hrl").

extends () -> nil .

%% ?PATTERN(monitor_timeout_pattern) -> {?LOGNAME,read,{'_', '_', fun(X)-> Diff = timer:now_diff(erlang:now(), X)/1000000, Diff > 100 end, running,'_','_'}};
?PATTERN(finished_pattern) -> {?LOGNAME, read, {'_','_','_',log}}. %% name, session, timestamp, state

?EVENT(finished_event)-> {eresye,finished_pattern}.

?ACTION(start) -> {finished_event,finished_action}.
%%TODO: check for the long running monitor

%% @doc check the completeness of monitoring sequence
finished_action(Self,EventType,Pattern,State) -> 
	{Name,Session,_,_} = Pattern,
	Pattern1 = {Name,Session,'_','_'},
	MonitorStateList = lists:keysort(3,eresye:query_kb(?LOGNAME, Pattern1)),  %%must be sorted based on timestamp to be right
%% 	io:format("[~w:~w] MonitorStateList=~w~n",[?MODULE,?LINE,MonitorStateList]),
	eresye:retract(?LOGNAME, MonitorStateList),
%% 	io:format("[~w:~w] MonitorStateList = ~w~n", [?MODULE,?LINE,lists:keysort(3,eresye:query_kb(?LOGNAME, Pattern1))]),
	%% TODO: check for counter and queue
	
	RowFormat = monitor_row_format(),
	MonitorStateTest = [
						 {ping1,1111,{1329,408188,548251},frequency},
						 {ping1,1111,{1329,408188,610250},allocate_resource},
						 {ping1,1111,{1329,408193,618252},update},
						 {ping1,1111,{1329,408195,692250},log}
						],
	
    EventSource  = exago_event:new_source("monitor_state_log", MonitorStateList, RowFormat),
%% 	io:format("[~w:~w] EventSource = ~w~n", [?MODULE,?LINE,EventSource]),
%%     EventSource = 
%% 	exago_event:new_source("monitor_state_log", MonitorStateList, RowFormat, 
%% 			       {?MODULE, monitor_input_filter},
%% 			       {?MODULE, monitor_input_modifier, 
%% 				[transition_input, "Counter", "QueueLen"]}),

    StateMachine = monitor_state_machine(),
%% 	io:format(EventSource),

    Result = exago_state_machine:analyse_event_source(EventSource, StateMachine),
	%% error alert based on the Result
	{result, SMResult, ExecutionAnalysis} = Result,
	{execution_analysis, {n_instances, N}, {history_analysis, HistoryAnalysis}} = ExecutionAnalysis,
	NofOk = exago_printer:count_acceptant_executions(HistoryAnalysis, 0) , 
%% 	io:format("[~w:~w] Success = ~w~n", [?MODULE,?LINE,HistoryAnalysis]),
	if NofOk < N 
		 -> 
		   	ResourceType = erlang:apply( object:getClass(Name), get_resource_type,[]),	
			Counter = length(eresye:query_kb(?POOLNAME, {ResourceType,'_','_','_'})),
			io:format("[~w:~w]!!! monitor [~w] NofOk/N=~w/~w,execution:~w, counter=~w~n", 
					  [?MODULE,?LINE,Name,NofOk,N,exago_printer:list_failing_executions(HistoryAnalysis, 0),Counter]),
			io:format("[~w:~w] MonitorStateList = ~w~n", [?MODULE,?LINE,MonitorStateList]),
			exago_printer:print_result(Result);
	   true -> 
%% 		   io:format("success monitor execution:~w~n", [exago_printer:list_acceptant_executions(HistoryAnalysis, 0)]),
		   ok
	end,
	
%% 	exago_printer:print_result(Result),

	Len = length(MonitorStateList),
	lists:foreach(
	  fun(MonitorState) -> 
			  {Name1,Session1,Timestamp,State1} = MonitorState
%% 			  io:format("[~w:~w] Name=~w,Session=~w,Timestamp=~w,Input=~w~n",
%% 						[?MODULE,?LINE, Name1,Session1,Timestamp,State1])
	  end, MonitorStateList),
ok.

%%@doc timeout monitoring
monitor_timeout_action (Self,EventType,Pattern,State) ->
	ok.  %%TODO

%% @doc The monitor state machine model
-spec(monitor_state_machine/0 :: () -> #state_machine{}).
monitor_state_machine() ->
    StateMachine =
	#state_machine{
      states= %%state, adding finish state
	  [#state{number=0, name=waiting},
	   #state{number=1, name=disabled},
	   #state{number=2, name=waiting_for_resource},
	   #state{number=3, name=running},
	   #state{number=4, name=logging},
	   #state{number=5, name=finished}],
      transitions= %%pattern
	  [#transition{from=0, to=2, input=frequency_request},
	   #transition{from=0, to=1, input=disable},
	   #transition{from=1, to=0, input=enable},
	   #transition{from=2, to=3, input=allocate_resource},
	   #transition{from=3, to=4, input=update},
	   #transition{from=4, to=5, input=log}],
      start=0,
      accept=[0,1,5]},
    StateMachine.

%% @doc Each log file should have a row format which specifies the required
%% fields (timestamp, transition_input and group_id), and any other
%% information. For example in the following row format, one row looks
%% like:  
%%     {"2010-10-12 16:50:03:0423338","38","close","1"} or
%%     {"2010-10-12 16:49:56:0753614","14","reset","2","closed","1"}
%%
%% As you can see the fields vary in size, and the input data is
%% reflected precisely in the definitions of the field parsers below.
%%
%% Annotations are useful especially in the input modifier, since they
%% can be used to modify the input in any way that you like.
%% As an example of this, the input modifier for this example uses
%% the "floor1" and "floor2" annotations to construct a transition
%% input.
-spec(monitor_row_format/0 :: () -> list()).
monitor_row_format() ->
    [
     exago_field:parser(annotation, "Name"),
     exago_field:parser(group_id),%% session
	 exago_field:parser(timestamp,noparse), %%time 
%% 	 exago_field:parser(timestamp, "yyyy-MM-dd hh:mm:ss:fffffff"), %%time 
     exago_field:parser(transition_input) %%state
%%      exago_field:parser(annotation, "Counter")
	].
%% name, session, timestamp, state,counter,queueLen

%% @doc In this example, we wish to modify the input to the transitions 
%% in the state machine, so that they better reflect the state machine 
%% model. This step is not necessary if your inputs already fit the model.
-spec(monitor_input_modifier/1 :: (list()) -> tuple()).
monitor_input_modifier(Fields) ->
    TransitionInput = proplists:get_value(transition_input, Fields),
    case TransitionInput of
	"reset"       ->
	    FloorN = proplists:get_value("floor2", Fields),
	    {transition_input, "reset_to_" ++ FloorN};
	"approaching" ->
	    FloorN = proplists:get_value("floor1", Fields),
	    {transition_input, "approaching_" ++ FloorN};
	"stopped_at"  ->
	    FloorN = proplists:get_value("floor1", Fields),
	    {transition_input, "stopped_at_" ++ FloorN};
	_             ->
	    {transition_input, TransitionInput}
    end.

%% @doc This is another optional function which is used to filter out
%% unwanted transition inputs. In this example we are only interested
%% in keeping the specified inputs (those that evaluate to true), and
%% so we return false to filter any unnecessary inputs. 
-spec(monitor_input_filter/1 :: (list()) -> true | false).
monitor_input_filter(Input) ->
    case Input of
	"frequency"       -> true;
	"disable"             -> true;
	"enable"            -> true;
	"resource_allocated" -> true;
	"update"  -> true;
	"logging"   -> true;
	_                  -> false
    end.

start()->
	Logger = object:new(?MODULE),
	object:start(Logger),
	eresye:start(?LOGNAME),
	Logger.

log_analyzer(Self)->
	?SETVALUE(name,?LOGOBJ),
	eresye:start(?VALUE(name)).

log_analyzer_(Self)->eresye:stop(?VALUE(name)).

