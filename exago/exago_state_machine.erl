%%%-------------------------------------------------------------------
%%% @author Edward Tate <edward.tate@erlang-solutions.com>
%%% @copyright (C) 2010, Erlang Solutions Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 15 Oct 2010 by Edward Tate <edward.tate@erlang-solutions.com>
%%%-------------------------------------------------------------------
%%% Copyright (c) 2009,2010 Erlang Solutions formerly Erlang Training & Consulting
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% * Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer. 
%%% * Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in the
%%%   documentation and/or other materials provided with the distribution.
%%% * Neither the name of the Erlang Solutions nor the names of its
%%%   contributors may be used to endorse or promote products
%%%   derived from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
%%% BE  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------
-module(exago_state_machine).

-include("exago_state_machine.hrl").
-include("exago_event.hrl").

-compile(export_all).

%% @doc Generate a state machine from a list
-spec(list_to_state_machine/1 :: (list()) -> #state_machine{}).
list_to_state_machine([States, Transitions, Start, Accept, Error]) ->
    StateRecords      = [#state{number=N, name=Name} || 
			    {N, Name} <- States],
    TransitionRecords = [#transition{from=From, to=To, input=Input} || 
			    {From, To, Input} <- Transitions],
    SM = #state_machine{states=StateRecords, transitions=TransitionRecords, 
			start=Start, accept=Accept, error=Error}, SM.

%% @doc Lookup a state in a state machine by state number
-spec(lookup_state/2 :: (integer(), list()) -> #state{} | undefined).
lookup_state(_, []) ->
    undefined;
lookup_state(StateN, [State|States]) ->
    case StateN =:= State#state.number of
	true  -> State;
	false -> lookup_state(StateN, States)
    end.

%% @doc Lookup a transition by transition input
-spec(lookup_transition/2 :: (list(), list()) -> #transition{} | undefined).
lookup_transition(_Input, []) ->
    undefined;
lookup_transition({CurrentStateN, Input}, [T|Ts]) ->
    case ((T#transition.input =:= Input) and (T#transition.from =:= CurrentStateN)) of
	true  -> T;
	false -> lookup_transition({CurrentStateN, Input}, Ts)
    end.

%% @doc Checks whether a state machine definition is deterministic
-spec(is_deterministic/1 :: (list()) -> true | {false, #transition{}}).
is_deterministic([]) ->
    true;
is_deterministic([T|Ts]) ->
    case exago_util:is_duplicate(T, Ts) of
	true  -> {false, T};
	false -> is_deterministic(Ts)
    end.

%% @doc Checks whether a transition exists by number
-spec(is_transition_existent/2 :: (#transition{}, list()) -> true | false).
is_transition_existent(_Transition, []) ->
    false;
is_transition_existent(Transition, [T|Ts]) ->
    case Transition =:= T of
	true  -> true;
	false -> is_transition_existent(Transition, Ts)
    end.

-spec(check_constraints/3 :: (list(), list(), list()) -> list()).
check_constraints([], _DeltaTime, Result) ->
    lists:reverse(Result);
check_constraints([Constraint|Cs], DeltaTime, Result) ->
    CTest = Constraint#constraint.test,
    check_constraints(Cs, DeltaTime, [{Constraint, CTest(DeltaTime)}|Result]).

%% @doc This function takes:
%% Input: A transition input (see event.hrl)
%% DeltaTime: The time it took to transition from the last event, to the current one, in second
%% StateMachine: The abstract state machine model
%% CurrentState: Information on the current state
-spec(execute_input/4 :: (list(), integer(), #state_machine{}, tuple()) -> list()).
execute_input(Input, DeltaTime, StateMachine, CurrentState) ->
    CurrentStateN         = CurrentState#state.number,
    SMTransitions         = StateMachine#state_machine.transitions,
    SMStates              = StateMachine#state_machine.states,
    Transition            = lookup_transition({CurrentStateN, Input}, SMTransitions),

    case Transition of
	undefined ->
	    NextState         = undefined,
	    ConstraintResults = undefined;
	_         ->
	    NextState         = lookup_state(Transition#transition.to, SMStates),
	    ConstraintResults = check_constraints(Transition#transition.constraints, DeltaTime, [])
    end,

    Result = [{transition_exists, Transition},
	      {next_state_exists, NextState},
	      {delta_time, DeltaTime},
	      {constraint_results, ConstraintResults}],

    case (lists:any(fun ({_,R}) -> 
			    case R of 
				undefined -> true;
				_         -> false
			    end
		    end, Result)) of
	true  ->
	    {failure, eof, [{input, Input}] ++ Result};
	false ->
	    {success, NextState, [{input, Input}] ++ Result}
    end.

-spec(execute_input_list/4 :: (list(), list(), #state_machine{}, tuple()) -> tuple()).
execute_input_list([], [], _StateMachine, {Status, State, _LastTime, Result}) ->
    {Status, State, Result};
execute_input_list([I|Is], [T|Ts], StateMachine, {CurrentStatus, CurrentState, LastTime, Acc}) ->
%%     io:format("[~w:~w]:~w,~w~n",[?MODULE,?LINE,T,LastTime]),
	{NextStatus, NextState, TransitionResult} =
	execute_input(I, exago_util:delta_time(T, LastTime), StateMachine, CurrentState),
    case NextState of
	eof ->
	    {CurrentStatus, CurrentState, [TransitionResult|Acc]};
	_   ->
	    execute_input_list(Is, Ts, StateMachine,
			       {NextStatus, NextState, T, [TransitionResult|Acc]})
    end.

-spec(execute/3 :: (list(), list(), #state_machine{}) -> tuple()).
execute(Input, Timeline, StateMachine) ->
    StartState = lookup_state(StateMachine#state_machine.start, 
			      StateMachine#state_machine.states),
    execute_input_list(Input, Timeline, StateMachine, {start, StartState, 0, []}).

%% @doc Preprocess validation of the state machine model
-spec(is_valid/1 :: (#state_machine{}) -> true | false).
is_valid(StateMachine) -> 
    IsDeterministic = is_deterministic(StateMachine#state_machine.transitions),
    IsDeterministic
    %% Does StateMachine refer to non-existent states in transitions?
    .

%% @doc Generate a state machine
generate_state_machine(States, Transitions, StartState, ErrorStates) ->
    SM = #state_machine{
      states=States,
      transitions=Transitions,
      start=StartState,
      error=ErrorStates
     }, SM.

%% @doc Executes an event source and accumulates the results.
-spec(execute_event_source/2 :: (#event_source{}, #state_machine{}) -> list()).
execute_event_source(EventSource, StateMachine) ->
    [{EventGroup#event_group.id,
	 execute(lists:map(fun(E) -> E#event.transition_input end,
			   EventGroup#event_group.events),
		 lists:map(fun(E) -> E#event.timestamp end,
			   EventGroup#event_group.events),
		 StateMachine)} || EventGroup <- EventSource#event_source.event_groups].

%% @doc Extract the history of an event group out of the history analysis (convenience function)
-spec(extract_history/1 :: (tuple()) -> list()).
extract_history({_, {_, _, History}}) -> History.

%% @doc Fetch the list of failing transitions for a list of event group histories.
-spec(failing_transitions/1 :: (list()) -> list()).
failing_transitions(History) ->
    [{failing_transition, I, Transition} ||
	[{input, I}, {transition_exists, Transition} | _Ignore] <- History,
	Transition =:= undefined].

%% @doc Fetch the list of failing states for a list of event group histories.
-spec(failing_states/1 :: (list()) -> list()).
failing_states(History) ->
    [{failing_state, I, State} || 
	[{input, I}, _, {next_state_exists, State} | _Ignore] <- History,
	State =:= undefined].

%% @doc Convenience function used to work out which constraints failed.
-spec(collect_failures/1 :: (list()) -> list()).
collect_failures(ConstraintHistory) ->
    [Result || {_Constraint, Result} <- ConstraintHistory, Result =:= true].

%% @doc Fetch the list of failing constraints for a list of event group histories.
-spec(failing_constraints/1 :: (list()) -> list()).
failing_constraints(EventGroupHistory) ->
    R = [Result || [_, _, _, _, {constraint_results, Result}] <- EventGroupHistory],
    lists:flatten(R).

%% @doc Check whether the state machine is in an accept state at the end of its
%% execution.
-spec(is_accept_state/2 :: (StateN::integer(), AcceptStates::list()) -> true | false).
is_accept_state(N, AcceptStates) ->
    lists:member(N, AcceptStates).

%% @doc Check the status of this instance of the state machine execution.
-spec(instance_status/2 :: (CurrentState::#state{}, #state_machine{}) 
			   -> InstanceStatus::tuple()).
instance_status(State, SM) ->
    case is_accept_state(State#state.number, SM#state_machine.accept) of
	true  -> {success, State#state.number};
	false -> {failure, State#state.number}
    end.

%% @doc Perform a history analysis on a set of histories.
-spec(perform_history_analysis/2 :: (list(), #state_machine{}) -> list()).
perform_history_analysis(Instances, StateMachine) ->
    [{GroupId,
      {instance_status,     instance_status(LastState, StateMachine)},
      {failing_transitions, failing_transitions(History)},
      {failing_states,      failing_states(History)},
      {failing_constraints, failing_constraints(History)}} 
     || {GroupId, {_, LastState, History}} <- Instances].

%% @doc Write execution instances to file.
-spec(write_execution_instances/2 :: (iodata(), list()) -> ok).
write_execution_instances(_IoDevice, []) ->
    ok;
write_execution_instances(IoDevice, [ExecutionInstance|ExecutionInstances]) ->
    io:format(IoDevice, "~p\n", [ExecutionInstance]),
    write_execution_instances(IoDevice, ExecutionInstances).

%% @doc Write the event source to file for future reference.
-spec(write_event_source/2 :: (list(), list()) -> tuple() | ok).
write_event_source(EventSourceName, ExecutionInstances) ->
    File = file:open(EventSourceName ++ ".event_source", [write]),
    case File of
	{ok, IoDevice} ->
	    io:format("Writing event source to file: ~p\n", [EventSourceName ++ ".event_source"]),
	    write_execution_instances(IoDevice, ExecutionInstances),
	    file:close(IoDevice);
	Error          ->
	    io:format("Error writing the event source to disk: ~p\n", [Error])
    end.

%% @doc apply preprocess operations on the event source / state machine
-spec(preprocess_event_source/2 :: (list(), #state_machine{}) -> true | false).
preprocess_event_source(_EventSource, StateMachine) ->
    SMValid = is_valid(StateMachine),
    SMValid.

%% @doc main operations to be applied to the event source / state machine
-spec(process_event_source/2 :: (list(), #state_machine{}) -> list()).
process_event_source(EventSource, StateMachine) ->
    SMExecutionResults = execute_event_source(EventSource, StateMachine),
    SMExecutionResults.

%% @doc operations to be applied to the event source after it has been executed
-spec(postprocess_event_source/2 :: (list(), #state_machine{}) -> list()).
postprocess_event_source(EventSource, StateMachine) ->
    %% generate_visualization(StateMachine, ...)
    ExecutionInstances = process_event_source(EventSource, StateMachine),
%%     write_event_source(EventSource#event_source.name, ExecutionInstances),
    NInstances         = length(ExecutionInstances),
    {execution_analysis,
     {n_instances, NInstances},
     {history_analysis, perform_history_analysis(ExecutionInstances, StateMachine)}}.

%% @doc Analyse the current event source and return a list of results
-spec(analyse_event_source/2 :: (#event_source{}, #state_machine{}) -> tuple()).
analyse_event_source(EventSource, StateMachine) ->
    SMValid  = preprocess_event_source(EventSource, StateMachine),
    ExecutionAnalysis = postprocess_event_source(EventSource, StateMachine),
    {result,
     {state_machine_result, SMValid},
     ExecutionAnalysis}.

%% @doc Generate an initial state machine visualization as a .dot file
generate_visualization(StateMachine, Path) ->
    States         = StateMachine#state_machine.states,
    Transitions    = StateMachine#state_machine.transitions,
    {ok, IoDevice} = file:open(Path, [write]),
    io:fwrite(IoDevice, "digraph G {\n#size=\"6,6\"\n", []),
    [write_state(IoDevice, 
		 StateMachine#state_machine.start,
		 StateMachine#state_machine.accept,
		 StateMachine#state_machine.error,
		 {S#state.number, S#state.name}) 
     || S <- States],
    [write_transition(IoDevice,
		      {T#transition.from, 
		       T#transition.to,
		       T#transition.input})
     || T <- Transitions],
    io:fwrite(IoDevice, "\n}\n", []),
    file:close(IoDevice).

%% @doc Write a state, depending on whether it is a normal state, and accept state or an 
%% error state to dot format
write_state(IoDevice, StartState, AcceptStates, ErrorStates, {StateNumber, StateName}) ->
    case {StateNumber =:= StartState, 
	  lists:member(StateNumber, AcceptStates),
	  lists:member(StateNumber, ErrorStates)} of
	{true, _, _} ->
	    write_start_state(IoDevice, {StateNumber, StateName});
	{_, true, _} ->
	    write_accept_state(IoDevice, {StateNumber, StateName});
	{_, _, true} ->
	    write_error_state(IoDevice, {StateNumber, StateName});
	_ ->
	    write_normal_state(IoDevice, {StateNumber, StateName})
    end.

write_start_state(IoDevice, {StateNumber, StateName}) ->
    io:fwrite(IoDevice, "st~p [shape=~s,color=\"~s\",style=filled,fontsize=10,"
	      "label=\"~s\"];\n", [StateNumber, "circle", "#ffffff", StateName]).

write_accept_state(IoDevice, {StateNumber, StateName}) ->
    io:fwrite(IoDevice, "st~p [shape=~s,color=\"~s\",style=filled,fontsize=10,"
	      "label=\"~s\"];\n", [StateNumber, "circle", "#aaeeaa", StateName]).

write_error_state(IoDevice, {StateNumber, StateName}) ->
    io:fwrite(IoDevice, "st~p [shape=~s,color=\"~s\",style=filled,fontsize=10,"
	      "label=\"~s\"];\n", [StateNumber, "circle", "#eeaaaa", StateName]).

write_normal_state(IoDevice, {StateNumber, StateName}) ->
    io:fwrite(IoDevice, "st~p [shape=~s,color=\"~s\",style=filled,fontsize=10,"
	      "label=\"~s\"];\n", [StateNumber, "circle", "#aaaaee", StateName]).

%% @doc Write a transition in dot format
write_transition(IoDevice, {N, M, T}) ->
    io:fwrite(IoDevice, "st~p -> st~p [fontsize=10,label=\"~s\"];\n",
	      [N, M, print_tuple(T)]);
write_transition(IoDevice, {N, M, T, _}) ->
    io:fwrite(IoDevice, "st~p -> st~p [fontsize=10,label=\"~s\"];\n",
	      [N, M, print_tuple(T)]).
    
print_tuple(T) when is_integer(T) ->
    integer_to_list(T);
print_tuple(T) when is_list(T) -> 
    "\\\"" ++ T ++ "\\\"";
print_tuple(T) when is_atom(T) -> 
    atom_to_list(T);
print_tuple(T) when is_tuple(T) -> 
    [E | L] = tuple_to_list(T),
    "{" ++
	lists:foldl(fun(TupEl, Acc) -> 
			    Acc ++ ", " ++ print_tuple(TupEl)
		       end, print_tuple(E), L)
	++ "}".
