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
-module(exago_printer).

-compile ([export_all]).

-spec(count_acceptant_executions/2 :: (list(), Acc::integer()) -> integer()).
count_acceptant_executions([], N) ->
    N;
count_acceptant_executions([{_, {instance_status, InstanceStatus}, _, _, _}|HistoryAnalysis], N) ->
    case InstanceStatus of
	{success, _StateN} ->
	    count_acceptant_executions(HistoryAnalysis, N+1);
	{failure, _StateN} ->
	    count_acceptant_executions(HistoryAnalysis, N)
    end.

-spec(list_acceptant_executions/2 :: (list(), list()) -> list()).
list_acceptant_executions([], Acc) ->
    Acc;
list_acceptant_executions([{_, {instance_status, InstanceStatus}, _, _, _}|HistoryAnalysis], Acc) ->
    case InstanceStatus of
	{success, _StateN} ->
	    list_acceptant_executions(HistoryAnalysis, [InstanceStatus|Acc]);
	{failure, _StateN} ->
	    list_acceptant_executions(HistoryAnalysis, Acc)
    end.

-spec(list_failing_executions/2 :: (list(), list()) -> list()).
list_failing_executions([], Acc) ->
    Acc;
list_failing_executions([{_, {instance_status, InstanceStatus}, _, _, _}|HistoryAnalysis], Acc) ->
    case InstanceStatus of
	{failure, _StateN} ->
	    list_failing_executions(HistoryAnalysis, [InstanceStatus|Acc]);
	{success, _StateN} ->
	    list_failing_executions(HistoryAnalysis, Acc)
    end.

-spec(count_failing_transitions/2 :: (list(), Acc::integer()) -> integer()).
count_failing_transitions([], N) ->
    N;
count_failing_transitions([{_, _, {failing_transitions, FailingTransitions}, _, _}|HistoryAnalysis], N) ->
    case FailingTransitions of
	[] ->
	    count_failing_transitions(HistoryAnalysis, N);
	_  ->
	    count_failing_transitions(HistoryAnalysis, N+1)
    end.

-spec(list_failing_transitions/2 :: (list(), Acc::integer()) -> list()).
list_failing_transitions([], Acc) ->
    Acc;
list_failing_transitions([{_, _, {failing_transitions, FailingTransitions}, _, _}|HistoryAnalysis], Acc) ->
    case FailingTransitions of
	[] -> list_failing_transitions(HistoryAnalysis, [FailingTransitions|Acc]);
	_  -> list_failing_transitions(HistoryAnalysis, Acc)
    end.

-spec(count_failing_states/2 :: (list(), Acc::integer()) -> integer()).
count_failing_states([], N) ->
    N;
count_failing_states([{_, _, _, {failing_states, FailingStates}, _}|HistoryAnalysis], N) ->
    case FailingStates of
	[] ->
	    count_failing_states(HistoryAnalysis, N);
	_  ->
	    count_failing_states(HistoryAnalysis, N+1)
    end.

-spec(count_failing_constraints/2 :: (list(), Acc::integer()) -> integer()).
count_failing_constraints([], N) ->
    N;
count_failing_constraints([{_, _, _, _, {failing_constraints, FailingConstraints}}|
			   HistoryAnalysis], N) ->
    case FailingConstraints of
	[] ->
	    count_failing_constraints(HistoryAnalysis, N);
	_  ->
	    count_failing_constraints(HistoryAnalysis, N+1)
    end.

%% filter_failing_constraints

-spec(print_state_machine_result/1 :: (tuple()) -> ok).
print_state_machine_result({state_machine_result, SMResult}) ->
    case SMResult of
	true ->
	    io:format("State machine validated correctly.\n");
	_    ->
	    io:format("State machine did not validate.\n")
    end.

-spec(print_execution_analysis/1 :: (tuple()) -> ok).
print_execution_analysis({execution_analysis, {n_instances, N}, {history_analysis, HistoryAnalysis}}) ->
    io:format("There were ~p event groups found in the log files that were successfully parsed.\n", [N]),
    io:format("Out of these event groups: \n\t~p finished in an accept state.\n", 
	      [count_acceptant_executions(HistoryAnalysis, 0)]),
    io:format("\t~p had failing transitions.\n",
	      [count_failing_transitions(HistoryAnalysis, 0)]),
    io:format("\t~p had failing states.\n",
	      [count_failing_states(HistoryAnalysis, 0)]),
    io:format("\t~p had failing constraints.\n",
	      [count_failing_constraints(HistoryAnalysis, 0)]).

-spec(print_result/1 :: (tuple()) -> ok).
print_result({result, SMResult, ExecutionAnalysis}) ->
    io:format("Results:\n"),
    print_state_machine_result(SMResult),
    print_execution_analysis(ExecutionAnalysis).
    
