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
-module(exago_user).

%% This module exists for the sole purpose of making it easier to develop a 
%% working state machine, row format, input modifier & input filter.
%% The user should use each function to test the input at each step of the
%% program. All of these tests are optional.

-export([test_state_machine/2, test_csv_log/2]).
-export([test_state_machine_file/2]).
-export([test_row_format/5, test_row_format/4, test_row_format/3]).
-export([test_row_format_file/3]).

%% @doc Test a state machine (at the moment this only checks if the state machine
%% is deterministic) and generate a visualization of it at Path.
-spec(test_state_machine/2 :: (tuple(), list()) -> ok).
test_state_machine(StateMachine, Path) ->
    %% Generate visualization
    io:format("Generating visualization...\n"),
    exago_state_machine:generate_visualization(StateMachine, Path ++ ".dot"),
    exago_util:dot_to_png(Path),
    io:format("Generated visualization at: ~p\n", [Path]),
    %% Is deterministic / states etc?
    IsValid = exago_state_machine:is_valid(StateMachine),
    case IsValid of
	true  ->
	    io:format("State machine is deterministic.\n");
	false ->
	    io:format("State machine is not deterministic. Please convert your state machine to a DFSM.\n")
    end.

-spec(test_state_machine_file/2 :: (tuple(), list()) -> ok).
test_state_machine_file(StateMachinePath, Path) ->
    case file:consult(StateMachinePath) of
	{ok, [StateMachineData|_]} ->
	    StateMachine = exago_state_machine:list_to_state_machine(StateMachineData),
	    test_state_machine(StateMachine, Path);
	_ ->
	    io:format("Error: Could not consult state machine file.\n")
    end.

%% @doc Write the data to a file one row at a time.
-spec(write_data/2 :: (iodata(), list()) -> ok).
write_data(_, []) ->
    ok;
write_data(IoDevice, [DataRow|Data]) ->
    io:format(IoDevice, "~p.\n", [DataRow]),
    write_data(IoDevice, Data).

%% @doc Test a CSV file by opening it, and parsing it, then write the result 
%% to ResultDataPath. This function should be used to ensure that your CSV
%% file is parsed correctly.
-spec(test_csv_log/2 :: (list(), list()) -> ok).
test_csv_log(FileName, ResultDataPath) ->
    %% Does file exist?
    io:format("Trying to open csv file...\n"),
    File = file:open(FileName, [read]),
    case File of
	{ok, IoDevice}  ->
	    io:format("File opened successfully\n"),
	    %% ... try parse here
	    ResultData = exago_parser:parse_csv(FileName),
	    io:format("Rows parsed successfully\n"),
	    WriteFile = file:open(ResultDataPath, [write]),
	    case WriteFile of
		{ok, IoDevice2} ->
		    io:format("Writing result data to: ~p\n", [ResultDataPath]),
		    write_data(IoDevice2, ResultData),
		    io:format("Done.\n"),
		    file:close(IoDevice2);
		Error           ->
		    io:format("Could not write result data to path provided, aborting :'(\n"),
		    io:format("Error was ~p\n", [Error])
	    end,
	    file:close(IoDevice);
	{error, enoent} ->
	    io:format("File could not be found, aborting, check the provided path\n");
	_               ->
	    io:format("File could not be opened, aborting\n")
    end.

%% @doc This function should be used to test a row format against a parsed log.
%% It expects a RowFormat as its first argument (so a list of field parser functions),
%% and a DataPath pointing to a previously parsed CSV log file. You can generate
%% this data file by running test_csv_log. ResultPath should point to the final
%% result data, which will be used as input to the state machine model.

%% Do rows contain required fields
%% Does row contain duplication?
-spec(test_row_format/3 :: (list(), list(), list()) -> ok).
test_row_format(RowFormat, DataPath, ResultPath) ->
    test_row_format(RowFormat, DataPath, ResultPath, 
		    {input_filter, {exago_event, default_input_filter}},
		    {input_modifier, {exago_event, default_input_modifier, [transition_input]}}).

-spec(test_row_format_file/3 :: (list(), list(), list()) -> ok).
test_row_format_file(RowFormatPath, DataPath, ResultPath) ->
    case file:consult(RowFormatPath) of
	{ok, RowFormatData} ->
	    RowFormat = lists:map(fun (Args) -> 
					  apply(exago_field, parser, Args)
				  end, RowFormatData),
	    test_row_format(RowFormat, DataPath, ResultPath);
	_ ->
	    io:format("Error: Failed to consult RowFormat file.\n")
    end.

-spec(test_row_format/4 :: (list(), list(), list(), tuple()) -> ok).
test_row_format(RowFormat, DataPath, ResultPath, {input_filter, InputFilter}) ->
    %% show sample rows after being filtered
    test_row_format(RowFormat, DataPath, ResultPath,
		    {input_filter, InputFilter},
		    {input_modifier, {exago_event, default_input_modifier, [transition_input]}});
test_row_format(RowFormat, DataPath, ResultPath, {input_modifier, InputModifier}) ->
    %% show sample rows after being modified
    test_row_format(RowFormat, DataPath, ResultPath,
		    {input_filter, {exago_event, default_input_filter}},
		    {input_modifier, InputModifier}).

%% @doc This is the same as the previous test_row_format, except it takes an InputFilter,
%% and an EventModifier, and uses these in combination with the row parsers to generate
%% the final input to the state machine.
-spec(test_row_format/5 :: (list(), list(), list(), tuple(), tuple()) -> ok).
test_row_format(RowFormat, DataPath, ResultPath, {input_filter, InputFilter}, {input_modifier, InputModifier}) ->
    %% show sample rows after being filtered & modified
    io:format("Opening data file...\n"),
    ParsedFile = file:consult(DataPath),
    case ParsedFile of
	{ok, Terms} ->
	    io:format("Data opened successfully\n"),
	    io:format("Parsing rows...\n"),
	    ParsedSource = exago_event:parse_source(lists:map(fun (T) -> tuple_to_list(T) end, Terms), 
						    RowFormat, [], InputFilter, InputModifier),
	    WriteFile = file:open(ResultPath, [write]),
	    case WriteFile of
		{ok, IoDevice} ->
		    io:format("Writing result to file: ~p \n", [ResultPath]),
		    io:format(IoDevice, "~p", [ParsedSource]),
		    io:format("Done.\n"),
		    file:close(IoDevice);
		Error          ->
		    io:format("Could not write result data to path provided, aborting :'(\n"),
		    io:format("Error was ~p\n", [Error])
	    end;
	Error       ->
	    io:format("Error consulting data file ~p\n", [Error])
    end.

