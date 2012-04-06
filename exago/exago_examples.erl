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
-module(exago_examples).

-compile(export_all).

-include("exago_state_machine.hrl").

%% @doc The tutorial 1 state machine
-spec(tutorial_state_machine/0 :: () -> #state_machine{}).
tutorial_state_machine() ->
    SM = #state_machine{
      states=
	  [#state{number=0, name="Start"},
	   #state{number=1, name="Room 1"},
	   #state{number=2, name="Room 2"},
	   #state{number=3, name="Room 3"}],
      transitions=
	  [#transition{from=0, to=1, input="forward"},
	   #transition{from=1, to=2, input="forward"},
	   #transition{from=2, to=3, input="forward"},
	   #transition{from=3, to=3, input="stop"}],
      start=0,
      accept=[3]
     }, SM.

%% @doc The tutorial 1 row format
-spec(tutorial_row_format/0 :: () -> list()).
tutorial_row_format() ->
    [exago_field:parser(group_id),
     exago_field:parser(timestamp, "yyyy-MM-dd hh:mm:ss:fffffff"), 
     exago_field:parser(transition_input)].

%% @doc The tutorial 1 example in full
-spec(tutorial_example/0 :: () -> tuple()).
tutorial_example() ->
    {ok, IoDevice} = file:open("./src/exago/sample_logs/circular.log", [write]),

    exago_util:write_csv_event(IoDevice, [exago_util:now_timestamp(), instance_1, "Start", "parse"]),
    InputRows    = exago_parser:parse_csv("./src/exago/sample_logs/tutorial.log"),
    exago_util:write_csv_event(IoDevice, [exago_util:now_timestamp(), instance_1, "Parse CSV File", "construct_state_machine"]),

    StateMachine = tutorial_state_machine(),
    exago_util:write_csv_event(IoDevice, [exago_util:now_timestamp(), instance_1, "Construct State Machine", "define_row_format"]),

    RowFormat    = tutorial_row_format(),
    exago_util:write_csv_event(IoDevice, [exago_util:now_timestamp(), instance_1, "Define Row Format", "create_event_source"]),

    EventSource  = exago_event:new_source("tutorial", InputRows, RowFormat),
	io:format(InputRows),
	io:format(EventSource),
    exago_util:write_csv_event(IoDevice, [exago_util:now_timestamp(), instance_1, "Create Event Source", "analyse_event_source"]),

    Result       = exago_state_machine:analyse_event_source(EventSource, StateMachine),
    exago_util:write_csv_event(IoDevice, [exago_util:now_timestamp(), instance_1, "Analyse Event Source", "success"]),

    file:close(IoDevice),
    Result.

%% @doc Row format for the events that we generate in the first tutorial example
-spec(circular_row_format/0 :: () -> list()).
circular_row_format() ->
    [exago_field:parser(timestamp, "yyyy-M-d hh:m:ss:f"),
     exago_field:parser(group_id, string),
     exago_field:parser(state_object),
     exago_field:parser(transition_input)].

%% @doc State machine representing the steps we are taking
-spec(circular_state_machine/0 :: () -> #state_machine{}).
circular_state_machine() ->
    SM = #state_machine{
      states=
	  [#state{number=0, name="Start"},
	   #state{number=1, name="Parse CSV File"},
	   #state{number=2, name="Construct State Machine"},
	   #state{number=3, name="Define Row Format"},
	   #state{number=4, name="Create a new Event Source"},
	   #state{number=5, name="Analyse Event Source"}],
      transitions=
	  [#transition{from=0, to=1, input="parse"},
	   #transition{from=1, to=2, input="construct_state_machine"},
	   #transition{from=2, to=3, input="define_row_format"},
	   #transition{from=3, to=4, input="create_event_source"},
	   #transition{from=4, to=5, input="analyse_event_source"}],
      start=0,
      accept=[5]
     }, SM.

%% @doc Analyses the tutorial event log
-spec(circular_example/0 :: () -> tuple()).
circular_example() ->
    InputRows    = exago_parser:parse_csv("./src/exago/sample_logs/circular.log"),
    StateMachine = circular_state_machine(),
    RowFormat    = circular_row_format(),
    EventSource  = exago_event:new_source("circular", InputRows, RowFormat),
    Result       = exago_state_machine:analyse_event_source(EventSource, StateMachine),
    Result.

%% @doc The elevator examples state machine model
-spec(elevator_state_machine/0 :: () -> #state_machine{}).
elevator_state_machine() ->
    StateMachine =
	#state_machine{
      states=
	  [#state{number=0, name="Init"},
	   #state{number=1, name="Closed at floor 1"},
	   #state{number=2, name="Open at floor 1"},
	   #state{number=3, name="Approaching floor 1"},
	   #state{number=4, name="Closed at floor 2"},
	   #state{number=5, name="Open at floor 2"},
	   #state{number=6, name="Approaching floor 2"},
	   #state{number=7, name="Closed at floor 3"},
	   #state{number=8, name="Open at floor 3"},
	   #state{number=9, name="Approaching floor 3"},
	   #state{number=10, name="Closed at floor 4"},
	   #state{number=11, name="Open at floor 4"},
	   #state{number=12, name="Approaching floor 4"}],
      transitions=
	  [#transition{from=0, to=1, input="reset_to_1"},
	   #transition{from=0, to=4, input="reset_to_2"},
	   #transition{from=0, to=7, input="reset_to_3"},
	   #transition{from=0, to=10, input="reset_to_4"},
	   #transition{from=1, to=2, input="open"},
	   #transition{from=2, to=1, input="close"},
	   #transition{from=1, to=6, input="approaching_2"},
	   #transition{from=6, to=3, input="approaching_1"},
	   #transition{from=6, to=9, input="approaching_3"},
	   #transition{from=6, to=4, input="stopped_at_2"},
	   #transition{from=4, to=5, input="open"},
	   #transition{from=5, to=4, input="close"},
	   #transition{from=4, to=3, input="approaching_1"},
	   #transition{from=4, to=9, input="approaching_3"},
	   #transition{from=9, to=7, input="stopped_at_3"},
	   #transition{from=7, to=8, input="open"},
	   #transition{from=7, to=6, input="approaching_2"},
	   #transition{from=8, to=7, input="close"},
	   #transition{from=3, to=1, input="stopped_at_1"},
	   #transition{from=10, to=11, input="open"},
	   #transition{from=11, to=10, input="close"},
	   #transition{from=12, to=10, input="stopped_at_4"},
	   #transition{from=7, to=12, input="approaching_4"},
	   #transition{from=9, to=12, input="approaching_4"},
	   #transition{from=10, to=9, input="approaching_3"},
	   #transition{from=10, to=9, input="approaching_2"},
	   #transition{from=9, to=6, input="approaching_2"}],
      start=0,
      accept=[0,1,2,3,4,5,6,7,8,9]},
    StateMachine.

%% @doc In this example, we wish to modify the input to the transitions 
%% in the state machine, so that they better reflect the state machine 
%% model. This step is not necessary if your inputs already fit the model.
-spec(elevator_input_modifier/1 :: (list()) -> tuple()).
elevator_input_modifier(Fields) ->
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
-spec(elevator_input_filter/1 :: (list()) -> true | false).
elevator_input_filter(Input) ->
    case Input of
	"reset"       ++ _ -> true;
	"open"             -> true;
	"close"            -> true;
	"approaching" ++ _ -> true;
	"stopped_at"  ++ _ -> true;
	_                  -> false
    end.

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
-spec(elevator_row_format/0 :: () -> list()).
elevator_row_format() ->
    [exago_field:parser(timestamp, "yyyy-MM-dd hh:mm:ss:fffffff"), 
     exago_field:parser(annotation, "event_id"),
     exago_field:parser(transition_input), 
     exago_field:parser(group_id),
     exago_field:parser(annotation, "floor1"),
     exago_field:parser(annotation, "floor2")].

-spec(elevator_example/0 :: () -> list()).
elevator_example() ->
    %% parse_csv is a helper function which comes with Exago, but you are
    %% certainly not required to use it. As long as the input data is a 
    %% list of tuples, everything will work fine.
    InputData = exago_parser:parse_csv("./src/exago/sample_logs/elevator_log.log"),

    RowFormat = elevator_row_format(),

    EventSource = 
	exago_event:new_source("elevator_log", InputData, RowFormat, 
			       {exago_examples, elevator_input_filter},
			       {exago_examples, elevator_input_modifier, 
				[transition_input, "floor1", "floor2"]}),

    StateMachine = elevator_state_machine(),
	io:format(InputData),
%% 	io:format(EventSource),

    Result = exago_state_machine:analyse_event_source(EventSource, StateMachine),

    Result.

-spec(sms_state_machine_time_constrained/0 :: () -> #state_machine{}).
sms_state_machine_time_constrained() ->
    StateMachine = 
    #state_machine{
      states=
	  [#state{number=0, name="State 0"},
	   #state{number=1, name="State 1"},
	   #state{number=2, name="State 2"},
	   #state{number=3, name="State 3"},
	   #state{number=4, name="State 4"},
	   #state{number=5, name="State 5"},
	   #state{number=6, name="State 6"}],
      transitions=
	  [#transition{from=0, to=1, input="req"},
	   #transition{from=1, to=2, input="reqSMS"},
	   #transition{from=2, to=3, input="ackSMS"},
	   #transition{from=3, to=4, input="req_ack", 
		       constraints=[#constraint{type=time, 
						test=fun (S) -> S < 1 end}]},
	   #transition{from=1, to=5, input="req_error"},
	   #transition{from=5, to=6, input="req_ack"},
	   #transition{from=5, to=6, input="ack_sms"},
	   #transition{from=4, to=6, input="req"},
	   #transition{from=4, to=6, input="reqSMS"}],
      start=0,
      accept=[4,5]},
    StateMachine.

-spec(sms_state_machine/0 :: () -> #state_machine{}).
sms_state_machine () ->
    StateMachine =
	#state_machine{
      states=
	  [#state{number=0, name="State 0"},
	   #state{number=1, name="State 1"},
	   #state{number=2, name="State 2"},
	   #state{number=3, name="State 3"},
	   #state{number=4, name="State 4"},
	   #state{number=5, name="State 5"},
	   #state{number=6, name="State 6"}],
      transitions=
	  [#transition{from=0, to=1, input="req"},
	   #transition{from=1, to=2, input="reqSMS"},
	   #transition{from=2, to=3, input="ackSMS"},
	   #transition{from=3, to=4, input="req_ack"},
	   #transition{from=1, to=5, input="req_error"},
	   #transition{from=5, to=6, input="req_ack"},
	   #transition{from=5, to=6, input="ack_sms"},
	   #transition{from=4, to=6, input="req"},
	   #transition{from=4, to=6, input="reqSMS"}],
      start=0,
      accept=[4,5]},
    StateMachine.

%% A more complex example:
-spec(sms_example/0 :: () -> list()).
sms_example() ->
    AckSMSData = exago_parser:parse_csv("./src/exago/sample_logs/etc_ex_AckSMS.log"),
    ReqAckData = exago_parser:parse_csv("./src/exago/sample_logs/etc_ex_ReqAck.log"),
    ReqErrData = exago_parser:parse_csv("./src/exago/sample_logs/etc_ex_ReqErr.log"),
    ReqData    = exago_parser:parse_csv("./src/exago/sample_logs/etc_ex_Req.log"),
    ReqSMSData = exago_parser:parse_csv("./src/exago/sample_logs/etc_ex_ReqSMS.log"),
    
    AckSMSFormat = [exago_field:parser(transition_input),
		    exago_field:parser(timestamp, "'yyyy-MM-dd hh:mm:ss:fffffff'"),
		    exago_field:foreign_key("ReqSMS", "AckSMSKey", [group_id],
				      exago_field:parser(group_id)),
		    exago_field:parser(annotation, "unknownAckSMS")],

    ReqAckFormat = [exago_field:parser(transition_input),
		    exago_field:parser(timestamp, "'yyyy-MM-dd hh:mm:ss:fffffff'"),
		    exago_field:parser(group_id),
		    exago_field:parser(annotation, "unknownReqAck")],

    ReqErrFormat = [exago_field:parser(transition_input),
		    exago_field:parser(timestamp, "'yyyy-MM-dd hh:mm:ss:fffffff'"),
		    exago_field:parser(group_id),
		    exago_field:parser(annotation, "message")],

    ReqFormat    = [exago_field:parser(transition_input),
		    exago_field:parser(timestamp, "'yyyy-MM-dd hh:mm:ss:fffffff'"),
		    exago_field:parser(group_id)],

    ReqSMSFormat = [exago_field:parser(transition_input),
		    exago_field:parser(timestamp, "'yyyy-MM-dd hh:mm:ss:fffffff'"),
		    exago_field:parser(group_id),
		    exago_field:parser(integer_annotation, "AckSMSKey"),
		    exago_field:parser(integer_annotation, "unknownReqSMS")],

    AckSMSSource = exago_event:new_source("AckSMS", AckSMSData, AckSMSFormat),
    ReqAckSource = exago_event:new_source("ReqAck", ReqAckData, ReqAckFormat),
    ReqErrSource = exago_event:new_source("ReqErr", ReqErrData, ReqErrFormat),
    ReqSource    = exago_event:new_source("Req", ReqData, ReqFormat),
    ReqSMSSource = exago_event:new_source("ReqSMS", ReqSMSData, ReqSMSFormat),
    
    EventSources = [AckSMSSource,ReqAckSource,ReqErrSource,ReqSource,ReqSMSSource],

    %% Here should be an argument to orderBy timestamp for example
    EventSource  = exago_event:merge_sources(EventSources, EventSources, []), 

    StateMachine = sms_state_machine_time_constrained(),
    Result       = exago_state_machine:analyse_event_source(EventSource, StateMachine),
    %% Outstanding issue here is that RowFormat differs depending on the file
    %% ** Extra - possible to match each event by arity?
    Result.

    
    
    
