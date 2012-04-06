%%%-------------------------------------------------------------------
%%% @author Edward Tate <edward.tate@erlang-solutions.com>
%%% @copyright (C) 2010, Erlang Solutions Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 15 Oct 2010 by Edward Tate <>
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
-module(exago_field).

-export([parser/1, parser/2, foreign_key/4]).

-spec(parser/2 :: (atom(), list()) -> fun()).
parser(timestamp, custom) ->
    fun (Timestamp) ->
	    {timestamp, parse_timestamp_custom(Timestamp)}
    end;
parser(timestamp, Format) ->
    fun (Timestamp) ->
	    {timestamp, exago_parser:parse_ts({Timestamp}, Format, 0)}
    end;
parser(group_id, Type) ->
    fun (GroupId) ->
	    case Type of
		integer ->
		    {group_id, parse_integer(GroupId)};
		string  ->
		    {group_id, GroupId}
	    end
    end;
parser(annotation, Label) ->
    fun (Annotation) ->
	    {Label, Annotation}
    end;
parser(integer_annotation, Label) ->
    fun (Annotation) ->
	    {Label, parse_integer(Annotation)}
    end.

-spec(parser/1 :: (atom()) -> fun()).
parser(group_id) ->
    fun (GroupId) ->
	    {group_id, parse_integer(GroupId)}
    end;
parser(transition_input) ->
    fun (TransitionInput) ->
	    {transition_input, TransitionInput}
    end;
parser(state_object) ->
    fun (StateObject) ->
	    {state_object, StateObject}
    end;
parser(ignore) ->
    fun (Value) ->
	    {ignore, Value}
    end.

-spec(foreign_key/4 :: (list(), list(), list(), fun()) -> fun()).
foreign_key(EventSourceName, FieldKey, FieldList, Parser) ->
    fun (Value) ->
	    {foreign_key, [{type, Parser(Value)},
			   {event_source_name, EventSourceName},
			   {field_key, FieldKey},
			   {field_list, FieldList}]}
    end.

-spec(parse_timestamp_custom/1 :: (list()) -> list()).
parse_timestamp_custom(CustomTimestamp) ->
    {_, Result, _} = io_lib:fread("~d-~d-~d_~d:~d:~d", CustomTimestamp),
    [Y, M, D, H, Min, S] = Result,
    {{{Y, M, D}, {H, Min, S}}, 0}.

-spec(parse_integer/1 :: (list()) -> integer() | error).
parse_integer(String) ->
    {Result, _} = string:to_integer(String),
    case Result of
	error ->
	    error;
	_     ->
	    Result
    end.
    
    
