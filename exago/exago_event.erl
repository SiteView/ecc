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
-module(exago_event).

-include("exago_event.hrl").

-export([new_source/3, new_source/5, merge_sources/3, parse_source/5]).
-export([default_input_modifier/1, default_input_filter/1]).

%% @doc Parses a field according to a parser type, refer to the field 
%% module for an overview of the default parsers.
-spec(parse_field/2 :: (String::list(), Parser::fun((_) -> any())) -> list()).
parse_field(Field, ParserType) ->
    ParserType(Field).

%% @doc Internal function to parse a row by field using each rows field parser
-spec(parse_row_/3 :: (Fields::list(), RowFormat::list(), Acc::list()) -> Acc::list()).
parse_row_([], [], Acc) ->
    lists:reverse(Acc);
parse_row_(_,  [], _)   ->
    {parse_error, "The row format contains less parsers than there are fields."};
parse_row_([], [_|Format], Acc) ->
    %% This occurs when there are more formats than rows
    parse_row_([], Format, Acc);
parse_row_([F|Fields], [ParserType|Format], Acc) ->
    case ParserType of
	rest ->
	    lists:map(fun (Field) ->
			      parse_field(Field, exago_field:parser(ignore))
		      end, Fields) ++ Acc;
	_    ->
	    parse_row_(Fields, Format, [parse_field(F, ParserType)|Acc])
    end.

%% @doc Interface function to parse_row_
-spec(parse_row/2 :: (Fields::list(), RowFormat::list()) -> Acc::list()).
parse_row(Fields, RowFormat) ->
    parse_row_(Fields, RowFormat, []).

%% @doc Converts a data row to an event. This enables us to have a way of parsing individual fields,
%% and thus the data is parsed when it is converted to an event.
-spec(row_to_event/2 :: (list(), list()) -> #event{}).
row_to_event(ParsedRow, ForeignKeys) ->
    Timestamp       = proplists:get_value(timestamp, ParsedRow),
    GroupId         = proplists:get_value(group_id, ParsedRow),
    TransitionInput = proplists:get_value(transition_input, ParsedRow),
    Rest            = exago_util:delete_all(?REQUIRED_FIELDS, ParsedRow),
    Event = #event{timestamp=Timestamp, 
		   group_id=GroupId, 
		   transition_input=TransitionInput,
		   foreign_keys=ForeignKeys,
		   rest=Rest},
    Event.

%% @doc This function is responsible for binding together the parsing, filtering, modifying,
%% resolution of foreign keys, and conversion from row to event record. 
-spec(parse_source/5 :: (list(), list(), list(), tuple(), tuple()) -> list()).
parse_source([], _Format, EventSource, _Filter, _Modifier) ->
    EventSource;
parse_source([R|Rows], Format, EventSource, {FilterModule, Filter}, {ModifierModule, Modifier, Fields}) ->
    ParsedRow      = parse_row(R, Format),
    ModifierFields = exago_util:extract_fields(Fields, ParsedRow),
    CutRow         = lists:filter(fun ({FieldKey, _}) ->
					  not lists:any(fun (Key) ->
								FieldKey =:= Key
							end, Fields)
				  end, ParsedRow),
    ModifiedInput  = ModifierModule:Modifier(ModifierFields),
    WholeRow       = CutRow ++ [ModifiedInput],
    ForeignKeys    = proplists:get_all_values(foreign_key, WholeRow),
    ResultRow      = proplists:delete(foreign_key, WholeRow),
    Event          = row_to_event(ResultRow, ForeignKeys),
    case FilterModule:Filter(Event#event.transition_input) of
	true  ->
	    parse_source(Rows, Format, [Event|EventSource], 
			 {FilterModule, Filter}, 
			 {ModifierModule, Modifier, Fields});
	false ->
	    parse_source(Rows, Format, EventSource, 
			 {FilterModule, Filter}, 
			 {ModifierModule, Modifier, Fields})
    end.

%% @doc Create a new group with the single element Event inside
-spec(new_group/1 :: (#event{}) -> #event_group{}).
new_group(Event) ->
    #event_group{id=Event#event.group_id, events=[Event]}.

%% @doc Add an event to an event_group
-spec(group_add/2 :: (#event{}, #event_group{}) -> #event_group{}).
group_add(Event, Group) ->
    NewGroup = #event_group{
      id=Group#event_group.id,
      timestamp=Group#event_group.timestamp,
      events=[Event|Group#event_group.events]
     }, NewGroup.

%% @doc Lookup a group in a group list by group_id
-spec(lookup_group/2 :: (integer(), list()) -> #event_group{} | undefined).
lookup_group(_GroupId, []) ->
    undefined;
lookup_group(GroupId, [G|Gs]) ->
    case GroupId =:= G#event_group.id of
	true  ->
	    G;
	false ->
	    lookup_group(GroupId, Gs)
    end.

%% @doc split the source into groups by group_id (internal)
-spec(split_source/2 :: (list(), list()) -> list()).
split_source([], ExistingGroups) ->
    ExistingGroups;
split_source([Event|EventSource], []) ->
    split_source(EventSource, [new_group(Event)]);
split_source([Event|EventSource], ExistingGroups) ->
    case lookup_group(Event#event.group_id, ExistingGroups) of
	undefined ->
	    split_source(EventSource, [new_group(Event)|ExistingGroups]);
	Group     ->
	    split_source(EventSource, 
			 [group_add(Event, Group)|
			  lists:delete(Group, ExistingGroups)])
    end.

%% @doc split the source into groups by group_id (convenience)
-spec(split/1 :: (list()) -> list()).
split(EventSource) ->
    split_source(EventSource, []).

%% @doc Creates a named event source which can be used later to reference foreign keys.
-spec(new_source/5 :: (list(), list(), list(), tuple(), tuple()) -> #event_source{}).
new_source(Name, Rows, Format, Filter, Modifier) ->
    NewSource = #event_source{
      name=Name,
      event_groups=
	  split(parse_source(lists:map(fun (Row) -> 
					       tuple_to_list(Row) 
				       end, Rows), 
			     Format, [], Filter, Modifier))
     }, NewSource.

%% @doc Creates a named event source, without being filtered or the input modified.
-spec(new_source/3 :: (list(), list(), list()) -> #event_source{}).
new_source(Name, Rows, Format) ->
    new_source(Name, Rows, Format, 
	       {exago_event, default_input_filter},
	       {exago_event, default_input_modifier, [transition_input]}).

%% @doc A default input modifier that doesn't modify the input.
%% This is here as a convenience, so that people aren't forced to modify
%% transition inputs, if they like the way it is parsed by default.
-spec(default_input_modifier/1 :: (list()) -> tuple()).
default_input_modifier(Fields) ->
    TransitionInput = proplists:get_value(transition_input, Fields),
    {transition_input, TransitionInput}.

%% @doc A default input filter that doesn't filter its input.
-spec(default_input_filter/1 :: (list()) -> true).
default_input_filter(_Input) ->
    true.

%% @doc Extract a list of fields from an event.
-spec(extract_event_field_list/2 :: (#event{}, list()) -> list()).
extract_event_field_list(Event, FieldList) ->
    Timestamp       = case lists:member(timestamp, FieldList) of
			   true  ->
			       Event#event.timestamp;
			   false ->
			       undefined
		       end,
    GroupId         =  case lists:member(group_id, FieldList) of
			   true  ->
			       Event#event.group_id;
			   false ->
			       undefined
		       end,
    TransitionInput = case lists:member(transition_input, FieldList) of
			   true  ->
			       Event#event.transition_input;
			   false ->
			       undefined
		      end,
    RestFields      = exago_util:extract_fields(FieldList, Event#event.rest),
    lists:filter(
      fun (R) ->
     	      case R of
		  {_, undefined} ->
		      false;
     		  _         ->
		      true
     	      end
      end, [{timestamp, Timestamp}, 
	    {group_id, GroupId}, 
	    {transition_input, TransitionInput}] ++ RestFields
     ).

%% Code below is used to resolve foreign keys, it looks hairier than
%% it actually is semantically due to the structure of the results.
%% event_source ("name") -> event_group -> event (field_key, field_list, value)
%% A re-implementation of this code should probably be done when
%% optimising the code for efficiency. 

%% @doc Lookup a foreign key in a list of events
-spec(lookup_key_in_events/2 :: (term(), list()) -> list() | undefined).
lookup_key_in_events(_Key, []) ->
    undefined;
lookup_key_in_events(Key, [E|Events]) -> 
    FieldKey   = proplists:get_value(field_key, Key),
    FieldList  = proplists:get_value(field_list, Key),
    FieldType  = proplists:get_value(type, Key),
    {_, FieldValue} = FieldType,
    EventValue = case FieldKey of
		     timestamp ->
			 case E#event.timestamp =:= FieldValue of
			     true  ->
				 extract_event_field_list(E, FieldList);
			     false ->
				 lookup_key_in_events(Key, Events)
			 end;
		     group_id ->
			 case E#event.group_id =:= FieldValue of
			     true  ->
				 extract_event_field_list(E, FieldList);
			     false ->
				 lookup_key_in_events(Key, Events)
			 end;
		     transition_input ->
			 case E#event.transition_input =:= FieldValue of
			     true  ->
				 extract_event_field_list(E, FieldList);
			     false ->
				 lookup_key_in_events(Key, Events)
			 end;
		     _             ->
			 EventField = proplists:get_value(FieldKey, E#event.rest),
			 case EventField =:= FieldValue of
			     true  ->
				 extract_event_field_list(E, FieldList);
			     false ->
				 lookup_key_in_events(Key, Events)
			 end
		 end,
    EventValue.

%% N.B. The three functions below should probably be replaced with some tree recursive 
%% function, but I find doing things this way preserves the structure of the data
%% built up using the event record types. So its a bit more repetitive but is easier
%% to read, and for now it works just fine.

%% @doc lookup a key in a list of groups (helper function)
-spec(lookup_key_in_groups/2 :: (term(), list()) -> list() | undefined).
lookup_key_in_groups(_Key, []) ->
    undefined;
lookup_key_in_groups(Key, [G|Groups]) ->
    case lookup_key_in_events(Key, G#event_group.events) of
	undefined ->
	    lookup_key_in_groups(Key, Groups);
	Result    ->
	    Result
    end.

%% @doc lookup a key in a list of sources (helper function)
-spec(lookup_key_in_sources/2 :: (term(), list()) -> list() | undefined).
lookup_key_in_sources(_Key, []) ->
    undefined;
lookup_key_in_sources(Key, [S|Sources]) ->
    case S#event_source.name =:= proplists:get_value(event_source_name, Key) of
	true  ->
	    case lookup_key_in_groups(Key, S#event_source.event_groups) of
		undefined ->
		    undefined;
		Result    ->
		    Result
	    end;
	false ->
	    lookup_key_in_sources(Key, Sources)
    end.

%% @doc lookup a key in a list of untouched sources 
-spec(lookup_keys/3 :: (list(), list(), list()) -> list()).
lookup_keys([], _RawSources, Acc) ->
    Acc;
lookup_keys([Key|ForeignKeys], RawSources, Acc) ->
    lookup_keys(ForeignKeys, RawSources, 
		[lookup_key_in_sources(Key, RawSources)|Acc]).

%% @doc Resolve the foreign keys in an event, and report any errors that occur.
%% Interior duplication is when there are duplicated fields within the event
%% record. Exterior duplication is when there are duplicated fields that belong
%% to events outside the one currently being checked. 
%% Apart from resolving foreign keys, this function also makes sure that all
%% events have the necessary required fields.
-spec(resolve_event/2 :: (#event{}, list()) -> #event{}).
resolve_event(Event, RawSources) ->
    ForeignKeys = Event#event.foreign_keys,
    case ForeignKeys of
	[] ->
	    Event;
	_  ->
	    ForeignKeyFields       = lookup_keys(Event#event.foreign_keys, RawSources, []),
	    ResolvedFields         = lists:flatten(ForeignKeyFields),

	    ResolvedTimestamp       = 
		proplists:get_value(timestamp, ResolvedFields, Event#event.timestamp),
	    ResolvedGroupId         = 
		proplists:get_value(group_id, ResolvedFields, Event#event.group_id),
	    ResolvedTransitionInput =
		proplists:get_value(transition_input, ResolvedFields, Event#event.transition_input),
	    ResolvedRest            = 
		exago_util:delete_all([timestamp, group_id, transition_input], ResolvedFields),

	    %% Check for duplication here in case of user error?
	    case exago_util:all_defined([ResolvedTimestamp, ResolvedGroupId, 
					 ResolvedTransitionInput]) of
		true  ->
		    #event{
		  timestamp=ResolvedTimestamp,
		  group_id=ResolvedGroupId,
		  transition_input=ResolvedTransitionInput,
		  foreign_keys=[],
		  rest=ResolvedRest
		 };
		false ->
		    {error, required_field_undefined}
	    end
    end.

%% @doc Classify events that have no event group, find their group Ids, 
%% merge them back into the events that have well defined Ids, then 
%% split them into their respective groups.
-spec(resolve_events/3 :: (list(), list(), list()) -> list()).
resolve_events([], _RawSources, Acc) ->
    lists:reverse(Acc);
resolve_events([E|Events], RawSources, Acc) ->
    resolve_events(Events, RawSources, [resolve_event(E, RawSources)|Acc]).

%% @doc Convenience function to create an event_group record around
%% a list of resolved events.
-spec(resolve_group/2 :: (#event_group{}, list()) -> #event_group{}).
resolve_group(Group, RawSources) ->
    NewGroup = #event_group{
      id=Group#event_group.id,
      timestamp=Group#event_group.timestamp,
      events=resolve_events(Group#event_group.events, RawSources, [])
     }, NewGroup.

%% @doc Convenience function to create event_groups out of resolved
%% events.
-spec(resolve_groups/3 :: (list(), list(), list()) -> list()).
resolve_groups([], _RawSources, Acc) ->
    Acc;
resolve_groups([G|Groups], RawSources, Acc) ->
    resolve_groups(Groups, RawSources, [resolve_group(G, RawSources)|Acc]).

%% @doc Same as above except doesn't include the accumulator
-spec(resolve_source/2 :: (#event_source{}, list()) -> list()).
resolve_source(Source, RawSources) ->
    resolve_groups(Source#event_source.event_groups, RawSources, []).

%% @doc Below we merge the resolved events. We have to resolve the events
%% before we can merge because we need to know the group_ids of the events
%% in order to merge groups. This particular ordering is what determines 
%% whether we allow a group_id to be part of a separate event source.
-spec(extract_events/2 :: (list(), list()) -> list()).
extract_events([], Events) ->
    Events;
extract_events([S|ResolvedSources], Events) ->
    extract_events(ResolvedSources, S#event_group.events ++ Events).

%% @doc helper function to convert a timestamp to a list
-spec(timestamp_to_list/1 :: (tuple()) -> list()).
timestamp_to_list({{TimestampDate, TimestampTime}, TimestampMicroseconds}) ->
    Date = tuple_to_list(TimestampDate),
    Time = tuple_to_list(TimestampTime),
    Date ++ Time ++ [TimestampMicroseconds].

%% @doc Check two timestamps in order to figure out which is earlier.
-spec(time_lt/2 :: (tuple(), tuple()) -> boolean()).
time_lt(Timestamp1, Timestamp2) ->
    Timestamp1List = timestamp_to_list(Timestamp1),
    Timestamp2List = timestamp_to_list(Timestamp2),
    if Timestamp1List == Timestamp2List -> false;
       Timestamp1List >  Timestamp2List -> true;
       Timestamp1List <  Timestamp2List -> false
    end.

%% @doc Sort the events by timestamp
-spec(sort_events/1 :: (list()) -> list()).
sort_events(Events) -> 
    lists:sort(fun ({_, Timestamp1, _, _, _, _},
		    {_, Timestamp2, _, _, _, _}) -> 
			   time_lt(Timestamp1, Timestamp2) 
		   end, Events).

%% @doc Sort the event groups by Id
-spec(sort_event_groups/1 :: (list()) -> list()).
sort_event_groups(EventGroups) ->
    lists:sort(fun ({_, EventId1, _, _}, {_, EventId2, _, _}) ->
		       EventId1 < EventId2
	       end, EventGroups).

%% @doc Resolve the foreign keys between event sources, then merge
%% the groups contained within the sources together.
-spec(merge_sources/3 :: (list(), list(), list()) -> #event_source{}).
merge_sources([], _RawSources, ResolvedSources) ->
    #event_source{name="merged_sources",
		  event_groups=sort_event_groups(
				 split(
				   sort_events(
				     extract_events(lists:flatten(ResolvedSources), []))))};
merge_sources([Source|EventSources], RawSources, ResolvedSources) ->
    merge_sources(EventSources, RawSources, [resolve_source(Source, RawSources)|ResolvedSources]).

    
