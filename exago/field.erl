-module(field).
-export([behaviour_info/1]).
-export([make_reader/2, make_writer/1, apply_field_format/3]).

-spec(behaviour_info/1 :: (term()) -> list() | undefined).
behaviour_info(callbacks) ->
    [{parse, 2},
     {read, 2},
     {write, 2},
     {reader, 0},
     {writer, 0}];
behaviour_info(_Other) ->
    undefined.

-spec(make_reader/2 :: (tuple(), tuple()) -> fun()).
make_reader({Module1, Parse, ParseArgs}, {Module2, Read, ReadArgs}) ->
    fun (String) ->
	    ParseResult = Module1:Parse(String, ParseArgs),
	    ReadResult  = Module2:Read(ParseResult, ReadArgs),
	    ReadResult
    end.

-spec(make_writer/1 :: (tuple()) -> fun()).
make_writer({Module, Write, WriteArgs}) ->
    fun (Datum) ->
	    DatumList = Module:Write(Datum, WriteArgs),
	    DatumList
    end.

-spec(apply_field_format/3 :: (atom(), list(), list()) -> list()).
apply_field_format(read, ReaderList, FieldList) ->
    [Read(Field) || 
	{Read, Field} <- lists:zip(ReaderList, FieldList)];
apply_field_format(write, WriterList, ListOfStringLists) ->
    lists:flatten(
      [Write(Output) || 
	  {Write, Output} <- lists:zip(WriterList, ListOfStringLists)]
     ).
