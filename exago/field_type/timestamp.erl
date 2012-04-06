-module(timestamp).
-behaviour(field).
-export([parse/2, read/2, write/2, reader/0, reader/1, writer/0, parse_timestamp/2, make_timestamp/1]).

extract_digits(String) ->
    re:split(String, "[^0-9]").

-spec(make_timestamp/1 :: (tuple()) -> tuple()).
make_timestamp(TimestampList) ->
    lists:map(fun ({BinaryString, Atom}) ->
		      [Atom, binary_to_list(BinaryString)]
	      end, TimestampList).

-spec(parse_timestamp/2 :: (list(), list()) -> tuple()).
parse_timestamp(TimestampTemplate, String)  ->
    Timestamp = extract_digits(String),
    lists:zip(Timestamp, TimestampTemplate).

%% @doc Turns a string into a tuple of values of the form
%% {Year, Month, Day, Hour, Minute, Second, Microsecond}
-spec(parse/2 :: (list(), list()) -> tuple()).
parse(String, Options) ->
    parse_timestamp(String, Options).

-spec(read/2 :: (list(), list()) -> tuple()).
read(Timestamp, _Options) ->
    {success, make_timestamp(Timestamp)}.

-spec(write/2 :: (tuple(), list()) -> list()).
write(String, _Options) ->
    String.

-spec(reader/0 :: () -> fun()).
reader() -> field:make_reader({?MODULE, parse, [year, month, day, hour, minute, second, microsecond]},
			      {?MODULE, read, []}).
-spec(reader/1 :: (list()) -> fun()).
reader(NOrder) -> field:make_reader({?MODULE, parse, NOrder}, {?MODULE, read, []}).
-spec(writer/0 :: () -> fun()).
writer() -> field:make_writer({?MODULE, write, []}).
