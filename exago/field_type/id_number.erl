-module(id_number).
-behaviour(field).
-export([parse/2, read/2, write/2, reader/0, writer/0]).

%% @doc Parses the input string into an integer
-spec(parse/2 :: (list(), list()) -> integer()).
parse(String, _Options) ->
    string:to_integer(String).

%% @doc Tags the value of the integer with the appropriate type (id_number)
-spec(read/2 :: (tuple(), list()) -> tuple()).
read({error, no_integer}, _Options) -> 
    {failure, no_integer};
read(N, _Options) -> 
    {success, {id_number, N}}.

%% @doc Writes the value as a string so that it may be used later in logging
-spec(write/2 :: (tuple(), list()) -> list()).
write({id_number, {N, _}}, _Options) -> 
    io_lib:format("~B", [N]).

%% @doc General purpose reader & writer for this type
-spec(reader/0 :: () -> fun()).
reader() -> field:make_reader({?MODULE, parse, []}, {?MODULE, read, []}).
-spec(writer/0 :: () -> fun()).
writer() -> field:make_writer({?MODULE, write, []}).
