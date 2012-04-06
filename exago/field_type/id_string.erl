-module(id_string).
-behaviour(field).
-export([parse/2, read/2, write/2, reader/0, writer/0]).

-spec(parse/2 :: (list(), list()) -> integer()).
parse(String, _Options) ->
    String.

-spec(read/2 :: (list(), list()) -> tuple()).
read(String, _Options) ->
    {success, {id_string, String}}.

-spec(write/2 :: (tuple(), list()) -> list()).
write(String, _Options) ->
    io_lib:format("~s", [String]).

-spec(reader/0 :: () -> fun()).
reader() -> field:make_reader({?MODULE, parse, []}, {?MODULE, read, []}).
-spec(writer/0 :: () -> fun()).
writer() -> field:make_writer({?MODULE, write, []}).
