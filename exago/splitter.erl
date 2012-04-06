-module(splitter).
-export([behaviour_info/1]).

-spec(behaviour_info/1 :: (term()) -> list() | undefined).
behaviour_info(callbacks) ->
    [{split_file,  2},
     {split_event, 2}];
behaviour_info(_Other) ->
    undefined.
