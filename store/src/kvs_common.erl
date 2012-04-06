-module(kvs_common).
-compile(export_all).

trace(X, Module, Line)->
    io:format("[~p.erl](~p)=>~p~n", [Module, Line, X]).
trace(X, Y, Module, Line)->
    io:format("[~p.erl](~p)=>" ++ X, [Module, Line] ++ Y).

id()->
     % 63429523200 = calendar:datetime_to_gregorian_seconds({{2010, 1, 1},{0, 0, 0}}).
     TimeSpan = calendar:datetime_to_gregorian_seconds(calendar:local_time()) - 63429523200,
     {X, Y, Z} = erlang:now(),
     MicroSecs = Z,
     random:seed(X, Y, Z),
     Random = random:uniform(9),
     TimeString = lists:flatten(io_lib:format("~9..0w~6..0w~1..0w",[TimeSpan, MicroSecs, Random])),
     list_to_integer(TimeString).