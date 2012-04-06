-module(topnProc_memoryHighest, [BASE]).
-extends(topn_atomic).

-compile(export_all).

new() ->
    Obj = topn_atomic:new(),
	{?MODULE,Obj}.
    
update() ->
    %%io:format("topnProc_memoryHighest Update~n"),
    BASE:update(),
    ok.