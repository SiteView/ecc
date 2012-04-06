%% ---
%%TopN  ½ø³Ì
%%
%%---
-module(topn_process, [BASE]).
-extends(topn_object).
-compile(export_all).

new() ->
    Obj = topn_object:new(),
	{?MODULE,Obj}.


