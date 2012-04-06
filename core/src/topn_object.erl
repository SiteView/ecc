-module(topn_object, [BASE]).
-extends(topn_attr_object).
-compile(export_all).

new() ->
    Obj = topn_attr_object:new(),
	{?MODULE,Obj}.