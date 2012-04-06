 
-module(sec_html_util).

-export([escape/1]).
-export([unescape/1]).  
escape(S) when is_list(S) ->    escape(S, []). 

escape([], Acc) ->    lists:reverse(Acc);
escape([$<|Rest], Acc) ->    escape(Rest, lists:reverse("&lt;", Acc));
escape([$>|Rest], Acc) ->    escape(Rest, lists:reverse("&gt;", Acc));
escape([$&|Rest], Acc) ->    escape(Rest, lists:reverse("&amp;", Acc));
escape([$\"|Rest], Acc) ->    escape(Rest, lists:reverse("&quot;", Acc));
escape([C|Rest], Acc) ->    escape(Rest, [C|Acc]).

unescape(S) when is_list(S) ->    unescape(S, []). 
unescape("&lt;" ++ Rest, Acc) ->    unescape(Rest, [$<|Acc]);
unescape("&gt;" ++ Rest, Acc) ->    unescape(Rest, [$>|Acc]);
unescape("&amp;" ++ Rest, Acc) ->    unescape(Rest, [$&|Acc]);
unescape("&quot;" ++ Rest, Acc) ->    unescape(Rest, [$"|Acc]);
unescape([H|R], Acc) ->    unescape(R, [H|Acc]);
unescape([], Acc) ->lists:reverse(Acc).