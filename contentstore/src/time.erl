-module(time).
-export([add/2, compare/2, to_string/0, to_string/1, to_time/1, getUTCTime/0]).

add({X, Y, Z}, {second, Value})->
    Temp = Y + Value,
    {X + (Temp div 1000000), Temp rem 1000000, Z}.

compare({X1, _, _}, {X2, _, _})when X1 > X2->greater;
compare({X1, _, _}, {X2, _, _})when X1 < X2 -> less;
compare({X, Y1, _}, {X, Y2, _})when Y1 > Y2 -> greater;
compare({X, Y1, _}, {X, Y2, _})when Y1 < Y2 -> less;
compare({X, Y, Z1}, {X, Y, Z2})when Z1 > Z2 -> greater;
compare({X, Y, Z1}, {X, Y, Z2})when Z1 > Z2 -> less;
compare(_, _)->equal.

% 2008-05-20T04:49:53.435Z
% 2008-05-26T09:17:11.146Z

getUTCTime() ->
    {MegaSecs, Secs, MicroSecs} = now(),
    DateTime = calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    calendar:datetime_to_gregorian_seconds(DateTime) * 1000000 + MicroSecs.
    
to_string() ->
    to_string(now()).

to_string(Time)when is_integer(Time)->
    Seconds = Time div 1000000 - 62167219200, %62167219200 = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).
    MegaSecs = Seconds div 1000000,
    Secs = Seconds rem 1000000,
    MicroSecs = Time rem 1000000,
    to_string({MegaSecs, Secs, MicroSecs});
    
to_string({MegaSecs, Secs, MicroSecs}) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~3..0wZ",[Year, Month, Day, Hour, Minute, Second, MicroSecs div 1000])).

% yyyy-mm-ddThh:mm:ss[.sss]{Z|{+|-}hh:mm} -> {MegaSecs, Secs, MicroSecs}

%to_time(TimeStr) ->
%    [Date, Time] = string:tokens(TimeStr, "T"),
%    D = parse_date(Date),
%    {T, MS} = parse_time(Time),
%    Seconds = calendar:datetime_to_gregorian_seconds({D, T}),
%    Seconds * 1000 + MS.
%
%% yyyy-mm-dd
%parse_date(Date) ->
%    [Y, M, D] = string:tokens(Date, "-"),
%    {list_to_integer(Y), list_to_integer(M), list_to_integer(D)}.
%
%% hh:mm:ss.sssZ
%parse_time(Time) ->
%    Len = length(Time),
%    TimeString = lists:sublist(Time, 1, Len -1),
%    [HMS_String | MS_String] =  string:tokens(TimeString, "."),
%    MS = case MS_String of
%         [] ->
%             0;
%         [Val] ->
%             list_to_integer(string:left(Val, 6, $0))
%    end,
%    [H, M, S] = lists:map(fun(X)->list_to_integer(X) end, string:tokens(HMS_String, ":")),
%    {{H, M, S}, MS}.

%to_time(TimeStr) ->
%    parse_datetime(TimeStr).

to_time(TimeStr) ->
    [Date, Time] = string:tokens(TimeStr, "T"),
    D = parse_date(Date),
    {T, MS, TZH, TZM} = parse_time(Time),
    S = calendar:datetime_to_gregorian_seconds({D, T}),
    Seconds = S - TZH * 60 * 60 - TZM * 60,
%    S1 = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
%    Seconds = (S - S1) - TZH * 60 * 60 - TZM * 60,
%    {Seconds div 1000000, Seconds rem 1000000, MS}.
    Seconds * 1000000 + MS.

% yyyy-mm-dd
parse_date(Date) ->
    [Y, M, D] = string:tokens(Date, "-"),
    Date1 = {list_to_integer(Y), list_to_integer(M), list_to_integer(D)},
    case calendar:valid_date(Date1) of
	true ->
	    Date1;
	_ ->
	    false
    end.

% hh:mm:ss[.sss]TZD
parse_time(Time) ->
    case string:str(Time, "Z") of
	0 ->
	    parse_time_with_timezone(Time);
	_ ->
	    [T | _] = string:tokens(Time, "Z"),
	    {TT, MS} = parse_time1(T),
	    {TT, MS, 0, 0}
    end.

parse_time_with_timezone(Time) ->
    case string:str(Time, "+") of
	0 ->
	    case string:str(Time, "-") of
		0 ->
		    false;
		_ ->
		    parse_time_with_timezone(Time, "-")
	    end;
	_ ->
	    parse_time_with_timezone(Time, "+")
    end.

parse_time_with_timezone(Time, Delim) ->
    [T, TZ] = string:tokens(Time, Delim),
    {TZH, TZM} = parse_timezone(TZ),
    {TT, MS} = parse_time1(T),
    case Delim of
	"-" ->
	    {TT, MS, -TZH, -TZM};
	"+" ->
	    {TT, MS, TZH, TZM}
    end.

parse_timezone(TZ) ->
    [H, M] = string:tokens(TZ, ":"),
    {[H1, M1], true} = check_list([{H, 12}, {M, 60}]),
    {H1, M1}.

parse_time1(Time) ->
    [HMS | T] =  string:tokens(Time, "."),
    MS = case T of
	     [] ->
		 0;
	     [Val] ->
		 list_to_integer(string:left(Val, 6, $0))
	 end,
    [H, M, S] = string:tokens(HMS, ":"),
    {[H1, M1, S1], true} = check_list([{H, 24}, {M, 60}, {S, 60}]),
    {{H1, M1, S1}, MS}.

check_list(List) ->
    lists:mapfoldl(
      fun({L, N}, B)->
	  V = list_to_integer(L),
	  if
	      (V >= 0) and (V =< N) ->
		  {V, B};
	      true ->
		  {false, false}
	  end
      end, true, List).
