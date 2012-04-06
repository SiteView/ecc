-module(parse).
-export([scan/1]).

-ifdef(debug).
-export([test/0, test/1,test1/0, test2/0, test3/0, test4/0]).
-endif.

-include("config.hrl").

%   =       equal to 
%   <>      not equal to 
%   eic     equal to, ignoring case 
%   neic    not equal to, ignoring case 
%   >       greater than 
%   <       less than 
%   >=      greater than or equal to 
%   <=      less than or equal to 
%   like    full-text search 
%   likeic  full-text search, ignoring case 
%   in      in a list of values 

%%"my.publishStatus>= 'publish'&author = '1j0hpe79jbdh4'&from = 0&to = 5&order = my.publishTime@D"
%%QueryString:"application = 'siteview'&type eic 'BlogPost'&(my.visibility = 'all'|(my.visibility = 'friends'&author in friends('1j0hpe79jbdh4'))|(my.visibility in ['me','friends']&author = '1j0hpe79jbdh4'))&my.publishStatus>= 'publish'&author = '1j0hpe79jbdh4'&from = 0&to = 5&order = my.publishTime@D"

%Condition = [
%		{begin, {"application", "=", "'siteview'"}},
%		{and, {"type", "eic", "BlogPost"}},
%		{and, [
%			{begin, {"my.visibility", "=", "all"}},
%			{or, [
%				{begin, {"my.visibility", "=", "friends"}},
%				{and, {"author", "in", "friends('1j0hpe79jbdh4')"}}
%			     ]
%                        },
%			{or, [
%				{begin, {"my.visibility", "in", "['me','friends']"}},
%				{and, {"author", "=", "'1j0hpe79jbdh4'"}}
%			     ]
%                        }
%		      ]
%		},
%		{and, {"my.publishStatus", ">=", "'publish'"}},
%		{and, {"author", ">=", "'publish'"}},
%		{and, {"from", "=", "0"}},
%		{and, {"to", "=", "5"}},
%		{and, {"order", "=", "my.publishTime@D"}}
%	     ].
    
test(N)->
    L = lists:seq(1, N),
    lists:map(fun(X)->X,test()end, L).

test()->
	T1 = test1(),
	io:format("~p~n", [T1]),
	io:format("~n"),
	T2 = test2(),
	io:format("~p~n", [T2]),
	io:format("~n"),
	T3 = test3(),
	io:format("~p~n", [T3]),
	io:format("~n"),
	T4 = test4(),
	io:format("~p~n", [T4]).

test1()->
	io:format("~n"),
	io:format("~p~n", ["=========================================================================="]),
	QS = "type eic 'BlogPost'&(my.visibility = 'all'|(my.visibility = 'friends'&author in friends('1j0hpe79jbdh4'))|(my.visibility in ['me','friends']&author = '1j0hpe79jbdh4'))&my.publishStatus>= 'publish'",
	io:format("QS: ~p~n", [QS]),
	io:format("~p~n", ["--------------------------------------------------------------------------"]),
	scan(QS).

test2()->
	io:format("~n"),
	io:format("~p~n", ["=========================================================================="]),
	QS = "t eic 'B'&(my.v = 'all'|(my.v = 'f'&author in fs('1j'))|(my.v in ['me','fs']&author = '1j'))&my.p>= 'p'",
	io:format("QS: ~p~n", [QS]),
	io:format("~p~n", ["--------------------------------------------------------------------------"]),
	scan(QS).

test3()->
	io:format("\n"),
	io:format("~p~n", ["=========================================================================="]),
	QS = "(type eic 'User'&author != null&my.xg_index_status != 'blocked'&my.xg_index_status != 'pending')&from=0&to=8&order=updated@D",
	io:format("QS: ~p~n", [QS]),
	io:format("~p~n", ["--------------------------------------------------------------------------"]),
	scan(QS).

test4()->
	io:format("~n"),
	io:format("~p~n", ["=========================================================================="]),
	QS = "((my.conversionStatus = null|my.conversionStatus = 'complete')&type = 'Video'&id != 2108193:Video:1271&my.searchText likeic null)&from=0&to=5&order=published@D",
	io:format("QS: ~p~n", [QS]),
	io:format("~p~n", ["--------------------------------------------------------------------------"]),
	scan(QS).


scan(QS)->
    scan(QS, []).

scan([], Parent)->
	lists:reverse(Parent);
scan([$ |R], Parent)->
	scan(R, Parent);
scan([$(|R], [])->
    ScanGroupResult = scanGroup(R),
    case ScanGroupResult of
        {ok, Group, Rest}->
            scan(Rest, [{?Begin, Group}]);
         _ ->
            ScanGroupResult
    end;
scan([$&|R], Parent)->
    ScanNextResult = scanNext(R),
    case ScanNextResult of
        {ok, Item, Rest}->
            scan(Rest, [{?And, Item}|Parent]);
        _ ->
            ScanNextResult
    end;
scan([$||R], Parent)->
    ScanNextResult = scanNext(R),
    case ScanNextResult of
        {ok, Item, Rest}->
            scan(Rest, [{?Or, Item}|Parent]);
        _ ->
            ScanNextResult
    end;
scan(QS, Parent)->
    ParseResult = parse(QS),
    case ParseResult of
        {ok, Item, Rest}->
            scan(Rest, [{?Begin, Item}|Parent]);
        _ ->
            ParseResult
    end.
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scanNext([$ |R])->
    scanNext(R);
scanNext([$(|R])->
    scanGroup(R);
scanNext([])->
    {error, scan, "data interrupted"};
scanNext(QS)->
    parse(QS).

scanGroup([$ |R])->
    scanGroup(R);
%scanGroup([$(|R], [$(|Rest])->
%    scanGroup(R, [$(] ++ [$(|Rest]);
scanGroup([$(|R])->
    ScanGroupResult = scanGroup(R),
    case ScanGroupResult of
        {ok, Group, Rest}->
            %%{ok, {?Begin, Group}, Rest};
	    scanGroup(Rest, [{?Begin, Group}]);
         _ ->
            ScanGroupResult
    end;
scanGroup(QS)->
    ParseResult = parse(QS),
    case ParseResult of
        {ok, Item, Rest}->
            Group = [{?Begin, Item}],
            scanGroup(Rest, Group);
        _ ->
            ParseResult
    end.

scanGroup([$ |R], Parent)->
    scanGroup(R, Parent);
scanGroup([$)|R], Parent)->
    {ok, lists:reverse(Parent), R};
scanGroup([$&|R], Parent)->
    ScanNextResult = scanNext(R),
    case ScanNextResult of
        {ok, Item, Rest}->
            scanGroup(Rest, [{?And, Item}|Parent]);
        _ ->
            ScanNextResult
    end;
scanGroup([$||R], Parent)->
    ScanNextResult = scanNext(R),
    case ScanNextResult of
        {ok, Item, Rest}->
            scanGroup(Rest, [{?Or, Item}|Parent]);
        _ ->
            ScanNextResult
    end;
scanGroup(_, _)->
    {error, group, "data interrupted"}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse(QS)->
    ParseKeyResult = parseKey(QS, []),
    case ParseKeyResult of
        {ok, Key, Rest1} ->
            ParseOperResult = parseOper(Rest1),
            case ParseOperResult of
                {ok, Oper, Rest2}->
                    ParseValueResult = parseValue(Rest2, []),
                    case ParseValueResult of
                        {ok, Value, Rest3} ->
                            {ok, {Key, Oper, Value}, Rest3};
                        _ ->
                            ParseValueResult
                    end;
                _ ->
                    ParseOperResult
            end;
        _ ->
            ParseKeyResult
    end.

parseKey([$ |R], [])->
	parseKey(R, []);
parseKey([$ |R], Stack)->
	{ok, lists:reverse(Stack), [$ |R]};
parseKey([C|R], Stack)->
    case name_char(C) of
        true ->
            parseKey(R, [C|Stack]);
        _ ->
            {ok, lists:reverse(Stack), [C|R]}
    end;
parseKey([], _)->
    {error, key, "data interrupted"}.

parseOper([$ |R])->
    parseOper(R);

%=:equal to 
parseOper("=" ++ Rest)->
    {ok, "=", Rest};

%in:in a list of values 
parseOper("in" ++ Rest)->
    {ok, "in", Rest};

%<>:not equal to 
parseOper("!=" ++ Rest)->
    {ok, "!=", Rest};

%eic:equal to, ignoring case 
parseOper("eic" ++ Rest)->
    {ok, "eic", Rest};

%neic:not equal to, ignoring case 
parseOper("neic" ++ Rest)->
    {ok, "neic", Rest};

%<=:less than or equal to 
parseOper("<=" ++ Rest)->
    {ok, "<=", Rest};

%<:less than 
parseOper("<" ++ Rest)->
    {ok, "<", Rest};

%>=:greater than or equal to 
parseOper(">=" ++ Rest)->
    {ok, ">=", Rest};

%>:greater than 
parseOper(">" ++ Rest)->
    {ok, ">", Rest};

%likeic:full-text search, ignoring case 
parseOper("likeic" ++ Rest)->
    {ok, "likeic", Rest};

%like:full-text search 
parseOper("like" ++ Rest)->
    {ok, "like", Rest};

%like:full-text search 
parseOper("!like" ++ Rest)->
    {ok, "!like", Rest};

parseOper([])->
    {error, oper, "data empty"};
parseOper(Rest)->
    {error, oper, Rest}.

parseValue([$ |R], [])->
    parseValue(R, []);
parseValue([$(|R], Stack)->
    parseValue(R, [$(|Stack], [$(]);
parseValue([$'|R], Stack)->
    parseValue(R, [$'|Stack], [$']);
parseValue([$[|R], Stack)->
    parseValue(R, [$[|Stack], [$[]);
parseValue([$ |R], Stack)->
    {ok, lists:reverse(Stack), R};
parseValue([$&|R], Stack)->
    {ok, lists:reverse(Stack), [$&|R]};
parseValue([$||R], Stack)->
    {ok, lists:reverse(Stack), [$||R]};
parseValue([$)|R], Stack)->
    {ok, lists:reverse(Stack), [$)|R]};
parseValue([C|R], Stack)->
    parseValue(R, [C|Stack]);
%parseValue([], [])->
%    {error, value, "data interrupted"};
parseValue([], Stack)->
    {ok, lists:reverse(Stack), []}.

parseValue([$'|R], Stack, [$'|Rest])->
    case Rest of
        [] ->
            %%parseValue(R, Stack ++ [$']);
            {ok, lists:reverse([$'|Stack]), R};
        _ ->
            parseValue(R, [$'|Stack], Rest)
    end;
parseValue([$'|_], _, [])->
    {error, value, "' not match"};
parseValue([$'|R], Stack, Symbol)->
    parseValue(R, [$'|Stack], [$'|Symbol]);

parseValue([$)|R], Stack, [$(|Rest])->
    case Rest of
        [] ->
            %%parseValue(R, Stack ++ [$)]);
            {ok, lists:reverse([$)|Stack]), R};
        _  ->
            parseValue(R, [$)|Stack], Rest)
    end;
parseValue([$)|R], Stack, [$'|Rest])->
    parseValue(R, [$)|Stack], [$'|Rest]);
parseValue([$)|_], _, _)->
    {error, value, " ) not match"};

parseValue([$(|R], Stack, [$'|Rest])->
    parseValue(R, [$(|Stack], [$'|Rest]);
parseValue([$(|R], Stack, Symbol)->
    parseValue(R, [$(|Stack], [$(|Symbol]);
    
parseValue([$]|R], Stack, [$[|Rest])->
    case Rest of
        [] ->
            %%parseValue(R, Stack ++ [$)]);
            {ok, lists:reverse([$]|Stack]), R};
        _  ->
            parseValue(R, [$]|Stack], Rest)
    end;
parseValue([$]|R], Stack, [$'|Rest])->
    parseValue(R, [$[|Stack], [$'|Rest]);
parseValue([$]|_], _, _)->
    {error, value, " [ not match"};
    
parseValue([$[|R], Stack, [$'|Rest])->
    parseValue(R, [$[|Stack], [$'|Rest]);
parseValue([$[|R], Stack, Symbol)->
    parseValue(R, [$[|Stack], [$[|Symbol]);

parseValue([$&|R], Stack, [$'|Rest])->
    parseValue(R, [$&|Stack], [$'|Rest]);
parseValue([$&|R], Stack, [])->
    parseValue([$&|R], Stack);
parseValue([$&|_], _, _)->
    {error, value, "data interrupted"};

parseValue([C|R], Stack, Symbol)->
    parseValue(R, [C|Stack], Symbol);
parseValue([], Stack, [])->
    {ok,  lists:reverse(Stack), []};
parseValue([], _, _)->
    {error, value, "data interrupted"}.

name_char(C) when C >= $a, C =< $z -> true;
name_char(C) when C >= $A, C =< $Z -> true;
name_char(C) when C >= $0, C =< $9 -> true;
name_char($.) -> true;
name_char($_) -> true;
name_char(_) -> false.
