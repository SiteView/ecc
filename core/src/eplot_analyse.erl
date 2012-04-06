

-module(eplot_analyse).
-compile(export_all).


fileconsult() ->
	Path= ".",
    File = filename:join([Path, "eplot.config"]),
    case file:consult(File) of
	{ok, Terms} -> Terms;
	{error, enoent} -> void
    end.

translate_xdatetime_to_number(Filename)->
	case read_data_file(Filename) of
    	{ok, Data} ->
			{ Min, Max, Data2 }= datetime_to_seconds(Data, [], calendar:datetime_to_gregorian_seconds({{3000,1,1},{0,0,0}}) , 0 ),
			lists:sort(Data2);
%% 		    datetime_to_number(Data2, [], Min, Max-Min);
		Failed ->
			Failed
	end.	

datetime_to_number([], Ret, Min, Range)->
	lists:reverse(Ret);
datetime_to_number([H|T], Ret, Min, Range) when Range=:=0 ->
	{X,Y}= H,
	datetime_to_number(T, [{5,Y}|Ret], Min, Range );
datetime_to_number([H|T], Ret, Min, Range)->
	{X,Y}= H,
	datetime_to_number(T, [{10*(X-Min)/Range , Y}|Ret], Min, Range ).

datetime_to_seconds([], Ret, Min, Max)->
	{ Min, Max, lists:reverse(Ret) };
datetime_to_seconds([H|T], Ret, Min, Max)->
	case catch to_seconds(H, Min, Max) of
		{'EXIT', _} -> datetime_to_seconds(T, Ret, Min, Max);
		{ok, I, Min2, Max2 } -> datetime_to_seconds(T, [I|Ret], Min2, Max2)
    end.
  
to_seconds( {X,Y}, Min, Max ) when is_integer(Y) orelse is_float(Y)->
	S= calendar:datetime_to_gregorian_seconds(X),
	case S<Min of
		true ->
			NMin= S;
		_ ->
			NMin= Min
	end,
	case S>Max of
		true ->
			NMax= S;
		_ ->
			NMax= Max
	end,	
	{ok, {S,Y}, NMin, NMax}.



%% file line example (前面的日期部分不可以有空格) ：{{2009,1,29},{10,20,57}} 10
%% return: {ok,[{'{{2009,1,29},{10,20,57}}','10'}, {'{{2009,10,29},{10,20,57}}','7'}]}
%% return: {error,{"data1.txt",{error,enoent}}}
read_data_file(Filename) ->
	case file:open(Filename, [read]) of
    	{ok, Fd} ->
		    { ok, parse_data_file(Fd, io:get_line(Fd, ""), []) };
		Failed ->
			{ error, {Filename, Failed} }
	end.

parse_data_file(Fd, eof, Out) -> file:close(Fd), lists:reverse(Out);
parse_data_file(Fd, String, Out) ->
    %% expected string is 'number()<whitespace(s)>number()'
    Tokens = string:tokens(String, " \t\n\r"),
	case length(Tokens) < 2 of
		true -> 
			parse_data_file(Fd, io:get_line(Fd, ""), Out);
		_ ->
		    Item = tokens2item(Tokens),
    		parse_data_file(Fd, io:get_line(Fd, ""), [Item|Out])
	end.
    
tokens2item(Tokens) ->
    Is = lists:map(fun
	(String) ->
	    list_to_term(String)
	end, Tokens),
    [X,Y|_] = Is,
    {X,Y}.

list_to_term(String) ->
    case catch list_to_term_2(String) of
		{'EXIT', _} -> 
			case catch list_to_atom(String) of
				{'EXIT', _} ->
					list_to_atom(String);
				A ->
					A
			end;
		I -> I
    end.

list_to_term_2(String) ->
    case catch list_to_number(String) of
		{'EXIT', _} -> 
			list_to_date_time(String);
		I -> I
    end.

list_to_number(String) ->
    case catch list_to_integer(String) of
		I when is_integer(I) -> I;
		_ -> list_to_float(String)
    end.

%% could trow exception
list_to_date_time(String) ->
	Tokens = string:tokens( stripBrace(String), ","),
	tokens2date(Tokens).

%% could trow exception, such as inputting 4 elements:   "{{2009},{10,20,57}}"
tokens2date(Tokens) ->
    Is = lists:map(fun
	(String) ->
	    list_to_number(String)
	end, Tokens),
    [A,B,C,D,E,F|_] = Is,
    {{A,B,C},{D,E,F}}.

stripBrace(String)->
	stripBrace(String, []).

stripBrace([],Str) ->
	lists:reverse(Str);
stripBrace([H|T], Str) when H =:= ${ orelse H =:= $} ->
	stripBrace(T, Str);
stripBrace([H|T], Str) ->
	stripBrace(T, [H|Str]).

	
get_topN_text(Filename)->
    {ok, Fd} = file:open(Filename, [read]),
    parse_data_file_topN_text(Fd, io:get_line(Fd, ""), []).


parse_data_file_topN_text(Fd, eof, Out) -> file:close(Fd), lists:reverse(Out);
parse_data_file_topN_text(Fd, String, Out) ->
    % expected string is 'number()<whitespace(s)>number()'
	case catch try_get_topN_text(String) of
		{'EXIT', _} -> 
			parse_data_file_topN_text(Fd, io:get_line(Fd, ""), Out);
		I -> 
			parse_data_file_topN_text(Fd, io:get_line(Fd, ""), [I|Out])
    end.
    
try_get_topN_text(String)->
	Tokens = string:tokens(String, " \t\n\r"),
	Last= lists:last(Tokens),
	{string:left(String, length(String)-length(Last)-2),list_to_term(Last)}.	


