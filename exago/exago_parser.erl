%%%-------------------------------------------------------------------
%%% @author Edward Tate <edward.tate@erlang-solutions.com>
%%% @author Aniko Nagyne Vig <aniko@erlang-solutions.com>
%%% @author Atilla Erdodi <atilla@erlang-solutions.com>
%%% @author Hans Svensson
%%% @copyright (C) 2010, Erlang Solutions Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 15 Oct 2010 by Edward Tate <edward.tate@erlang-solutions.com>
%%%-------------------------------------------------------------------
%%% Copyright (c) 2009,2010 Erlang Solutions formerly Erlang Training & Consulting
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% * Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer. 
%%% * Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in the
%%%   documentation and/or other materials provided with the distribution.
%%% * Neither the name of the Erlang Solutions nor the names of its
%%%   contributors may be used to endorse or promote products
%%%   derived from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
%%% BE  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------
-module(exago_parser).

-export([parse_csv/1, parse_ts/3]).

-spec(parse_csv/1 :: (list()) -> list()).
parse_csv(FileName) ->
    parse_file(FileName, csv, [{delimiter, ","}]).

%%--- Exago1 code re-used below ------------------------------------------------

%% @spec init(List::list()) -> List2::list()
%% @doc Removes the tail of a list
-spec init(list()) -> list().
init(List) ->
    lists:reverse(tl(lists:reverse(List))).

%% @spec parse_file(File::string(), ParseFun::list(), ParseOpts::list()) ->
%% Data::list(tuple())
%% @doc Read and parse a CSV-logfile
-spec parse_file(string(), function(), list()) -> list(tuple()).
parse_file(File, ParseFun, ParseOpts) ->
    {ok, IoDev} = file:open(File, [read]),
    Data = parse_file(IoDev, file:read_line(IoDev), ParseFun, ParseOpts),
    file:close(IoDev),
    Data.

parse_file(_IoDev, eof, _ParseFun, _ParseOpts) ->
    [];
parse_file(IoDev, {ok, Str}, ParseFun, ParseOpts) ->
    [parse_str(init(Str), ParseFun, ParseOpts) |
     parse_file(IoDev, file:read_line(IoDev), ParseFun, ParseOpts)].

parse_str(Str, csv, ParseOpts) ->
    Delimiter = proplists:get_value(delimiter, ParseOpts),
    SStr = tokens(Str,Delimiter),
    list_to_tuple(SStr);
parse_str(Str, ParseFun, ParseOpts) ->
    try ParseFun(Str, ParseOpts) of
	Result ->
	    Result
    catch
	Class:Error ->
	    throw({parsefun_error, Class, Error})
    end.

%% @spec tokens(String::string(), Seperators::string()) ->
%% ListOfStrings::list(string())
%% @doc Return a list of tokens seperated by characters in Seperators.
%% Unlike the function in the string module,
%% it handles empty fields as well.
-spec tokens(string(), string()) -> list(string()).
tokens(S, Seps) ->
    tokens_acc(S, Seps, [], []).

tokens_acc([C|S], Seps, Toks, Cs) ->
    case lists:member(C, Seps) of
	true  -> tokens_acc(S, Seps, [lists:reverse(Cs)|Toks], []);
	false -> tokens_acc(S, Seps, Toks, [C|Cs])
    end;
tokens_acc([], _Seps, Toks, Cs) ->
    lists:reverse([lists:reverse(Cs)|Toks]).

%% @spec parse_ts(TsString::string(), Format::string() | atom(),
%% Offset::integer()) -> ParsedTs::tuple(integer())
%% @doc Parse timestamp string
-spec parse_ts(string()|tuple(), string() | atom, integer()) ->
		      {tuple(integer()), integer()}.
parse_ts(TimestampStr, TsParseFun, Offset) when is_function(TsParseFun)->
    case (catch TsParseFun(TimestampStr)) of
	{{{Year, Month, Day}, {Hour, Minute, Second}}, MicroSec} ->
	    case (catch add_offset({{{Year, Month, Day},
				     {Hour, Minute, Second}}, MicroSec}, Offset)) of
		{{{Year2, Month2, Day2}, {Hour2, Minute2, Second2}}, MicroSec2} ->
		    {{{Year2, Month2, Day2}, {Hour2, Minute2, Second2}}, MicroSec2};
		_ ->
		    parse_error
	    end;
	_ ->
	    parse_error
    end;
parse_ts(Timestamp, noparse, _Offset) ->
    Timestamp;
parse_ts({TimestampStr}, rfc, Offset) ->
    case httpd_util:convert_request_date(TimestampStr) of
	bad_date ->
	    parse_error;
	{{Year, Month, Day}, {Hour, Minute, Second}} ->
	    MicroSec = 0,
	    add_offset({{{Year, Month, Day}, {Hour, Minute, Second}}, MicroSec}, Offset)
    end;
parse_ts({MegaSecStr, SecStr, MicroSecStr}, erlang, Offset) ->
    case catch {list_to_integer(MegaSecStr),
		list_to_integer(SecStr),
		list_to_integer(MicroSecStr)} of
	{MegaSec, Sec, MicroSec} ->
	    {{Year, Month, Day}, {Hour, Minute, Second}} =
		calendar:now_to_datetime({MegaSec, Sec, MicroSec}),
	    add_offset({{{Year, Month, Day}, {Hour, Minute, Second}}, MicroSec}, Offset);
	_ ->
	    parse_error
    end;
parse_ts({TimestampStr}, FormatStr, Offset) ->
    case parse_ts_(TimestampStr, FormatStr) of
	parse_error ->
	    parse_error;
	{{Year, Month, Day, Hour, Minute, Second, SecFract}, FracLength} ->
	    MicroSec = trunc(SecFract * math:pow(10, 6 - FracLength)),
	    add_offset({{{Year, Month, Day},
			 {Hour, Minute, Second}}, MicroSec}, Offset)
    end.

parse_ts_(TimestampStr, FormatStr) ->
    case parse_ts_acc(TimestampStr, FormatStr, {[],[],[],[],[],[],[]}) of
	parse_error ->
	    parse_error;
	{StrList, FracLength} ->
	    ParsedList = [element(1, string:to_integer(Str)) || Str <- StrList],
	    case lists:any(fun(Int) -> Int == error end, ParsedList) of
		true -> parse_error;
		false -> {list_to_tuple(ParsedList), FracLength}
	    end
    end.

parse_ts_acc([], [], {Year, Month, Day, Hour, Minute, Second, SecFrac}) ->
    Month2 = case string:to_upper(lists:reverse(Month)) of
		 "JAN" -> "01";
		 "FEB" -> "02";
		 "MAR" -> "03";
		 "APR" -> "04";
		 "MAY" -> "05";
		 "JUN" -> "06";
		 "JUL" -> "07";
		 "AUG" -> "08";
		 "SEP" -> "09";
		 "OCT" -> "10";
		 "NOV" -> "11";
		 "DEC" -> "12";
		 Int -> Int
	     end,
    {[lists:reverse(Year), Month2, lists:reverse(Day),
      lists:reverse(Hour), lists:reverse(Minute), lists:reverse(Second),
      lists:reverse(SecFrac)], length(SecFrac)};
parse_ts_acc([], _, _) ->
    parse_error;
parse_ts_acc(_, [], _) ->
    parse_error;
parse_ts_acc([TsHead | TsRest], [FHead | FRest],
	     {Year, Month, Day, Hour, Minute, Second, SecFract}) ->
    case FHead of
	$y -> parse_ts_acc(TsRest, FRest,
			   {[TsHead | Year], Month, Day,
			    Hour, Minute, Second, SecFract});
	$M -> parse_ts_acc(TsRest, FRest,
			   {Year, [TsHead | Month], Day,
			    Hour, Minute, Second, SecFract});
	$d -> parse_ts_acc(TsRest, FRest,
			   {Year, Month, [TsHead | Day],
			    Hour, Minute, Second, SecFract});
	$h -> parse_ts_acc(TsRest, FRest,
			   {Year, Month, Day,
			    [TsHead | Hour], Minute, Second, SecFract});
	$m -> parse_ts_acc(TsRest, FRest,
			   {Year, Month, Day,
			    Hour, [TsHead | Minute], Second, SecFract});
	$s -> parse_ts_acc(TsRest, FRest,
			   {Year, Month, Day,
			    Hour, Minute, [TsHead | Second], SecFract});
	$f -> parse_ts_acc(TsRest, FRest,
			   {Year, Month, Day,
			    Hour, Minute, Second, [TsHead | SecFract]});
	%% Space in the format string indicates arbitrary character
	32 -> parse_ts_acc(TsRest, FRest,{Year, Month, Day,
					  Hour, Minute, Second, SecFract});
	TsHead -> parse_ts_acc(TsRest, FRest,
			       {Year, Month, Day,
				Hour, Minute, Second, SecFract});
	_Other -> parse_error
    end.

add_offset({{{Year, Month, Day}, {Hour, Minute, Second}}, MicroSec}, Offset) ->
    MSSum = MicroSec + Offset,
    MicroSec2 = MSSum rem 1000000,
    MSDiv = MSSum div 1000000,
    SecSum = Second + MSDiv,
    Second2 = SecSum rem 60,
    SecDiv = SecSum div 60,
    MinSum = Minute + SecDiv,
    Minute2 = MinSum rem 60,
    MinDiv = MinSum div 60,
    Hour2 = Hour + MinDiv,
    {{{Year, Month, Day}, {Hour2, Minute2, Second2}}, MicroSec2}.
