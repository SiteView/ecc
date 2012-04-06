%%%-------------------------------------------------------------------
%%% @author Edward Tate <edward.tate@erlang-solutions.com>
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
-module(exago_util).

-export([delete_all/2, dot_to_png/1, all_defined/1]).
-export([extract_fields/2]).
-export([delta_time/2]).
-export([is_duplicate/2]).
-export([now_timestamp/0]).
-export([write_csv_event/2]).

%% @doc Deletes all the tuples found in the property list with the given 
%% keys. 
-spec(delete_all/2 :: (list(), list()) -> list()).
delete_all([], List) ->
    List;
delete_all([K|Keys], List) ->
    delete_all(Keys, proplists:delete(K, List)).

%% @doc Tries to find the dot program, if it is found then converts a .dot
%% file to a .png file. The filename should not include an extension.
-spec(dot_to_png/1 :: (list()) -> ok).
dot_to_png(FileName) ->
    case os:find_executable("dot") of
	false ->
	    io:format("Graphviz 'dot' was not found, please install it in order to see state machine visualizations.");
	_     ->
	    os:cmd("dot -T png " ++ FileName ++ ".dot -o " ++ FileName ++ ".png")
    end.

%% @doc Are all the elements of the list defined? 
-spec(all_defined/1 :: (list()) -> true | false).
all_defined([]) ->
    true;
all_defined([X|List]) ->
    case X of
	undefined ->
	    false;
	_         ->
	    all_defined(List)
    end.

%% @doc Internal function which extracts the fields from an already parsed row by property
-spec(extract_fields_/3 :: (list(), list(), list()) -> list()).
extract_fields_([], _, Acc) ->
    lists:reverse(Acc);
extract_fields_([F|Fields], ParsedRow, Acc) ->
    extract_fields_(Fields, ParsedRow, [{F, proplists:get_value(F, ParsedRow, false)}|Acc]).

%% @doc Interface to internal extract_fields_
-spec(extract_fields/2 :: (list(), list()) -> list()).
extract_fields(Fields, Row) ->
    extract_fields_(Fields, Row, []).

%% @doc Calculate the difference in time between two timestamps
-spec(delta_time/2 :: (tuple(), tuple() | 0) -> integer()).
delta_time({_D1, _M1}, 0) ->
    0;
delta_time({{_Mega1,_Sec1,_MilliSec1}}, 0) ->
    0;
delta_time({{Mega1,Sec1,MilliSec1}}, {{Mega2,Sec2,MilliSec2}}) ->
	timer:now_diff({Mega1,Sec1,MilliSec1}, {Mega2,Sec2,MilliSec2})/1000000;
delta_time({D1, M1}, {D2, M2}) ->
    {{Days, {Hours, Minutes, Seconds}}, MicroSeconds} = {calendar:time_difference(D2, D1), M1-M2},
    DeltaS = Seconds * 1000000,
    DeltaM = Minutes * 60 * 1000000,
    DeltaH = Hours * 60 * 60 * 1000000,
    DeltaD = Days * 24 * 60 * 60 * 1000000,
    (DeltaD + DeltaH + DeltaM + DeltaS + MicroSeconds) / 1000000.

%% @doc Checks whether an item is duplicated in a list
-spec(is_duplicate/2 :: (any(), list()) -> boolean()).
is_duplicate(_X, []) ->
    false;
is_duplicate(X, [X|_]) ->
    true;
is_duplicate(X, [_|T]) ->
    is_duplicate(X, T).

%% @doc Generates a timestamp as a string
now_timestamp() ->
    {{Year, Month, Day}, Time} = calendar:now_to_local_time(now()),
    {Hour, Minute, Second}     = Time,
    %% hack
    IOList = io_lib:format("~p-~p-~p ~p:~p:~p:~p", [Year, Month, Day, Hour, Minute, Second, 0]),
    lists:flatten(IOList).

%% @doc Writes an event to a file in csv format
write_csv_event(IoDevice, []) ->
    io:format(IoDevice, "~n", []);
write_csv_event(IoDevice, [String|[]]) ->
    io:format(IoDevice, "~s", [String]),
    write_csv_event(IoDevice, []);
write_csv_event(IoDevice, [String|Rest]) ->
    io:format(IoDevice, "~s,", [String]),
    write_csv_event(IoDevice, Rest).

    
