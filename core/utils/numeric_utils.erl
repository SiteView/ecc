%% Author: Administrator
%% Created: 2009-7-3
%% Description: TODO: Add description to str_utils
-module(numeric_utils).
-compile(export_all).

%%
%% Include files
%%


%%
%% API Functions
%%

%% -------------------------------------------------------------------------------%%
%% @spec numeric_to_string(Numeric) -> string().
%% Numeric = integer() | float().
%% numeric_to_string(Numeric) ->
%% 	if 
%% 		erlang:is_integer(Numeric) ->
%% 			Int = Numeric,
%% 			Float = erlang:float(Int),
%% 			erlang:float_to_list(Float);
%% 		erlang:is_float(Numeric) ->
%% 			erlang:float_to_list(Numeric);
%% 		true ->
%% 			{error, "n/a"}
%% 	end.


%% -------------------------------------------------------------------------------%%
%% @spec string_to_numeric(String) -> numeric().
%% String = string(),
%% numeric() = integer() | float().

string_to_numeric(String) ->
	if
		erlang:is_float(String) ->
			erlang:list_to_float(String);
		erlang:is_integer(String) ->
			Int = erlang:list_to_integer(String),
			Float = erlang:float(Int),			
			erlang:list_to_float(Float);
		true ->
			{error, String ++ "can not be converted into Numeric type. "}
	end.



%% -------------------------------------------------------------------------------%%
%% spec numeric_round(Numeric, Precise) -> numeric() | {error, "args error"}.
%% Numeric = integer() | float(),
%% Precise = integer().

numeric_round(Numeric, Precise) ->
	case (erlang:is_float(Numeric) orelse erlang:is_integer(Numeric)) andalso (erlang:is_integer(Precise)) of
		true ->			
			N = erlang:float(Numeric) * math:pow(10, Precise), 
			T = erlang:round(N),
			T / math:pow(10, Precise);
		false ->
			io:format("badarg, arg: [~p] and [~p] must be numeric. return 0 as default.\n", [Numeric, Precise]),
			0
	end.


%%	------------------------------------------------------------------------------%%
parse_numeric(Numeric_Str) when (is_list(Numeric_Str))->
	Index = string:chr(Numeric_Str, $.),
	try		
		if
			Index > 0 ->
				list_to_float(Numeric_Str);
			true ->
				Int = list_to_integer(Numeric_Str),
				Int * 1.0
		end
	catch
		_:_ ->
			io:format("badarg: ~p, parse numeric string error. return 0.0 as default. ~n", [Numeric_Str]),
			0.0
	end;
parse_numeric(Term) ->
	case Term of
		Term when (is_float(Term)) ->
			Term;
		Term when (is_integer(Term)) ->
			Term * 1.0;
		_ ->
			io:format("arg error, arg must be a numeric string. return 0.0 as default.\n"),
			0.0
	end.


	
			


