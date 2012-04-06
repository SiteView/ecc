%% Author: Administrator
%% Created: 2009-7-3
%% Description: TODO: Add description to str_utils
-module(str_utils).
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
numeric_to_string(Numeric) ->
	case Numeric of
		N when erlang:is_integer(N) ->			
			erlang:integer_to_list(N);
		N when (erlang:is_float(N)) ->
			erlang:float_to_list(N);
		Str when (erlang:is_list(Str)) ->
			Str;
		true ->
			"n/a"
	end.

numeric_to_string(Numeric, Precise) ->
	N = numeric_round(Numeric, Precise),
	numeric_to_string(N).


%% -------------------------------------------------------------------------------%%
%% @spec string_to_numeric(String) -> numeric().
%% String = string(),
%% numeric() = integer() | float().

%% string_to_numeric(String) ->
%% 	if
%% 		erlang:is_float(String) ->
%% 			erlang:list_to_float(String);
%% 		erlang:is_integer(String) ->
%% 			Int = erlang:list_to_integer(String),
%% 			Float = erlang:float(Int),			
%% 			erlang:list_to_float(Float);
%% 		true ->
%% 			{error, String ++ "can not be converted into Numeric type. "}
%% 	end.



%% -------------------------------------------------------------------------------%%
%% spec numeric_round(Numeric, Precise) -> numeric() | {error, "args error"}.
%% Numeric = integer() | float(),
%% Precise = integer().

numeric_round(Numeric, Precise) ->
	case (erlang:is_float(Numeric) orelse erlang:is_float(Numeric)) andalso (erlang:is_integer(Precise)) of
		true ->			
			N = erlang:float(Numeric) * math:pow(10, Precise), 
			T = erlang:round(N),
			T / math:pow(10, Precise);
		false ->
			io:format("badarg, first arg: [~p] must be integer() or float(),  and second arg: [~p] must be ingeger(). return 0 as default. ", [Numeric, Precise]),
			0
	end.


%% --------------------------------------------------------------------------------%%
contain_spaces(String) ->
	Str = string:strip(String),
	case string:chr(Str, $\s) of
		0 ->
			false;
		_ ->
			true
	end.
	



