%% Author: Administrator
%% Created: 2009-7-13
%% Description: TODO: Add description to file_utils
-module(file_utils).
-compile(export_all).



%%
%% API Functions
%%

read_file(File_path) ->
	case file:read_file(File_path) of
		{ok, Bin} ->
			binary_to_list(Bin);
		{error, Reason} ->
			io:format("wrong file path: ~p, error reason: ~p~n ", [File_path, Reason]),
			""
	end.




%%
%% Local Functions
%%

