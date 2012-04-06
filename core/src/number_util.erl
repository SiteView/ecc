-module(number_util).
-compile(export_all).

get_value(Value) ->
	if
		is_list(Value) ->
			case re:run(Value, "^-*[0-9]+$") of
				nomatch ->
					case re:run(Value, "^-*[0-9]+\\.[0-9]+$") of
						nomatch ->
							if
								length(Value) == 0 ->
									"n/a";
								true ->
									Value
							end;
						_ ->
							list_to_float(Value)
					end;
				_ ->
					list_to_integer(Value)
			end;
		true ->
			Value
	end.
	