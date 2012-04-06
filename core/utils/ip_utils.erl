-module(ip_utils).
-compile(export_all).

check_ip(Str)->
	Tokens = string:tokens(Str,"."),
	if
		length(Tokens)=/=4 ->
			{error,format_error};
		true ->
			F = fun(X)->
				case string:to_integer(X) of
					{V,[]}->
						if
							V>=0 andalso V =<255->
								true;
							true ->
								false
						end;
					_->
						false
				end
			end,
			Ret = lists:map(F,Tokens),
			case lists:member(false,Ret) of
				true->
					{error,ip_format_error};
				_->
					{ok,is_ip}
			end
	end.

