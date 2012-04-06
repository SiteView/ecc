-module(monitor_template).
-compile(export_all).

get_templates()->
	case file:consult("conf/monitor_template.conf") of
		{ok,R}->
			F = fun(X,Y)->element(2,X)<element(2,Y) end,
			lists:sort(F,R);
		_->
			[]
	end.
	
get_template_name(Key)->
	case file:consult("conf/monitor_template.conf") of
		{ok,R}->
			case lists:keysearch(Key,1,R) of
				{value,Mt}->
					element(2,Mt);
				_->
					""
			end;
		_->
			""
	end.
	
get_template_platform(Key)->
	case file:consult("conf/monitor_template.conf") of
		{ok,R}->
			case lists:keysearch(Key,1,R) of
				{value,Mt}->
					proplists:get_value(platform,element(3,Mt),[]);
				_->
					""
			end;
		_->
			""
	end.	