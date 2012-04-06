%% ---
%%propertied_object_static
%%
%%---
-module(propertied_object_static).
-compile(export_all).

loop(P,A)->
	receive
		{From,stop}->
			From ! {self(),{ok,stopped}};
		{From,{set_attribute,{Name,Val}}}->
			From ! {self(),{ok,set_attribute_ok}},
			loop(P,set_attribute(Name,Val,A));
		{From,{remove_attribute,Name}}->
			case lists:keysearch(Name,1,A) of
					false->
						From ! {self(),{error,{Name,not_found}}},
						loop(P,A);
					{value,{Name,_}}->
						NA = [{N,V}||{N,V}<-A,N=/=Name],
						From ! {self(),{ok,{Name,removed}}},
						loop(P,NA)
			end;
		{From,{remove_attribute_match,Reg}}->
			NA = try
				[{N,V}||{N,V}<-A,nomatch=:=re:run(case is_list(N) of true-> N;_->lists:flatten(io_lib:format("~p",[N])) end,Reg)]
				catch
					_:_->
						A
				end,
			From ! {self(),{ok,{Reg,removed}}},
			loop(P,NA);
		{From,remove_attributes}->
			From ! {self(),{ok,remove_attributes_ok}},
			loop(P,[]);
		{From,{get_attribute,Name}}->
			From ! {self(),get_attribute(Name,A)},
			loop(P,A);
		{From,get_attributes}->
			From ! {self(),A},
			loop(P,A);
		{From,{add_properties,Properties}}->
			case is_list(Properties) of
				true->	
					From ! {self(),{ok,add_properties_ok}},
					% loop(P ++ Properties,A);
					loop(merge_attributes(P, Properties),A);
				_->
					From ! {self(),{error,properties_is_not_list}},
					loop(P,A)
			end;
		{From,{add_attributes,Attributes}}->
			case is_list(Attributes) of
				true->	
					From ! {self(),{ok,add_attributes_ok}},
					% loop(P,A ++ Attributes);
					loop(P,merge_attributes(A, Attributes));
				_->
					From ! {self(),{error,attributes_is_not_list}},
					loop(P,A)
			end;
		{From,{get_property,Key}}->
			From ! {self(),get_attribute(Key,P)},
			loop(P,A);
		{From,remove_properties}->
			From ! {self(),{ok,remove_properties_ok}},
			loop([],A);
		{From,get_properties}->
			From ! {self(),P},
			loop(P,A);
		{From,{set_property,{Key,Val}}}->
			From ! {self(),{ok,set_property_ok}},
			loop(set_attribute(Key,Val,P),A);
		{From,{remove_property,Key}}->
			case lists:keysearch(Key,1,P) of
				false->
					From ! {self(),{error,{Key,not_found}}},
					loop(P,A);
				{value,{Key,_}}->
					NP = [{N,V}||{N,V}<-P,N=/=Key],
					From ! {self(),{ok,{Key,removed}}},
					loop(NP,A)
			end;
		{From,{remove_property_match,Reg}}->
			NP = try
				[{N,V}||{N,V}<-P,nomatch=:=re:run(case is_list(N) of true-> N;_->lists:flatten(io_lib:format("~p",[N])) end,Reg)]
				catch
					_:_->
						P
				end,
			From ! {self(),{ok,{Reg,removed}}},
			loop(NP,A);
		_->
			loop(P,A)
	end.

set_attribute(Name,Val,Data)->
	case lists:keysearch(Name,1,Data) of
		false->
			Data ++ [{Name,Val}];
		_->
			lists:map(fun(X)-> {A,_}=X,case A of Name-> {Name,Val}; _->X end end,Data)
	end.

get_attribute(Name,Data)->
	case lists:keysearch(Name,1,Data) of
		false->
			{error,{Name,not_found}};
		{value,Val}->
			{ok,Val};
		Else->
			{error,Else}
	end.
	
merge_attributes(A1,[])->A1;
merge_attributes(A1,[{K,V}|T])->
	case lists:keymember(K,1,A1) of
		true->
			merge_attributes(lists:keyreplace(K,1,A1,{K,V}),T);
		_->
			merge_attributes(A1++[{K,V}],T)
	end.
		
	