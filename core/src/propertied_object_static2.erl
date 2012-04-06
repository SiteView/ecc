%% ---
%%propertied_object_static
%%
%%---
-module(propertied_object_static2).
-compile(export_all).

loop(P,A)->
	receive
		{From,stop}->
			From ! {self(),{ok,stopped}};
		{From,{set_attribute,{Name,Val}}}->
			From ! {self(),{ok,set_attribute_ok}},
			loop(P,dict:store(Name,Val,A));
		{From,{remove_attribute,Name}}->
			case dict:is_key(Name,P) of
				false->
					From ! {self(),{error,{Name,not_found}}},
					loop(P,A);
				_ ->
					From ! {self(),{ok,{Name,removed}}},
					loop(P,dict:erase(Name,A))
			end;
		{From,{remove_attribute_match,Reg}}->
			NA = try
				[{N,V}||{N,V}<-dict:to_list(A),nomatch=:=regexp:match(case is_list(N) of true-> N;_->lists:flatten(io_lib:format("~p",[N])) end,Reg)]
				catch
					E1:_->
						dict:to_list(A)
				end,
			% NA = [{N,V}||{N,V}<-dict:to_list(A),nomatch=:=regexp:match(atom_to_list(N),Reg)],
			From ! {self(),{ok,{Reg,removed}}},
			loop(P,dict:from_list(NA));
		{From,remove_attributes}->
			From ! {self(),{ok,remove_attributes_ok}},
			loop(P,dict:new());
		{From,{get_attribute,Name}}->
			case dict:find(Name,A) of
				{ok,R}->
					From ! {self(),{ok,{Name,R}}};
				_->
					From ! {self(),{error,{Name,not_found}}}
			end,
			loop(P,A);
		{From,get_attributes}->
			From ! {self(),dict:to_list(A)},
			loop(P,A);
		{From,{add_properties,Properties}}->
			case is_list(Properties) of
				true->	
					From ! {self(),{ok,add_properties_ok}},
					loop(dict:from_list(dict:to_list(P)++Properties),A);
				_->
					From ! {self(),{error,properties_is_not_list}},
					loop(P,A)
			end;
		{From,{add_attributes,Attributes}}->
			case is_list(Attributes) of
				true->	
					From ! {self(),{ok,add_attributes_ok}},
					loop(P,dict:from_list(dict:to_list(A)++Attributes));
				_->
					From ! {self(),{error,attributes_is_not_list}},
					loop(P,A)
			end;
		{From,{get_property,Key}}->
			case dict:find(Key,P) of
				{ok,R}->
					From ! {self(),{ok,{Key,R}}};
				_->
					From ! {self(),{error,{Key,not_found}}}
			end,
			loop(P,A);
		{From,remove_properties}->
			From ! {self(),{ok,remove_properties_ok}},
			loop(dict:new(),A);
		{From,get_properties}->
			From ! {self(),dict:to_list(P)},
			loop(P,A);
		{From,{set_property,{Key,Val}}}->
			From ! {self(),{ok,set_property_ok}},
			loop(dict:store(Key,Val,P),A);
		{From,{remove_property,Key}}->
			case dict:is_key(Key,P) of
				false->
					From ! {self(),{error,{Key,not_found}}},
					loop(P,A);
				_ ->
					From ! {self(),{ok,{Key,removed}}},
					loop(dict:erase(Key,P),A)
			end;
		{From,{remove_property_match,Reg}}->
			NP = try
				[{N,V}||{N,V}<-dict:to_list(P),nomatch=:=regexp:match(case is_list(N) of true-> N;_->lists:flatten(io_lib:format("~p",[N])) end,Reg)]
				catch
					E1:_->
						dict:to_list(P)
				end,
			% NP = [{N,V}||{N,V}<-dict:to_list(P),nomatch=:=regexp:match(atom_to_list(N),Reg)],
			From ! {self(),{ok,{Reg,removed}}},
			loop(dict:from_list(NP),A);
		_->
			loop(P,A)
	end.
