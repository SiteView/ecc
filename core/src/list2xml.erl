%% ---
%% list2xml
%%
%%---
-module(list2xml).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").

to_childs(List) when not is_list(List)-> {error,parameter_error};
to_childs([])-> "<childs></childs>";
to_childs(List)->
	"<childs>" ++ to_childsxml(List) ++ "</childs>".

to_childsxml([])->"";
to_childsxml([M|T])->
	to_objxml(M) ++ to_childsxml(T).


to_mt(List)->
	"<mt>" ++  process_list(List)  ++ "</mt>".

to_mtlist(List)->
	"<mtlist>" ++  process_list(List)  ++ "</mtlist>".

to_result(List)->
	"<response>" ++ process_list(List) ++ "</response>".
	
%%新加入
to_response(List) ->
    "<response>"++process_list(List)++"</response>".
	
to_alertlist(List)->
    "<alertlist>"++process_list(List)++"</alertlist>".
	
to_versionlist(List)->
    "<versionlist>"++"<version>"++process_list(List)++"</version>"++"</versionlist>".
	
to_versionhistory(List) ->
    "<versionhistory>"++process_list(List)++"</versionhistory>".
%%以上为新加入部分

to_statexml(List)->
	"<state>" ++ process_list(List) ++ "</state>".

to_objxml(List)->
	case process_type(List) of
		{error,Reason}->
			{error,Reason};
		_->
			%%xmerl_ucs:to_utf8("<obj type='" ++ process_type(List) ++ "'>" ++  process_list(List) ++ "</obj>")
			"<obj type='" ++ process_type(List) ++ "'>" ++  process_list(List) ++ "</obj>"
	end.

process_type(List)->
	case lists:keysearch(class,1,List) of
		{value,{class,Val}} when is_atom(Val)->
			atom_to_list(Val);
		{value,{class,Val}} when is_list(Val)->
			Val;
		_->
			{error,not_found_class_property}
	end.

process_list([])->"";
%%process_list([{class,_}|T])->
%%	process_list(T);
%%process_list([{"class",_}|T])->
%%	process_list(T);
process_list([{K,V}|T]) when is_list(V),is_atom(K)->
	case is_string(V) of
		true->
			"<" ++ atom_to_list(K) ++ ">" ++ V ++ "</" ++ atom_to_list(K) ++ ">" ++  process_list(T);
		false->
			"<" ++ atom_to_list(K) ++ ">" ++ process_list(V) ++ "</" ++ atom_to_list(K) ++ ">" ++  process_list(T)
	end;
process_list([{K,V}|T]) when is_list(V),is_list(K)->
	case is_string(V) of
		true->
			"<" ++ K ++ ">" ++ V ++ "</" ++ K ++ ">" ++  process_list(T);
		false->
			"<" ++ K ++ ">" ++ process_list(V) ++ "</" ++ K ++ ">" ++  process_list(T)
	end;
process_list([{K,V}|T]) when is_atom(V),is_atom(K)->
	"<" ++ atom_to_list(K) ++ ">" ++ atom_to_list(V) ++ "</" ++ atom_to_list(K) ++ ">" ++  process_list(T);
process_list([{K,V}|T]) when is_atom(V),is_list(K)->
	"<" ++ K ++ ">" ++ atom_to_list(V) ++ "</" ++ K ++ ">" ++  process_list(T);
process_list([{K,V}|T]) when is_tuple(V),is_atom(K)->
	"<" ++ atom_to_list(K) ++ ">" ++ process_tuple(V) ++ "</" ++ atom_to_list(K) ++ ">" ++  process_list(T);
process_list([{K,V}|T]) when is_tuple(V),is_list(K)->
	"<" ++ K ++ ">" ++ process_tuple(V) ++ "</" ++ K ++ ">" ++  process_list(T);
process_list([{K,V}|T]) when is_integer(V),is_atom(K)->
	"<" ++ atom_to_list(K) ++ ">" ++ integer_to_list(V) ++ "</" ++ atom_to_list(K) ++ ">" ++  process_list(T);
process_list([{K,V}|T]) when is_integer(V),is_list(K)->
	"<" ++ K ++ ">" ++ float_to_list(V) ++ "</" ++ K ++ ">" ++  process_list(T);
process_list([{K,V}|T]) when is_float(V),is_atom(K)->
	"<" ++ atom_to_list(K) ++ ">" ++ integer_to_list(V) ++ "</" ++ atom_to_list(K) ++ ">" ++  process_list(T);
process_list([{K,V}|T]) when is_float(V),is_list(K)->
	"<" ++ K ++ ">" ++ float_to_list(V) ++ "</" ++ K ++ ">" ++  process_list(T);

process_list([_|T])-> process_list(T);
process_list(_)->	{error,process_error}.


process_tuple({K,V}) when is_atom(K),is_atom(V)->
	"<" ++ atom_to_list(K) ++ ">" ++ atom_to_list(V) ++ "</" ++ atom_to_list(K) ++ ">";
process_tuple({K,V}) when is_list(K),is_atom(V)->
	"<" ++ K ++ ">" ++ atom_to_list(V) ++ "</" ++ K ++ ">";
process_tuple({K,V}) when is_atom(K),is_list(V)->
	case is_string(V) of
		true->
			"<" ++ atom_to_list(K) ++ ">" ++ V ++ "</" ++ atom_to_list(K) ++ ">";
		false->
			"<" ++ atom_to_list(K) ++ ">" ++ process_list(V) ++ "</" ++ atom_to_list(K) ++ ">"
	end;
process_tuple({K,V}) when is_list(K),is_list(V)->
	case is_string(V) of
		true->
			"<" ++ K ++ ">" ++ V ++ "</" ++ K ++ ">";
		false->
			"<" ++ K ++ ">" ++ process_list(V) ++ "</" ++ K ++ ">"
	end;
process_tuple({K,V}) when is_atom(K),is_tuple(V)->
	"<" ++ atom_to_list(K) ++ ">" ++ process_tuple(V) ++ "</" ++ atom_to_list(K) ++ ">";
process_tuple({K,V}) when is_list(K),is_tuple(V)->
	"<" ++ K ++ ">" ++ process_tuple(V) ++ "</" ++ K ++ ">";
process_tuple(_)->"".

is_string(V)->
	try list_to_binary(V),true
	catch
	_:_->false
	end.