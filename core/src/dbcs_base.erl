%% ---
%%dbcs_base
%%
%%---
-module(dbcs_base).
-compile(export_all).

%%term2db(K,V)->{K,Type,V}
%%
%%
%term2db(K,V) when is_integer(V)->{K,dbtype(V),list_to_binary(integer_to_list(V))};
term2db(K,V) when is_integer(V)->{K,number,list_to_binary(integer_to_list(V))};  %by zhangyan
term2db(K,V) when is_float(V)->{K,dbtype(V),list_to_binary(float_to_list(V))};
term2db(K,V) when is_atom(V)->{K,dbtype(V),list_to_binary(atom_to_list(V))};
term2db(K,V) when is_list(V)->
	case dbtype(V) of
		string->
			{K,dbtype(V),list_to_binary(V)};
		_->
			{K,dbtype(V),term_to_binary(V)}
	end;
term2db(K,V)->{K,dbtype(V),term_to_binary(V)}.

%%db2term(K,T,V)->{K,V}
%%
%%
db2term(K,T,V) when not is_binary(V)->{K,V};
db2term(K,T,V) when T=:= number ->
	NV = binary_to_list(V),
	case string:to_float(NV) of
		{error,_}->
			{K,list_to_integer(NV)};
		_->
			{K,list_to_float(NV)}
	end;
db2term(K,T,V) when T=:= atom ->{K,list_to_atom(binary_to_list(V))};
db2term(K,T,V) when T=:= string ->{K,binary_to_list(V)};
db2term(K,_,V)->{K,binary_to_term(V)}.


%%dbtype(V)->Type
%%
%%
dbtype(V) when is_number(V)->number;
dbtype(V) when is_atom(V)->atom;
dbtype(V) when is_list(V)->
	try 
		Tv = binary_to_list(list_to_binary(V)),
		if 
			Tv =:= V ->
				string;
			true->
				tuple
		end
	catch
	_:_->tuple
	end;
dbtype(_)->tuple.


get_id()->
	case now() of
		{A,B,C}->
			list_to_atom(integer_to_list(A)++ ":" ++ integer_to_list(B) ++ ":" ++ integer_to_list(C));
		_->
			list_to_atom(integer_to_list(random:uniform(1000000)))
	end.
	
get_app()->
	list_to_atom(db_ecc:domain(get(hostname))).

set_app(undefined)->
	case get(hostname) of
		undefined->
			put(hostname,localhost);
		_->
			psss
	end;
set_app(App)->
	case get(hostname) of
		undefined->
			put(hostname,App);
		_->
			pass
	end.
	
set_app(App,false)->
	set_app(App);
set_app(undefined,true)->
	put(hostname,localhost);
set_app(App,true)->
	put(hostname,App).

uuid()->
	crypto:start(),
	<<I:160/integer>> = crypto:sha(term_to_binary({make_ref(), now()})),
	list_to_atom(lists:flatten(io_lib:fwrite("~40..0s", [erlang:integer_to_list(I, 16)]))).
	
uuid2()->
	crypto:start(),
	<<I:160/integer>> = crypto:sha(term_to_binary({make_ref(), now()})),
	lists:flatten(io_lib:fwrite("~40..0s", [erlang:integer_to_list(I, 16)])).
	
is_string(V) when is_list(V)->
	if 
		length(V) == 0->
			true;
		length(V) == 1->
			is_integer(lists:nth(1,V));
		true->
			is_integer(lists:nth(1,V)) and is_integer(lists:nth(length(V),V))
	end;
is_string(_)->false.

html_encode([]) -> [];
html_encode([H|T]) ->
	case H of
		% $\s -> "&nbsp;" ++ html_encode(T);
		% $\t -> "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" ++ html_encode(T);
		$< -> "&lt;" ++ html_encode(T);
		$> -> "&gt;" ++ html_encode(T);
		$" -> "&quot;" ++ html_encode(T);
		$' -> "&#39;" ++ html_encode(T);
		% $& -> "&amp;" ++ html_encode(T);
		$\n -> "<br>" ++ html_encode(T);
		_ -> [H|html_encode(T)]
	end.

% erlang data to ofbiz

atom2xml(V)->
	B = list_to_binary(html_encode(atom_to_list(V))),
	<<"<e:a>",B/binary ,"</e:a>">>.
	
string2xml(V)->
	B = list_to_binary(html_encode(V)),
	<<"<e:s>",B/binary , "</e:s>">>.
	
integer2xml(V)->
	B = list_to_binary(io_lib:format("~p",[V])),
	<<"<e:i>",B/binary,"</e:i>">>.
	
float2xml(V)->
	B = list_to_binary(io_lib:format("~p",[V])),
	<<"<e:f>", B/binary, "</e:f>">>.
	
boolean2xml(V)->
	B = atom_to_binary(V,utf8),
	<<"<e:b>",B/binary,"</e:b>">>.
binary2xml(V)->
	B = base64:encode(V),
	<<"<e:binary>",B/binary ,"</e:binary>">>.
	
data_error(V)->
	B = list_to_binary(io_lib:format("~p",[V])),
	<<"<e:error>" ,B/binary,"</e:error>">>.

term2xml(V)when is_atom(V)->atom2xml(V);
% term2xml(V)when is_string(V)->string2xml(V);
term2xml(V)when is_integer(V)->integer2xml(V);
term2xml(V)when is_float(V)->float2xml(V);
term2xml(V)when is_boolean(V)->boolean2xml(V);
term2xml(V)when is_binary(V)->binary2xml(V);
term2xml(V)when is_list(V)->
	case is_string(V) of
		true->
			string2xml(V);
		_->
			list2xml(V)
	end;
term2xml(V)when is_tuple(V)->tuple2xml(V);
term2xml(V)->data_error(V).

list2xml(List)->
	B = list2xml(List,<<"">>),
	<<"<e:l>",B/binary,"</e:l>">>.
	
list2xml([],Acc)->Acc;
list2xml([H|T],Acc)->
	B = term2xml(H),
	list2xml(T,<<Acc/binary,B/binary>>).


tuple2xml(Tuple)->
	B = list2xml(tuple_to_list(Tuple),<<"">>),
	<<"<e:t>",B/binary, "</e:t>">>.

key2ofbiz(K) when is_atom(K)->atom_to_list(K);
key2ofbiz(K) when is_list(K)->
	case is_string(K) of
		true->
			K;
		_->
			term2xml(K)
	end;
key2ofbiz(K)->term2xml(K).


term2ofbiz(K,V)when is_boolean(V)->{key2ofbiz(K),V};
term2ofbiz(K,V)when is_atom(V)->{key2ofbiz(K),atom2xml(V)};
term2ofbiz(K,V)when is_float(V)->{key2ofbiz(K),V};
term2ofbiz(K,V)when is_integer(V)->{key2ofbiz(K),V};
term2ofbiz(K,V)when is_binary(V)->{key2ofbiz(K),V};
% term2ofbiz(K,V)when is_string(V)->{key2ofbiz(K),V};
term2ofbiz(K,V)when is_list(V)->
	case is_string(V) of
		true->
			{key2ofbiz(K),V};
		_->
			{key2ofbiz(K),list2xml(V)}
	end;
term2ofbiz(K,V)when is_tuple(V)->{key2ofbiz(K),tuple2xml(V)};
term2ofbiz(K,V)->{key2ofbiz(K),term2xml(V)}.


% ofbiz data to erlang
-include("xmerl.hrl").


xml2atom([])->'';
xml2atom([El=#xmlText{}])->list_to_atom(El#xmlText.value).

xml2string([])->"";
xml2string([El=#xmlText{}])->El#xmlText.value.

xml2integer([])->0;
xml2integer([El=#xmlText{}])->list_to_integer(El#xmlText.value).

xml2float([])->0.0;
xml2float([El=#xmlText{}])->list_to_float(El#xmlText.value).

xml2boolean([])->false;
xml2boolean([El=#xmlText{}])->list_to_atom(El#xmlText.value).

xml2binary([])->false;
xml2binary([El=#xmlText{}])->base64:decode(El#xmlText.value).


xml2list([])->[];
xml2list(Xl)->
	elements2list(Xl,[]).
	
elements2list([],Acc)->Acc;
elements2list([H|T],Acc)->
	elements2list(T,Acc ++ [element2term(H)]).
	
xml2tuple([])->{};
xml2tuple(Xl)->
	list_to_tuple(elements2list(Xl,[])).
	
element2term(El)->
	case El#xmlElement.name of
		'e:a'->
			xml2atom(El#xmlElement.content);
		'e:s'->
			xml2string(El#xmlElement.content);
		'e:i'->
			xml2integer(El#xmlElement.content);
		'e:f'->
			xml2float(El#xmlElement.content);
		'e:b'->
			xml2boolean(El#xmlElement.content);
		'e:binary'->
			xml2binary(El#xmlElement.content);
		'e:l'->
			xml2list(El#xmlElement.content);
		'e:t'->
			xml2tuple(El#xmlElement.content)
	end.

xml2term(V)->
	case xmerl_scan:string(V) of
		{El,Rest}->
			element2term(El);
		_->
			V
	end.
	
is_xml(V) when is_list(V)->
	NewV = string:strip(V),
	Rv = lists:reverse(NewV),
	case lists:prefix("<e:",NewV) and lists:prefix(">",Rv) of
		true->
			try
				xmerl_scan:string(V),
				true
			catch
				_:_->false
			end;
		_->
			false
	end;
is_xml(_)->false.


			

ofbiz2key(K)when is_list(K)->
	case is_string(K) of
		true->
			case is_xml(K) of
				true->
					xml2term(K);
				_->
					list_to_atom(K)
			end;
		_->
			K
	end;
ofbiz2key(K)->K.

ofbiz2term(K,V)when is_boolean(V)->{ofbiz2key(K),V};
ofbiz2term(K,V)when is_float(V)->{ofbiz2key(K),V};
ofbiz2term(K,V)when is_integer(V)->{ofbiz2key(K),V};
ofbiz2term(K,V)when is_binary(V)->
	Str = binary_to_list(V),
	case is_string(Str) of
		true->
			case is_xml(Str) of
				true->
					{ofbiz2key(K),xml2term(Str)};
				_->
					{ofbiz2key(K),V}
			end;
		_->	
			{ofbiz2key(K),V}
	end;
ofbiz2term(K,V)when is_list(V)->
	case is_string(V) of
		true->
			% case is_xml(V) of
				% true->
					% {ofbiz2key(K),xml2term(V)};
				% _->
					% {ofbiz2key(K),V}
			% end;
			{ofbiz2key(K),V};
		_->
			error({K,V})
	end;
ofbiz2term(K,V)->error({K,V}).

% thrift call interface
-define(ofbiz_service_ip, server_conf:getServerConf(ofbiz_host,"localhost")).
-define(ofbiz_service_port, server_conf:getServerConf(ofbiz_port,9090)).

call_ofbiz_service(ServiceName,Data)->
	{ok, Client0} = thrift_client_util:new(?ofbiz_service_ip,
							   ?ofbiz_service_port,
							   remoteDispatcher_thrift,
							   []),
	EncData = [term2ofbiz(K,V)||{K,V}<-Data],
	io:format("==call ofbiz in:~p~n",[EncData]),
	RR = [ofbiz2term(K,V)||{K,V}<-EncData],
	Ret = ofbiz_service_client:callService(Client0,ServiceName,EncData),
	io:format("==call ofbiz out:~p~n",[Ret]),
	thrift_client:close(Client0),
	[ofbiz2term(K,V)||{K,V} <- Ret].