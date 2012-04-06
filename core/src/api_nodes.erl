-module(api_nodes).
-export([nodes/0]).

nodes() ->
	case file:read_file("conf\\service.conf") of
		{ok,Bd} ->
			Ld = binary_to_list(Bd),
			analogue(Ld,[]);
		_ -> []
	end.

analogue([],ACC) -> ACC;
analogue(Data,ACC) ->
	case length(Data) > 0 of
		true -> 
			Rp = "-sname ",
			case string:str(Data,Rp) of
				Index when Index > 0 ->
					Sub = string:substr(Data,Index + length(Rp)),
					case string:str(Sub," -") of
						Ind when Ind > 0 ->
							Subn = string:substr(Sub,1,Ind - 1),
							{ok,Hostname} = inet:gethostname(),
							Node = lists:flatten([Subn,"@",Hostname]),
							analogue(Sub,[{list_to_atom(Node),ping(list_to_atom(Node))}] ++ ACC);
						_ ->
							analogue(Sub,ACC)
					end;
				_ -> analogue([],ACC)
			end;
		_ -> analogue([],ACC)
	end.

ping(Node) ->
	case net_adm:ping(Node) of
		pang -> false;
		_ -> true
	end.