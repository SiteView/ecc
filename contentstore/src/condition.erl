-module(condition).
-import(lists, [foreach/2]).
-compile(export_all).


to_upper([C|R])-> [string:to_upper(C)|to_upper(R)];
to_upper([])-> [].
    
to_lower([C|R])-> [string:to_lower(C)|to_lower(R)];
to_lower([])-> [].

parseList(ListString)->
    Len = length(ListString),
    Sub = lists:sublist(ListString, 2, Len - 2),
    string:tokens(Sub, ",").
    
parseList(atom, ListString)->
    List = parseList(ListString),
    lists:map(fun(X)-> list_to_atom(string:strip(X, both, $')) end, List);

parseList(binary, ListString)->
    List = parseList(ListString),
    lists:map(fun(X)-> list_to_binary(string:strip(X, both, $')) end, List);

parseList(integer, ListString)->
    List = parseList(ListString),
    lists:map(fun(X)-> list_to_integer(string:strip(X, both, $')) end, List).

parse([H|_]) when is_list(H)->
    parse(H);
parse(Condition)->
        Items = string:tokens(Condition, "&"),
        if
            length(Items) > 0 ->
               parseItems(Items);
            true ->
                []
        end.
  
parseItems([First|Rest])->
    Item = parseItem(First),
    case Item of
        error ->
            parseItems(Rest);
        _ ->
            [Item|parseItems(Rest)]
    end;
     
parseItems([])->
    [].
    
%   =       equal to 
%   <>      not equal to 
%   eic     equal to, ignoring case 
%   neic    not equal to, ignoring case 
%   >       greater than 
%   <       less than 
%   >=      greater than or equal to 
%   <=      less than or equal to 
%   like    full-text search 
%   likeic  full-text search, ignoring case 
%   in      in a list of values 

tokens([H|T], Key)->
    if
         H =:= 32 ->
                Temp = string:strip(T, left, 32),
                case Temp of
                   "=" ++ Value ->
                        [Key, "=", string:strip(Value, left, 32)];
                    "in" ++ Value ->
                        [Key, "in", string:strip(Value, left, 32)];
                    "!=" ++ Value ->
                        [Key, "!=", string:strip(Value, left, 32)];
                    "eic" ++ Value ->
                        [Key, "eic", string:strip(Value, left, 32)];
                    "neic" ++ Value ->
                        [Key, "neic", string:strip(Value, left, 32)];
                    ">=" ++ Value ->
                        [Key, ">=", string:strip(Value, left, 32)];
                    "<=" ++ Value ->
                        [Key, "<=", string:strip(Value, left, 32)];
                    ">" ++ Value ->
                        [Key, ">", string:strip(Value, left, 32)];
                    "<" ++ Value ->
                        [Key, "<", string:strip(Value, left, 32)];
                    "likeic" ++ Value ->
                        [Key, "likeic", string:strip(Value, left, 32)];
                    "like" ++ Value ->
                        [Key, "like", string:strip(Value, left, 32)];
                    _ ->
                        []
                end;
         H =:= $! orelse H =:= $= orelse H =:= $> orelse H =:= $< ->
               Temp = [H] ++ T,
               case Temp of
                   "=" ++ Value ->
                        [Key, "=", string:strip(Value, left, 32)];
                    "in" ++ Value ->
                        [Key, "in", string:strip(Value, left, 32)];
                    "!=" ++ Value ->
                        [Key, "!=", string:strip(Value, left, 32)];
                    "eic" ++ Value ->
                        [Key, "eic", string:strip(Value, left, 32)];
                    "neic" ++ Value ->
                        [Key, "neic", string:strip(Value, left, 32)];
                    ">=" ++ Value ->
                        [Key, ">=", string:strip(Value, left, 32)];
                    "<=" ++ Value ->
                        [Key, "<=", string:strip(Value, left, 32)];
                    ">" ++ Value ->
                        [Key, ">", string:strip(Value, left, 32)];
                    "<" ++ Value ->
                        [Key, "<", string:strip(Value, left, 32)];
                    "likeic" ++ Value ->
                        [Key, "likeic", string:strip(Value, left, 32)];
                    "like" ++ Value ->
                        [Key, "like", string:strip(Value, left, 32)];
                    _ ->
                        []
                end;
        true ->
              tokens(T, Key ++ [H])
    end;

tokens([], _)->
        [].

parseItem(Item)->
    Results = tokens(string:strip(Item, both, 32), ""),
    if
        length(Results) > 0 ->
            [Key, Operator, Value] = Results,
             {Key, Operator, Value};
	true ->
	   error
end.