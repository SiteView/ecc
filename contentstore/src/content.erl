-module(content).

-export([predicateMap/2]).
-export([transformCondition/2]).

-export([sortDocument/2]).

-export([
		 	test/0,
			get/4,
            equalOfMy/2,
            nequalOfMy/2,
            inOfMy/2,
            greaterOfMy/2, 
            lessOfMy/2,
            lessEOfMy/2,
            greaterEOfMy/2, 
            likeOfMy/2,
            likeicOfMy/2,
            nlikeOfMy/2,
            eicOfMy/2,
            neicOfMy/2
        ]).
-export([create/3, get/4, update/4, delete/3]).

-include_lib("stdlib/include/qlc.hrl").
-include("config.hrl").

operatorItemOfMy(Fun, Key, Value, [H|T])->
    {MyKey, _, MyValue} = H,
    case Key =:= MyKey of
        true ->
            Fun(MyValue, Value);
        _ ->
            operatorItemOfMy(Fun, Key, Value, T)
    end;
operatorItemOfMy(_, _, _, _)->
     false.
 
operatorOfMy(Fun, L,R)->
    {Key, Value} = R,
    operatorItemOfMy(Fun, Key, Value, L).
    
equalOfMy(L,R)->
    operatorOfMy(fun operator:equal/2, L, R).
nequalOfMy(L,R)->
    operatorOfMy(fun operator:nequal/2, L, R).

inOfMy(L,R)->
    operatorOfMy(fun operator:in/2, L, R).

greaterOfMy(L,R)->
    operatorOfMy(fun operator:greater/2, L, R).
lessOfMy(L,R)->
    operatorOfMy(fun operator:less/2, L, R).
lessEOfMy(L,R)->
    operatorOfMy(fun operator:lessE/2, L, R).
greaterEOfMy(L,R)->
    operatorOfMy(fun operator:greaterE/2, L, R).
     
likeOfMy(L,R)->
    operatorOfMy(fun operator:like/2, L, R).
likeicOfMy(L,R)->
    operatorOfMy(fun operator:likeic/2, L, R).
nlikeOfMy(L,R)->
    operatorOfMy(fun operator:nlike/2, L, R).

eicOfMy(L,R)->
    operatorOfMy(fun operator:eic/2, L, R).
neicOfMy(L,R)->
    operatorOfMy(fun operator:neic/2, L, R).

transformCondition({"id", Operator, Value}, [{application, Application}|_])->
    case Value of
        "'" ++ Rest ->
            ID = string:strip(Rest, right, $'),
            case common:indexOfReverse(ID, $/) of
                Index when Index > 0 ->
                    {"id", Operator, {Application, list_to_atom(lists:nthtail(Index, ID))}};
                _ ->
                    {"error", Operator, "id format error"}
            end;
        _ ->
            case Operator of
                "in"->
                     {"id", "in", lists:map(fun(X)-> {Application, X} end, condition:parseList(atom, Value))};
                 _ ->
                     {"id", Operator, {Application, list_to_atom(Value)}}
            end
   end;
transformCondition({"my." ++ Field, Operator, Value}, _)->
    NewValue = case Value of
            "null"->
                null;
            _ ->
                case Operator of
                        "in" ->
                            condition:parseList(binary, Value);
                        "eic" ->
                            list_to_binary(condition:to_upper(string:strip(Value, both, $')));
                        "neic" ->
                            list_to_binary(condition:to_upper(string:strip(Value, both, $')));
                        "likeic" ->
                            list_to_binary(condition:to_upper(string:strip(Value, both, $')));
                        _ ->
                            list_to_binary(string:strip(Value, both, $'))
                end
    end,
    {"my", Operator, {list_to_atom(Field),  NewValue}};
transformCondition({"createdDate", Operator, Value}, _)->
    {"published", Operator, time:to_time(string:strip(Value, both, $'))};
transformCondition({"updated", Operator, Value}, _)->
    {"updated", Operator, time:to_time(string:strip(Value, both, $'))};
transformCondition({"author", Operator, Value}, _)->
    case Operator of
         "eic"->
             {"author", "eic", list_to_binary(condition:to_upper(string:strip(Value, both, $')))};
         _ ->
             {"author", "in", condition:parseList(binary, Value)}
    end;
transformCondition({"type", _, _}, _)->
    {"true", "=", true};
transformCondition({Key, Operator, _}, _)->
   {"error", Key, Operator}.

predicateMap(Field, Operator)->
    case Field of
	  "my" ->
            case Operator of
                "=" ->
                    {ok, {7, fun content:equalOfMy/2}};
                "!=" ->
                    {ok, {7, fun content:nequalOfMy/2}};
                "in" ->
                    {ok, {7, fun content:inOfMy/2}};
                ">" ->
                    {ok, {7, fun content:greaterOfMy/2}};
                "<" ->
                    {ok, {7, fun content:lessOfMy/2}};
                "<=" ->
                    {ok, {7, fun content:lessEOfMy/2}};
                ">=" ->
                    {ok, {7, fun content:greaterEOfMy/2}};
                "like" ->
                    {ok, {7, fun content:likeOfMy/2}};
                "likeic" ->
                    {ok, {7, fun content:likeicOfMy/2}};
               "!like" ->
                    {ok, {7, fun content:nlikeOfMy/2}};
                "eic" ->
                    {ok, {7, fun content:eicOfMy/2}};
                "neic" ->
                    {ok, {7, fun content:neicOfMy/2}};
                _ ->
                    {error, "content_" ++ Field ++ "_" ++ Operator}
            end;
         "id" ->
            case Operator of
                "in" ->                                      
                    {ok, {2, fun operator:in/2}};
                "=" -> 
                    {ok, {2, fun operator:equal/2}};
                "!=" ->
                    {ok, {2, fun operator:nequal/2}};
                _ ->
                    {error, "content_" ++ Field ++ "_" ++ Operator}
            end;
        "application" ->
            case Operator of
                "=" -> 
                    {ok, {3, fun operator:equal/2}};
                _ ->
                    {error, "content_" ++ Field ++ "_" ++ Operator}
            end;
        "published" ->
            case Operator of
                "=" ->
                    {ok, {4, fun operator:equal/2}};
                "!=" ->
                    {ok, {4, fun operator:nequal/2}};
                "<" ->
                    {ok, {4, fun operator:less/2}};
                ">" ->
                    {ok, {4, fun operator:greater/2}};
                "<=" ->
                    {ok, {4, fun operator:lessE/2}};
                ">=" ->
                    {ok, {4, fun operator:greaterE/2}};
                _ ->
                    {error, "content_" ++ Field ++ "_" ++ Operator}
            end;
        "updated" ->
            case Operator of
                "=" ->
                    {ok, {5, fun operator:equal/2}};
                "!=" ->
                    {ok, {5, fun operator:nequal/2}};
                "<" ->
                    {ok, {5, fun operator:less/2}};
                ">" ->
                    {ok, {5, fun operator:greater/2}};
                "<=" ->
                    {ok, {5, fun operator:lessE/2}};
                ">=" ->
                    {ok, {5, fun operator:greaterE/2}};
                _ ->
                    {error, "content_" ++ Field ++ "_" ++ Operator}
            end;
        "author" ->
            case Operator of
                "=" ->
                    {ok, {6, fun operator:equal/2}};
                "!=" ->
                    {ok, {6, fun operator:nequal/2}};
                "in" ->
                    {ok, {6, fun operator:in/2}};
                "eic" ->
                    {ok, {6, fun operator:eic/2}};
                _ ->
                    {error, "content_" ++ Field ++ "_" ++ Operator}
            end;
	"true" ->
            case Operator of
                "=" ->
                    {ok, {1, fun operator:always/2}};
                 _ ->
                    {error, "content_" ++ Field ++ "_" ++ Operator}
            end;
         _ ->
           {error, "content_" ++ Field ++ "_" ++ Operator}
      end.

getMyValue(Key, [{Key, _, Value}|_])->
    Value;
getMyValue(Key, [_|T])->
    getMyValue(Key, T);
getMyValue(_, [])->
    null.

sortDocument(Field, Documents)->
    case Field of
        "published@D"->
            FPD = fun(X, Y)-> element(4, X) > element(4, Y) end,
            lists:sort(FPD, Documents);
        "published@A"->
            FPA = fun(X, Y)-> element(4, X) < element(4, Y) end,
            lists:sort(FPA, Documents);
        "updated@D"->
            FUD = fun(X, Y)-> element(5, X) > element(5, Y) end,
            lists:sort(FUD, Documents);
        "updated@A"->
            FUA = fun(X, Y)-> element(5, X) < element(5, Y) end,
            lists:sort(FUA, Documents);
        "my." ++ MyField->
            Len = string:len(MyField),
            Key = list_to_atom(string:substr(MyField, 1, Len-2)),
            Order = string:substr(MyField, Len-1, 2),
            case Order of
                "@D"->
                    FMD = fun(X, Y)-> getMyValue(Key, element(7, X)) > getMyValue(Key, element(7, Y)) end,
                    lists:sort(FMD, Documents);
                "@A"->
                    FMA = fun(X, Y)-> getMyValue(Key, element(7, X)) < getMyValue(Key, element(7, Y)) end,
                    lists:sort(FMA, Documents);
                _ ->
                    Documents
            end;
        _->
          Documents
  end.
 
create(ApplicationQS, TypeQS, Content) when is_record(Content, content)->
    ?Trace("create ApplicationQS:~p  TypeQS:~p \n",[ApplicationQS, TypeQS]),
    Application = list_to_atom(ApplicationQS),
    Table = list_to_atom(TypeQS),
    case cache:get(fragments, Table) of
	undefined->
		mnesia:create_table(Table, [{disc_copies, [node()]}, {index, [application]}, {record_name, document}, {attributes, record_info(fields, document)}]),
		cache:set(fragments, Table, Table);
	_ ->
		nothing
    end,

    ID = {Application, Content#content.id},
    Document = converter:content2document(Content),
    case mnesia:dirty_read(Table, ID) of
	[] ->
		mnesia:dirty_write(Table, Document),
		{ok, Content};
	_ ->
		{error, existed}
    end.
test() ->ok.

get(ApplicationQS, TypeQS, WhereQS, OtherQS)->
	io:format("*************content get*********************"),
    ?Trace("get ApplicationQS:~p  TypeQS:~p WhereQS:~p OtherQS:~p \n",[ApplicationQS, TypeQS, WhereQS, OtherQS]),
    Application = list_to_atom(ApplicationQS),
    Table = list_to_atom(TypeQS),
    case cache:get(fragments, Table) of
	Table->
		Conditions = parse:scan(WhereQS),
		Result = case Conditions of
			[{?Begin,{"id","=", ID}}]->
				{ok, mnesia:dirty_read(Table, {Application, list_to_atom(ID)})};
			_ ->
				TransformedResult = check:process(Conditions, fun content:predicateMap/2, fun content:transformCondition/2, [{application, Application}]),
				case TransformedResult of
					{ok, TransformedCondition}->
						NormalRecords = mnesia:dirty_index_read(Table, Application, application),
						{ok, [X || X <- NormalRecords, check:predicate(TransformedCondition, X)]};
					_ ->
					    TransformedResult
				end
		end,
		case Result of
			{ok, Records}->
				OtherCondition = condition:parse(OtherQS),
				SortedDocuments = common:sortRecord(fun sortDocument/2, OtherCondition, Records),
				SortedContents = converter:document2content(list_to_binary(TypeQS), SortedDocuments),
				common:extractRecord(OtherCondition, SortedContents);
			Error->
				Error
		end;
	_ ->
		{ok, {0,100,0, []}}
    end.

update(ApplicationQS, TypeQS, WhereQS, Content)when is_record(Content, content) ->
    ?Trace("update ApplicationQS:~p TypeQS:~p, WhereQS:~p\n",[ApplicationQS, TypeQS, WhereQS]),
    Application = list_to_atom(ApplicationQS),
    Table = list_to_atom(TypeQS),
    case cache:get(fragments, Table) of
	Table->
		Conditions = condition:parse(WhereQS),
		case Conditions of
			[{"id", "=", Value}] ->
				ID = list_to_atom(string:strip(Value, both, $')),
				Document = converter:content2document(Content),
				case mnesia:dirty_read(Table, {Application, ID}) of
				    [DocumentOld]->
					DocumentNew = updateDocument(Document, DocumentOld),
					mnesia:dirty_write(Table, DocumentNew),
					ContentNew = converter:document2content(list_to_binary(TypeQS), DocumentNew),
					{ok, ContentNew};
				    _ ->
					{ok, nothing}
				 end;
			_->
			    {error, "condtion error"}
		end;
	_ ->
		{error, Table}
   end.

%%-record(document, {id, application, published, updated, author, my}).
updateDocument(NewDocument, OldDocument)->   
    Updated = time:getUTCTime(),
    
    New_My = NewDocument#document.my,
    Old_My = OldDocument#document.my,

    My = case New_My of
        [_|_]->
            updateMy(New_My, Old_My, []);
         _->
            Old_My
     end,

     OldDocument#document
     {
         updated = Updated,
         my = My
     }.
     
     
find(List, Key)->
    find(List, Key, [], null).
     
find([{Key, _, _} = H|R], Key, Acc, _)->
    find(R, Key, Acc, H);
find([H|R], Key, Acc, Element)->
    find(R, Key, [H|Acc], Element);
find([], _, Acc, Element)->
    {lists:reverse(Acc), Element}.

updateMy(New_My, [H|T], Result)->
    {OldKey, OldType, _} = H,
    {Rest, Element} = find(New_My, OldKey),
    case Element of
        null ->
            updateMy(Rest, T, [{OldKey, OldType, null}|Result]);
         _ ->
            updateMy(Rest, T, [Element|Result])
    end;
updateMy(Rest, [], Result)->
    updateMyRest(Rest, Result).

updateMyRest([H|T],Result)->
    {NewKey, _, _} =  H,
    {Rest, Element} = find([H|T], NewKey),
    updateMyRest(Rest, [Element|Result]);
updateMyRest([], Result)->
    lists:reverse(Result).

processConditions([{"id", "=", Value}])->
    {ok, [list_to_atom(string:strip(Value, both, $'))]};
processConditions([{"id", "in", Value}])->
    {ok, condition:parseList(atom, Value)};
processConditions(_)->
    {error, "invitation delete condition error"}.

delete(ApplicationQS, TypeQS, WhereQS)->
    ?Trace("delete ApplicationQS:~p TypeQS:~p WhereQS:~p\n",[ApplicationQS, TypeQS, WhereQS]),
    Application = list_to_atom(ApplicationQS),
    Table = list_to_atom(TypeQS),
    case cache:get(fragments, Table) of
	Table->
	    Conditions = condition:parse(WhereQS),
	    ProcessConditionsResult = processConditions(Conditions),
	    case  ProcessConditionsResult of
		{ok, DocumentIDList}->
		    lists:foreach(fun(Key)-> mnesia:dirty_delete(Table, {Application, Key}) end, DocumentIDList),
		    {ok, deleted};
		  _ ->
		     ProcessConditionsResult
	    end;
	_ ->
		{error, Table}
   end.