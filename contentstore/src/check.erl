-module(check).
-export([predicate/2, process/4]).

-include("config.hrl").

predicate([H|R], Record)->
    {?Begin, Item} = H,
    Result = case is_list(Item) of
        true->
            predicate(Item, Record);
        _ ->
            {Fun, Index, Value} = Item,
            Fun(element(Index, Record), Value)
    end,
    checkCondition(R, Record, Result);
predicate([], _)->
	true.

checkCondition([{?And, {Fun, Index, Value}}|R], Record, true)->
    Result = Fun(element(Index, Record), Value),
    case Result of
        true ->
            checkCondition(R, Record, true);
        _ ->
            false
    end;
checkCondition([{?And,  Group}|R], Record, true)->
    Result = predicate(Group, Record),
    case Result of
        true ->
            checkCondition(R, Record, true);
        _ ->
            false
    end;
checkCondition([{?Or, _}|_], _, true)->
    true;
checkCondition([{?And, _}|_], _, false)->
    false;
checkCondition([{?Or, {Fun, Index, Value}}|R], Record, false)->
    Result = Fun(element(Index, Record), Value),
    case Result of
        true ->
            true;
        _ -> 
            checkCondition(R, Record, false)
    end;
checkCondition([{?Or, Group}|R], Record, false)->
    Result = predicate(Group, Record),
    case Result of
        true ->
            true;
        _ ->
            checkCondition(R, Record, false)
    end;
checkCondition([], _, Result)->
    Result.

process(Conditions, PredicateFun, TransformFun, HelperCondition)->
    processCondition(Conditions, PredicateFun, TransformFun, HelperCondition, []).

processCondition([H|R], PredicateFun, TransformFun, HelperCondition, Result)->
    {Logic, Item} = H,
    case is_list(Item) of
        true->
            ProcessGroupResult =processCondition(Item, PredicateFun, TransformFun, HelperCondition, []),
            case ProcessGroupResult of
                {ok, ProcessedGroup}->
                    processCondition(R, PredicateFun, TransformFun, HelperCondition, [{Logic, ProcessedGroup}|Result]);
                _ ->
                    ProcessGroupResult
            end;
        _ ->
            {NewKey, NewOperator, NewValue} = TransformFun(Item, HelperCondition),

            TransformResult = case PredicateFun(NewKey, NewOperator) of
                {ok, {Index, Function}}->
                    {ok, {Function, Index, NewValue}};
                Error->
                    Error
            end,

            case TransformResult of
                {ok, ProcessedCondition} ->
                    processCondition(R, PredicateFun, TransformFun, HelperCondition, [{Logic, ProcessedCondition}|Result]);
                _ ->
                    TransformResult
            end
    end;
processCondition([], _, _, _, Result)->
    {ok, lists:reverse(Result)}.
		
	