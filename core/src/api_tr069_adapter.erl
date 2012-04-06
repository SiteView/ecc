-module(api_tr069_adapter).
-compile(export_all).
-include("head.hrl").
-include("monitor.hrl").

-define(Table,"tr069_device").
-define(Author,<<"ning.cai@dragonflow.com">>).
-define(DBName,server_conf:get_db_node()).

-export([get_untreatedAlarm/2,get_device/2]).


%% *****************************************************************************
%% *****************************************************************************
%% **********General treatment, (Where a database and assembled into Order)
%% *****************************************************************************
%% *****************************************************************************
build_condition(Condition=#query_condition1{}) ->
    Index = Condition#query_condition1.index,
    Count = Condition#query_condition1.count,
    Where = 
        case Condition#query_condition1.where of
            ConWhere = #query_condition_where{} ->
                ParseWhere = parse_where_condition(ConWhere#query_condition_where.where),
                ParseWhere1 = string:strip(ParseWhere),
                LastStr = 
                    case ParseWhere1 of
                        [] ->
                            [];
                        _ ->
                            string:substr(ParseWhere1, string:len(ParseWhere1))
                    end,
                %%io:format("LastStr = ~p~n", [LastStr]),
                if
                    LastStr=:="&" ->
                        string:substr(ParseWhere1, 1, string:len(ParseWhere1)-2);
                    LastStr=:="|" ->
                        string:substr(ParseWhere1, 1, string:len(ParseWhere1)-2);
                    true ->
                        ParseWhere1
                end;
            OWhere ->
                OWhere
        end,
    %%io:format("Where = ~p~n", [Where]),
    Sort = Condition#query_condition1.sort,
    SortType = Condition#query_condition1.sortType,
    SortContent =
    case Sort of
        [] ->
            [];
        V4 when erlang:is_list(Sort) ->
            "&order=my." ++ Sort++SortType;
        _ ->
            []
    end,
    IndexStr = 
        try erlang:integer_to_list(Index) of
            V when erlang:is_list(V) ->
                V;
            _ ->
                "0"
        catch
            _:_ ->
                "0"
        end,
    CountStr = 
        try erlang:integer_to_list(Index+Count) of
            V1 when erlang:is_list(V1) ->
                V1;
            _ ->
                "0"
        catch
            _:_ ->
                "0"
        end,
    Order = "from="++IndexStr++"& to="++CountStr++SortContent,%%++"&order=my."++Order,
    #query_beam_condition1{
        where=Where,
        order=Order
    }.

%% Where parsing query
parse_where_condition([]) ->
    [];
parse_where_condition([ConditionWhere=#query_condition_where{}|T]) ->
    "(" ++ parse_where_condition(ConditionWhere#query_condition_where.where) ++ ")" ++ " | " ++
    parse_where_condition(T);
parse_where_condition([{Field,Operation,Value,Relation}|T]) ->
    case string:strip(Relation) of
        "" ->
            lists:append(lists:append([Field," ",Operation," ", Value]),
                            parse_where_condition(T));
        Re ->
            lists:append(lists:append([Field," ",Operation, " ", Value, " ", Re, " "]),
                            parse_where_condition(T))
    end;
parse_where_condition([H|T]) ->
    parse_where_condition(T).
    
    
%% @spec get_untreatedAlarm(Hostname, Condition=#query_condition1{}) -> Result
%%	Hostname = (string())
%%	Index = (integer())
%%	Count = (integer())
%%	Sort = (string())
%%	WhereCondition = (#query_condition_where{})
%%	Result = ([#realtimealarm{}])
%% @doc get alerm from Index to Index+Count, and order by Sort
%% Real-time alarms, alarm history, common interface, all interfaces are based on this approach based on Layer 2 interfaces <<warning Common Interface>>
get_untreatedAlarm(Hostname, Condition=#query_condition1{}) ->
    %%HostName = erlang:list_to_atom(Hostname),
    BeamCondition = build_condition(Condition),
    Where = BeamCondition#query_beam_condition1.where,
    Order = BeamCondition#query_beam_condition1.order,
    %%io:format("Where !!!!!!!!!!!!! ~n~p~n", [Where]),
    dbcs_tr069:get_untreatedAlarm(Hostname, Where, Order).
    
%% @spec get_device(Hostname,  Condition=#query_condition1{}) ->({ok,Result}|{error,Reason})
%%	Condition = (list())
%%	Hostname = (string())
%%	Index = (integer())
%%	Count = (integer())
%%	Sort = (#query_condition_where{})
%%	WhereCondition = (string())
%%	Result = [#tr069_device{}|T]
%%	Reason = atom()
%% @doc find a configure item by query conditions
%% Query the device, common interface <<General Equipment Query Interface>>
get_device(Hostname,  Condition=#query_condition1{}) ->
    %%HostName = erlang:list_to_atom(Hostname),
    BeamCondition = build_condition(Condition),
    Where = BeamCondition#query_beam_condition1.where,
    Order = BeamCondition#query_beam_condition1.order,
    %%io:format("Where = ~p~n", [Where]),
    %%io:format("Order = ~p~n", [Order]),
    %%io:format("HostName = ~p~n", [Hostname]),
    dbcs_tr069:get_all_where(Hostname, Where, Order).
    

%% New interface (temporary)
get_dev(Hostname,  Condition=#query_condition1{}) ->
    BeamCondition = build_condition(Condition),
    Where = BeamCondition#query_beam_condition1.where,
    Order = BeamCondition#query_beam_condition1.order,
    dbcs_tr069:get_cpe_where(Hostname, Where, Order).