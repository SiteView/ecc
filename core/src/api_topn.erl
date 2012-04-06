-module(api_topn).

-export([getTopnMonitorsById/1, getAllTopn/0, getTopnById/1]).

-include("topn.hrl").

%% @spec getTopnMonitorsById(Id)-> Result
%% where
%%		Id = string()
%%		Result=[[{Key,Value}|T]|Other]
%% @doc get topn monitors by topn id
getTopnMonitorsById(Id) ->
    case topn_object_table:read(Id) of
        [{Id, Obj}] ->
            case Obj:get_attribute(?TOPN_RESULT_MONITORS) of
                {ok, {?TOPN_RESULT_MONITORS, ReMonitors}} ->
                    toProperties(ReMonitors, []);
                _ ->
                    []
            end;
        _ ->
            []
    end.

%% @spec getAllTopn()-> Result
%% where
%%		Result=[[{Key,Value}|T]|Other]
%% @doc get all topn
getAllTopn() ->
    case topn_object_table:read() of
        V when erlang:is_list(V) ->
            getAllTopn_t(V, []);
        _ ->
            []
    end.
getAllTopn_t([], Results) ->
    Results;
getAllTopn_t([{Id, Topn}|T], Results) ->
    Ps =
    case Topn:get_properties() of
        {ok, Properties} ->
            Properties;
        _ ->
            []
    end,
    getAllTopn_t(T, [Ps|Results]);
getAllTopn_t([H|T], Results) ->
    getAllTopn_t(T, Results).
    

%% @spec getTopnById(Id)-> Result
%% where
%%		Id = string()
%%		Result=[[{Key,Value}|T]|Other]
%% @doc get topn by id
getTopnById(Id) ->
    case topn_object_table:read(Id) of
        [{Id, Obj}] ->
            case Obj:get_properties() of
                {ok, Ps} ->
                    Ps;
                _ ->
                    []
            end;
        _ ->
            []
    end.
    
    
    
toProperties([], Results) ->
    lists:reverse(Results);
toProperties([{Id, Value, Monitor}|T], Results) ->
    toProperties(T, [Monitor:get_properties()|Results]);
toProperties([H|T], Results) ->
    toProperties(T, Results).
    