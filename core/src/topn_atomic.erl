-module(topn_atomic, [BASE]).
-extends(topn_process).
-compile(export_all).

-include("topn.hrl").

new()->
    Obj = topn_process:new(),
	{?MODULE,Obj}.
    
update() ->
    Target = 
        case THIS:get_property(?TOPN_TARGET) of
            {ok, {?TOPN_TARGET, Tgs}} when erlang:is_list(Tgs) ->
                Tgs;
            _ ->
                []
        end,
    Results = stat_target(Target, []),
    THIS:set_attribute(?TOPN_RESULT_MONITORS, Results),
    %%io:format("Results: ~p~n", [Results]),
    %%CPUMonitors = siteview:get_object_by_class(browsa_cpu_utilization),
    ok.

stat_target([], Results) ->
    StatOrder = 
        case THIS:get_property(?TOPN_STATORDER) of
            {ok, {?TOPN_STATORDER, SO}} ->
                SO;
            _ ->
                0
        end,
    Res = lists:keysort(2, Results),
    NRes =
        case StatOrder of
            1 ->
                Res;
            _ ->
                lists:reverse(Res)
        end,
    Od =
        case THIS:get_property(?TOPN_ORDER) of
            {ok, {?TOPN_ORDER, Order}} ->
                Order;
            _ ->
                "left"
        end,
    Index = 
        case THIS:get_property(?TOPN_INDEX) of
            {ok, {?TOPN_INDEX, Idx}} ->
                Idx;
            _ ->
                1
        end,
    Count = 
        case THIS:get_property(?TOPN_COUNT) of
            {ok, {?TOPN_COUNT, Ct}} ->
                Ct;
            _ ->
                0
        end,
    NNRes =
    case Od of
        "right" ->
            RR = lists:reverse(NRes),
            lists:sublist(RR, Index, Count);
        _ ->
            lists:sublist(NRes, Index, Count)
    end;
stat_target([{MonitorClass, Attrs}|T], Results) ->
    Monitors = siteview:get_object_by_class(MonitorClass),
    Values =
    case Monitors of
        Ms when erlang:is_list(Monitors) ->
            stat_attrs(Monitors, Attrs, []);
        _ ->
            []
    end,
    stat_target(T, lists:append(Values,Results));
stat_target([H|T], Results) ->
    stat_target(T, Results).

stat_attrs(Monitors, [], Results) ->
    Results;
stat_attrs(Monitors, [{Attr, Priority}|T], Results) ->
    Re = stat_monitor(Monitors, {Attr, Priority}, Results),
    stat_attrs(Monitors, T, Re);
stat_attrs(Monitors, [{Attr, Priority}|T], Results) ->
    stat_attrs(Monitors, T, Results).
    

stat_monitor([], {Attr, Priority}, Results) ->
    Results;
stat_monitor([Monitor|OM], {Attr, Priority}, Results) ->
    if
        Priority =:= 1 ->
            Value =
            case Monitor:get_attribute(Attr) of
                {ok, {Attr, V}} when erlang:is_number(V) ->
                    V;
                _ ->
                    -1
            end,
            {ok, {id, Id}} = Monitor:get_property(id),
            stat_monitor(OM, {Attr, Priority}, [{Id, Value, Monitor}|Results]);
        true ->
            Results
    end.

%%stat_monitor_priority({Attr, Priority}, )
    
    
    
    