-module(itsm_incident).
-compile(export_all).
-include("itsm_structure.hrl").

%% ***********************************************
%% ***filter condition********************************
%% ***********************************************

parse_filters_to_contenstore_where(Filters) ->
    Fil = string:tokens(string:strip(Filters), "^"),
    FF =
    case Fil of
        [] ->
            "";
        _ ->
            build_where(Fil)
    end,
    Len = string:len(FF),
    Len1 = string:rstr(FF, "&"),
    FF1 = 
    if
        Len1 =:= Len,Len > 0 ->
            %%io:format("sss~n"),
            string:substr(FF, 1, Len1-1);
        true ->
            FF
    end,
    %%io:format("FF1 = ~p~n", [FF1]),
    FF1.
    

build_where([]) ->
    [];
build_where([H|T]) ->
    Where =
    case erlang:is_list(H) of
        true ->
            Fils = string:tokens(H, "="),
            case Fils of
                [] ->
                    "";
                [Field,Condition] ->
                    NCondition =
                    case Condition of
                        "NULL" ->
                            "''";
                        _ ->
                            case Condition of
                                "@me" ->
                                    {UId,_,_} = wf:session(loginuseraccount),
                                    UId;
                                _ ->
                                    Condition
                            end
                    end,
                    "my."++string:strip(Field)++"="++string:strip(NCondition)++"&";
                _ ->
                    ""
                    
            end;
        _ ->
            []
    end,
    Where ++ build_where(T).

%% Convert a list of items [{Id, Project}|T]
get_kv_projects([]) ->
    [];
get_kv_projects([Project=#itsm_sys_app_application{}|T]) ->
    %%io:format("sdfsdf~n"),
    [{Project#itsm_sys_app_application.id, Project}] ++ 
    get_kv_projects(T);
get_kv_projects([H|T]) ->
    get_kv_projects(T).

%% ------ filter

%% get contentstore where
get_contentstore_where(FId, AppId) ->
    case api_itsm:read_filters(FId) of
        {ok, VFF} ->
            %%io:format("VFF = ~p~n", [VFF]),
            build_contentstore_where(VFF, AppId);
        _ ->
            []
    end.
    
get_contentstore_where_ex(VFF=#itsm_filters{}, AppId) ->
    exe_contentstore_where(VFF#itsm_filters.condition, 1, AppId).
    
%% build contentstore  where
build_contentstore_where([], AppId) ->
    [];
build_contentstore_where([Fil=#itsm_filters{}|T], AppId) ->
    exe_contentstore_where(Fil#itsm_filters.condition, 1, AppId) ++
    build_contentstore_where(T, AppId);
build_contentstore_where([H|T], AppId) ->
    build_contentstore_where(T, AppId).
    
exe_contentstore_where([], Count, AppId) ->
    [];
exe_contentstore_where([{Id,Condition=#fielter_condition{}}|T], Count, AppId) ->
    Relation = Condition#fielter_condition.relation,
    Field = Condition#fielter_condition.field,
    Operation = Condition#fielter_condition.operation,
    Value = 
        case Condition#fielter_condition.value of
            "@me" ->
                {UId,_,_} = wf:session(loginuseraccount),
                UId;
            "@project" ->
                case AppId of
                    [] ->
                        "''";
                    _ ->
                        AppId
                end;
            [] ->
                "''";
            _ ->
                Condition#fielter_condition.value
        end,
    case Count of
        1 ->
            "my." ++ Field ++ " " ++ Operation ++ " " ++ Value;
        _ ->
            Relation ++ "my." ++  Field ++ " " ++ Operation ++ " " ++ Value
    end ++
    exe_contentstore_where(T, Count+1, AppId);
exe_contentstore_where([H|T], Count, AppId) ->
    exe_contentstore_where(T, Count, AppId).


%% ****************************************************


%% build  Number
get_new_incident_number(Number, AppId, MaxNumId) ->
    %%ioio:format("Number = ~p~n", [Number]),
    ProjectName = 
    case api_itsm:get_application(AppId) of
        {ok, Application=#itsm_sys_app_application{}} ->
            Application#itsm_sys_app_application.name;
        _ ->
            {error, "project_delete"}
    end,
    case ProjectName of
        {error, Vs} ->
            {error, Vs};
        _ ->
            %%IsNumber = is_incident_number(Number),
            NewNumber = erlang:integer_to_list(erlang:list_to_integer(Number)+1),
            %%    if
            %%        IsNumber =:= true  ->
            %%            ONb = string:substr(Number, 1, 3),
            %%            TNb = string:substr(Number, 4, 2),
            %%            TrNb = string:substr(Number, 6, 2),
            %%            ONInt = erlang:list_to_integer(ONb),
            %%            TNbInt = erlang:list_to_integer(TNb),
            %%            TrNbInt = erlang:list_to_integer(TrNb),
            %%            build_TrNumber(TrNbInt,TNbInt, ONInt);
            %%        true ->
            %%            "0010101"
            %%    end,
            api_itsm:save_max_incident_number(MaxNumId,NewNumber),
            ProjectName++"-"++NewNumber
    end.
    
build_TrNumber(TrNbInt,TNbInt, ONInt) ->
    if
        TrNbInt < 99 ->
            Tr = erlang:integer_to_list(TrNbInt + 1),
            build_TNbInt(TNbInt, ONInt)++string:right(Tr, 2, $0);
        TrNbInt >= 99 ->
            build_TNbInt(TNbInt+1, ONInt)++"01"
    end.
    
build_TNbInt(TNbInt,ONInt) ->
    if
        TNbInt =< 99 ->
            T = erlang:integer_to_list(TNbInt),
            build_ONInt(ONInt)++string:right(T, 2, $0);
        TNbInt > 99 ->
            build_ONInt(ONInt+1)++"01"
    end.

build_ONInt(ONInt) ->
    if
        ONInt =< 999 ->
            O = erlang:integer_to_list(ONInt),
            string:right(O, 3, $0);
        ONInt > 999 ->
            "001"
    end.
    
%% Regular work orders to determine whether the number
is_incident_number(Number) ->
    RegExp = "[0-9][0-9][0-9][0-9][0-9][0-9][0-9]",   %%data
    case Number of
        [] ->
            false;
        V when erlang:is_list(Number) ->
            case regexp:match(Number, RegExp) of
                {match,1,7} ->
                    true;
                _ ->
                    false
            end;
        _ ->
            false
    end.
    
%% get all user§[{Name, Id}]
get_users([]) ->
    [];
get_users([{Id, Name, Title, Disabled}|T]) ->
    [{Name,Id}] ++
    get_users(T);
get_users([H|T]) ->
    get_users(T).

%% From [{Name, Id} | T] to obtain the specified Value of Name
getname_fromkv(Vs,Value) ->
    case lists:keysearch(Value, 2, Vs) of
        {value, {Name, Id}} ->
            Name;
        false ->
            [];
        _ ->
            []
    end.
