-module(incident_filter).
-define(Path, "/templates.incident/filters/").
-compile(export_all).
-include("itsm_structure.hrl").

is_lock(EngineID) ->
	global:set_lock({EngineID, atom_to_list(node()) ++ pid_to_list(self())}, [node()|nodes()]).	
					
release_lock(EngineID) ->	
	global:del_lock({EngineID, atom_to_list(node()) ++ pid_to_list(self())}, [node()|nodes()]).

%% save filter
save_filters(Host, Filters=#itsm_filters{}) ->  
    FId = Filters#itsm_filters.id,
    FileName = "filters_" ++ FId ++ ".filter",
    is_lock(FileName),
    RootPath = platform:getRoot(),
    AppPath = filename:nativename(RootPath ++ ?Path ++ Host ++ "/"),
    make_dir(AppPath),
    FilePath = filename:nativename(AppPath ++ "/" ++ FileName),    
    Conditions = format_filters(Filters#itsm_filters.condition),
    Fil = Filters#itsm_filters{condition=Conditions},
    Result = 
    case file:open(FilePath, write) of
        {ok, S} ->
            try io:format(S, "~p.~n", [Fil]) of
                _ ->
                    {ok, "incident filter ok"}
            catch
                _:_ ->
                    {error, "incident filter fail"}
            after
                file:close(S)
            end;
        _ ->
            {error, "incident filter fail"}
    end,
    release_lock(FileName),
    Result.

format_filters([]) ->
    [];
format_filters([{Id, Condition=#fielter_condition{}}|T]) ->
    [{Id, Condition}] ++
    format_filters(T);
format_filters([H|T]) ->
    format_filters(T).

make_dir(Path) ->
    case filelib:is_dir(Path) of
        true ->
            {ok, "exited"};
        _ ->
            case file:make_dir(Path) of
                ok ->
                    {ok, "create ok"};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% read filter
read_filters(Host, FId) ->
    RootPath = platform:getRoot(),
    AppPath = filename:nativename(RootPath ++ ?Path ++ Host ++ "/"),
    %%io:format("AppPath = ~p~n", [AppPath]),
    FileName = "filters_" ++ FId ++ ".filter",
    FilePath = filename:nativename(AppPath ++ "/" ++ FileName),    
    %%io:format("FilePath = ~p~n", [FilePath]),
    file:consult(FilePath).
    
%% read filter, Read non-designated application filters
read_filters(FId) ->
    RootPath = platform:getRoot(),
    AppPath = filename:nativename(RootPath ++ ?Path ++ "/"),
    %%io:format("AppPath = ~p~n", [AppPath]),
    FileName = "filters_" ++ FId ++ ".filter",
    FilePath = filename:nativename(AppPath ++ "/" ++ FileName),    
    %%io:format("FilePath = ~p~n", [FilePath]),
    file:consult(FilePath).

%% delete filter
delete_filters(Host, FId) ->
    RootPath = platform:getRoot(),
    AppPath = filename:nativename(RootPath ++ ?Path ++ Host ++ "/"),
    FileName = "filters_" ++ FId ++ ".filter",
    FilePath = filename:nativename(AppPath ++ "/" ++ FileName),    
    case file:delete(FilePath) of
        ok ->
            {ok, "delete ok"};
        {error, Reason} ->
            {error, Reason}
    end.
    