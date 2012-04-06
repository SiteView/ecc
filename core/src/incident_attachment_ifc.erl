-module(incident_attachment_ifc).
-compile(export_all).
-include("itsm_structure.hrl").

is_lock(EngineID) ->
	global:set_lock({EngineID, atom_to_list(node()) ++ pid_to_list(self())}, [node()|nodes()]).	
					
release_lock(EngineID) ->	
	global:del_lock({EngineID, atom_to_list(node()) ++ pid_to_list(self())}, [node()|nodes()]).

%% 保存附件  <<界面操作>>
save_attachments(RealPath, FileExt) ->  
    Root = platform:getRoot(),
    Host = get(hostname),
    RealName = RealPath,
    Name = filename:basename(RealName)++FileExt,
    AppPath = filename:nativename(Root ++ ?AttachMentPath ++ Host ++ "/"),
    %%io:format("AppPath = ~p~n", [AppPath]),
    FilePath = filename:nativename(AppPath ++ "/" ++ Name),  
    make_dir(AppPath),
    %%io:format("FilePath = ~p~n", [FilePath]),
    %%io:format("RealName = ~p~n", [RealName]),
    case file:copy(RealName, FilePath) of
        {ok, _} ->
            %%io:format("okok~n"),
            file:delete(RealName),
            Url = ?AttachMentUrl ++ Host ++ "/" ++ Name,
            {ok, FilePath, Url};
        {error, Reason} ->
            %%io:format("Reason = ~p~n", [Reason]),
            {error, "upload_error"}
    end.

%% 删除附件
delete_attachments(FileName) ->
    case file:delete(FileName) of
        ok ->
            {ok, "delete_attachment_ok"};
        _ ->
            {error, "delete_attachment_fail"}
    end.

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


    