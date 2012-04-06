-module(incident_init).
-compile(export_all).
-include("itsm_structure.hrl").

init_project() ->
    %%RootPath = platform:getRoot(),
    %%DefultProjFileName = filename:nativename(RootPath ++ "/templates.incident/default_project.incident"),
    %%io:format("FileName = ~p~n", [DefultProjFileName]),
    %%case file:consult(DefultProjFileName) of
    %%    {ok, Aps} ->
    %%        create_defualt_project(Aps),
    %%        ok;
    %%    Ots ->
    %%        Ots
    %%end.
    ok.

create_defualt_project([]) ->
    [];
create_defualt_project([Aps=#itsm_sys_app_application{}|T]) ->
    Id = Aps#itsm_sys_app_application.id,
    Title = Aps#itsm_sys_app_application.title,
    Name = Aps#itsm_sys_app_application.name,
    Hint = Aps#itsm_sys_app_application.hint,
    Active = Aps#itsm_sys_app_application.active,
    Order = Aps#itsm_sys_app_application.order,
    Category = Aps#itsm_sys_app_application.category,
    Roles = Aps#itsm_sys_app_application.roles,
    Device_type = Aps#itsm_sys_app_application.device_type,
    Workflow = Aps#itsm_sys_app_application.workflow,
    case api_itsm:get_application(Id) of
        {ok, V=#itsm_sys_app_application{}} ->
            [];
        _ ->
            api_itsm:save_application_withdefaultmodule(Id,Title,Name,Hint,Active,Order,Category,Roles,Device_type, Workflow)
    end,
    create_defualt_project(T);
create_defualt_project([H|T]) ->
    create_defualt_project(T).