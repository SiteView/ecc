-module(wmiScript).
-compile(export_all).

-define(VBS_DIR, "vbs").

wmiExeBat(Hosts, User, Pwd, Cmd) ->
    io:format("Files: ~p~n", [filename:nativename(Cmd)]),
    Host = get_host(Hosts),
    %%case file:read_file(filename:nativename(Cmd)) of
    %%    {ok, CmdBinary} ->
    %%        VBS = filename:nativename(?VBS_DIR++"/"++"wmiExeCmd.vbs"),
    %%        CmdStr = erlang:binary_to_list(CmdBinary),
    %%        CmdList = string:tokens(CmdStr, "\r\n"),
    %%        CmdS = wmiExeBat_t(CmdList),
    %%        Cm = VBS++" "++Host++" "++User++" "++Pwd++" \""++ CmdS ++"\"",
    %%        io:format("Cm: ~p~n", [Cm]),
    %%        {ok, os:cmd(Cm)};
    %%    _ ->
    %%        {error, ""}
    %%end.
    case script(Host, User, Pwd, filename:nativename(Cmd)) of
        {ok, CmdStr} ->
            {ok, os:cmd(CmdStr)};
        _ ->
            {error, ""}
    end.

    

get_host([]) ->
    [];
get_host([92|Host]) ->
    get_host(Host);
get_host(Host) ->
    Host.
    
script(Host, User, Pwd, Cmd) ->  
    RootName = filename:nativename(filename:rootname(Cmd)),
    Length = string:len(RootName),
    AfterName = string:substr(Cmd, Length+1),
    case string:str(AfterName, ".") of
        1 ->
            {Ext, Af} = script_ext(AfterName, []),
            FileName = RootName ++ Ext,
            case file:read_file(FileName) of
                {ok, CmdBinary} ->
                    VBS = filename:nativename(?VBS_DIR++"/"++"wmiExeCmd.vbs"),
                    CmdStr = erlang:binary_to_list(CmdBinary),
                    CmdList = string:tokens(CmdStr, "\r\n"),
                    CmdS = wmiExeBat_t(CmdList),
                    Env = "set V1=" ++ Af ++ "&&",
                    %%Env = "",
                    Cm = VBS++" "++Host++" "++User++" "++Pwd++" \""++ Env ++ CmdS ++"\"",
                    io:format("Cm: ~p~n", [Cm]),
                    {ok, Cm};
                _ ->
                    {error, ""}
            end;
        _ ->
            {error, ""}
    end.
script_ext([], R) ->
    {R, []};
script_ext([32|T], R) ->
    {R, string:strip(T)};
script_ext([H|T], R) ->
    script_ext(T, R++[H]).
wmiExeBat_t([]) ->
    [];
wmiExeBat_t([Cmd|T]) when erlang:is_list(Cmd) ->
    case string:strip(T) of
        "" ->
            Cmd;
        _ ->
            Cmd ++ "&&"
    end ++
    wmiExeBat_t(T);
wmiExeBat_t([H|T]) ->
    wmiExeBat_t(T).
    
	