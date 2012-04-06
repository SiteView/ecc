-module(snmp_machine_config).

-compile(export_all).


-define(SYSOIDCFG, "templates.nnm/specialoidlist.dat").

%% 读取SysOid所对应的设备厂商型号所对应的配置, 配置包含取其cpu, memory的oid
readSysOidConfig(SysOid) ->
    Path = filename:nativename(?SYSOIDCFG),
    case file:consult(Path) of
        {ok, Config} ->
            readSysOidConfig_t(Config, SysOid);
        _ ->
            {error, 'read_error'}
    end.
readSysOidConfig_t([], _SysOid) ->
    {error, 'no_config'};
readSysOidConfig_t([{Oid, Param}|T], SysOid) ->    
	case lists:prefix(Oid, SysOid) of
        true ->
            {ok, Param};
        _ ->
            readSysOidConfig_t(T, SysOid)
    end;
readSysOidConfig_t([_H|T], SysOid) ->
    readSysOidConfig_t(T, SysOid).
    
