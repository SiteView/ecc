-module(monitorc2erlang).
-export([getrefreshedmonitor/3]).
-on_load(init/0).
init() ->
    ok = erlang:load_nif("ecc8monitor/monitorConn", 0).
getrefreshedmonitor(_paras,_monitordll,_monitorfun) ->
    exit(nif_library_not_loaded).
