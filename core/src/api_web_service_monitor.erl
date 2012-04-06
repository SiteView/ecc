-module(api_web_service_monitor).
-compile(export_all).
-include("monitor_template.hrl").
-include("monitor.hrl").

-define(PATH,"templates.wsdl/").

get_file() ->
    Files = filelib:wildcard("templates.wsdl/*.wsdl"),
    if
        length(Files)>0 ->
            {ok, Files};
        true ->
            {error, "not found"}
    end.

get_params_from_wsdl(File) ->
    service_monitor_client:parse_wsdl(File).
    
add_monitor(Id, Params, Classifier) ->
    service_monitor_client:add_monitor(Id, Params, Classifier).