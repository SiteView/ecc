-module(service_monitor_client).
-compile(export_all).
-include("monitor_template.hrl").
-include("monitor.hrl").

parse_wsdl(File) ->
    try
    WSDL = yaws_soap_lib:initModel(File),
    Operations = yaws_soap_lib:wsdl_operations(WSDL),
    %mothed 's parameters
    Params = yaws_soap_lib:getparams(WSDL),
    %
    Opers = parse_operation(Operations,Params,[]),
    Ns = yaws_soap_lib:wsdl_namespace(WSDL),
    {ok, [{nameSpace,Ns},{operation,Opers}]}
    catch
    _:_ ->{error,"parse wsdl error"}
    end.
    
parse_operation([],_,Result) ->Result;
parse_operation([F|R],Params,Result) ->
    Name = yaws_soap_lib:wsdl_op_operation(F),
    Service = yaws_soap_lib:wsdl_op_service(F),
    Port = yaws_soap_lib:wsdl_op_port(F),
    ServiceURL = yaws_soap_lib:wsdl_op_address(F),
    Action = yaws_soap_lib:wsdl_op_action(F),
    Argument = get_argument(proplists:get_value(Name,Params)),
    parse_operation(R,Params,[{Name,Service,Port,ServiceURL,Action,Argument}|Result]).
    
get_argument(undefined) ->"";
get_argument(Arg) ->
    web_servicemonitor_final:generateAnArgforUI(Arg).
    
add_monitor(Id, Params, Classifier) when is_atom(Id)->
    M = url_sequence_monitor:new(),
    Properties = api_monitor_template:get_template(web_service_monitor),
    %Refresh the page to get all the values ??of some
    Config = url_sequence_client:prepare_params_for_add(Properties,Params,[]),
    M:delete(),
    ClassifierData = url_sequence_client:build_classifier(Classifier,Properties,[]),
    api_monitor:create(Id, [{id,undefined},{class,web_service_monitor}]++ClassifierData++Config);
add_monitor(_,_,_) ->{error, params_error}.

