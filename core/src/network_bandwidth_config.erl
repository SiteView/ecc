%% ---
%% network_bandwidth_config
%%
%%---
-module(network_bandwidth_config).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").

%%xmlfile nameºÍets table name
-define(XMLFILE, "templates.applications/netbandwidth.xml").
-define(ETSNAME, ecc_network_bandwidth_config_ets).

%%global string
-define(DEVICE, "device").
-define(IDENTIFIER, "identifier").
-define(DISPLAY_NAME, "displayName").
-define(DEVICE_METRICS, "deviceMetrics").
-define(METRIC, "metric").
-define(OID, "OID").
-define(NAME, "metricName").
-define(NAME_OID, "metricNameOID").
-define(UNITS, "units").
-define(REAL_TIME, "realTime").
-define(SAME_GRAPH, "sameGraph").
-define(MULTIPLE_INSTANCES, "multipleInstances").


make_a_set() ->
    ets:new(?ETSNAME, [public,named_table]). 
    
    
get_attribute(Item) ->
    case ets:lookup(?ETSNAME, Item) of
        undefined ->
            {error, undefine_item};
        [{Item, Value}] ->
            {ok, Value};
        Other ->
            {error, Other}
    end.
    
set_attribute(Item, Value) ->
    ets:insert(?ETSNAME,{Item, Value}).

new() ->
    make_a_set(),
    set_attribute(maxProperties, 25),
    set_attribute(deviceIDToDisplayName, []),
    set_attribute(deviceIDToNode, []).
    
    
execute_static() ->
    new(),
    set_attribute(configFilePath, ?XMLFILE),
    set_attribute(configDocument, null),
    {ok, Path} = get_attribute(configFilePath),
    set_attribute(maxProperties, 20),
    case parse(Path) of
        {ok, ConfigDocument} ->
            set_attribute(configDocument, ConfigDocument),
            NodeList = getElementsByTagName(ConfigDocument, device),
            setDeviceTypeHs(NodeList),
            %%io:format("*********deviceIDToDisplayName = ~p~n", [get_attribute(deviceIDToDisplayName)]),
            "start network_bandwidth_config successful";
        {error, Reasons} ->
            {error, Reasons};
        Other ->
            {error, Other}
    end.
    
execute_getIDsandDisplayNames() ->
    F = fun(X1) ->
            case X1 of
                {Id, Dis} ->
                    {Dis, Id};
                _->
                    []
            end
        end,
    case get_attribute(deviceIDToDisplayName) of
        {ok, IdToDis} ->
            [F(X) || X <- IdToDis];
        {error, Reason} ->
            io:format("******error~n"),
            {error, Reason};
        Other ->
            io:format("******error~n"),
            {error, Other}
    end.
 
execute_getDeviceInstance(S, MonitorObj) ->  
    Node = deviceIDToNodeGet(S),
    if
        Node =:= [] ->
            [];
        true ->
            buildDevice(Node, MonitorObj)
    end. 

buildDevice(Element=#xmlElement{}, MonitorObj) ->
    Content = parseElement(Element#xmlElement.content),
    case proplists:get_value(identifier, Content) of
        undefined ->
            Id = [];
        ID ->
            Id = getTextFromXmlText(ID)
    end,
    case proplists:get_value(displayName, Content) of
        undefined ->
            Dis = [];
        DIS ->
            Dis = getTextFromXmlText(DIS)
    end,
    case proplists:get_value(deviceMetrics, Content) of
        undefined ->
            DevMetrics = [];
        DEVMetrics ->
            DevMetrics = parseDevMetrics(DEVMetrics)
    end,
    %%io:format("*************DevMetrics = ~p~n", [DevMetrics]),
    
    DtNow = sv_datetime:now(),
    OldSeed = random:seed0(),
    if
        OldSeed =:= {DtNow, 0, 0} ->
            random:seed(0, DtNow, 0);
        OldSeed =:= {0, DtNow, 0} ->
            random:seed(0, 0, DtNow);
        OldSeed =:= {0, 0, DtNow} ->
            random:seed(DtNow, 0, 0);
        true ->
            random:seed(DtNow, 0, 0)
    end,
    Ran = random:uniform(),
    Pidatom = list_to_atom("ecc_snmpdev_"++pid_to_list(self()) ++ float_to_list(Ran)),
    if
        Id =/= [] andalso Dis =/= [] andalso DevMetrics =/= [] ->
            M = snmp_device:new(Id, Dis, DevMetrics, MonitorObj),
            try M:init() of
                _ ->
                    M
            catch
                _:_ -> []
            end;
        true ->    
            []
    end.        



setDeviceTypeHs([]) ->
    [];
setDeviceTypeHs([Element=#xmlElement{}|T]) ->
    Content = getDeviceTypeId(Element#xmlElement.content),
    case proplists:get_value(identifier, Content) of
        undefined ->
            Id = [];
        ID ->
            Id = ID
    end,
    case proplists:get_value(displayName, Content) of
        undefined ->
            Dis = [];
        DIS ->
            Dis = DIS
    end,
    case get_attribute(deviceIDToDisplayName) of
        {ok, DeviceIDToDisplayName} ->
            proplists:delete(Id, DeviceIDToDisplayName),
            set_attribute(deviceIDToDisplayName,DeviceIDToDisplayName ++ [{Id, Dis}]);
            %%io:format("****deviceIDToDisplayName = ~p~n", [get_attribute(deviceIDToDisplayName)]);
        _->
            do_nothing
    end,
    case get_attribute(deviceIDToNode) of
        {ok, DeviceIDToNode} ->
            proplists:delete(Id, DeviceIDToNode),
            set_attribute(deviceIDToNode,DeviceIDToNode ++ [{Id, Element}]);
        _ ->
            do_nothing
    end,
    setDeviceTypeHs(T);
setDeviceTypeHs([H|T]) ->
    setDeviceTypeHs(T).
    
    
getDeviceTypeId([]) ->
    [];
getDeviceTypeId([Element=#xmlElement{}|T]) ->
    case Element#xmlElement.name of
        identifier ->
            [{identifier,getTextFromXmlText(Element#xmlElement.content)}] ++
            getDeviceTypeId(T);
        displayName ->
            [{displayName,getTextFromXmlText(Element#xmlElement.content)}] ++
            getDeviceTypeId(T);
        _->
            getDeviceTypeId(T)
    end;
getDeviceTypeId([H|T]) ->
    getDeviceTypeId(T).
    
deviceIDToNodeGet(S) ->
    case get_attribute(deviceIDToNode) of
        {ok, NodeList} ->
            getNodeByEId(S, NodeList);
        {error, Reason} ->
            [];
        Other ->
            []
    end.    


parseDevMetrics([]) ->
    [];
parseDevMetrics([Element=#xmlElement{}|T]) ->
    NewElement = parseDevMetric(Element#xmlElement.content),
    case proplists:get_value('OID', NewElement) of
        undefined ->
            OID = [];
        OID ->
            ok
    end,
    if
        OID =:= [] ->
            [];
        true ->
            [{OID, NewElement}]
    end ++
    parseDevMetrics(T);
parseDevMetrics([H|T]) ->
    parseDevMetrics(T).


parseDevMetric([]) ->
    [];
parseDevMetric([Element=#xmlElement{}|T]) ->
    [{Element#xmlElement.name, getTextFromXmlText(Element#xmlElement.content)}] ++
    parseDevMetric(T);
parseDevMetric([H|T]) ->
    parseDevMetric(T).


%%********************************
%%***********Xml option**************
%%********************************


%%Xml element from a list of elements which identify the sub-elements called S
getNodeByEId(S, NodeList) ->
    case proplists:get_value(S, NodeList) of
                undefined ->
                    [];
                Node ->
                    Node
    end.
    

getTextFromXmlText([Text=#xmlText{}]) ->
    Text#xmlText.value;
getTextFromXmlText(H) ->
    [].
    
%%Parsing an xml element for a list of attributes, each attribute as a child xml element, each attribute key name for the child elements, value value for the sub-elements
parseElement([]) ->
    [];
parseElement([Element=#xmlElement{}|T]) ->
    [{Element#xmlElement.name, Element#xmlElement.content}] ++
    parseElement(T);
parseElement([H|T]) ->
    parseElement(T).

%%Through the xml file name for the xml file
parse(FileName) ->
    case (catch xmerl_scan:file(FileName)) of
        {Xml=#xmlElement{}, Rest} ->
            %%io:format("*****Xml=~p~n", [Xml]),
            {ok, Xml};
        Other ->
            {error, Other}
    end.
    
getElementsByTagName(Element=#xmlElement{}, NodeName) ->
    cysContent(Element#xmlElement.content, NodeName);
getElementsByTagName(T, NodeName) ->
    [].


cysContent([], NodeName) ->
    [];
cysContent([Element=#xmlElement{}|T], NodeName) ->
    Name = Element#xmlElement.name,
    if
        Name =:= NodeName ->
            [Element] ++
            cysContent(T, NodeName);
        true ->
            cysContent(T, NodeName)
    end;
cysContent([K|T], NodeName) ->
    cysContent(T, NodeName).
 
     
    
%%********************
%%**External Interface *********
%%********************

static() ->
    try execute_static() of
        Value ->
            {ok,Value}
    catch
        _:Reason ->
            {error,Reason}
    end.
    
getIDsandDisplayNames() ->
    try execute_getIDsandDisplayNames() of
        Value ->
            {ok, Value}
    catch
        _:Reason ->
            {error, Reason}
    end.
    
getDeviceInstance(S, Snmpsession, L, MonitorObj) ->
    Device = getDeviceInstance(S, MonitorObj),
    if
        Device =:= [] ->
            [];
        true ->
            Device:setSession(Snmpsession),
            Device:setRealTimeDataWindow(L),
            Device
    end.

getDeviceInstance(S, MonitorObj) ->
    execute_getDeviceInstance(S, MonitorObj).


    