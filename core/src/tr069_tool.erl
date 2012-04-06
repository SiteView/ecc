-module(tr069_tool).
-compile(export_all).
-include("monitor.hrl").
-include("config.hrl"). 


%% According to tr069 construct their id list, device list
getIdListFromDevices([]) ->
    [];
getIdListFromDevices([Dev = #tr069_device{}|T]) ->
    [genDeviceId(Dev)] ++ 
    getIdListFromDevices(T);
getIdListFromDevices([H|T]) ->
    getIdListFromDevices(T).
    

%% According to the list of construction equipment manufacturers list tr069
getManufactureListFromDevices(Devs) ->
    getManufactureListFromDevices_t(Devs, []).
getManufactureListFromDevices_t([], Results) ->
    Results;
getManufactureListFromDevices_t([Dev = #tr069_device{}|T], Results) ->
    NResult = 
        case lists:member(Dev#tr069_device.manufacturer, Results) of
            true ->
                Results;
            _ ->
                lists:append(Results, [Dev#tr069_device.manufacturer])
        end,
    getManufactureListFromDevices_t(T, NResult);
getManufactureListFromDevices_t([H|T], Results) ->
    getManufactureListFromDevices_t(T, Results).
    

%% List of devices constructed according to tr069 oui list
getOuiListFromDevices(Devs) ->
    getOuiListFromDevices_t(Devs, []).
getOuiListFromDevices_t([],Results) ->
    Results;
getOuiListFromDevices_t([Dev = #tr069_device{}|T], Results) ->
    NResult = 
        case lists:member(Dev#tr069_device.oui, Results) of
            true ->
                Results;
            _ ->
                lists:append(Results, [Dev#tr069_device.oui])
        end,
    getOuiListFromDevices_t(T, NResult);
getOuiListFromDevices_t([H|T], Results) ->
    getOuiListFromDevices_t(T, Results).
    
 
%% According to the list of devices constructed Serialnumber list tr069
getSerialnumberListFromDevices(Devs) ->
    getSerialnumberListFromDevices_t(Devs, []).
getSerialnumberListFromDevices_t([], Results) ->
    Results;
getSerialnumberListFromDevices_t([Dev = #tr069_device{}|T], Results) ->
    NResult = 
        case lists:member(Dev#tr069_device.serialnumber, Results) of
            true ->
                Results;
            _ ->
                lists:append(Results, [Dev#tr069_device.serialnumber])
        end,
    getSerialnumberListFromDevices_t(T, NResult);
getSerialnumberListFromDevices_t([H|T], Results) ->
    getSerialnumberListFromDevices_t(T, Results).



%% Id according to device information structure
genDeviceId(Dev = #tr069_device{}) ->
    Manufacturer = Dev#tr069_device.manufacturer,
    Oui = Dev#tr069_device.oui,
    SerialNumber = Dev#tr069_device.serialnumber,
    textutils:replacespace(Manufacturer)++"_"++Oui++"_"++SerialNumber.
    
    
    