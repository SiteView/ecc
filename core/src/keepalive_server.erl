%
%keepalive_server
%
-module(keepalive_server).
-compile(export_all).
-include("monitor.hrl").

-define(Table,"tr069_device").
-define(Author,<<"lei.lin@dragonflow.com">>).
%-define(DBName,server_conf:get_db_node()).

-define(TimeMultiples,3).

-record(application, {id, title, published, updated, author, description,
                        viewSourceUrl, privateSource, runOwnAds,
                        xn_premiumService,  xn_tag, xn_category, xn_domain,
                        xn_layoutVersion, xn_active, link, seqenceNumber, storeLimit, contentCount = 0}). 

start(DBNode) ->
	case whereis('keepalive_server') of
		undefined->
			%%register('keepalive_server',spawn(node(),?MODULE,loop,[DBNode]));
            {ok, "donothing"};
		_->
			{error,{"Keepalive_server",already_started}}
	end.     

loop(DB) ->
    Apps = all_app(DB),
    presss_statu(Apps),
    timer:sleep(15000),
    loop(DB).


presss_statu(Apps) ->
    presss_statu_t(Apps,length(Apps)).
presss_statu_t(_A,0) -> ok;
presss_statu_t([A|B],Len) ->
    DeviceList = dbcs_tr069:get_keepalive_alive_all(A),
    device_statu(DeviceList,A),
    presss_statu_t(B,Len-1).     
     
all_app(DB) ->     
    case rpc:call(DB,ets,tab2list,[application]) of
    Apps when is_list(Apps) ->
        F = fun(X) -> X#application.id end,
        lists:map(F,Apps);
    _ ->
        []
    end.        
     

device_statu(DeviceList,App) ->
    device_statu_t(DeviceList,length(DeviceList),App).
device_statu_t(_D,0,_A) -> ok;
device_statu_t([A|B],Len,App) ->
    Flag = (calendar:datetime_to_gregorian_seconds(erlang:localtime()) > (A#tr069_device.timestamp + A#tr069_device.keepalivetime * ?TimeMultiples)),
    if Flag ->
        if A#tr069_device.description == undefined ->
            New = A#tr069_device{timestamp = integer_to_list(A#tr069_device.timestamp),state = "dead",keepalive = integer_to_list(A#tr069_device.keepalive),keepalivetime = integer_to_list(A#tr069_device.keepalivetime),description=""},        
            dbcs_tr069:update_device(New,App),
            device_statu_t(B,Len-1,App);
        true ->
            New = A#tr069_device{timestamp = integer_to_list(A#tr069_device.timestamp),state = "dead",keepalive = integer_to_list(A#tr069_device.keepalive),keepalivetime = integer_to_list(A#tr069_device.keepalivetime)},        
            dbcs_tr069:update_device(New,App),
            device_statu_t(B,Len-1,App)
        end; 
    true ->
        device_statu_t(B,Len-1,App)  
    end.
        
     
     
     