%% ---
%% dbcs_tr069
%%
%%---
-module(dbcs_tr069).
-compile(export_all).
-include("monitor.hrl").
-include("config.hrl"). 

-define(Table,"tr069_device").
-define(Author,<<"lei.lin@dragonflow.com">>).
-define(DBName,server_conf:get_db_node()).
-define(RealtimeAlarmTable,"tr069_RealtimeAlarm").
-define(ParamCachesTable, "tr069_ParamCaches").
-define(NewParamCachesTable, "tr069_ParamCachesNew").
-define(UpSiteTable, "tr069_UpgradeSite").
-define(UpFileTable, "tr069_UpgradeFile").
-define(UpgradeStatusTable, "tr069_UpgradeStatus").
-define(LabelTable, "tr069_label").
-define(LabelIdTable, "tr069_label_id").
-define(UpradeAmount, "tr069_upgradeamount").


create_device(DeviceRecord,Host) ->
    case lc_util:wouldExceedLimit(1) of
            true ->
                %%io:format("license_limit = ~p~n", [true]),
                {error,license_limited};
            Other ->
                %%io:format("license_limit = ~p~n", [Other]),
                %%Adv = device_to_db(DeviceRecord),
                %%DeviceId =  list_to_atom(textutils:replacespace(DeviceRecord#tr069_device.manufacturer) ++"_"++DeviceRecord#tr069_device.oui ++"_"++ DeviceRecord#tr069_device.serialnumber),
                %%insert_data(Host,?DBName, ?Table, {content, list_to_atom(?Table), DeviceId, <<"device">>,null,null,null,null,?Author,null,null,null,null,null,Adv})
                %% cache cpe
                tr069_devicecache_server:save_cache_cpe(Host, [DeviceRecord])
    end.
 
update_device(DeviceRecord,Host) ->
	%%Id = textutils:replacespace(DeviceRecord#tr069_device.manufacturer) ++"_"++DeviceRecord#tr069_device.oui ++"_"++ DeviceRecord#tr069_device.serialnumber,
	%%Where = "id=" ++ Id,	
	%%Adv = device_to_db(DeviceRecord),
	%%NewRecord = {content, list_to_atom(?Table), list_to_atom(Id), <<"device">>, null, null, null, null, ?Author, null, null, null, null, null, Adv},
	%%update_data(Host,?DBName, ?Table, Where, NewRecord).
    %% cache cpe
    tr069_devicecache_server:save_cache_cpe(Host, [DeviceRecord]).

get_device(Host,Manufacturer,Oui,Serialnumber) ->    
    %%Ret = get_data(Host,?DBName, ?Table, "id =" ++  textutils:replacespace(Manufacturer) ++"_"++Oui++"_"++Serialnumber),
	%%case is_list(Ret) of
	%%	false ->
	%%		[];
	%%	true ->
	%%		Devs = [db_to_device(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret],
    %%        Devs
	%%end.
    %% cache cpe
    Ids = [textutils:replacespace(Manufacturer) ++"_"++Oui++"_"++Serialnumber],
    tr069_devicecache_server:get_one_cache_cpe(Host, Ids).

get_device(Manufacturer,Oui,Serialnumber) ->    
    %%Ret = db_ecc:get_data(?DBName, ?Table, "id =" ++  textutils:replacespace(Manufacturer) ++"_"++Oui++"_"++Serialnumber),
	%%case is_list(Ret) of
	%%	false ->
	%%		[];
	%%	true ->
	%%		[db_to_device(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	%%end.
    %% cache cpe
    %%Host = domain(get(hostname)),
    Host = textutils:any_to_list(get(hostname)),
    Ids = [textutils:replacespace(Manufacturer) ++"_"++Oui++"_"++Serialnumber],
    tr069_devicecache_server:get_one_cache_cpe(Host, Ids).

%-record(realtimealarm,{id,manufacturer,oui,productclass,serialnumber,xevent,description="",statu="untreated",notes="",timestamp,number}).
install_alarm(Manufacturer,Oui,ProductClass,Serialnumber,Xevent,Host) ->
    Timestamp = integer_to_list(calendar:datetime_to_gregorian_seconds(erlang:localtime())), 
    Deviceid = textutils:replacespace(Manufacturer) ++"_"++Oui++"_"++Serialnumber,
    Id=Deviceid++"_"++Timestamp,
    case get_samealarm(Manufacturer,Oui,Serialnumber,Xevent,Host) of
    [] -> 
        Description = event_to_description(Xevent),    
        Mach = #realtimealarm{id=Id,manufacturer=Manufacturer,oui=Oui,productclass=ProductClass,serialnumber=Serialnumber,xevent=Xevent,description=Description,statu="untreated",notes="",timestamp=Timestamp,number="1",timeofoccurrence=Timestamp},    
        Adv = alarm_to_db(Mach),
        insert_data(Host,?DBName, ?RealtimeAlarmTable, {content, list_to_atom(?RealtimeAlarmTable), list_to_atom(Id), <<"alarm">>,null,null,null,null,?Author,null,null,null,null,null,Adv});
    [Old|_] ->
        Oldid = Old#realtimealarm.id,  
        Num = Old#realtimealarm.number, 
        New = Old#realtimealarm{id=Oldid,number= Num+1,timeofoccurrence=Timestamp}, 
        update_alarm(Oldid,New,Host)       
    end.   

event_to_description(Xevent) ->
    AppExit = (string:str(Xevent,"AppExit") > 0 ),
    AppRestart = (string:str(Xevent,"AppRestart") > 0 ),
    DS1Down = (string:str(Xevent,"DS1Down") > 0 ),     
    DS1Up = (string:str(Xevent,"DS1Up") > 0 ), 
    ISDNDown = (string:str(Xevent,"ISDNDown") > 0 ),    
    ISDNUp = (string:str(Xevent,"ISDNUp") > 0 ),
    RegFail = (string:str(Xevent,"RegFail") > 0 ),
    RegOk = (string:str(Xevent,"RegOk") > 0 ),
    if AppExit ->
        Description = "AppExit";
    true ->
        if AppRestart ->
            Description = "AppRestart";
        true ->
            if DS1Down ->
                [_,LineNum] = string:tokens(Xevent,"-"),
                Description = "E1 "++ LineNum++" "++"DS1Down";
            true ->
                if DS1Up ->      
                    [_,LineNum] = string:tokens(Xevent,"-"),
                    Description = "E1 "++ LineNum++" "++"DS1Up";
                true ->
                    if ISDNDown ->
                        [_,LineNum] = string:tokens(Xevent,"-"),
                        Description = "Line "++ LineNum++" "++"ISDNDown";
                    true ->
                        if ISDNUp ->
                            [_,LineNum] = string:tokens(Xevent,"-"),
                            Description = "Line "++ LineNum++" "++"ISDNUp";
                        true ->
                            if RegFail ->
                                [_,LineNum] = string:tokens(Xevent,"-"),
                                if LineNum == "0" ->
                                    Description = "Overall "++" "++"RegFail";
                                true ->
                                    Description = "Line "++ LineNum++" "++"RegFail" 
                                end;
                            true ->
                                Description = "" 
                            end  
                        end
                    end  
                end 
            end  
        end 
    end. 


%-record(realtimealarm,{id,manufacturer,oui,productclass,serialnumber,xevent,description="",statu="untreated",notes="",timestamp,number}).
update_alarm(Id,AlarmRecord,Host) ->
    %%io:format("Id:~p~nAlarmRecord:~p~nHost:~p~n",[Id,AlarmRecord,Host]),
    Where = "id="++Id,    
    Timestamp = integer_to_list(calendar:datetime_to_gregorian_seconds(erlang:localtime())),  
    Num =AlarmRecord#realtimealarm.number, 
    Record = AlarmRecord#realtimealarm{timestamp=Timestamp,number=integer_to_list(Num)},  
    Adv = alarm_to_db(Record),
	NewRecord = {content, list_to_atom(?RealtimeAlarmTable), list_to_atom(Id), <<"alarm">>, null, null, null, null, ?Author, null, null, null, null, null, Adv},   
    
    update_data(Host,?DBName, ?RealtimeAlarmTable, Where, NewRecord).

page_update_alarm(Id,AlarmRecord) ->
    Where = "id="++Id,    
    Timestamp = integer_to_list(calendar:datetime_to_gregorian_seconds(erlang:localtime())),
    if AlarmRecord#realtimealarm.timeofoccurrence == undefined ->
        Timeofoccurrence = Timestamp;
    true ->
        Timeofoccurrence = integer_to_list(AlarmRecord#realtimealarm.timeofoccurrence)  
    end, 
    Num =AlarmRecord#realtimealarm.number, 
    Record = AlarmRecord#realtimealarm{timestamp=Timestamp,number=integer_to_list(Num),timeofoccurrence = Timeofoccurrence}, 
    Adv = alarm_to_db(Record),
	NewRecord = {content, list_to_atom(?RealtimeAlarmTable), list_to_atom(Id), <<"alarm">>, null, null, null, null, ?Author, null, null, null, null, null, Adv},   
    db_ecc:update_data(?DBName, ?RealtimeAlarmTable, Where, NewRecord).


get_samealarm(Manufacturer,Oui,Serialnumber,Xevent,Host) ->
    Where = "my.manufacturer="++Manufacturer++"&"++"my.oui="++Oui++"&"++"my.serialnumber="++Serialnumber++"&"++"my.xevent="++ Xevent++"&"++"my.statu="++"untreated",
    Ret = get_data(Host,?DBName, ?RealtimeAlarmTable,Where),
    case is_list(Ret) of
		false ->
			[];
		true ->
            if Ret == [] ->
                [];
            true ->                
			    [db_to_alarm(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret]
            end
    end.         


get_alarmbyid(AlarmId) ->
	Ret = db_ecc:get_data(?DBName, ?RealtimeAlarmTable, "id="++ AlarmId),
	case Ret of		
		[{content, _,_MId, _, _, _, _, _, _, _, _, _, _, _, Advance}] ->
			db_to_alarm(Advance);
 		_ ->
			#realtimealarm{}
	end.     

get_historyalarm() ->
    case wf:session(loginuseraccount) of
    undefined ->
        [];
    {"admin_user",_,_} ->        
	    Ret = db_ecc:get_data(?DBName, ?RealtimeAlarmTable, "my.statu=processed"),
	    case is_list(Ret) of
		    false ->
			    [];
		    true ->
			     [db_to_alarm(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	    end;
    {UserId,_,_} ->
        UserList = api_user_spl:get_user(UserId),
        case proplists:get_value(cpe,UserList) of
        undefined ->
            [];
        CpeList ->
	       Ret = db_ecc:get_data(?DBName, ?RealtimeAlarmTable, "my.statu=processed"),
	        case is_list(Ret) of
		        false ->
			        [];
		        true ->               
			       [db_to_alarm(Advance) ||{content, _, Id, _, _, _, _, _, _, _, _, _, _, _, Advance}<- Ret,lists:member(get_deviceid_from_alarmid(atom_to_list(Id)),CpeList)]                   
	        end                
        end;
    _ ->
        []    
    end. 
   %Ret = db_ecc:get_data(?DBName, ?RealtimeAlarmTable, "my.statu=processed"),
	%case is_list(Ret) of
		%false ->
			%[];
		%true ->
            %if Ret == [] ->
                %[];
            %true ->                
			    %[db_to_alarm(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret]
            %end
    %end.  

get_untreatedAlarmBydevice(Manufacturer,OUI,SerialNumber,Host) ->
    Ret = get_data(Host,?DBName, ?RealtimeAlarmTable, "my.manufacturer="++Manufacturer++"&"++"my.oui="++OUI++"&"++"my.serialnumber="++SerialNumber++"&"++"my.statu="++"untreated"),
	case is_list(Ret) of
		false ->
			[];
		true ->
            if Ret == [] ->
                [];
            true ->                
			    [db_to_alarm(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret]
            end
    end.            

get_untreatedAlarm() ->
    case wf:session(loginuseraccount) of
    undefined ->
        [];
    {"admin_user",_,_} ->        
	    Ret = db_ecc:get_data(?DBName, ?RealtimeAlarmTable, "my.statu="++"untreated"),
	    case is_list(Ret) of
		    false ->
			    [];
		    true ->
			     [db_to_alarm(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	    end;
    {UserId,_,_} ->
        UserList = api_user_spl:get_user(UserId),
        case proplists:get_value(cpe,UserList) of
        undefined ->
            [];
        CpeList ->
	        Ret = db_ecc:get_data(?DBName, ?RealtimeAlarmTable, "my.statu="++"untreated"),
	        case is_list(Ret) of
		        false ->
			        [];
		        true ->               
			       [db_to_alarm(Advance) ||{content, _, Id, _, _, _, _, _, _, _, _, _, _, _, Advance}<- Ret,lists:member(get_deviceid_from_alarmid(atom_to_list(Id)),CpeList)]                   
	        end                
        end;
    _ ->
        []    
    end. 
    %Ret = db_ecc:get_data(?DBName, ?RealtimeAlarmTable, "my.statu="++"untreated"),
	%case is_list(Ret) of
		%false ->
			%[];
		%true ->
            %if Ret == [] ->
                %[];
            %true ->                
			    %[db_to_alarm(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret]
            %end
    %end.    


%% @spec get_untreatedAlarm(Hostname, Where, Order) -> Result
%%	Hostname = (string())
%%	Where = (string())
%%	Order = (string())
%%	Result = ([#realtimealarm{}])
%% @doc get alerm by where and order
get_untreatedAlarm(Hostname, Where, Order) ->
    case wf:session(loginuseraccount) of
    undefined ->
        [];
    {"admin_user",_,_} ->     
        Ret = get_data2(Hostname, ?DBName, ?RealtimeAlarmTable, Where,Order),
        %%io:format("Ret = ~p~n", [Ret]),
	    %%Ret = db_ecc:get_data(?DBName, ?RealtimeAlarmTable, "my.statu="++"untreated"),
	    build_alert(Ret);
    {UserId,_,_} ->
        UserList = api_user_spl:get_user(UserId),
        case proplists:get_value(cpe,UserList) of
        undefined ->
            [];
        CpeList ->
	        Ret = get_data2(Hostname, ?DBName, ?RealtimeAlarmTable, Where,Order),
            %%io:format("AlertRet = ~p~n", [Ret]),
	        build_alert(Ret, CpeList)         
        end;
    _ ->
        []    
    end. 
    
    
build_alert({R1, R2, Count,[]}) ->
    [];
build_alert({R1, R2, Count,[{content, _,Id, _, _, _, _, _, _, _, _, _, _, _, Advance}|T]}) ->
    VCount = erlang:list_to_binary(erlang:integer_to_list(Count)),
    NAdv =  [{total, number, VCount}],
    [db_to_alarm(Advance++NAdv)] ++
    build_alert({R1, R2, Count,T});
build_alert({R1, R2, Count,[H|T]}) ->
    build_alert({R1, R2, Count,T});
build_alert(Other) ->
    [].


build_alert({R1, R2, Count,[]}, CpeList) ->
    [];
build_alert({R1, R2, Count,[{content, _,Id, _, _, _, _, _, _, _, _, _, _, _, Advance}|T]},CpeList) ->
    VCount = erlang:list_to_binary(erlang:integer_to_list(Count)),
    NAdv =  [{total, number, VCount}],
    Realtimealarm = db_to_alarm(Advance++NAdv),
    Manufacturer = Realtimealarm#realtimealarm.manufacturer,
    Oui = Realtimealarm#realtimealarm.oui,
    Productclass = Realtimealarm#realtimealarm.productclass,
    Serialnumber = Realtimealarm#realtimealarm.serialnumber,
    DeviceId = textutils:replacespace(Manufacturer)++"_"++Oui++"_"++Serialnumber, 
    IsRi = lists:member(DeviceId,CpeList),
    case IsRi of
        true ->
            [Realtimealarm];
        _ ->
            []
    end ++
    build_alert({R1, R2, Count,T},CpeList);
build_alert({R1, R2, Count,[H|T]},CpeList) ->
    build_alert({R1, R2, Count,T},CpeList);
build_alert(Other,CpeList) ->
    [].

get_deviceid_from_alarmid(AlarmId) ->
    List = string:tokens(AlarmId,"_"),
    [M] = lists:sublist(List,1,1),
    [O] = lists:sublist(List,2,1),
    [S] = lists:sublist(List,3,1), 
    M++"_"++O++"_"++S. 


get_all()->
    Devs = 
    case wf:session(loginuseraccount) of
    undefined ->
        [];
    {"admin_user",_,_} ->        
	    Ret = db_ecc:get_data(?DBName, ?Table, ""),
	    case is_list(Ret) of
		    false ->
			    [];
		    true ->
			    [db_to_device(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	    end;
    {UserId,_,_} ->
        UserList = api_user_spl:get_user(UserId),
        case proplists:get_value(cpe,UserList) of
        undefined ->
            [];
        CpeList ->
	        Ret = db_ecc:get_data(?DBName, ?Table, ""),
	        case is_list(Ret) of
		        false ->
			        [];
		        true ->                 
			       [db_to_device(Advance) ||{content, _, Id, _, _, _, _, _, _, _, _, _, _, _, Advance}<- Ret,lists:member(atom_to_list(Id),CpeList)]                   
	        end                
        end;
    _ ->
        []    
    end,
    %% cache cpe
    %%Host = domain(get(hostname)),
    Host = textutils:any_to_list(get(hostname)),
    NDevs = tr069_devicecache_server:get_cache_cpe(Host, Devs),
    NDevs.
    
get_all_where(Hostname, Where, Order)->
    Devs =
    case wf:session(loginuseraccount) of
    undefined ->
        [];
    {"admin_user",_,_} ->        
        Ret = get_data2(Hostname, ?DBName, ?Table, Where,Order),
        %%io:format("Order = ~p~n", [Order]),
		build_device(Ret);	    
    {UserId,_,_} ->
        UserList = api_user_spl:get_user(UserId),
        case proplists:get_value(cpe,UserList) of
        undefined ->
            [];
        CpeList ->
	        Ret = get_data2(Hostname, ?DBName, ?Table, Where,Order),                
			build_device(Ret, CpeList)
        end;
    _ ->
        []    
    end,
    %% cache cpe
    NDevs = tr069_devicecache_server:get_cache_cpe(Hostname, Devs),
    %%NDevs.
    NDevs.


get_cpe_where(Hostname, Where, Order)->
    Ret = get_data2(Hostname, ?DBName, ?Table, Where,Order),
        %%io:format("Order = ~p~n", [Order]),
	Devs = build_device(Ret),
    %% cache cpe
    NDevs = tr069_devicecache_server:get_cache_cpe(Hostname, Devs),
    %%NDevs.
    NDevs.
    
    
build_device({R1, R2, Count,[]}) ->
    [];
build_device({R1, R2, Count,[{content, _,Id, _, _, _, _, _, _, _, _, _, _, _, Advance}|T]}) ->
    VCount = erlang:list_to_binary(erlang:integer_to_list(Count)),
    NAdv =  [{total, number, VCount}],
    [db_to_device(Advance++NAdv)] ++
    build_device({R1, R2, Count,T});
build_device({R1, R2, Count,[H|T]}) ->
    build_device({R1, R2, Count,T});
build_device(Other) ->
    [].


build_device({R1, R2, Count,[]}, CpeList) ->
    [];
build_device({R1, R2, Count,[{content, _,Id, _, _, _, _, _, _, _, _, _, _, _, Advance}|T]},CpeList) ->
    IsRi = lists:member(atom_to_list(Id),CpeList),
    case IsRi of
        true ->
            VCount = erlang:list_to_binary(erlang:integer_to_list(Count)),
            NAdv =  [{total, number, VCount}],
            [db_to_device(Advance++NAdv)];
        _ ->
            []
    end ++
    build_device({R1, R2, Count,T},CpeList);
build_device({R1, R2, Count,[H|T]},CpeList) ->
    build_device({R1, R2, Count,T},CpeList);
build_device(Other,CpeList) ->
    [].


get_cpe_by_user(User)->
    Devs =
    case User of
    "admin_user" ->        
	    Ret = db_ecc:get_data(?DBName, ?Table, ""),
	    case is_list(Ret) of
		    false ->
			    [];
		    true ->
			    [db_to_device(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	    end;
    UserId ->
        UserList = api_user_spl:get_user(UserId),
        case proplists:get_value(cpe,UserList) of
        undefined ->
            [];
        CpeList ->
	        Ret = db_ecc:get_data(?DBName, ?Table, ""),
	        case is_list(Ret) of
		        false ->
			        [];
		        true ->                 
			       [db_to_device(Advance) ||{content, _, Id, _, _, _, _, _, _, _, _, _, _, _, Advance}<- Ret,lists:member(atom_to_list(Id),CpeList)]                   
	        end                
        end;
    _ ->
        []    
    end,
    %% cache cpe
    %%Host = domain(get(hostname)),
    Host = textutils:any_to_list(get(hostname)),
    NDevs = tr069_devicecache_server:get_cache_cpe(Host, Devs),
    NDevs.

get_surplus_all(Tag) ->
    CpeList = get_all(),
    filter_cpe(CpeList,Tag).
    
filter_cpe(CpeList,Tag) ->
    filter_cpe_t(CpeList,length(CpeList),Tag,[]).
filter_cpe_t(_,0,_,Cpe) -> Cpe;
filter_cpe_t([A|B],Len,Ta,C) ->
    case A#tr069_device.label of
    undefined ->
        filter_cpe_t(B,Len-1,Ta,[A|C]);
    [] ->        
        filter_cpe_t(B,Len-1,Ta,[A|C]);
    TagList ->
        case lists:member(Ta,TagList) of
        true ->
            filter_cpe_t(B,Len-1,Ta,C);
        _ ->
            filter_cpe_t(B,Len-1,Ta,[A|C]) 
        end  
    end. 

get_alive_all()->
    Devs = 
    case wf:session(loginuseraccount) of
    undefined ->
        [];
    {"admin_user",_,_} ->        
	    Ret = db_ecc:get_data(?DBName, ?Table, "my.state=alive"),
	    case is_list(Ret) of
		    false ->
			    [];
		    true ->
			    [db_to_device(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	    end;
    {UserId,_,_} ->
        UserList = api_user_spl:get_user(UserId),
        case proplists:get_value(cpe,UserList) of
        undefined ->
            [];
        CpeList ->
	        Ret = db_ecc:get_data(?DBName, ?Table,"my.state=alive"),
	        case is_list(Ret) of
		        false ->
			        [];
		        true ->                 
			       [db_to_device(Advance) ||{content, _, Id, _, _, _, _, _, _, _, _, _, _, _, Advance}<- Ret,lists:member(atom_to_list(Id),CpeList)]                   
	        end                
        end;
    _ ->
        []    
    end,
    %% cache cpe
    %%Host = domain(get(hostname)),
    Host = textutils:any_to_list(get(hostname)),
    NDevs = tr069_devicecache_server:get_cache_cpe(Host, Devs),
    NDevs.

get_alive_all(Label)->
    Devs =
    case wf:session(loginuseraccount) of
    undefined ->
        [];
    {"admin_user",_,_} ->        
	    Ret = db_ecc:get_data(?DBName, ?Table, "my.state=alive"),
	    case is_list(Ret) of
		    false ->
			    [];
		    true ->
			    Device = [db_to_device(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret],
                get_label_device(Label,Device)                
	    end;
    {UserId,_,_} ->
        UserList = api_user_spl:get_user(UserId),
        case proplists:get_value(cpe,UserList) of
        undefined ->
            [];
        CpeList ->
	        Ret = db_ecc:get_data(?DBName, ?Table,"my.state=alive"),
	        case is_list(Ret) of
		        false ->
			        [];
		        true ->                 
			        Device = [db_to_device(Advance) ||{content, _, Id, _, _, _, _, _, _, _, _, _, _, _, Advance}<- Ret,lists:member(atom_to_list(Id),CpeList)],
                    get_label_device(Label,Device)                      
	        end                
        end;
    _ ->
        []    
    end,
    %% cache cpe
    %%Host = domain(get(hostname)),
    Host = textutils:any_to_list(get(hostname)),
    NDevs = tr069_devicecache_server:get_cache_cpe(Host, Devs),
    NDevs.




get_dead_all()->
    Devs =
    case wf:session(loginuseraccount) of
    undefined ->
        [];
    {"admin_user",_,_} ->        
	    Ret = db_ecc:get_data(?DBName, ?Table, "my.state=dead"),
	    case is_list(Ret) of
		    false ->
			    [];
		    true ->
			    [db_to_device(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	    end;
    {UserId,_,_} ->
        UserList = api_user_spl:get_user(UserId),
        case proplists:get_value(cpe,UserList) of
        undefined ->
            [];
        CpeList ->
	        Ret = db_ecc:get_data(?DBName, ?Table,"my.state=dead"),
	        case is_list(Ret) of
		        false ->
			        [];
		        true ->                 
			       [db_to_device(Advance) ||{content, _, Id, _, _, _, _, _, _, _, _, _, _, _, Advance}<- Ret,lists:member(atom_to_list(Id),CpeList)]                   
	        end                
        end;
    _ ->
        []    
    end,
    %% cache cpe
    %%Host = domain(get(hostname)),
    Host = textutils:any_to_list(get(hostname)),
    NDevs = tr069_devicecache_server:get_cache_cpe(Host, Devs),
    NDevs.

get_unregistered_all()->
    Devs =
    case wf:session(loginuseraccount) of
    undefined ->
        [];
    {"admin_user",_,_} ->        
	    Ret = db_ecc:get_data(?DBName, ?Table, "my.state=unregistered"),
	    case is_list(Ret) of
		    false ->
			    [];
		    true ->
			    [db_to_device(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret]           
	    end;
    {UserId,_,_} ->
        UserList = api_user_spl:get_user(UserId),
        case proplists:get_value(cpe,UserList) of
        undefined ->
            [];
        CpeList ->
	        Ret = db_ecc:get_data(?DBName, ?Table,"my.state=unregistered"),
	        case is_list(Ret) of
		        false ->
			        [];
		        true ->                 
			        [db_to_device(Advance) ||{content, _, Id, _, _, _, _, _, _, _, _, _, _, _, Advance}<- Ret,lists:member(atom_to_list(Id),CpeList)]                     
	        end                
        end;
    _ ->
        []    
    end,
    %% cache cpe
    %%Host = domain(get(hostname)),
    Host = textutils:any_to_list(get(hostname)),
    NDevs = tr069_devicecache_server:get_cache_cpe(Host, Devs),
    NDevs.

get_unregistered_all(Label)->
    Devs = 
    case wf:session(loginuseraccount) of
    undefined ->
        [];
    {"admin_user",_,_} ->        
	    Ret = db_ecc:get_data(?DBName, ?Table, "my.state=unregistered"),
	    case is_list(Ret) of
		    false ->
			    [];
		    true ->
			    Device = [db_to_device(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret],
                get_label_device(Label,Device)                 
	    end;
    {UserId,_,_} ->
        UserList = api_user_spl:get_user(UserId),
        case proplists:get_value(cpe,UserList) of
        undefined ->
            [];
        CpeList ->
	        Ret = db_ecc:get_data(?DBName, ?Table,"my.state=unregistered"),
	        case is_list(Ret) of
		        false ->
			        [];
		        true ->                 
			        Device = [db_to_device(Advance) ||{content, _, Id, _, _, _, _, _, _, _, _, _, _, _, Advance}<- Ret,lists:member(atom_to_list(Id),CpeList)],
                    get_label_device(Label,Device)                     
	        end                
        end;
    _ ->
        []    
    end,
    %% cache cpe
    %%Host = domain(get(hostname)),
    Host = textutils:any_to_list(get(hostname)),
    NDevs = tr069_devicecache_server:get_cache_cpe(Host, Devs),
    NDevs.

get_dead_all(Label)->
    Devs =
    case wf:session(loginuseraccount) of
    undefined ->
        [];
    {"admin_user",_,_} ->        
	    Ret = db_ecc:get_data(?DBName, ?Table, "my.state=dead"),
	    case is_list(Ret) of
		    false ->
			    [];
		    true ->
			    Device = [db_to_device(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret],
                get_label_device(Label,Device)               
	    end;
    {UserId,_,_} ->
        UserList = api_user_spl:get_user(UserId),
        case proplists:get_value(cpe,UserList) of
        undefined ->
            [];
        CpeList ->
	        Ret = db_ecc:get_data(?DBName, ?Table,"my.state=dead"),
	        case is_list(Ret) of
		        false ->
			        [];
		        true ->                 
			        Device = [db_to_device(Advance) ||{content, _, Id, _, _, _, _, _, _, _, _, _, _, _, Advance}<- Ret,lists:member(atom_to_list(Id),CpeList)],
                    get_label_device(Label,Device)                    
	        end                
        end;
    _ ->
        []    
    end,
    %% cache cpe
    %%Host = domain(get(hostname)),
    Host = textutils:any_to_list(get(hostname)),
    NDevs = tr069_devicecache_server:get_cache_cpe(Host, Devs),
    NDevs.

get_cpebylabel(Label) ->
    Devs = 
    case wf:session(loginuseraccount) of
    undefined ->
        [];
    {"admin_user",_,_} ->        
	    Ret = db_ecc:get_data(?DBName, ?Table,""),
	    case is_list(Ret) of
		    false ->
			    [];
		    true ->
			    Device = [db_to_device(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret],
                get_label_device(Label,Device)              
	    end;
    {UserId,_,_} ->
        UserList = api_user_spl:get_user(UserId),
        case proplists:get_value(cpe,UserList) of
        undefined ->
            [];
        CpeList ->
	        Ret = db_ecc:get_data(?DBName, ?Table,""),
	        case is_list(Ret) of
		        false ->
			        [];
		        true ->                 
			        Device =  [db_to_device(Advance) ||{content, _, Id, _, _, _, _, _, _, _, _, _, _, _, Advance}<- Ret,lists:member(atom_to_list(Id),CpeList)],
                    get_label_device(Label,Device)                    
	        end                
        end;
    _ ->
        []    
    end,
    %% cache cpe
    %%Host = domain(get(hostname)),
    Host = textutils:any_to_list(get(hostname)),
    NDevs = tr069_devicecache_server:get_cache_cpe(Host, Devs),
    NDevs.

get_label_device(Label,DeviceList) ->
    get_label_device_t(Label,DeviceList,length(DeviceList),[]).
get_label_device_t(_,_,0,E) -> E;
get_label_device_t(Label,[A|B],Len,En) ->
    case A#tr069_device.label  of
    undefined ->
        if Label == undefined ->
            get_label_device_t(Label,B,Len-1,[A|En]);  
        true -> 
            get_label_device_t(Label,B,Len-1,En) 
        end;    
    [] ->
        if Label == "undefined" ->
            get_label_device_t(Label,B,Len-1,[A|En]);  
        true -> 
            get_label_device_t(Label,B,Len-1,En) 
        end;
    _ ->        
        case lists:member(Label,A#tr069_device.label) of
        true ->
            get_label_device_t(Label,B,Len-1,[A|En]);
        _ ->  
            get_label_device_t(Label,B,Len-1,En)  
        end
    end.        


get_keepalive_alive_all(App)->
	Ret = get_data(App,?DBName, ?Table, "my.state=alive&my.keepalive=0"),
    Devs = 
	case is_list(Ret) of
		false ->
			[];
		true ->
			[db_to_device(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	end,
    %% cache cpe
    NDevs = tr069_devicecache_server:get_cache_cpe(App, Devs),
    NDevs.


get_deviceidstru(DeviceId)->
	%%Ret = db_ecc:get_data(?DBName, ?Table, "id=" ++ DeviceId),
	%%case is_list(Ret) of
	%%	false ->
	%%		[];
	%%	true ->
	%%		[Device|_] = [db_to_device(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret],
    %%        {ok,Device#tr069_device.manufacturer ++"_"++ Device#tr069_device.oui++"_"++Device#tr069_device.productclass++"_"++ Device#tr069_device.serialnumber}		
	%%end.
    %% cache cpe
    %%Host = domain(get(hostname)),
    Host = textutils:any_to_list(get(hostname)),
    NDevs = tr069_devicecache_server:get_one_cache_cpe(Host, [DeviceId]),
    case NDevs of
		[Device] ->
            {ok,Device#tr069_device.manufacturer ++"_"++ Device#tr069_device.oui++"_"++Device#tr069_device.productclass++"_"++ Device#tr069_device.serialnumber};
        _ ->
            []
	end.

get_deviceById(Id)->
	%%Ret = db_ecc:get_data(?DBName, ?Table, "id="++ Id),
	%%case Ret of		
	%%	[{content, _, MId, _, _, _, _, _, _, _, _, _, _, _, Advance}] ->
	%%		db_to_device(Advance);
 	%%	_ ->
	%%		#tr069_device{}
	%%end.
    %% cache cpe
    %%Host = domain(get(hostname)),
    Host = textutils:any_to_list(get(hostname)),
    NDevs = tr069_devicecache_server:get_one_cache_cpe(Host, [Id]),
    case NDevs of
		[Device] ->
            Device;
        _ ->
            #tr069_device{}
	end.
    
get_deviceById(Host,Id)->
	%%Ret = get_data(Host,?DBName, ?Table, "id="++ Id),
	%%case Ret of		
	%%	[{content, _, MId, _, _, _, _, _, _, _, _, _, _, _, Advance}] ->
	%%		db_to_device(Advance);
 	%%	_ ->
	%%		#tr069_device{}
	%%end.    
    %% cache cpe    
    NDevs = tr069_devicecache_server:get_one_cache_cpe(Host, [Id]),
    case NDevs of
		[Device] ->
            Device;
        _ ->
            #tr069_device{}
	end.

%-record(realtimealarm,{id,manufacturer,oui,productclass,serialnumber,xevent,description="",statu,notes,timestamp}).
db_to_alarm(Advance) ->
    Data = [db2term(K, T, V) || {K, T, V} <- Advance],
    #realtimealarm{
            id = proplists:get_value(id, Data),
            manufacturer = proplists:get_value(manufacturer, Data),
            oui = proplists:get_value(oui, Data),
            productclass = proplists:get_value(productclass, Data),
            serialnumber = proplists:get_value(serialnumber, Data),            
            xevent = proplists:get_value(xevent, Data),
            description = proplists:get_value(description, Data),                    
            statu = proplists:get_value(statu, Data),
            notes = proplists:get_value(notes, Data),             
            timestamp = proplists:get_value(timestamp, Data),
            number = proplists:get_value(number, Data),
            timeofoccurrence = proplists:get_value(timeofoccurrence, Data),
            total = 
                case proplists:get_value(total, Data) of
                    V1 when erlang:is_integer(V1) ->
                        V1;
                    _ ->
                        0
                end            
    }.    

%-record(realtimealarm,{id,manufacturer,oui,productclass,serialnumber,xevent,description="",statu,notes,timestamp}).
alarm_to_db(Mach) ->
	[
        {id,string,list_to_binary(Mach#realtimealarm.id)}, 
        {manufacturer,string,list_to_binary(Mach#realtimealarm.manufacturer)},
        {oui,string,list_to_binary(Mach#realtimealarm.oui)},
        {productclass,string,list_to_binary(Mach#realtimealarm.productclass)},
        {serialnumber,string,list_to_binary(Mach#realtimealarm.serialnumber)},        
        {xevent,string,list_to_binary(Mach#realtimealarm.xevent)}, 
        {description,string,list_to_binary(Mach#realtimealarm.description)},
        {statu,string,list_to_binary(Mach#realtimealarm.statu)}, 
        {notes,string,list_to_binary(Mach#realtimealarm.notes)}, 
        {timestamp,number,list_to_binary(Mach#realtimealarm.timestamp)},
        {number,number,list_to_binary(Mach#realtimealarm.number)},
        {timeofoccurrence,number,list_to_binary(Mach#realtimealarm.timeofoccurrence)}        
    ].    



delete(Id)->
	%%db_ecc:delete_data(?DBName, ?Table, "id="++ Id).
    %% cache cpe
    %%Host = domain(get(hostname)),
    Host = textutils:any_to_list(get(hostname)),
    tr069_devicecache_server:delete_cache_cpe(Host, [Id]).
    
delete(Host, Id)->
    tr069_devicecache_server:delete_cache_cpe(Host, [Id]).

db_to_device(Advance) ->
	Data = [db2term(K, T, V) || {K, T, V} <- Advance],
	#tr069_device{               
			  ip=proplists:get_value(ip, Data),
			  manufacturer=proplists:get_value(manufacturer, Data),
			  oui=proplists:get_value(oui, Data),
              productclass=proplists:get_value(productclass, Data),
              serialnumber=proplists:get_value(serialnumber, Data),
              profile=proplists:get_value(profile, Data),
              deviceport=proplists:get_value(deviceport, Data),             
              keepalive=proplists:get_value(keepalive, Data),
              keepalivetime=proplists:get_value(keepalivetime, Data), 
              authtype=proplists:get_value(authtype, Data),
              user=proplists:get_value(user, Data),
              password=proplists:get_value(password, Data),
              keyfile=proplists:get_value(keyfile, Data),
              acsip=proplists:get_value(acsip, Data),
              acsname=proplists:get_value(acsname, Data),
              timestamp=proplists:get_value(timestamp, Data),
              state=proplists:get_value(state, Data),
              label=proplists:get_value(label, Data),
              description=proplists:get_value(description, Data),
              total = 
                case proplists:get_value(total, Data) of
                    V1 when erlang:is_integer(V1) ->
                        V1;
                    _ ->
                        0
                end
			  }. 


    
device_to_db(Mach) ->
	[
        {ip,string,list_to_binary(Mach#tr069_device.ip)},       
        {manufacturer,string,list_to_binary(Mach#tr069_device.manufacturer)},
        {oui,string,list_to_binary(Mach#tr069_device.oui)},
        {productclass,string,list_to_binary(Mach#tr069_device.productclass)},
        {serialnumber,string,list_to_binary(Mach#tr069_device.serialnumber)},
        {profile,string,list_to_binary(Mach#tr069_device.profile)},
        {deviceport,string,list_to_binary(Mach#tr069_device.deviceport)},      
        {keepalive,number,list_to_binary(Mach#tr069_device.keepalive)},
        {keepalivetime,number,list_to_binary(Mach#tr069_device.keepalivetime)},
        {authtype,string,list_to_binary(Mach#tr069_device.authtype)}, 
        {user,string,list_to_binary(Mach#tr069_device.user)}, 
        {password,string,list_to_binary(Mach#tr069_device.password)},
        {keyfile,string,list_to_binary(Mach#tr069_device.keyfile)},
        {acsip,string,list_to_binary(Mach#tr069_device.acsip)},
        {acsname,string,list_to_binary(Mach#tr069_device.acsname)},
        {timestamp,number,list_to_binary(Mach#tr069_device.timestamp)},
        {state,string,list_to_binary(Mach#tr069_device.state)},
        {label,term,term_to_binary(Mach#tr069_device.label)}, 
        {description,string,list_to_binary(Mach#tr069_device.description)}        
	 ].


db2term(K,T,V) when not is_binary(V)->{K,V};
db2term(K,T,V) when T=:= number ->
	NV = binary_to_list(V),
	case string:to_float(NV) of
		{error,_}->
            if NV /= [] -> 
			    {K,list_to_integer(NV)};
            true ->
                {K,0} 
            end; 
		_->
			{K,list_to_float(NV)}
	end;
db2term(K,T,V) when T=:= string ->  {K,binary_to_list(V)};
db2term(K,T,V) when T=:= string ->  {K,binary_to_list(V)};
db2term(K,_,V)->{K,binary_to_term(V)}. 


domain(undefined) -> "localhost";
domain("localhost") -> "localhost";
domain(Host) when is_atom(Host) -> atom_to_list(Host);
domain(Host) ->
  case string:str(Host,".") of
      0 -> "localhost";
	  Pos ->
			case regexp:match(Host,"^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+$") of
				{match,_,_}->
					"localhost";
				_->
                    case regexp:match(Host,"^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\:[0-9]+$") of
                    {match,_,_} ->
                        "localhost";
                    _ ->                        
					    lists:sublist(Host,1,Pos-1)
                    end    
			end
      %{ok,lists:sublist(Host,1,Pos-1),lists:nthtail(Pos,Host)}
      %lists:sublist(Host,1,Pos-1)
  end.
  
  
insert_data(_,_,_,{})->{error,parameter_error};
insert_data(Host,DbName,Table,Data)->
    AppName = domain(Host),
    Newdata = Data#content{xn_type = list_to_binary(Table),application=list_to_atom(AppName)},
    rpc:call(DbName,content,create,[AppName, Table, Newdata]). 
    
    
update_data(_,_,_,_,{})->{error,parameter_error};
update_data(Host,DbName,Table,Where,Data)->
    AppName = domain(Host),
    Newdata = Data#content{xn_type = list_to_binary(Table),application=list_to_atom(AppName)},
    %io:format("_____1____:~p~n",[Newdata]),
	Result = rpc:call(DbName,content,update,[AppName, Table, Where, Newdata]),
    %io:format("_____1____:~p~n",[Result]),
    Result.

delete_data(Host,DbName,Table,Where)->
    AppName = domain(Host),
%%     io:format("hostname:~p~n",[AppName]),
	rpc:call(DbName,content,delete,[AppName, Table, Where]).

get_data(Host,DbName, Table, [])->
    AppName = domain(Host),
%%     io:format("hostname:~p~n",[AppName]),
	%case rpc:call(DbName, content, get, [[{application,Table},{content,Where},"from=0&to=100000"]]) of
    case rpc:call(DbName, content, get, [[{application,AppName},{content,"type = '"++Table++"'"},"from=0&to=100000"]]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			Else
	end;

get_data(Host,DbName, Table, Where)->
    AppName = domain(Host),
    %%io:format("dbcs tr069 App:~p~n",[AppName]),
%%     io:format("hostname:~p~n",[AppName]),
	%case rpc:call(DbName, content, get, [[{application,Table},{content,Where},"from=0&to=100000"]]) of
    case rpc:call(DbName, content, get, [AppName, Table, Where,"from=0&to=100000"]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			Else
	end.
get_data(Host,DbName, Table, [],Order) when is_list(Order),length(Order)>0->
    AppName = domain(Host),
	case rpc:call(DbName, content, get, [AppName,Table,"","from=0&to=100000" ++ "&" ++ Order]) of
    %case rpc:call(DbName, content, get, [[{application,Table},{content,Where},"from=0&to=100000" ++ "&" ++ Order]]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			Else
	end;
get_data(Host,DbName, Table, Where,Order) when is_list(Order),length(Order)>0->
    AppName = domain(Host),
	case rpc:call(DbName, content, get, [AppName,Table, Where,"from=0&to=100000" ++ "&" ++ Order]) of
    %case rpc:call(DbName, content, get, [[{application,Table},{content,Where},"from=0&to=100000" ++ "&" ++ Order]]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			Else
	end;
get_data(Host,DbName, Table, [],Order) when is_list(Order)->
    AppName = domain(Host),
	case rpc:call(DbName, content, get, [AppName,Table,"","from=0&to=100000"]) of
    %case rpc:call(DbName, content, get, [[{application,Table},{content,Where},"from=0&to=100000"]]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			Else
	end;
get_data(Host,DbName, Table, Where,Order) when is_list(Order)->
    AppName = domain(Host),
	case rpc:call(DbName, content, get, [AppName, Table, Where,"from=0&to=100000"]) of
    %case rpc:call(DbName, content, get, [[{application,Table},{content,Where},"from=0&to=100000"]]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			Else
	end;
get_data(_,_, _, _,_)->{error,parameter_error}.


get_data2(Host,DbName, Table, [],Order) when is_list(Order)->
    AppName = domain(Host),
    %%io:format("get_data2 hostname:~p~n",[AppName]),
	case rpc:call(DbName, content, get, [AppName, Table, "",Order]) of
		{ok,R}->
			R;
		Else->
			Else
	end;
get_data2(Host,DbName, Table, Where,Order) when is_list(Order) andalso is_list(Where)->
    AppName = domain(Host),
    %%io:format("get_data2 hostname:~p~n",[AppName]),
	case rpc:call(DbName, content, get, [AppName,Table,Where,Order]) of
		{ok,R}->
			R;
		Else->
			Else
	end;
get_data2(_,_, _, _,_)->{error,parameter_error}.


batch_insert_update_paramcaches(Host,DeviceId, []) ->
    ok;
batch_insert_update_paramcaches(Host,DeviceId, [ParamCache=#tr069_paramcaches{}|T]) ->
    InsertResult = insert_update_paramcaches(Host,DeviceId, ParamCache),
    %%io:format("InsertResult = ~p~n", [InsertResult]),
    batch_insert_update_paramcaches(Host,DeviceId, T);
batch_insert_update_paramcaches(Host,DeviceId, [H|T]) ->
    batch_insert_update_paramcaches(Host,DeviceId, T).
    
    

insert_update_paramcaches(Host,DeviceId, ParamCache=#tr069_paramcaches{}) -> 
    Id = build_paramcache_id(DeviceId,ParamCache#tr069_paramcaches.paramname),
    Where = "id="++Id,   
    Adv = param_to_db(ParamCache),
    Content = {content, erlang:list_to_atom(?ParamCachesTable), erlang:list_to_atom(Id), <<"param">>,null,null,null,null,?Author,null,null,null,null,null,Adv},
    Re = insert_data(Host,?DBName,?ParamCachesTable,Content),
    case Re of
        {ok,_} ->
            Re;
        {error,content_id_existed} ->
            update_data(Host,?DBName, ?ParamCachesTable, Where, Content);
        R ->
            R
    end;
insert_update_paramcaches(Host,DeviceId, _) ->
    {error, error_record}.

%% [] | [#tr069_paramcaches{}|T]
get_paramcaches(Host,DeviceId, []) ->
    [];
get_paramcaches(Host,DeviceId, [Param|T]) ->
    case get_data(Host,?DBName, ?ParamCachesTable, "my.deviceid="++ DeviceId++"&my.paramname="++Param) of
        V when erlang:is_list(V) ->
            proc_content_to_param(V);
        _ ->
            [#tr069_paramcaches{deviceid=DeviceId,paramname=Param}]
    end ++
    get_paramcaches(Host,DeviceId, T).
    

get_paramcaches(DeviceId) ->
    case db_ecc:get_data(?DBName, ?ParamCachesTable, "my.deviceid="++ DeviceId) of
        V when erlang:is_list(V) ->
            proc_content_to_param(V);
        _ ->
            []
    end.


get_all_paramcaches() ->
    Where = "my.is_paracache_table=true",
    case db_ecc:get_data(?DBName, ?ParamCachesTable, Where) of
        V when erlang:is_list(V) ->
            proc_content_to_param(V);
        _ ->
            []
    end.


proc_content_to_param([]) ->
    [];
proc_content_to_param([Content=#content{}|T]) ->
    Advance = Content#content.my,
    [db_to_param(Advance)] ++
    proc_content_to_param(T);
proc_content_to_param([H|T]) ->
    proc_content_to_param(T).



delete_paramcaches(Id) ->
    Where = "id="++Id,
    db_ecc:delete_data(?DBName,?ParamCachesTable,Where).


delete_paramcaches_by_deviceid(DeviceId)->
	case get_paramcaches(DeviceId) of
		[]->
			{error,not_found};
		R->
			Ret = [delete_paramcaches(build_paramcache_id(X#tr069_paramcaches.deviceid,X#tr069_paramcaches.paramname)) || X<-R],
			case lists:keymember(error,1,Ret) of
				true->
					{error,Ret};
				_->
					{ok,removed}
			end
	end.


delete_paramcachestable() ->
    case get_all_paramcaches() of
		[]->
			{error,not_found};
		R->
			Ret = [delete_paramcaches(build_paramcache_id(X#tr069_paramcaches.deviceid,X#tr069_paramcaches.paramname)) || X<-R],
			case lists:keymember(error,1,Ret) of
				true->
					{error,Ret};
				_->
					{ok,removed}
			end
	end.


build_paramcache_id(DeviceId, ParaName) ->
    DeviceId ++ "_" ++ ParaName.
    
    

batch_insert_update_paramcaches_new(Host,DeviceId, ParamCaches) ->
    DicParamDev =
    case get_paramcaches_dev(Host, DeviceId) of
        ParaCahesDev = #tr069_paramcachesdev{} ->
            dict:from_list(ParaCahesDev#tr069_paramcachesdev.params);
        _ ->
            dict:from_list([])
    end,
    %%io:format("ParamDev = ~p~n", [dict:to_list(DicParamDev)]),
    NDicParamDev = batch_insert_update_paramcaches_new_t(Host,DeviceId, ParamCaches, DicParamDev),
    %%io:format("NParamDev = ~p~n", [dict:to_list(NDicParamDev)]),
    %%io:format("NParamDev = ~p~n", [dict:to_list(NDicParamDev)]),
    Id = build_paramcache_id_new(DeviceId),
    Paraches = #tr069_paramcachesdev{
            id = Id,
            params = dict:to_list(NDicParamDev)
    },
    save_paramcaches_dev(Host, DeviceId, Paraches).
batch_insert_update_paramcaches_new_t(Host,DeviceId, [], DicParamDev) ->
    DicParamDev;
batch_insert_update_paramcaches_new_t(Host,DeviceId, [ParamCache=#tr069_paramcaches{}|T], DicParamDev) ->
    NDicParamDev = dict:store(ParamCache#tr069_paramcaches.paramname, ParamCache, DicParamDev),
    PKvs = [{ParamCache#tr069_paramcaches.paramname,ParamCache}],
    batch_insert_update_paramcaches_new_t(Host,DeviceId, T, NDicParamDev);
batch_insert_update_paramcaches_new_t(Host,DeviceId, [H|T], DicParamDev) ->
    batch_insert_update_paramcaches_new_t(Host,DeviceId, T, DicParamDev).
    


build_paramcache_id_new(DeviceId) ->
    "CpeParamcaches_"++DeviceId.
    


get_paramcaches_dev(Host,DeviceId) ->
    Id = build_paramcache_id_new(DeviceId),
    case get_data(Host,?DBName, ?NewParamCachesTable, "id="++Id) of
        [] ->
            %%io:format("empty1~n"),
            [];
        [V=#content{}] ->
            db_to_paramdev(V#content.my);
        _ ->
            %%io:format("empty2~n"),
            []
    end.


save_paramcaches_dev(Host, DeviceId, ParaCahesDev=#tr069_paramcachesdev{}) ->
    Id = build_paramcache_id_new(DeviceId),
    Where = "id="++Id,   
    Adv = paramdev_to_db(ParaCahesDev),
    Content = {content, erlang:list_to_atom(?NewParamCachesTable), erlang:list_to_atom(Id), <<"tr069_ParamCachesNew">>,null,null,null,null,?Author,null,null,null,null,null,Adv},
    Re = insert_data(Host,?DBName,?NewParamCachesTable,Content),
    case Re of
        {ok,_} ->
            Re;
        {error,content_id_existed} ->
            update_data(Host,?DBName, ?NewParamCachesTable, Where, Content);
        R ->
            R
    end.
    

paramdev_to_db(Mach) ->
    [
        dbcs_base:term2db(id,Mach#tr069_paramcachesdev.id),
        %%dbcs_base:term2db(params,Mach#tr069_paramcachesdev.params)
        {params,term,term_to_binary(Mach#tr069_paramcachesdev.params)}
	 ].


db_to_paramdev(Advance) ->
    Data = [dbcs_base:db2term(K, T, V) || {K, T, V} <- Advance],
    #tr069_paramcachesdev{
            id = proplists:get_value(id, Data),
            params = proplists:get_value(params, Data)
    }.      
    
    
%% ********************************************************************************************************************
    
param_to_db(Mach) ->
    [
        {deviceid,string,list_to_binary(Mach#tr069_paramcaches.deviceid)},       
        {paramname,string,list_to_binary(Mach#tr069_paramcaches.paramname)},
        {value,string,list_to_binary(Mach#tr069_paramcaches.value)},
        {is_paracache_table,string,list_to_binary(Mach#tr069_paramcaches.is_paracache_table)},
        {updatetime,number,erlang:list_to_binary(erlang:integer_to_list(Mach#tr069_paramcaches.updatetime))}
	 ].
     
db_to_param(Advance) ->
    Data = [db2term(K, T, V) || {K, T, V} <- Advance],
    #tr069_paramcaches{
            deviceid = proplists:get_value(deviceid, Data),
            paramname = proplists:get_value(paramname, Data),
            value = proplists:get_value(value, Data),
            is_paracache_table = 
                case proplists:get_value(is_paracache_table, Data) of
                    "true" ->
                        "true";
                    _ ->
                        "false"
                end,
            updatetime = 
                case proplists:get_value(updatetime, Data) of
                    undefined ->
                        0;
                    UpdateTime ->
                        UpdateTime
                end
    }.   
    
    

save_sitedb(UpSite = #tr069_upgradesite{}) ->
    Id = get_siteid(),
    Where = "id="++Id,   
    Adv = upgrade_site_to_db(UpSite),
    Content = {content, erlang:list_to_atom(?UpSiteTable), erlang:list_to_atom(Id), <<"upgrade_site">>,null,null,null,null,?Author,null,null,null,null,null,Adv},
    Re = db_ecc:insert_data(?DBName,?UpSiteTable,Content),
    case Re of
        {ok,_} ->
            Re;
        {error,content_id_existed} ->
            db_ecc:update_data(?DBName, ?UpSiteTable, Where, Content);
        R ->
            R
    end;
save_sitedb(_) ->
    {error, error_record}.


get_site_db() ->
    Id = get_siteid(),
    case db_ecc:get_data(?DBName, ?UpSiteTable, "id="++ Id) of
        [V=#content{}] ->
            db_to_upgrade_site(V#content.my);
        Other ->
            %%io:format("Other = ~p~n", [Other]),
            #tr069_upgradesite{}
    end.


get_site_db(Host) ->
    Id = get_siteid(),
    case get_data(Host,?DBName, ?UpSiteTable, "id="++ Id) of
        [V=#content{}] ->
            db_to_upgrade_site(V#content.my);
        Other ->
            %%io:format("Other = ~p~n", [Other]),
            #tr069_upgradesite{}
    end.


get_siteid() ->
    "tr069_UpgradeSite".


upgrade_site_to_db(UpSite = #tr069_upgradesite{}) ->
    [
        {devaccessaddr,string,list_to_binary(UpSite#tr069_upgradesite.devaccessaddr)},       
        {clientaccessaddr,string,list_to_binary(UpSite#tr069_upgradesite.clientaccessaddr)}, 
        {username,string,list_to_binary(UpSite#tr069_upgradesite.username)}, 
        {password,string,list_to_binary(UpSite#tr069_upgradesite.password)}, 
        {protocol,string,list_to_binary(UpSite#tr069_upgradesite.protocol)}
	 ].


db_to_upgrade_site(Advance) ->
    Data = [db2term(K, T, V) || {K, T, V} <- Advance],
    #tr069_upgradesite{
            devaccessaddr = proplists:get_value(devaccessaddr, Data),
            clientaccessaddr = proplists:get_value(clientaccessaddr, Data),
            username = proplists:get_value(username, Data),
            password = proplists:get_value(password, Data),
            protocol = proplists:get_value(protocol, Data)
    }.   


get_productclass_bymenufac(Menufac) ->
    Where = "my.manufacturer="++Menufac,
    Ret = db_ecc:get_data(?DBName, ?Table, Where),
    Devs =
	case is_list(Ret) of
		false ->
			[];
		true ->
			[db_to_device(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	end,
    %% cache cpe    
    %%Host = domain(get(hostname)),
    Host = textutils:any_to_list(get(hostname)),
    NDevs = tr069_devicecache_server:get_cache_cpe(Host, Devs),
    NDevs.


save_upfiledb(UpFile = #tr069_upgradefile{}) ->
    Id = get_upfileid(UpFile),
    Where = "id="++Id,   
    Adv = upgrade_file_to_db(UpFile),
    Content = {content, erlang:list_to_atom(?UpFileTable), erlang:list_to_atom(Id), <<"upgrade_file">>,null,null,null,null,?Author,null,null,null,null,null,Adv},
    Re = db_ecc:insert_data(?DBName,?UpFileTable,Content),
    case Re of
        {ok,_} ->
            Re;
        {error,content_id_existed} ->
            db_ecc:update_data(?DBName, ?UpFileTable, Where, Content);
        R ->
            R
    end;
save_upfiledb(_) ->
    {error, error_record}.


get_upfileid(UpFile = #tr069_upgradefile{}) ->
    ?UpFileTable ++ 
    "_" ++
    UpFile#tr069_upgradefile.manufacturer++
    "_" ++
    UpFile#tr069_upgradefile.productclass++
    "_" ++
    UpFile#tr069_upgradefile.version++
    "_" ++
    UpFile#tr069_upgradefile.filename.


delete_upfile(UpFile = #tr069_upgradefile{}) ->
    Id = get_upfileid(UpFile),
    db_ecc:delete_data(?DBName, ?UpFileTable, "id="++ Id).
    

upgrade_file_to_db(UpSite = #tr069_upgradefile{}) ->
    [
        {manufacturer,string,list_to_binary(UpSite#tr069_upgradefile.manufacturer)},       
        {productclass,string,list_to_binary(UpSite#tr069_upgradefile.productclass)}, 
        {version,string,list_to_binary(UpSite#tr069_upgradefile.version)}, 
        {filename,string,list_to_binary(UpSite#tr069_upgradefile.filename)}, 
        {filetype,string,list_to_binary(UpSite#tr069_upgradefile.filetype)},
        {filesize,string,list_to_binary(UpSite#tr069_upgradefile.filesize)},
        {is_upgradefile,string,list_to_binary(UpSite#tr069_upgradefile.is_upgradefile)}
	 ].


db_to_upgrade_file(Advance) ->
    Data = [db2term(K, T, V) || {K, T, V} <- Advance],
    #tr069_upgradefile{
            manufacturer = proplists:get_value(manufacturer, Data),
            productclass = proplists:get_value(productclass, Data),
            version = proplists:get_value(version, Data),
            filename = proplists:get_value(filename, Data),
            filetype = proplists:get_value(filetype, Data),
            filesize = proplists:get_value(filesize, Data),
            is_upgradefile = 
                case proplists:get_value(filesize, Data) of
                    "true" ->
                        "true";
                    _ ->
                        "false"
                end
    }.   
    

get_all_upgrade_files() ->
    Where = "",
    %%my.is_upgradefile=true
    case db_ecc:get_data(?DBName, ?UpFileTable, Where) of
        V when erlang:is_list(V) ->
            proc_content_to_upfile(V);
        _ ->
            []
    end.

get_all_upgrade_files(Login) ->
    case Login of
    undefined ->
        [];
    {"admin_user",_,_} ->     
        Where = "",
        Ret = db_ecc:get_data(?DBName, ?UpFileTable, Where),
	    %%Ret = db_ecc:get_data(?DBName, ?RealtimeAlarmTable, "my.statu="++"untreated"),
	    build_upFile(Ret);
    {UserId,_,_} ->
        Where = "",
        UserList = api_user_spl:get_user(UserId),
        case proplists:get_value(cpe,UserList) of
        undefined ->
            [];
        CpeList ->
	        Ret = db_ecc:get_data(?DBName, ?UpFileTable, Where),
            %%io:format("AlertRet = ~p~n", [Ret]),
	        build_upFile(Ret, CpeList)         
        end;
    _ ->
        []    
    end. 
    
build_upFile([]) ->
    [];
build_upFile([{content, _,Id, _, _, _, _, _, _, _, _, _, _, _, Advance}|T]) ->
    [db_to_upgrade_file(Advance)] ++
    build_upFile(T);
build_upFile([H|T]) ->
    build_upFile(T);
build_upFile(Other) ->
    [].


build_upFile([], CpeList) ->
    [];
build_upFile([{content, _,Id, _, _, _, _, _, _, _, _, _, _, _, Advance}|T],CpeList) ->
    UpFile = db_to_upgrade_file(Advance),
    Manufacturer = UpFile#tr069_upgradefile.manufacturer,
    IsRi = is_cpelist_menufacturer(Manufacturer, CpeList),
    case IsRi of
        true ->
            [UpFile];
        _ ->
            []
    end ++
    build_upFile(T,CpeList);
build_upFile([H|T],CpeList) ->
    build_upFile(T,CpeList);
build_upFile(Other,CpeList) ->
    [].


is_cpelist_menufacturer(Manufacturer, CpeList) ->
    lists:any(fun(X) -> lists:prefix(Manufacturer, X) end, CpeList).
    


delete_upfilestable() ->
    case get_all_upgrade_files() of
		[]->
			{error,not_found};
		R->
			Ret = [delete_upfilebyid(get_upfileid(X)) || X<-R],
			case lists:keymember(error,1,Ret) of
				true->
					{error,Ret};
				_->
					{ok,removed}
			end
	end.
    
delete_upfilebyid(Id) ->
    Where = "id="++Id,
    db_ecc:delete_data(?DBName,?UpFileTable,Where).


get_file_bymenufac(Menufac) ->
    Where = "my.manufacturer="++Menufac,
    %%my.is_upgradefile=true
    case db_ecc:get_data(?DBName, ?UpFileTable, Where) of
        V when erlang:is_list(V) ->
            proc_content_to_upfile(V);
        _ ->
            []
    end.


get_file_bymenu_prd_ver(Menufac, Productclass, Version) ->
    Where = "my.manufacturer="++Menufac++"&my.productclass="++Productclass++"&my.version="++Version,
    %%my.is_upgradefile=true
    case db_ecc:get_data(?DBName, ?UpFileTable, Where) of
        V when erlang:is_list(V) ->
            proc_content_to_upfile(V);
        _ ->
            []
    end.


get_file_bymenu_prd_ver_name(Menufac, Productclass, Version, FileName) ->
    Where = "my.manufacturer="++Menufac++"&my.productclass="++Productclass++"&my.version="++Version++"&my.filename="++FileName,
    %%my.is_upgradefile=true
    case db_ecc:get_data(?DBName, ?UpFileTable, Where) of
        V when erlang:is_list(V) ->
            proc_content_to_upfile(V);
        _ ->
            []
    end.
    

get_file_bymenu_prd_ver_name(Host,Menufac, Productclass, Version, FileName) ->
    Where = "my.manufacturer="++Menufac++"&my.productclass="++Productclass++"&my.version="++Version++"&my.filename="++FileName,
    %%my.is_upgradefile=true
    case get_data(Host,?DBName, ?UpFileTable, Where) of
        V when erlang:is_list(V) ->
            proc_content_to_upfile(V);
        _ ->
            []
    end.


get_dev_byMenProd(Menufac, Productclass) ->
    Where = "my.manufacturer="++Menufac++"&my.productclass="++Productclass,
    %%my.is_upgradefile=true
    case db_ecc:get_data(?DBName, ?UpFileTable, Where) of
        V when erlang:is_list(V) ->
            proc_content_to_upfile(V);
        _ ->
            []
    end.


proc_content_to_upfile([]) ->
    [];
proc_content_to_upfile([V=#content{}|T]) ->
    [db_to_upgrade_file(V#content.my)] ++ 
    proc_content_to_upfile(T);
proc_content_to_upfile([H|T]) ->
    proc_content_to_upfile(T).


get_dev_from_Devtb(Menufac, Productclass, CondAlive) ->
    Where1 = "my.manufacturer="++Menufac++"&my.productclass like "++Productclass,
    Where = Where1,
    %%Where = Where1 ++
    %%case CondAlive of
    %%    true ->
    %%        "&my.state=alive";
    %%    _ ->
    %%        ""
    %%end,
    Ret = db_ecc:get_data(?DBName, ?Table, Where),
    Devs =
	case is_list(Ret) of
		false ->
			[];
		true ->
			[db_to_device(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	end,
    %%io:format("Devs = ~p~n", [Devs]),
    %% cache cpe    
    %%Host = domain(get(hostname)),
    Host = textutils:any_to_list(get(hostname)),
    %%io:format("Productclass = ~p~n", [Productclass]),
    RDevs = tr069_devicecache_server:get_cache_cpe(Host, Devs),
    %%io:format("RDevs = ~p~n", [RDevs]),
    NDevs = get_dev_from_Devtb_t(RDevs, Productclass, CondAlive),
    %%io:format("NDevs = ~p~n", [NDevs]),
    NDevs.
get_dev_from_Devtb_t([], Productclass, CondAlive) ->
    [];
get_dev_from_Devtb_t([M=#tr069_device{}|T], Productclass, CondAlive) ->
    IsPro = textutils:startsWith(M#tr069_device.productclass, Productclass),
    IsOk = 
        case CondAlive of
            true ->
                if
                    M#tr069_device.state =:= "alive" ->
                        true;
                    true ->
                        false
                end;
            _ ->
                true
        end,
    if
        IsPro =:= true, IsOk =:= true ->
            [M] ++
            get_dev_from_Devtb_t(T, Productclass, CondAlive);
        true ->
            get_dev_from_Devtb_t(T, Productclass, CondAlive)
    end;
get_dev_from_Devtb_t([H|T], Productclass, CondAlive) ->    
    get_dev_from_Devtb_t(T, Productclass, CondAlive).
    
    

save_upstatusdb_t(UpStatus = #tr069_upgradestatus{}) ->
    Id = UpStatus#tr069_upgradestatus.commandkey,
    Where = "id="++Id,   
    %%io:format("UpStatus = ~p~n", [UpStatus]),
    Adv = (catch upgrade_status_to_db(UpStatus)),
    %%io:format("Adv = ~p~n", [Adv]),
    Content = {content, erlang:list_to_atom(?UpgradeStatusTable), erlang:list_to_atom(Id), <<"upgrade_status">>,null,null,null,null,?Author,null,null,null,null,null,Adv},
    Re = db_ecc:insert_data(?DBName,?UpgradeStatusTable,Content),
    case Re of
        {ok,_} ->
            Re;
        {error,content_id_existed} ->
            db_ecc:update_data(?DBName, ?UpgradeStatusTable, Where, Content);
        R ->
            R
    end;
save_upstatusdb_t(_) ->
    {error, error_record}.
    
save_upstatusdb(UpStatus = #tr069_upgradestatus{}) ->
    Where = "id="++UpStatus#tr069_upgradestatus.commandkey,
    Ret = db_ecc:get_data(?DBName, ?UpgradeStatusTable, Where),
    R1 =
	case is_list(Ret) of
		false ->
			[];
		true ->
			[db_to_upgrade_status(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	end,
    case R1 of
        [] ->
            save_upstatusdb_t(UpStatus);
        [Ups = #tr069_upgradestatus{}] ->
            NUps = Ups#tr069_upgradestatus{upgradestatus=UpStatus#tr069_upgradestatus.upgradestatus},
            save_upstatusdb_t(NUps)
    end;
save_upstatusdb(_) ->
    {error, error_record}.
    

save_upstatusdb(Host,UpStatus = #tr069_upgradestatus{}) ->
    Where = "id="++UpStatus#tr069_upgradestatus.commandkey,
    Ret = get_data(Host,?DBName, ?UpgradeStatusTable, Where),
    R1 =
	case is_list(Ret) of
		false ->
			[];
		true ->
			[db_to_upgrade_status(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	end,
    case R1 of
        [] ->
            save_upstatusdb_t(Host,UpStatus);
        [Ups = #tr069_upgradestatus{}] ->
            Commandkey = 
                case UpStatus#tr069_upgradestatus.commandkey of
                    [] ->
                        Ups#tr069_upgradestatus.commandkey;
                    _ ->
                        UpStatus#tr069_upgradestatus.commandkey
                end,
            Devid = 
                case UpStatus#tr069_upgradestatus.devid of
                    [] ->
                        Ups#tr069_upgradestatus.devid;
                    _ ->
                        UpStatus#tr069_upgradestatus.devid
                end,
            Upgradestatus = 
                case UpStatus#tr069_upgradestatus.upgradestatus of
                    [] ->
                        Ups#tr069_upgradestatus.upgradestatus;
                    _ ->
                        UpStatus#tr069_upgradestatus.upgradestatus
                end,
            Version = 
                case UpStatus#tr069_upgradestatus.version of
                    [] ->
                        Ups#tr069_upgradestatus.version;
                    _ ->
                        UpStatus#tr069_upgradestatus.version
                end,
            Datetime = 
                case UpStatus#tr069_upgradestatus.datetime of
                    [] ->
                        Ups#tr069_upgradestatus.datetime;
                    _ ->
                        UpStatus#tr069_upgradestatus.datetime
                end,
            Begindatetime = 
                case UpStatus#tr069_upgradestatus.begindatetime of
                    [] ->
                        Ups#tr069_upgradestatus.begindatetime;
                    _ ->
                        UpStatus#tr069_upgradestatus.begindatetime
                end,
            Enddatetime = 
                case UpStatus#tr069_upgradestatus.enddatetime of
                    [] ->
                        Ups#tr069_upgradestatus.enddatetime;
                    _ ->
                        UpStatus#tr069_upgradestatus.enddatetime
                end,
            NUps = Ups#tr069_upgradestatus{commandkey=Commandkey,        
                                            devid =Devid,        
                                            upgradestatus=Upgradestatus,   
                                            version=Version,           
                                            datetime=Datetime,           
                                            begindatetime=Begindatetime,         
                                            enddatetime= Enddatetime     },
            save_upstatusdb_t(Host,NUps)
    end;
save_upstatusdb(Host,_) ->
    {error, error_record}.
    

save_upstatusdb_t(Host,UpStatus = #tr069_upgradestatus{}) ->
    Id = UpStatus#tr069_upgradestatus.commandkey,
    Where = "id="++Id,   
    Adv = upgrade_status_to_db(UpStatus),
    Content = {content, erlang:list_to_atom(?UpgradeStatusTable), erlang:list_to_atom(Id), <<"upgrade_status">>,null,null,null,null,?Author,null,null,null,null,null,Adv},
    Re = insert_data(Host,?DBName,?UpgradeStatusTable,Content),
    case Re of
        {ok,_} ->
            Re;
        {error,content_id_existed} ->
            update_data(Host,?DBName, ?UpgradeStatusTable, Where, Content);
        R ->
            R
    end;
save_upstatusdb_t(Host,_) ->
    {error, error_record}.
    
%% 
save_dev_upstatusdb_t(Host,UpStatus = #tr069_upgradestatus{}) ->
    Id = UpStatus#tr069_upgradestatus.commandkey,
    Where = "id="++Id,   
    Adv = upgrade_status_to_db(UpStatus),
    Content = {content, erlang:list_to_atom(?UpgradeStatusTable), erlang:list_to_atom(Id), <<"upgrade_status">>,null,null,null,null,?Author,null,null,null,null,null,Adv},
    update_data(Host,?DBName, ?UpgradeStatusTable, Where, Content);

save_dev_upstatusdb_t(_,_) ->
    {error, error_record}.


save_dev_upstatusdb(Host, UpStatus = #tr069_upgradestatus{}) ->
    Where = "id="++UpStatus#tr069_upgradestatus.commandkey,
    Ret = get_data(Host,?DBName, ?UpgradeStatusTable, Where),
    R1 =
	case is_list(Ret) of
		false ->
			[];
		true ->
			[db_to_upgrade_status(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	end,
    case R1 of
        [] ->
            save_dev_upstatusdb_t(Host,UpStatus);
        [Ups = #tr069_upgradestatus{}] ->
            Commandkey = 
                case UpStatus#tr069_upgradestatus.commandkey of
                    [] ->
                        Ups#tr069_upgradestatus.commandkey;
                    _ ->
                        UpStatus#tr069_upgradestatus.commandkey
                end,
            Devid = 
                case UpStatus#tr069_upgradestatus.devid of
                    [] ->
                        Ups#tr069_upgradestatus.devid;
                    _ ->
                        UpStatus#tr069_upgradestatus.devid
                end,
            Upgradestatus = 
                case UpStatus#tr069_upgradestatus.upgradestatus of
                    [] ->
                        Ups#tr069_upgradestatus.upgradestatus;
                    _ ->
                        UpStatus#tr069_upgradestatus.upgradestatus
                end,
            Version = 
                case UpStatus#tr069_upgradestatus.version of
                    [] ->
                        Ups#tr069_upgradestatus.version;
                    _ ->
                        UpStatus#tr069_upgradestatus.version
                end,
            Datetime = 
                case UpStatus#tr069_upgradestatus.datetime of
                    [] ->
                        Ups#tr069_upgradestatus.datetime;
                    _ ->
                        UpStatus#tr069_upgradestatus.datetime
                end,
            Begindatetime = 
                case UpStatus#tr069_upgradestatus.begindatetime of
                    [] ->
                        Ups#tr069_upgradestatus.begindatetime;
                    _ ->
                        UpStatus#tr069_upgradestatus.begindatetime
                end,
            Enddatetime = 
                case UpStatus#tr069_upgradestatus.enddatetime of
                    [] ->
                        Ups#tr069_upgradestatus.enddatetime;
                    _ ->
                        UpStatus#tr069_upgradestatus.enddatetime
                end,
            NUps = Ups#tr069_upgradestatus{commandkey=Commandkey,        
                                            devid =Devid,         
                                            upgradestatus=Upgradestatus,   
                                            version=Version,             
                                            datetime=Datetime,               
                                            begindatetime=Begindatetime,         
                                            enddatetime= Enddatetime     },
            save_dev_upstatusdb_t(Host,NUps)
    end;
save_dev_upstatusdb(_,_) ->
    {error, error_record}.
    

%%query_all_upstatus(Hostname) ->
%%    Where = "",    
%%    Ret = get_data(Hostname,?DBName, ?UpgradeStatusTable, Where),
%%	case is_list(Ret) of
%%		false ->
%%			[];
%%		true ->
%%			[db_to_upgrade_status(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
%%	end.


query_all_upstatus() ->
    Where = "",    
    Ret = db_ecc:get_data(?DBName, ?UpgradeStatusTable, Where),
	case is_list(Ret) of
		false ->
			[];
		true ->
			[db_to_upgrade_status(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	end.


query_all_upstatus(Login) ->
    case Login of
    undefined ->
        [];
    {"admin_user",_,_} ->     
        Where = "",
        Ret = db_ecc:get_data(?DBName, ?UpgradeStatusTable, Where),
	    %%Ret = db_ecc:get_data(?DBName, ?RealtimeAlarmTable, "my.statu="++"untreated"),
	    build_upStatus(Ret);
    {UserId,_,_} ->
        Where = "",
        UserList = api_user_spl:get_user(UserId),
        case proplists:get_value(cpe,UserList) of
        undefined ->
            [];
        CpeList ->
	        Ret = db_ecc:get_data(?DBName, ?UpgradeStatusTable, Where),
            %%io:format("AlertRet = ~p~n", [Ret]),
	        build_upStatus(Ret, CpeList)         
        end;
    _ ->
        []    
    end. 


build_upStatus([]) ->
    [];
build_upStatus([{content, _,Id, _, _, _, _, _, _, _, _, _, _, _, Advance}|T]) ->
    [db_to_upgrade_status(Advance)] ++
    build_upStatus(T);
build_upStatus([H|T]) ->
    build_upStatus(T);
build_upStatus(Other) ->
    [].

build_upStatus([], CpeList) ->
    [];
build_upStatus([{content, _,Id, _, _, _, _, _, _, _, _, _, _, _, Advance}|T],CpeList) ->
    UpStatus = db_to_upgrade_status(Advance),
    Manufacturer = UpStatus#tr069_upgradestatus.manufacturer,
    Oui = UpStatus#tr069_upgradestatus.oui,
    Productclass = UpStatus#tr069_upgradestatus.productclass,
    DeviceId = UpStatus#tr069_upgradestatus.devid,
    IsRi = lists:member(DeviceId,CpeList),
    case IsRi of
        true ->
            [UpStatus];
        _ ->
            []
    end ++
    build_upStatus(T,CpeList);
build_upStatus([H|T],CpeList) ->
    build_upStatus(T,CpeList);
build_upStatus(Other,CpeList) ->
    [].

    
get_upstatusid(X=#tr069_upgradestatus{}) ->
    X#tr069_upgradestatus.commandkey.
    

query_upstatus_where(Hostname, Where) ->
    Ret = get_data(Hostname,?DBName, ?UpgradeStatusTable, Where),
	case is_list(Ret) of
		false ->
			[];
		true ->
			[db_to_upgrade_status(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	end.
    
    
    

upgrade_status_to_db(UpSite = #tr069_upgradestatus{}) ->
    %%io:format("UpStatus = ~p~n", [UpSite]),
    [
        {commandkey,string,list_to_binary(UpSite#tr069_upgradestatus.commandkey)},  
        {devid,string,list_to_binary(UpSite#tr069_upgradestatus.devid)},       
        {upgradestatus,string,list_to_binary(UpSite#tr069_upgradestatus.upgradestatus)}, 
        {version,string,list_to_binary(UpSite#tr069_upgradestatus.version)},
        {datetime,string,list_to_binary(UpSite#tr069_upgradestatus.datetime)},
        {begindatetime,string,list_to_binary(UpSite#tr069_upgradestatus.begindatetime)},
        {enddatetime,string,list_to_binary(UpSite#tr069_upgradestatus.enddatetime)},
        {ip,string,list_to_binary(UpSite#tr069_upgradestatus.ip)},
        {mac,string,list_to_binary(UpSite#tr069_upgradestatus.mac)},
        {group,string,list_to_binary(UpSite#tr069_upgradestatus.group)},
        {manufacturer,string,list_to_binary(UpSite#tr069_upgradestatus.manufacturer)},
        {oui,string,list_to_binary(UpSite#tr069_upgradestatus.oui)},
        {productclass,string,list_to_binary(UpSite#tr069_upgradestatus.productclass)},
        {anothorname,string,list_to_binary(UpSite#tr069_upgradestatus.anothorname)}
	 ].


db_to_upgrade_status(Advance) ->
    Data = [db2term(K, T, V) || {K, T, V} <- Advance],
    #tr069_upgradestatus{
            commandkey=proplists:get_value(commandkey, Data),
            devid = proplists:get_value(devid, Data),
            upgradestatus = proplists:get_value(upgradestatus, Data),
            version = proplists:get_value(version, Data),
            datetime = proplists:get_value(datetime, Data),
            begindatetime = proplists:get_value(begindatetime, Data),
            enddatetime = proplists:get_value(enddatetime, Data),
            ip = proplists:get_value(ip, Data),
            mac = proplists:get_value(mac, Data),
            group = proplists:get_value(group, Data),
            manufacturer = proplists:get_value(manufacturer, Data),
            oui = proplists:get_value(oui, Data),
            productclass = proplists:get_value(productclass, Data),
            anothorname = proplists:get_value(anothorname, Data),
            total = proplists:get_value(total, Data)
    }.  


get_statusstru(CommandKey)->
	Ret = db_ecc:get_data(?DBName, ?UpgradeStatusTable, "id=" ++ CommandKey),
	case is_list(Ret) of
		false ->
			[];
		true ->
			[Status|_] = [db_to_upgrade_status(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret],
            {ok, Status#tr069_upgradestatus.devid ++ "_" ++ Status#tr069_upgradestatus.commandkey}
	end.



delete_upstatusbyid(Hostname,Id) ->
    Where = "id="++Id,
    delete_data(Hostname,?DBName,?UpgradeStatusTable,Where).
    

delete_upstatustable(Hostname) ->
    case query_all_upstatus(Hostname) of
		[]->
			{error,not_found};
		R->
			Ret = [delete_upstatusbyid(Hostname,get_upstatusid(X)) || X<-R],
			case lists:keymember(error,1,Ret) of
				true->
					{error,Ret};
				_->
					{ok,removed}
			end
	end.


delete_upstatusbyid(Id) ->
    Where = "id="++Id,
    db_ecc:delete_data(?DBName,?UpgradeStatusTable,Where).


get_upstatus_where(Hostname, Where, Order) ->
    Ret = get_data2(Hostname, ?DBName, ?UpgradeStatusTable, Where,Order),
	case Ret of		
		{R1, R2, Count, R} when erlang:is_list(R) ->
            %%io:format("Count = ~p~n", [Count]),
            %%io:format("R = ~p~n", [R]),
			{ok,build_upstatus({R1, R2, Count, R})};
 		Ots ->
            %%io:format("Ots = ~p~n", [Ots]),
			{error, empty}
	end. 

build_upstatus([]) ->
    [];
build_upstatus([{content, _,_MId, _, _, _, _, _, _, _, _, _, _, _, Advance}|T]) ->
    [db_to_upgrade_status(Advance)] ++
    build_upstatus(T);
build_upstatus([H|T]) ->
    build_upstatus(T);
build_upstatus({R1, R2, Count,[]}) ->
    [];
build_upstatus({R1, R2, Count,[{content, _,_MId, _, _, _, _, _, _, _, _, _, _, _, Advance}|T]}) ->
    VCount = erlang:list_to_binary(erlang:integer_to_list(Count)),
    NAdv =  [{total, number, VCount}],
    [db_to_upgrade_status(Advance++NAdv)] ++
    build_upstatus({R1, R2, Count,T});
build_upstatus({R1, R2, Count,[H|T]}) ->
    build_upstatus({R1, R2, Count,T}).
    
    

upgrade_amount_to_db(UpMount = #tr069_upgradeamount{}) ->
    [
        {id,string,list_to_binary(UpMount#tr069_upgradeamount.id)},  
        {amount,string,list_to_binary(UpMount#tr069_upgradeamount.amount)}
	 ].


db_to_upgrade_amount(Advance) ->
    Data = [db2term(K, T, V) || {K, T, V} <- Advance],
    #tr069_upgradeamount{
            id=proplists:get_value(id, Data),
            amount = proplists:get_value(amount, Data)
    }.  
    

save_upgrade_amount(Host, UpradeAmount = #tr069_upgradeamount{}) ->
    Id = UpradeAmount#tr069_upgradeamount.id,
    Where = "id="++Id,   
    Adv = upgrade_amount_to_db(UpradeAmount),
    Content = {content, erlang:list_to_atom(?UpradeAmount), erlang:list_to_atom(Id), <<"tr069_upgrade_amount">>,null,null,null,null,?Author,null,null,null,null,null,Adv},
    Re = insert_data(Host,?DBName,?UpradeAmount,Content),
    case Re of
        {ok,_} ->
            {ok,Re};
        {error,content_id_existed} ->
            update_data(Host, ?DBName, ?UpradeAmount, Where, Content);
        R ->
            {error,R}
    end;
save_upgrade_amount(Host, _) ->
    {error, error_record}.
    

get_upgrade_amount(Host, Id) ->
    Where = "id="++Id,
    Ret = get_data(Host, ?DBName, ?UpradeAmount, Where),
	case Ret of		
        [] ->
            {ok,#tr069_upgradeamount{}};
		[{content, _,_MId, _, _, _, _, _, _, _, _, _, _, _, Advance}] ->
			{ok,db_to_upgrade_amount(Advance)};
 		_ ->
			{error, empty}
	end. 
    

    
get_label_id() ->
    Host = get(hostname),
    is_lock(Host),
	Ret = db_ecc:get_data(?DBName, ?LabelIdTable, "id=id"),
	case Ret of		
		[{content, _,_MId, _, _, _, _, _, _, _, _, _, _, _, Advance}] ->
			Record = db_to_tr069labelid(Advance);
 		_->
			Record = #tr069_label_id{}
	end,    
    case Record#tr069_label_id.number of     
    undefined ->
        Adv = #tr069_label_id{number="1"}, 
        db_ecc:insert_data(?DBName, ?LabelIdTable, {content, list_to_atom(?LabelIdTable), list_to_atom("id"), <<"id">>,null,null,null,null,?Author,null,null,null,null,null,tr069labelid_to_db(Adv)}),
        N = 1;        
    Num ->
        Adv = #tr069_label_id{number=integer_to_list(Num+1)},
        Where = "id=" ++"id",
        NewRecord = {content, list_to_atom(?LabelIdTable), list_to_atom("id"), <<"id">>, null, null, null, null, ?Author, null, null, null, null, null, tr069labelid_to_db(Adv)},
        db_ecc:update_data(?DBName, ?LabelIdTable, Where, NewRecord),
        N = Num+1
    end,
    release_lock(Host),
    N.     
  
tr069labelid_to_db(Record) ->
    [
        {number,number,list_to_binary(Record#tr069_label_id.number)} 
    ]. 

db_to_tr069labelid(DB) ->
    Data = [db2term(K, T, V) || {K, T, V} <- DB], 
    #tr069_label_id{ 
        number = proplists:get_value(number, Data)           
    }.  

tr069label_to_db(Record) ->
    [
        {name,string,list_to_binary(Record#tr069_label.name)},
        {index,number,list_to_binary(Record#tr069_label.index)}         
    ]. 


db_to_tr069label(DB) ->
    Data = [db2term(K, T, V) || {K, T, V} <- DB], 
    #tr069_label{ 
        name = proplists:get_value(name, Data),
        index = proplists:get_value(index, Data)        
    }. 

is_lock(Host) ->
	global:set_lock({Host, atom_to_list(node()) }, [node()|nodes()]).	
					
release_lock(Host) ->	
	global:del_lock({Host, atom_to_list(node())}, [node()|nodes()]).


is_lock() ->
	global:set_lock({"",atom_to_list(node())++ pid_to_list(self())}, [node()|nodes()]).	
					
release_lock() ->	
	global:del_lock({"",atom_to_list(node())++ pid_to_list(self())}, [node()|nodes()]).

get_all_label() ->
    Ret = db_ecc:get_data(?DBName, ?LabelTable,""),  
    case is_list(Ret) of
		false ->
			[];
		true ->
            if Ret == [] ->
                [];
            true ->                
			    [db_to_tr069label(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret]
            end
    end. 

%index is int  ,Label is string
update_label_table(Label,Index) ->
    Host = get(hostname), 
    is_lock({Host,Label}), 
    Ret = db_ecc:get_data(?DBName, ?LabelTable,"id="++Label), 
    case is_list(Ret) of
		false ->
			{error,""};
		true ->
            if Ret == [] ->
                Record = #tr069_label{name=Label,index="1"},  
                Adv = tr069label_to_db(Record),
	            NewRecord = {content, list_to_atom(?LabelTable), list_to_atom(Label), <<"label">>, null, null, null, null, ?Author, null, null, null, null, null, Adv},   
                db_ecc:insert_data(?DBName, ?LabelTable, NewRecord);  
            true ->                
			    [Re] = [db_to_tr069label(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret],
                Where = "id="++Label,    
                Num =Re#tr069_label.index, 
                Record = Re#tr069_label{index=integer_to_list(Num+Index)},  
                Adv = tr069label_to_db(Record),
	            NewRecord = {content, list_to_atom(?LabelTable), list_to_atom(Label), <<"label">>, null, null, null, null, ?Author, null, null, null, null, null, Adv},   
                db_ecc:update_data(?DBName, ?LabelTable, Where, NewRecord)              
            end
    end,
    release_lock({Host,Label}).    
 
add_cpe_to_taggroup(Tag,CpeList) ->
    add_cpe_to_taggroup_t(CpeList,length(CpeList),Tag).
add_cpe_to_taggroup_t(_,0,T) -> ok;
add_cpe_to_taggroup_t([A|B],Len,Ta) ->
    add_label(Ta,A),
    add_cpe_to_taggroup_t(B,Len-1,Ta). 

add_label(LabelName,DeviceId) ->
    Host = get(hostname), 
    is_lock({Host,DeviceId}),
    Where = "id="++DeviceId,  
    Cpe = get_deviceById(DeviceId),
    if Cpe#tr069_device.description == undefined ->
        Description = "";
    true -> 
        Description = Cpe#tr069_device.description 
    end,
    if Cpe#tr069_device.label == undefined ->
        Label = [];
    true ->
        Label =  Cpe#tr069_device.label 
    end,
    NLabel = 
        case lists:member(LabelName, Label) of
            true ->
                Label;
            _ ->
                [LabelName|Label]
        end,
    DeviceRecord = Cpe#tr069_device{keepalive=integer_to_list(Cpe#tr069_device.keepalive),keepalivetime=integer_to_list(Cpe#tr069_device.keepalivetime),timestamp=integer_to_list(Cpe#tr069_device.timestamp),label= NLabel,description=Description},  	
    %% cache cpe
    tr069_devicecache_server:save_cache_cpe(Host, [DeviceRecord]),
	%%Adv = device_to_db(DeviceRecord),
	%%NewRecord = {content, list_to_atom(?Table), list_to_atom(DeviceId), <<"device">>, null, null, null, null, ?Author, null, null, null, null, null, Adv},
    %%db_ecc:update_data(?DBName, ?Table, Where, NewRecord),
    update_label_table(LabelName,1),
    release_lock({Host,DeviceId}). 
    

delete_label(LabelName,DeviceId) ->
    Host = get(hostname), 
    is_lock({Host,DeviceId}),
    Where = "id="++DeviceId,  
    Cpe = get_deviceById(DeviceId),
    if Cpe#tr069_device.description == undefined ->
        Description = "";
    true -> 
        Description = Cpe#tr069_device.description 
    end,
    if Cpe#tr069_device.label == undefined ->
        Label = [];
    true ->
        Label =  Cpe#tr069_device.label 
    end,
    Nowlabel = lists:delete(LabelName,Label), 
    DeviceRecord = Cpe#tr069_device{keepalive=integer_to_list(Cpe#tr069_device.keepalive),keepalivetime=integer_to_list(Cpe#tr069_device.keepalivetime),timestamp=integer_to_list(Cpe#tr069_device.timestamp),label= Nowlabel,description=Description},  	
    %% cache cpe
    tr069_devicecache_server:save_cache_cpe(Host, [DeviceRecord]),
	%%Adv = device_to_db(DeviceRecord),
	%%NewRecord = {content, list_to_atom(?Table), list_to_atom(DeviceId), <<"device">>, null, null, null, null, ?Author, null, null, null, null, null, Adv},
    %%db_ecc:update_data(?DBName, ?Table, Where, NewRecord),
    update_label_table(LabelName,-1),
    release_lock({Host,DeviceId}).         

delete_label_group(Label) ->     
	Ret = db_ecc:get_data(?DBName, ?Table, ""),
	List = case is_list(Ret) of
		false ->
		    [];
		true ->
			[db_to_device(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
    end,
    case delete_label_device(Label,List) of
    ok ->
        %%db_ecc:delete_data(?DBName, ?Table, "id="++ Label);
        %% cache cpe
        %%Host = domain(get(hostname)),
        Host = textutils:any_to_list(get(hostname)),
        tr069_devicecache_server:delete_cache_cpe(Host, [Label]);
    _ ->
        {error,""} 
    end.


delete_label_device(Label,DeviceList) ->
    delete_label_device_t(Label,DeviceList,length(DeviceList)).
delete_label_device_t(_,_,0) -> ok;
delete_label_device_t(Label,[A|B],Len) ->
    case A#tr069_device.label  of
    undefined ->
        if Label == "undefined" ->
            delete_label_device_t(Label,B,Len-1);  
        true -> 
            delete_label_device_t(Label,B,Len-1) 
        end;    
    [] ->
        if Label == "undefined" ->
            delete_label_device_t(Label,B,Len-1);  
        true -> 
            delete_label_device_t(Label,B,Len-1) 
        end;
    _ ->        
        case lists:member(Label,A#tr069_device.label) of
        true ->
            delete_label(Label,A#tr069_device.manufacturer++"_"++A#tr069_device.oui++"_"++A#tr069_device.serialnumber), 
            delete_label_device_t(Label,B,Len-1);
        _ ->  
            delete_label_device_t(Label,B,Len-1)  
        end
    end.     
    
all_app() ->     
    case rpc_proxy:call(?DBName,ets,tab2list,[application]) of
    Apps when is_list(Apps) ->
        F = fun(X) -> X#application.id end,
        lists:map(F,Apps);
    _ ->
        []
    end.      