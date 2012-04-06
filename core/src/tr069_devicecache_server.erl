-module(tr069_devicecache_server).
-define(CpeCache,'tr069_device_cache').
-define(Author,<<"ning.cai@dragonflow.com">>).
-define(DBName,server_conf:get_db_node()).
-define(Table,"tr069_device").

-include("monitor.hrl").
-record(state, {
                    update_cep=dict:new()       %% ���޸ĵ�cpe, ���û���ƶ�ֵ����ʼ��Ϊһ��hash��
                    }).
-define(SERVER,'ecc_tr069_devicecache_server').
-include("config.hrl"). 
-compile(export_all).


%% -------------------------------------------
%% ------------�ڲ����ߺ���-------------------
%% -------------------------------------------


%% ����cpe id
make_cepid(DeviceRecord) ->
    textutils:replacespace(DeviceRecord#tr069_device.manufacturer) ++"_"++DeviceRecord#tr069_device.oui ++"_"++ DeviceRecord#tr069_device.serialnumber.

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
  end.
  
insert_data(_,_,_,{})->{error,parameter_error};
insert_data(Host,DbName,Table,Data)->
    AppName = domain(Host),
    Newdata = Data#content{xn_type = list_to_binary(Table),application=list_to_atom(AppName)},
    rpc:call(DbName,content,create,[[{application,AppName},{version,2}],Newdata]).  
    
    
update_data(_,_,_,_,{})->{error,parameter_error};
update_data(Host,DbName,Table,Where,Data)->
    AppName = domain(Host),
    Newdata = Data#content{xn_type = list_to_binary(Table),application=list_to_atom(AppName)},
	Result = rpc:call(DbName,content,update,[[{application,AppName},{content,Where}],Newdata]),
    Result.

delete_data(Host,DbName,Table,Where)->
    AppName = domain(Host),
	rpc:call(DbName,content,delete,[[{application,AppName},{content,Where}]]).
    
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
    case rpc:call(DbName, content, get, [[{application,AppName},{content,"type = '"++Table++"'&"++Where},"from=0&to=100000"]]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			Else
	end.
get_data(Host,DbName, Table, [],Order) when is_list(Order),length(Order)>0->
    AppName = domain(Host),
	case rpc:call(DbName, content, get, [[{application,AppName},{content,"type = '"++Table++"'"},"from=0&to=100000" ++ "&" ++ Order]]) of
    %case rpc:call(DbName, content, get, [[{application,Table},{content,Where},"from=0&to=100000" ++ "&" ++ Order]]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			Else
	end;
get_data(Host,DbName, Table, Where,Order) when is_list(Order),length(Order)>0->
    AppName = domain(Host),
	case rpc:call(DbName, content, get, [[{application,AppName},{content,"type = '"++Table++"'&"++Where},"from=0&to=100000" ++ "&" ++ Order]]) of
    %case rpc:call(DbName, content, get, [[{application,Table},{content,Where},"from=0&to=100000" ++ "&" ++ Order]]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			Else
	end;
get_data(Host,DbName, Table, [],Order) when is_list(Order)->
    AppName = domain(Host),
	case rpc:call(DbName, content, get, [[{application,AppName},{content,"type = '"++Table++"'"},"from=0&to=100000"]]) of
    %case rpc:call(DbName, content, get, [[{application,Table},{content,Where},"from=0&to=100000"]]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			Else
	end;
get_data(Host,DbName, Table, Where,Order) when is_list(Order)->
    AppName = domain(Host),
	case rpc:call(DbName, content, get, [[{application,AppName},{content,"type = '"++Table++"'&"++Where},"from=0&to=100000"]]) of
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
	case rpc:call(DbName, content, get, [[{application,AppName},{content,"type = '"++Table ++ "'"},Order]]) of
		{ok,R}->
			R;
		Else->
			Else
	end;
get_data2(Host,DbName, Table, Where,Order) when is_list(Order) andalso is_list(Where)->
    AppName = domain(Host),
    %%io:format("get_data2 hostname:~p~n",[AppName]),
	case rpc:call(DbName, content, get, [[{application,AppName},{content,"type = '"++Table++"'&"++Where},Order]]) of
		{ok,R}->
			R;
		Else->
			Else
	end;
get_data2(_,_, _, _,_)->{error,parameter_error}.
    
%% ----------------------------------------------------

%% ----------------------------------------------------
%% -----------------------�����-------------------------
%% ----------------------------------------------------

%% ��cpe��¼�����[{Id, A=#tr069_device{}}|T]
build_cpekv([]) ->
    [];
build_cpekv([A=#tr069_device{}|B]) ->
    Id = make_cepid(A),
    [{Id, A}] ++
    build_cpekv(B);
build_cpekv([A|B]) ->
    build_cpekv(B).

%% �ַ����͵��ֶ�,������Ͷ���Ϊnumber,��ô�洢����ݿ�ͱ����integer����,��ô������������ͻ����ַ�������,Ҫ�������б����������͵��ֶ�ת��Ϊ�������� 
transform_list_integer([]) ->
    [];
transform_list_integer([A=#tr069_device{}|T]) ->
    [A#tr069_device{
            keepalive = 
                case erlang:is_number(A#tr069_device.keepalive) of
                    true ->
                        A#tr069_device.keepalive;
                    _ ->
                        try erlang:list_to_integer(A#tr069_device.keepalive) of
                            IntKeepAlive ->
                                IntKeepAlive
                        catch 
                            _:_ ->
                                0
                        end
                end,
            keepalivetime =
                case erlang:is_number(A#tr069_device.keepalivetime) of
                    true ->
                        A#tr069_device.keepalivetime;
                    _ ->
                        try erlang:list_to_integer(A#tr069_device.keepalivetime) of
                            IntKeepAliveTime ->
                                IntKeepAliveTime
                        catch 
                            _:_ ->
                                15
                        end
                end,
            timestamp = 
                case erlang:is_number(A#tr069_device.timestamp) of
                    true ->
                        A#tr069_device.timestamp;
                    _ ->
                        try erlang:list_to_integer(A#tr069_device.timestamp) of
                            TimeStamp ->
                                TimeStamp
                        catch
                            _:_ ->
                                0
                        end
                end
    }] ++
    transform_list_integer(T);
transform_list_integer([A|T]) ->
    transform_list_integer(T).
    

%% ----------------------------------------------------

%% -------------------------------------------
%% --------------------api--------------------
%% -------------------------------------------




%% ********************************************************************************************
%% *******************�������api**************************************************************
%% ********************************************************************************************





%% �޸�cpe�Ļ������ cpe api
cache_cpe(Host, []) ->
    {ok, "ok"};
cache_cpe(Host, [{Id,A=#tr069_device{}}|B]) ->
    CpeStatus =
    case tr069_CpeCache:member(Host, Id) of
        true ->
            Cpe = tr069_CpeCache:getCpeByCache(Host, Id),
            Object = {Id, A},
            [CacheCpe] = transform_list_integer([A]),
            if
                A#tr069_device.ip =/= Cpe#tr069_device.ip ->
                    tr069_CpeCache:saveCpeByCache(Host, CacheCpe),
                    {old_cpe,Object};
                A#tr069_device.manufacturer =/= Cpe#tr069_device.manufacturer ->
                    tr069_CpeCache:saveCpeByCache(Host, CacheCpe),
                    {old_cpe,Object};
                A#tr069_device.oui =/= Cpe#tr069_device.oui ->
                    tr069_CpeCache:saveCpeByCache(Host, CacheCpe),
                    {old_cpe,Object};
                A#tr069_device.productclass =/= Cpe#tr069_device.productclass ->
                    tr069_CpeCache:saveCpeByCache(Host, CacheCpe),
                    {old_cpe,Object};
                A#tr069_device.serialnumber =/= Cpe#tr069_device.serialnumber ->
                    tr069_CpeCache:saveCpeByCache(Host, CacheCpe),
                    {old_cpe,Object};
                A#tr069_device.profile =/= Cpe#tr069_device.profile ->
                    tr069_CpeCache:saveCpeByCache(Host, CacheCpe),
                    {old_cpe,Object};
                A#tr069_device.deviceport =/= Cpe#tr069_device.deviceport ->
                    tr069_CpeCache:saveCpeByCache(Host, CacheCpe),
                    {old_cpe,Object};
                A#tr069_device.authtype =/= Cpe#tr069_device.authtype ->
                    tr069_CpeCache:saveCpeByCache(Host, CacheCpe),
                    {old_cpe,Object};
                A#tr069_device.user =/= Cpe#tr069_device.user ->
                    tr069_CpeCache:saveCpeByCache(Host, CacheCpe),
                    {old_cpe,Object};
                A#tr069_device.password =/= Cpe#tr069_device.password ->
                    tr069_CpeCache:saveCpeByCache(Host, CacheCpe),
                    {old_cpe,Object};
                A#tr069_device.keyfile =/= Cpe#tr069_device.keyfile ->
                    tr069_CpeCache:saveCpeByCache(Host, CacheCpe),
                    {old_cpe,Object};
                A#tr069_device.acsip =/= Cpe#tr069_device.acsip ->
                    tr069_CpeCache:saveCpeByCache(Host, CacheCpe),
                    {old_cpe,Object};
                A#tr069_device.acsname =/= Cpe#tr069_device.acsname ->
                    tr069_CpeCache:saveCpeByCache(Host, CacheCpe),
                    {old_cpe,Object};
                A#tr069_device.keepalive =/= Cpe#tr069_device.keepalive ->
                    tr069_CpeCache:saveCpeByCache(Host, CacheCpe),
                    {old_cpe,Object};
                A#tr069_device.keepalivetime =/= Cpe#tr069_device.keepalivetime ->
                    tr069_CpeCache:saveCpeByCache(Host, CacheCpe),
                    {old_cpe,Object};
                A#tr069_device.label =/= Cpe#tr069_device.label ->
                    tr069_CpeCache:saveCpeByCache(Host, CacheCpe),
                    {old_cpe,Object};
                A#tr069_device.description =/= Cpe#tr069_device.description ->
                    tr069_CpeCache:saveCpeByCache(Host, CacheCpe),
                    {old_cpe,Object};
                true ->
                    tr069_CpeCache:saveCpeByCache(Host, CacheCpe),
                    []
            end;
        _ ->
            Object = {Id, A},
            [CacheCpe] = transform_list_integer([A]),
            %%CacheObject = {Id, CacheCpe},
            %%ets:insert(?CpeCache, CacheObject),
            tr069_CpeCache:saveCpeByCache(Host, CacheCpe),
            {new_cpe, Object}
    end,
    case CpeStatus of
        {new_cpe,{Ids, Dev}} ->
            Adv = device_to_db(Dev),
            DeviceId =  list_to_atom(textutils:replacespace(Dev#tr069_device.manufacturer) ++"_"++Dev#tr069_device.oui ++"_"++ Dev#tr069_device.serialnumber),
            case insert_data(Host,?DBName, ?Table, {content, list_to_atom(?Table), DeviceId, <<"device">>,null,null,null,null,?Author,null,null,null,null,null,Adv}) of
                {ok, _} ->
                    siteview:addPoints(1);
                _ ->
                    ok
            end;
        {old_cpe,{Ids, Dev}} ->
            Where = "id=" ++ Id,
            Adv = device_to_db(Dev),
            NewRecord = {content, list_to_atom(?Table), list_to_atom(Id), <<"device">>, null, null, null, null, ?Author, null, null, null, null, null, Adv},
            update_data(Host,?DBName, ?Table, Where, NewRecord);
        _ ->
            ok
    end,
    cache_cpe(Host, B);
cache_cpe(Host, [A|B]) ->
    cache_cpe(Host, B).

delete_cache_cpe_t(Host, []) ->
    {ok, "delete_ok"};
delete_cache_cpe_t(Host, [Id|B]) ->
    %%io:format("Id = ~p~n", [Id]),
    delete_data(Host, ?DBName, ?Table, "id="++ Id),
    tr069_CpeCache:deleteCpeByCache(Host, Id),
    delete_cache_cpe_t(Host, B).


%% ����cpe�Ļ������ <<API>>
save_cache_cpe(Host, Cpes) ->       %% ��ӻ��޸Ļ����ͬʱ���ı�contentstore���豸������ʱ�����״̬�ı仯
    KvCpes = build_cpekv(Cpes),
    try cache_cpe(Host, KvCpes) of
        {ok, Re} ->
            {ok, Re};
        _ ->
            {error, "save_error"}
    catch    
        _:_ ->
            {error, "save_error"}
    end.

%% ɾ��cpe�Ļ������ <<API>>       %% ɾ����ͬʱ��ɾ��contentstore���豸
delete_cache_cpe(Host, CpeIds) ->
    try delete_cache_cpe_t(Host, CpeIds) of
        {ok, ErrorRe} ->
            {ok, ErrorRe};
        _ ->
            {error, "delete_error"}
    catch
        _:_ ->
            {error, "delete_error"}
    end.

%% ��ȡcpe��Ļ������  <<API>>     %% �Դӷ�����ȡ�����豸�����뻺��ȶԣ����»���
get_cache_cpe(Host, Cpes) ->
    KvCpes = build_cpekv(Cpes),
    %%io:format("KvCpes = ~p~n", [KvCpes]),
    Result = 
    try get_cache_cpe_t(Host, KvCpes) of
        Re ->
            Re
    catch
        _:Other ->
            Other
    end,
    %%io:format("Result = ~p~n", [Result]),
    Result.
    
%% ��ȡcpe��Ļ������  <<API>>     %% ��id�ڻ����������,�ҵ��ʹӻ�������ȡ����,����ӷ�����ȡ
get_one_cache_cpe(Host, Ids) ->
    get_one_cache_cpe_t(Host, Ids).

%% ********************************************************************************************
%% ********************************************************************************************
%% ********************************************************************************************

    


get_one_cache_cpe_t(Host, []) ->
    [];
get_one_cache_cpe_t(Host, [Id|B]) ->
    case tr069_CpeCache:member(Host, Id) of
        true ->
            Cpe = tr069_CpeCache:getCpeByCache(Host, Id),
            [Cpe];
        _ ->
            Where = "id="++Id,
            Order = [],
            Ret = get_data(Host,?DBName, ?Table, Where, []),
            Dev = 
            case is_list(Ret) of
                false ->
                    [];
                true ->
                    [db_to_device(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret]
            end,
            case Dev of
                [Dv] ->
                    [CacheCpe] = transform_list_integer([Dv]),
                    tr069_CpeCache:saveCpeByCache(Host, CacheCpe),
                    [CacheCpe];
                _ ->
                    []
            end
    end ++
    get_one_cache_cpe_t(Host, B).

    
get_cache_cpe_t(Host, []) ->
    [];
get_cache_cpe_t(Host, [{Id,A=#tr069_device{}}|B]) ->
    Object = {Id,A},
    case tr069_CpeCache:member(Host, Id) of
        true ->
            Cpe = tr069_CpeCache:getCpeByCache(Host, Id),
            NCpe = Cpe#tr069_device{total=A#tr069_device.total},
            [CacheCpe] = transform_list_integer([NCpe]),
            tr069_CpeCache:saveCpeByCache(Host, CacheCpe),
            [CacheCpe];
        _ ->
            [CacheCpe] = transform_list_integer([A]),
            tr069_CpeCache:saveCpeByCache(Host, CacheCpe),
            [CacheCpe]
    end ++
    get_cache_cpe_t(Host, B);
get_cache_cpe_t(Host, [A|B]) ->
    get_cache_cpe_t(Host, B).
    



 
    
    
    

            
    
    
    
