%% 
%% @doc api of main system
%% @version{1.0}
%% @copyright 2009 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>
%%
-module(api_siteview).
-compile(export_all).
-include("monitor.hrl").
-export([get_object_name/1,get_parent_id/1,
		get_parent_name/1,get_full_name/1,get_object_path/1,
		get_nodes/0,get_childs/1,get_started_time/0,getAllGroups/0,
		getGroupInfoForTuopu/0,is_topid/1]).

%% @spec find_object(Id)->[SvObject]
%% @doc find a svecc object by id
%% 
find_object(Id) when is_list(Id)->
	find_object(list_to_atom(Id));
find_object(Id)->
	siteview:get_object(Id).
	%%SV = siteview:get_current_siteview(),
	%%{ok,{childs,Childs}} = SV:get_attribute(childs),
	%%find_object(Id ,Childs).


%% @spec find_object(GID,Groups)->[SvObject]
%% @doc find a svecc object
%% @deprecated will not be used
find_object(_,[])->[];
find_object(GID,Groups)->
	[G|T] = Groups,
	Ret = case G:get_property(class) of
			{ok,{class,group}}->
				case G:get_property(id) of
					{ok,{id,GID}}->
						[G];
					_->
						[]
				end;
			{ok,{class,device}}->
				case G:get_property(id) of
					{ok,{id,GID}}->
						[G];
					_->
						[]
				end;
			_->
				case G:get_property(id) of
					{ok,{id,GID}}->
						[G];
					_->
						[]
				end
			end,
	if
		length(Ret) > 0->
			Ret;
		true->
			case G:get_property(class) of
				{ok,{class,group}}->
					{ok,{childs,Childs}} = G:get_attribute(childs),
					find_object(GID,T++Childs);
				{ok,{class,device}}->
					{ok,{childs,Childs}} = G:get_attribute(childs),
					find_object(GID,T++Childs);
				_->
					find_object(GID,T)
			end
	end.

%% @spec get_object_name(Id)->string()
%% where
%%	Id = atom()
%% @doc get a object's name,if not found this object will return "",
%% if this object not have name,will return id.object include group,monitor,rule etc.
get_object_name(Id)->
	case find_object(Id) of
		[]->
			"";
		[M|_]->
			case M:get_property(?NAME) of
				{ok,{?NAME,Name}}->
					Name;
				_->
					atom_to_list(Id)
			end
	end.

%% @spec get_parent_id(Id)->atom()
%% @doc get parent id of a object
%%		if not found object or this object not have parent will return ''
get_parent_id(Id)->
	case find_object(Id) of
		[]->
			'';
		[M|_]->
			case M:get_property(?PARENT) of
				{ok,{?PARENT,Parent}}->
					Parent;
				_->
					''
			end
	end.

%% @spec get_parent_name(Id)->atom()
%% @doc get parent name of a object
%%		if not found object or this object not have parent will return ""
get_parent_name(Id)->
	case find_object(Id) of
		[]->
			"";
		[M|_]->
			case M:get_property(?PARENT) of
				{ok,{?PARENT,Parent}}->
					case find_object(Parent) of
						[]->
							"";
						[G|_]->
							case G:get_property(?NAME) of
								{ok,{?NAME,Name}}->
									Name;
								_->
									Parent
							end
					end;
				_->
					""
			end
	end.

%% @spec get_full_name(Id)->string()
%% where
%%	Id = atom()
%% @doc get full name of a object
%%
get_full_name(Id)->
	case find_object(Id) of
		[]->
			"";
		[M|_]->
			M:get_full_name()
	end.

%% @spec get_object_path(Id)->[{Name,Id}]
%% where
%%		Name = string()
%%		Id = atom()
%% @doc get a object's full path 
get_object_path(Id)->
	case find_object(Id) of
		[]->
			[];
		[M|_]->
			{ok,{id,Id}} = M:get_property(id),
			get_object_path_(M:get_parent()) ++ [{M:get_name(),Id}]
	end.

get_object_path_({ok,{parent,Obj}})->
	{ok,{id,Id}} = Obj:get_property(id),
	get_object_path_(Obj:get_parent())  ++ [{Obj:get_name(),Id}];

get_object_path_(_)->[].

%% @spec get_nodes()->[{ServerId,ServerName}]
%% where
%%	ServerId = atom()
%%	ServerName = string()
%% @doc get ecc nodes,ServerName is the name of Node
get_nodes()->
	[{siteview:getServerID(),"root"}].
	
	
is_topid(Id)->
	case lists:keysearch(Id,1,get_nodes()) of
		{value,_}->
			true;
		_->
			false
	end.
	


%% @spec get_childs(ServerId)->[Child]
%% where
%%	ServerId = atom()
%%	Child = [{key,value}]
%% @doc obtain the server's children,return a list of Child,each Child is a key-value tuple list
get_childs(ServerId)->
	SV = siteview:get_current_siteview(),
	HealthId = list_to_atom(atom_to_list(siteview:getServerID()) ++ ".health"),
	F = fun(X)->
		case X:get_property(id) of
			{ok,{_,HealthId}}->
				false;
			_->
				true
		end
	end,
	Childs = lists:filter(F,SV:get_childs()),
	[X:get_properties()++X:get_run_info()||X<-Childs].

%% @spec getAllGroupsMonitors()->[{Name,Id}]
%% where
%%	Name = string()
%%	Id = atom()
%% @doc get all groups and monitors in this siteview group
getAllGroupsMonitors()->
	SV = siteview:get_current_siteview(),
	F = fun(X)->
			{ok,{id,Id}} = X:get_property(id),
			{X:get_full_name(),atom_to_list(Id)}
		end,
	lists:map(F,SV:getGroupsMonitors(true)).
	
%% @spec getAllGroups()->[{Name,Id}]
%% where
%%	Name = string()
%%	Id = atom()
%% @doc get all groups in this siteview group
getAllGroups()->
	Groups = siteview:get_object_by_class(monitor_group),
	F = fun(X)->
		{ok,{_,Id}} = X:get_property(id),
		{X:get_full_name(),Id}
	end,
	List = [F(X)||X<-Groups].

%% @spec getGroupInfoForTuopu()->[{Name,Id, run_info}]
%% where
%%	Name = string()
%%	Id = atom()
%% @doc get all groups 's runinfo in this siteview group
getGroupInfoForTuopu()->
	SV = siteview:get_current_siteview(),
	F = fun(X)->
			{ok,{id,Id}} = X:get_property(id),			
			case length(X:get_run_info()) of
				3 ->
					{atom_to_list(Id), X:get_run_info() ++ [{name, iconv:convert("utf-8","gb2312", X:get_name())}]};
				_ ->
					{}
			end
	end,	
	Tmp = lists:map(F,SV:getGroupsMonitors(true)),
	
	F1 = fun(X1)->
		case X1 of
			{} ->
				false;
			_->
				true
		end
	end,	
	[{groupinfo, lists:filter(F1, Tmp)}].

%% @spec getMonitorInfoForTuopu()->[{Name,Id, run_info}]
%% where
%%	Name = string()
%%	Id = atom()
%% @doc get all monitors's runinfo in this siteview group
getMonitorInfoForTuopu()->
	SV = siteview:get_current_siteview(),
	F = fun(X)->
			{ok,{id,Id}} = X:get_property(id),			
			case length(X:get_run_info()) of
				3 ->
					{};
				_ ->
					{atom_to_list(Id), X:get_run_info() ++ [{name, iconv:convert("utf-8","gb2312", X:get_name())}]}
			end
	end,	
	Tmp = lists:map(F,SV:getGroupsMonitors(true)),
	
	F1 = fun(X1)->
		case X1 of
			{} ->
				false;
			_->
				true
		end
	end,
	[{monitorinfo, lists:filter(F1, Tmp)}].

%% @spec getMachineInfoForTuopu()->[{Name,Id, Host}]
%% where
%%	Name = string()
%%	Id = atom()
%% @doc get monitors's machineinfo in this siteview group
getMachineInfoForTuopu()->
	SV = siteview:get_current_siteview(),
	F = fun(X)->
			{ok,{id,Id}} = X:get_property(id),
			case X:get_property(hostname) of 
			{error,{_, _}} ->
					case X:get_property(machine) of 
					{error,{_, _}} ->
						{};
					{ok,{_,HostAddress}} ->
						case dbcs_machine:get_machine_by_host(HostAddress) of
							[] ->
								{atom_to_list(Id),[{name, iconv:convert("utf-8","gb2312", X:get_name())}, {hostName, iconv:convert("utf-8","gb2312", "this server")}, {hostAddress, "127.0.0.1"}]};						
							Machine ->
								[Info] = Machine,
								{atom_to_list(Id),[{name, iconv:convert("utf-8","gb2312", X:get_name())}, {hostName, iconv:convert("utf-8","gb2312", element(3, Info))}, {hostAddress, HostAddress}]}					
						end
					end;
			{ok,{_,HostName}} ->											
				case dbcs_machine:get_machine(HostName) of
					[] ->
						{atom_to_list(Id),[{name, iconv:convert("utf-8","gb2312", X:get_name())}, {hostName, iconv:convert("utf-8","gb2312", HostName)}, {hostAddress, ""}]};						
					Machine ->
						[Info] = Machine,
						{atom_to_list(Id),[{name,  iconv:convert("utf-8","gb2312", X:get_name())}, {hostName, iconv:convert("utf-8","gb2312", HostName)}, {hostAddress, element(4, Info)}]}					
				end				
			end
		end,
	Tmp = lists:map(F,SV:getGroupsMonitors(true)),
	
	F1 = fun(X1)->
		case X1 of
			{} ->
				false;
			_->
				true
		end
	end,
	[{machineinfo, lists:filter(F1, Tmp)}].

%% @spec get_current_monitors_per_minute()->integer()
%% @doc get current monitors per minutes
%%
get_current_monitors_per_minute()-> 0.

%% @spec get_current_monitors_running()->integer()
%% @doc get current running monitor count
%%
get_current_monitors_running()->0.

%% @spec get_current_monitors_waiting()->integer()
%% @doc get monitor count waiting to runing
%%
get_current_monitors_waiting()->0.

%% @spec get_started_time()->integer()
%% @doc get siteview started time,unit is microsecond
%%
get_started_time()->
	siteview:get_started_time().

 reset()->
	ParentId = siteview:getServerID(),
	case siteview:reset(localhost) of
		{ok,_}->
			{ok,Gid} = dbcs_group:get_next_id(ParentId),
			case dbcs_group:create_group([{id,Gid},{parent,ParentId},{name,"server1"},{class,group},{frequency,0},{depends_on,"none"},{depends_condition,"good"}]) of
				{ok,Group}->
					Id = proplists:get_value(id,Group),
					{ok,Mid1} = dbcs_group:get_next_id(Id),
					dbcs_monitor:create_monitor([{id,Mid1},{parent,Id},{browse,[{"utilization","utilization"}]},
												{class,browsa_cpu_utilization},{name,"CPU Utilization"},{frequency,600},{disabled,false},{verify_error,false},
												{error_frequency,0},{depends_on,"none"},{depends_condition,"good"},{schedule,"all"},{machine,""}]
												),
					{ok,Mid2} = dbcs_group:get_next_id(Id),
					dbcs_monitor:create_monitor([{id,Mid2},{parent,Id},{class,memory_monitor},{name,"Memory"},
												{frequency,600},{disabled,false},{verfiy_error,false},{error_frequency,0},
												{depends_on,"none"},{depends_condition,"good"},{schedule,"all"},{machine,[]}
												]
												);
					% {ok,Mid3} = dbcs_group:get_next_id(Id),
					% dbcs_monitor:create_monitor([{id,Mid3},{parent,Id},{class,diskspace_monitor},{name,"Disk(C)"},{disk,"C"},
												% {frequency,600},{disabled,false},{verfiy_error,false},{error_frequency,0},
												% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},{machine,[]}
												% ]
												% );
				_->
					{error,create_group_error}
			end,
			{ok,Gid2} = dbcs_group:get_next_id(ParentId),
			case dbcs_group:create_group([{id,Gid2},{parent,ParentId},{name," network1"},{class,group},{frequency,0},{depends_on,"none"},{depends_condition,"good"}]) of
				{ok,Group2}->
					{ok,Mid21} = dbcs_group:get_next_id(Gid2),
					dbcs_monitor:create_monitor([{id,Mid21},{parent,Gid2},{hostname,"localhost"},{timeout,3000},{size,32},
												{class,ping_monitor},{name,"Ping:localhost"},{frequency,600},{disabled,false},{verify_error,false},
												{error_frequency,0},{depends_on,"none"},{depends_condition,"good"},{schedule,"all"}]
												),
					{ok,Mid22} = dbcs_group:get_next_id(Gid2),
					dbcs_monitor:create_monitor([{id,Mid22},{parent,Gid2},{class,url_monitor},{name,"URL Monitor:http://www.quikview.net"},
												{frequency,600},{disabled,false},{verfiy_error,false},{error_frequency,0},
												{depends_on,"none"},{depends_condition,"good"},{schedule,"all"},{url,"http://www.quikview.net"},
												{contentMatch,[]},{timeout,60},{proxy,[]},{errorContent,[]},{checkContent,"no content checking"},
												{userName,[]},{password,[]},{domain,[]},{whenToAuthenticate,"Use Global Preference"},{proxyUserName,[]},
												{proxyPassword,[]},{postData,[]},{errorOnRedirect,false},{urlEncoding,[]}, {httpVersion10,false},{retries,0},
												{acceptAllUntrustedCerts,false},{acceptInvalidCerts,false},{encodePostData,"contentTypeUrlencoded"}
												]
												);
				_->
					{error,create_group_error}
			end;
		_->
			{error,siteview_init_error}
	end.
	
reset(App)->
	ParentId = siteview:getServerID(),
	case siteview:reset(App) of
		{ok,_}->
			dbcs_base:set_app(App),
			{ok,Gid} = dbcs_group:get_next_id(ParentId),
			case dbcs_group:create_group([{id,Gid},{parent,ParentId},{name,"server1"},{class,group},{frequency,0},{depends_on,"none"},{depends_condition,"good"}]) of
				{ok,Group}->
					Id = proplists:get_value(id,Group),
					{ok,Mid1} = dbcs_group:get_next_id(Id),
					dbcs_monitor:create_monitor([{id,Mid1},{parent,Id},{browse,[{"utilization","utilization"}]},
												{class,browsa_cpu_utilization},{name,"CPU Utilization"},{frequency,600},{disabled,false},{verfiy_error,false},
												{error_frequency,0},{depends_on,"none"},{depends_condition,"good"},{schedule,"all"},{machine,""}]
												),
					{ok,Mid2} = dbcs_group:get_next_id(Id),
					dbcs_monitor:create_monitor([{id,Mid2},{parent,Id},{class,memory_monitor},{name,"Memory"},
												{frequency,600},{disabled,false},{verfiy_error,false},{error_frequency,0},
												{depends_on,"none"},{depends_condition,"good"},{schedule,"all"},{machine,[]}
												]
												);
					% {ok,Mid3} = dbcs_group:get_next_id(Id),
					% dbcs_monitor:create_monitor([{id,Mid3},{parent,Id},{class,diskspace_monitor},{name,"Disk(C)"},{disk,"C"},
												% {frequency,600},{disabled,false},{verfiy_error,false},{error_frequency,0},
												% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},{machine,[]}
												% ]
												% ),
					
				_->
					{error,create_group_error}
			end,
			{ok,Gid2} = dbcs_group:get_next_id(ParentId),
			case dbcs_group:create_group([{id,Gid2},{parent,ParentId},{name," network1"},{class,group},{frequency,0},{depends_on,"none"},{depends_condition,"good"}]) of
				{ok,Group2}->
					{ok,Mid21} = dbcs_group:get_next_id(Gid2),
					dbcs_monitor:create_monitor([{id,Mid21},{parent,Gid2},{hostname,"localhost"},{timeout,3000},{size,32},
												{class,ping_monitor},{name,"Ping:localhost"},{frequency,600},{disabled,false},{verfiy_error,false},
												{error_frequency,0},{depends_on,"none"},{depends_condition,"good"},{schedule,"all"}]
												),
					{ok,Mid22} = dbcs_group:get_next_id(Gid2),
					dbcs_monitor:create_monitor([{id,Mid22},{parent,Gid2},{class,url_monitor},{name,"URL Monitor:http://www.quikview.net"},
												{frequency,600},{disabled,false},{verfiy_error,false},{error_frequency,0},
												{depends_on,"none"},{depends_condition,"good"},{schedule,"all"},{url,"http://www.quikview.net"},
												{contentMatch,[]},{timeout,60},{proxy,[]},{errorContent,[]},{checkContent,"no content checking"},
												{userName,[]},{password,[]},{domain,[]},{whenToAuthenticate,"Use Global Preference"},{proxyUserName,[]},
												{proxyPassword,[]},{postData,[]},{errorOnRedirect,false},{urlEncoding,[]}, {httpVersion10,false},{retries,0},
												{acceptAllUntrustedCerts,false},{acceptInvalidCerts,false},{encodePostData,"contentTypeUrlencoded"}
												]
												),
					siteview:check_install_date(),
					SV = siteview:get_current_siteview(),
					SV:load_app(App);
				_->
					{error,create_group_error}
			end;
		_->
			{error,siteview_init_error}
	end.
