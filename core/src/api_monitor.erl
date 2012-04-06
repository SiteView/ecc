%% 
%% @doc api of monitor operation
%% @version{1.0}
%% @copyright 2009 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>

-module(api_monitor).
-extends(api_siteview).

-export([create/2,update/1,delete/1,info/1,get_run_info/1,move/2,copy/2,get_classifier/2,get_hostname/1,change_name/2,
		get_all_classifier/1,getBrowseData/2,get_all_monitors/0, get_all_monitorsforjs/0, get_conprop/2]).

-export([disable/4,enable/1,disable/2,enable/2,refresh/1,getStatePropertyObjects/1,browse/2,get_all_ids/0,get_monitorids_by_ip/1,get_monitors_by_ip/1,get_monitors_by_node/2]).

-export([get_stat/0,get_running/0,get_recent/0,get_log/1,get_log/2,get_log/5,query_log/5,query_log/7]).

-export([disable_alert/3,disable_alert/4,enable_alert/1,getAvailableInstances/1]).

-export([getclassifiers/2,runMonitor/1]).

-include("monitor.hrl").

%% @spec create(ParentId,MonitorData)-> ({ok,Result}|{error,Reason})
%% where
%%	ParentId = atom()
%%	MonitorData = [{Key,Value}]
%%	Key = atom()
%%	Value = term()
%%	Result = [{atom(),term()}]
%%	Reason = (parameter_error | parent_not_exist | start_monitor_error | save_monitor_error |id_generate_error | {verify_error,Err})
%%	Err = [{FieldName,ErrDesc}]
%%	FieldName = atom()
%%	ErrDesc = string()
%% @doc create a monitor with parent id and list of data
%%	<br>MonitorData is a key-value tuple list,it must contains monitor's property,name of property can get by api_monitor_template:get_template/1.</br>  
%%	<br>example:api_monitor_template:get_template(ping_monitor).</br>
%% 	<br>you can call api_monitor_template:get_templates/0 to get monitor types</br>
%%  <br>error reason:</br>
%%	<dl>
%%		<dt>id_generate_error</dt><dd>can not get id of the monitor.</dd>
%%		<dt>parent_not_exist</dt><dd>parent group not exist </dd>
%%		<dt>parameter_error</dt><dd>input parameter is error</dd>
%%		<dt>start_monitor_error</dt><dd>failed to start monitor</dd>
%%		<dt>save_monitor_error</dt><dd>failed to save monitor data.</dd>
%%		<dt>{verify_error,Err}</dt><dd>verify data error, MonitorData contains invalid data,Err is a list of tuple contains {key,error_string}</dd>
%%	</dl>
create('',_)->{error,parameter_error};
create(ParentId,MonitorData) when is_atom(ParentId),is_list(MonitorData)->
	create(api_group:find_group(ParentId),ParentId,MonitorData);
create(_,_)->
	{error,parameter_error}.

create([],_,_)->{error,parent_not_exist};
create([G|_],ParentId,MonitorData)->
	Class = proplists:get_value(?CLASS,MonitorData),
	M = Class:new(),
	M:add_properties(MonitorData),
	Cp = M:getCostInLicensePoints(),
	case lc_util:wouldExceedLimit(Cp) of
		true ->
			M:delete(),
			{error,license_limited};
		_->
			case dbcs_group:get_next_id(ParentId) of
				{ok,NextId}->
					NewData = case lists:keysearch(parent,1,MonitorData) of
								  {value,_}->
									  MonitorData;
								  _->
									  MonitorData ++ [{parent,ParentId}]
							  end,
					
					NewData2 = case proplists:get_value(?NAME,NewData) of
								   ""->
									   Title = M:defaultTitle(NewData),
									   
									   lists:keyreplace(?NAME,1,NewData,{?NAME,Title});
								   OldName->
										NewData
									   % ClassName = api_monitor_template:get_template_name(Class),
									   % case regexp:match(OldName,"^" ++ ClassName) of
										   % {match,_,_}->
											   % Title = M:defaultTitle(NewData),
											   % lists:keyreplace(?NAME,1,NewData,{?NAME,Title});
										   % _->
											   % NewData
									   % end
							   end,
					case M:verify(NewData2) of
						{ok,_}->
							M:delete(),
							% io:format("api_monitor:create,~p~n",[get(hostname)]),
							Ret = dbcs_monitor:create_monitor([{id,NextId}] ++ proplists:delete(id,NewData2)),
							%%G:reload(),
							case Ret of
								{ok,_}->
									case siteview:create_object_by_id(NextId) of
										{error,Err}->
											dbcs_monitor:remove_monitor(NextId),
											{error,create_monitor_error};
										Obj->
											% add proxy mapping
											case proplists:get_value('_proxy',MonitorData,"default") of
												"default"->
													pass;
												Proxy->
													api_monitor_proxy:add_proxy_mapping(NextId,list_to_atom(Proxy))
											end,
											G:add_child(Obj),
											Obj:set_parent(G),
											% create index
											%index_store:update(monitor,[{id,NextId}] ++ NewData2),
											case Obj:startMonitor(Obj) of
												{ok,_}->
													Ret;
												Else->
													G:remove_child(Obj),
													Obj:delete(),
													dbcs_monitor:remove_monitor(NextId),
													%% delete proxy mapping
													case api_monitor_proxy:get_mapping_by_monitor(NextId) of
														{ok,Pm}->
															[api_monitor_proxy:remove_proxy_mapping(X)||X<-Pm];
														_->
															ok
													end,
													% io:format("create monitor:~p~n",[Else]),
													{error,start_monitor_error}
											end
									end;
								{error,Err}->
									{error,save_monitor_error}
							end;
						{error,Err}->
							M:delete(),
							{error,{verify_error,Err}}
					end;
				_->
					M:delete(),
					{error,id_generate_error}
			end
	end.

%% @spec update(MonitorData)->({ok,Result}|{error,Reason})
%% where
%%	MonitorData = [{key,value}]
%%	Result = atom()
%%  Reason = (parameter_error | monitordata_error | not_found_parent | {verify_error,Err})
%%	Err = [{FieldName,ErrDesc}]
%%	FieldName = atom()
%%	ErrDesc = string()
%% @doc update monitor, MonitorData is a key-value tuple list
%%  <br>error reason:</br>
%%	<dl>
%%		<dt>not_found_parent</dt><dd>parent group not exist </dd>
%%		<dt>parameter_error</dt><dd>input parameter is error</dd>
%%		<dt>monitordata_error</dt><dd>MonitorData is invalid,maybe not contains id</dd>
%%		<dt>save_monitor_error</dt><dd>failed to save monitor data.</dd>
%%		<dt>{verify_error,Err}</dt><dd>data verify error, GroupData contains invalid data,Err is a list of tuple contains {key,error_string}</dd>
%%	</dl>
update(MonitorData) when is_list(MonitorData)->
	% io:format("monitor update:~p~n",[MonitorData]),
	case lists:keysearch(?ID,1,MonitorData) of
		{value,{?ID,Id}}->
			io:format("monitor update:~p~n",[Id]),
			ParentId = api_siteview:get_parent_id(Id),
			Ret = api_siteview:find_object(ParentId),
			Class = proplists:get_value(?CLASS,MonitorData),
			M = Class:new(),
            %Editing monitor whether the overflow check lincence
            LicenceLimit = checkLicenceLimit(M,MonitorData,Id),
            %
            if
                LicenceLimit ->
                    M:delete(),
                    {error,license_limited};
                true ->
                    if
                        length(Ret) > 0 ->
                            NewData2 = case proplists:get_value(?NAME,MonitorData) of
                                    ""->
                                        Title = M:defaultTitle(MonitorData),
                                        
                                        lists:keyreplace(?NAME,1,MonitorData,{?NAME,Title});
                                    OldName->
										MonitorData
                                        % ClassName = api_monitor_template:get_template_name(Class),
                                        % case regexp:match(OldName,"^" ++ ClassName) of
                                            % {match,_,_}->
                                                % Title = M:defaultTitle(MonitorData),
                                                % lists:keyreplace(?NAME,1,MonitorData,{?NAME,Title});
                                            % _->
                                                % MonitorData
                                        % end
                                end,
                            case M:verify(NewData2) of
                                {ok,_}->
                                    M:delete(),
                                    case dbcs_monitor:update_monitor(NewData2) of
                                        {error,_}->
                                            {error,[{error,"update monitor error"}]};
                                        _->
											% update proxy mapping
											case proplists:get_value('_proxy',NewData2,"default") of
												"default"->
													case api_monitor_proxy:get_mapping_by_monitor(Id) of
														{ok,Pm}->
															[api_monitor_proxy:remove_proxy_mapping(X)||X<-Pm];
														_->
															ok
													end;
												Proxy->
													case api_monitor_proxy:get_mapping_by_monitor(Id) of
														{ok,Pm}->
															[api_monitor_proxy:remove_proxy_mapping(X)||X<-Pm];
														_->
															ok
													end,
													api_monitor_proxy:add_proxy_mapping(Id,list_to_atom(Proxy))
											end,
											% create index
											%index_store:update(monitor,NewData2),
											
                                            MM = dbcs_monitor:get_monitor(Id),
                                            R = api_siteview:find_object(Id),
                                            [X:stopMonitor(X)||X<-R],
                                            [X:init(X,MM)||X<-R],
                                            [X:startMonitor(X)||X<-R],
											[refresh(Id)||X<-R],
                                            %%[X:reload()||X<-Ret],
                                            {ok,update_monitor_ok}
                                    end;
                                {error,Err}->
                                    M:delete(),
                                    {error,{verify_error,Err}}
                            end;
                        true->
                            M:delete(),
                            {error,not_found_parent}
                    end
            end;
		_->
			{error,monitordata_error}
	end;
update(_)->
	{error,parameter_error}.


checkLicenceLimit(M,MonitorData,Id) ->
    %io:format("monitor data :~p~n",[MonitorData]),
    M:add_properties(lists:keyreplace(id,1,MonitorData,{id,undefined})),    %If the id will be deleted with this instance will affect the current lincence
    Ret = api_siteview:find_object(Id),
    if
        length(Ret)>0 ->
            OriginalMonitor = hd(Ret),
            OriginalCostPoints = OriginalMonitor:getCostInLicensePoints(),
            NewCostPoints = M:getCostInLicensePoints(),
            io:format("point :~p,~p~n",[OriginalCostPoints,NewCostPoints]),
            if
                NewCostPoints>OriginalCostPoints ->
                    lc_util:wouldExceedLimit(NewCostPoints - OriginalCostPoints);
                true ->
                    false
            end;
        true ->
            false
    end.

%% @spec delete(MonitorId)->({error,Reason} | {ok,Result})
%% where
%%	MonitorId = atom()
%%	Reason = monitor_is_running | parameter_error | monitor_not_found
%%	Result = atom()
%% @doc delete monitor by monitor id
%%
%%  <br>error reason:</br>
%%	<dl>
%%		<dt>monitor_not_found</dt><dd>monitor not exist </dd>
%%		<dt>parameter_error</dt><dd>input parameter is error</dd>
%%		<dt>monitor_is_running</dt><dd>monitor can not delete because it is running</dd>
%%	</dl>


delete(MonitorId) when is_atom(MonitorId)->
        Monitors = get_all_monitors(),
	Groups=api_siteview:getAllGroups(),
	Ids_Monitors = [element(2,{_Name,Monitorid})||{_Name,Monitorid}<-Monitors],
	Ids_Groups= [element(2,{_GroupName,Groupid})||{_GroupName,Groupid}<-lists:sublist(Groups,2,length(Groups))],
	F = fun(Monitorid) ->
		Monitor_info = info(list_to_atom(Monitorid)),
		Monitor_depends_on = proplists:get_value(depends_on,Monitor_info),
		%io:format("Depends_on:~p~n",[Monitor_depends_on]),
		lists:member(atom_to_list(MonitorId),[Monitor_depends_on])
	end,
	X = fun(Groupid) ->
		Group_info = api_group:info(Groupid),
		Group_Depends_on = proplists:get_value(depends_on,Group_info),
		%io:format("Depends_on:~p~n",[Depends_on]),
		lists:member(atom_to_list(MonitorId),[Group_Depends_on])
	end,
	case lists:any(F,Ids_Monitors) or lists:any(X,Ids_Groups) of 
		true ->
			%io:format(" it have Depends_on");
			{error,monitor_has_been_depended};
		false ->
			%io:format(" it have not Depends_on")
			delete(MonitorId,api_siteview:find_object(MonitorId))
	end;
delete(_)->{error,parameter_error}.

delete(MonitorId,[])->
	dbcs_monitor:remove_monitor(MonitorId),
	{error,monitor_not_found};
delete(MonitorId,[Monitor|_])->
		% io:format("host:~p~n",[get(hostname)]),
		case Monitor:unschedule(Monitor) of
			{ok,_}->
				{ok,{parent,Parent}} = Monitor:get_parent(),
				Monitor:stopMonitor(Monitor),
				Parent:remove_child(Monitor),
				io:format("host:~p~n",[get(hostname)]),
				Ret = dbcs_monitor:remove_monitor(MonitorId),
				%dbcs_baseline:remove_baseline(MonitorId),
				% Parent:reload(),
				% remove index
				index_store:remove(monitor,MonitorId),
				Monitor:delete(),
				%% delete proxy mapping
				case api_monitor_proxy:get_mapping_by_monitor(MonitorId) of
					{ok,Pm}->
						[api_monitor_proxy:remove_proxy_mapping(X)||X<-Pm];
					_->
						ok
				end,
				Ret;
			{error,is_running}->
				{error,monitor_is_running};
			{error,Err}->
				{error,Err};
			Else->
				Else
		end.

%% @spec move(MonitorId,ParentId)->({ok,Result}|{error,Reason})
%% where 
%%	MonitorId = atom()
%%	ParentId = atom()
%%	Result = atom()
%%	Reason = parameter_error | not_found_monitor | not_found_parent
%% @doc move monitor to a group
%%  <br>error reason:</br>
%%	<dl>
%%		<dt>not_found_monitor</dt><dd>monitor not exist </dd>
%%		<dt>not_found_parent</dt><dd>target group not exist </dd>
%%		<dt>parameter_error</dt><dd>input parameter is error</dd>
%%	</dl>
move(MonitorId,ParentId) when is_atom(MonitorId),is_atom(ParentId)->
	move(MonitorId,ParentId,api_siteview:find_object(MonitorId));
move(_,_)->{error,parameter_error}.
	

move(_,_,[])->{error,not_found_monitor};
move(MonitorId,ParentId,[M|_])->
	move(MonitorId,ParentId,M,api_group:find_group(ParentId)).

move(_,_,_,[])->{error,not_found_parent};
move(_,ParentId,M,[_|_])->
	{ok,{parent,OldParent}} = M:get_owner(),
	M:set_property(parent,ParentId),
	dbcs_monitor:update_monitor(M:get_properties()),
	%%io:format("~p~n~p~n",[OldParent,G]),
	OldParent:reload(),
	case api_group:find_group(ParentId) of
		[G|_]->
			G:reload();
		_->
			io:format("not found parent after reload~n")
	end,
	{ok,move_monitor_ok}.

%% @spec copy(MonitorId,ParentId)->({ok,Result}|{error,Reason})
%% where 
%%	MonitorId = atom()
%%	ParentId = atom()
%%	Result = atom()
%%	Reason = (create_monitor_error | not_found_parent | parameter_error)
%% @doc copy monitor to a group
%%  <br>error reason:</br>
%%	<dl>
%%		<dt>create_monitor_error</dt><dd>failed to create monitor data </dd>
%%		<dt>not_found_parent</dt><dd>target group not exist </dd>
%%		<dt>parameter_error</dt><dd>input parameter is error</dd>
%%	</dl>
copy(MonitorId,ParentId) when is_atom(MonitorId),is_atom(ParentId)->
	copy(MonitorId,ParentId,api_siteview:find_object(MonitorId));
copy(_,_)->{error,parameter_error}.

copy(_,_,[])->{error,not_found_monitor};
copy(MonitorId,ParentId,[M|_])->
	copy(MonitorId,ParentId,M,api_group:find_group(ParentId)).

copy(_,_,_,[])->{error,not_found_parent};
copy(_,ParentId,M,[G|_])->
	Cp = M:getCostInLicensePoints(),
	case lc_util:wouldExceedLimit(Cp) of
		true ->
			% M:delete(),
			{error,license_limited};
		_->
			D = M:get_properties(),
			N = lists:keydelete(parent,1,D),
			NN = lists:keydelete(id,1,N),
			case create(ParentId,NN) of
				{ok,Data}->
					{ok,Data};
				_->
					{error,create_monitor_error}
			end
	end.
	
%% @spec info(MonitorId)->({ok,Result}|{error,Reason})
%% where
%%	MonitorId = atom()
%%	Result = [{key,value}]
%%	Reason = (monitor_not_found | parameter_error)
%% @doc information of a monitor
%%  <br>error reason:</br>
%%	<dl>
%%		<dt>monitor_not_found</dt><dd>monitor not exist </dd>
%%		<dt>parameter_error</dt><dd>input parameter is error</dd>
%%	</dl>
info(MonitorId) when is_atom(MonitorId)->
	info(MonitorId,api_siteview:find_object(MonitorId));
info(_)->{error,parameter_error}.

info(_,[])->{error,monitor_not_found};
info(MonitorId,[Monitor|_])->
	%%dbcs_monitor:get_monitor(MonitorId).
	Monitor:get_properties().

%% @spec get_run_info(MonitorId)->({ok,Result}|{error,Reason})
%% where
%%	MonitorId = atom()
%%	Result = [{key,value}]
%%	Reason = (monitor_not_found | parameter_error)
%% @doc runtime information of a monitor
%%  <br>error reason:</br>
%%	<dl>
%%		<dt>monitor_not_found</dt><dd>monitor not exist </dd>
%%		<dt>parameter_error</dt><dd>input parameter is error</dd>
%%	</dl>
get_run_info(MonitorId)when is_atom(MonitorId)->
	get_run_info(MonitorId,api_siteview:find_object(MonitorId));
get_run_info(_)->{error,parameter_error}.


get_run_info(_,[])->{error,monitor_not_found};
get_run_info(MonitorId,[Monitor|_])->
	[{id,MonitorId}] ++ Monitor:get_run_info().

%% @spec get_classifier(Id,Category)->([Classfier]|{error,Reason})
%% where
%%	Id = atom()
%%	Category = (error|warning|good)
%%	Classfier = [{Prop,Oper,Value}]
%%	Prop = atom() | string()
%%	Oper = atom()
%%	Value = term()
%%	Reason = not_found_monitor
%% @doc get classifier of a monitor
get_classifier(Id,Category)->
	get_classifier(Id,Category,api_siteview:find_object(Id)).

get_classifier(_,Category,[Monitor|_])->
	Monitor:get_classifier(Category);
get_classifier(_,_,[])->{error,not_found_monitor}.

%% @spec get_all_classifier(Id)->([{Key,Classfier}]|{error,Reason})
%% where
%%	Id = atom()
%%	Key = (error | warning | good)
%%	Classfier = [{Prop,Oper,Value}]
%%	Prop = atom() | string()
%%	Oper = atom()
%%	Value = term()
%% @doc get classifier of a monitor
get_all_classifier(Id)->
	get_all_classifier(Id,api_siteview:find_object(Id)).

get_all_classifier(_,[Monitor|_])->
	[{error,Monitor:get_classifier(error)},{warning,Monitor:get_classifier(warning)},{good,Monitor:get_classifier(good)}];
get_all_classifier(_,[])->[].

%% @spec disable(MonitorId,DisableDesc)->({error,Reason} | {ok,Result})
%% where
%%		MonitorId = atom()
%%		DisableDesc = string()
%%		Result = atom()
%%		Reason = (not_found_monitor | save_monitor_error | parameter_error)
%% @doc disable monitor
%%  <br>error reason:</br>
%%	<dl>
%%		<dt>not_found_monitor</dt><dd>monitor not exist </dd>
%%		<dt>parameter_error</dt><dd>input parameter is error</dd>
%%		<dt>save_monitor_error</dt><dd>failed to save monitor data</dd>
%%	</dl>
disable(MonitorId,DisableDesc) when is_atom(MonitorId)->
	disable(MonitorId,DisableDesc,api_siteview:find_object(MonitorId));
disable(_,_)->{error,parameter_error}.

%% @spec disable(MonitorId,DisableDesc,StartTime,EndTime)->({error,Reason} | {ok,Result})
%% where
%%		MonitorId = atom()
%%		DisableDesc = string()
%%		StartTime = tuple()
%%		EndTime = tuple()
%%		Reason = (not_found_monitor | save_monitor_error | parameter_error)
%% @doc disable monitor within a periodic time,the format of StartTime and EndTime is {{Y,M,D},{HH,MM,SS}} 
%%	<br>(example:{{2009,11,9},{12,11,23}})</br>
%%  <br>error reason:</br>
%%	<dl>
%%		<dt>not_found_monitor</dt><dd>monitor not exist </dd>
%%		<dt>parameter_error</dt><dd>input parameter is error</dd>
%%		<dt>save_monitor_error</dt><dd>failed to save monitor data</dd>
%%	</dl>
disable(MonitorId,DisableDesc,{{Y,M,D},{HH,MM,SS}},{{Y2,M2,D2},{HH2,MM2,SS2}})->
	disable(MonitorId,DisableDesc,{{Y,M,D},{HH,MM,SS}},{{Y2,M2,D2},{HH2,MM2,SS2}},api_siteview:find_object(MonitorId)).

disable(_,_,[])->{error,not_found_monitor};
disable(_,DisableDesc,[Monitor|_])->
	Monitor:set_property(?DISABLED,true),
	Monitor:set_property(?DISABLED_DESCRIPTION,DisableDesc),
	case Monitor:save_monitor() of
		{ok,_}->
			Monitor:runUpdate(Monitor,false,false),
			{ok,disable_monitor_ok};
		_->
			{error,save_monitor_error}
	end;
disable(_,_,_)->{error,parameter_error}.

disable(_,_,_,_,[])->{error,not_found_monitor};
disable(_,DisableDesc,{{Y,M,D},{HH,MM,SS}},{{Y2,M2,D2},{HH2,MM2,SS2}},[Monitor|_])->
	Monitor:set_property(?TIMED_DISABLE,{{{Y,M,D},{HH,MM,SS}},{{Y2,M2,D2},{HH2,MM2,SS2}}}),
	Monitor:set_property(?DISABLED_DESCRIPTION,DisableDesc),
	Monitor:set_property(?DISABLED,false),
	case Monitor:save_monitor() of
		{ok,_}->
			Monitor:runUpdate(Monitor,false,false),
			{ok,disable_monitor_ok};
		Err->
			Err
	end;
disable(_,_,_,_,_)->{error,parameter_error}.


%% @spec enable(MonitorId)->{error,Reason} | {ok,Result}
%% where
%%	MonitorId = atom()
%%	Reason = (parameter_error | not_found_monitor | save_monitor_error)
%%	Result = atom()
%% @doc enable a monitor
%%  <br>error reason:</br>
%%	<dl>
%%		<dt>not_found_monitor</dt><dd>monitor not exist </dd>
%%		<dt>parameter_error</dt><dd>input parameter is error</dd>
%%		<dt>save_monitor_error</dt><dd>failed to save monitor data</dd>
%%	</dl>
enable(MonitorId) when is_atom(MonitorId)->
	enable(MonitorId,api_siteview:find_object(MonitorId),false);
enable(_) -> {error,parameter_error}.

%% enable(MonitorId,Flag)->{error,Reason} | {ok,Result}
%% where
%%	MonitorId  = atom()
%%	Reason = (parameter_error | not_found_monitor | save_monitor_error)
%%	Result = atom()
%%	Flag=(ture|false) 
%% @doc enable monitor. if Flag is true,Only enable items that are temporarily disabled 
%%  <br>error reason:</br>
%%	<dl>
%%		<dt>not_found_monitor</dt><dd>monitor not exist </dd>
%%		<dt>parameter_error</dt><dd>input parameter is error</dd>
%%		<dt>save_monitor_error</dt><dd>failed to save monitor data</dd>
%%	</dl>
enable(MonitorId,Flag) when is_atom(MonitorId)->
	enable(MonitorId,api_siteview:find_object(MonitorId),Flag);
enable(_,_) -> {error,parameter_error}.


enable(_,[],_)->{error,not_found_monitor};
enable(_,[Monitor|_],false)->
	Monitor:set_property(?DISABLED_DESCRIPTION,""),
	Monitor:set_property(?TIMED_DISABLE,undefined),
	Monitor:set_property(?DISABLED,false),
	case Monitor:save_monitor() of
		{ok,_}->
			Monitor:runUpdate(Monitor,false,false),
			{ok,enable_monitor_ok};
		_->
			{error,save_monitor_error}
	end;
enable(_,[Monitor|_],true)->
	Monitor:set_property(?TIMED_DISABLE,undefined),
	case Monitor:save_monitor() of
		{ok,_}->
			Monitor:runUpdate(Monitor,false,false),
			{ok,enable_monitor_ok};
		_->
			{error,save_monitor_error}
	end.

%% @spec refresh(Id)->true | false
%% where
%%	Id = atom()
%% @doc refresh a monitor
refresh(Id) when is_atom(Id)->
	refreshObj(api_siteview:find_object(Id),Id);
refresh(_)->
	false.

refreshObj([M|_],_)->
	case M:isRunning() of
		true->
			true;
		_->
			M:runUpdate(M,false,false),
			true
	end;
refreshObj(_,_)->
	false.

%% @spec getBrowseData(Oper,Params)-> Result
%% where
%%		Oper = string()
%%		Params = [{Key,Value}]
%%		Key = atom()
%%		Value = term()
%%		Result = [{Id,Title}]
%%		Id = string()
%%		Title = string()
%% @doc get data of  browsable property ,Oper is the operation status when call this function,when add monitor Oper will be "add",edit monitor is "edit"
getBrowseData(Oper,Params)->
%% 	io:format("************Params33*********~p~n",[Params]),
%% 	io:format("******value **********~p~n", [proplists:get_value('_proxy',Params,"default")]),
	case proplists:get_value('_proxy',Params,"default") of
		"default"->
			io:format("******default**********~n"),

			case Oper of
				"add"->
					% io:format("getBrowseData:~p~n",[Params]),
					Class = proplists:get_value(class,Params),
					Key = list_to_atom(Class),
					M = Key:new(),
					try
					M:set_property(?PAGE_PARAMS,Params),
					Ret = M:getBrowseData(Params),
					Ret
					catch
						_:Err->
							{error,Err}
					after
						M:delete()
					end;
				"edit"->
					Id = proplists:get_value(id,Params),
					case api_siteview:find_object(list_to_atom(Id)) of
						[]->
							[];
						[M2|_]->
							M2:getBrowseData(Params)
					end;
				_->
					[]
			end;
		Proxy->
%% 			io:format("************Proxy*********~p~n",[Proxy]),
			rpc:call(list_to_atom(Proxy), remoteMachineConfig, start_link, []),
			case Oper of
				"add"->
%% 					io:format("************Proxy1*********~p~n",[Proxy]),
					[T|Param]=Params,
					io:format("*******!!!*Param>>**!!*****~p~n", [Param]),
%% 					[Para]=Param,
					case T of
						"add" ->
%% 							io:format("************add*************"),
							Ret = rpc:call(list_to_atom(Proxy),monitor_runner,getBrowseData,["add",zlib:zip(term_to_binary(Params)),zlib:zip(term_to_binary({}))]);
						_->	
%% 							io:format("************!!!!add*************"),
							Ret = rpc:call(list_to_atom(Proxy),monitor_runner,getBrowseData,["add",zlib:zip(term_to_binary(Params)),zlib:zip(term_to_binary({}))])		
					end,							

%% 					io:format("*****Ret:~p~n********",[Ret]),
					binary_to_term(zlib:unzip(Ret));
				"edit"->
					io:format("************Proxy2edit*********~p~n",[Proxy]),
					Id = proplists:get_value(id,Params),
					case api_siteview:find_object(list_to_atom(Id)) of
						[]->
							[];
						[M2|_]->
							Data = zlib:zip(term_to_binary({M2:get_properties(),M2:get_attributes()})),
							Ret = rpc:call(list_to_atom(Proxy),monitor_runner,getBrowseData,["edit",zlib:zip(term_to_binary(Params)),Data]),
							binary_to_term(zlib:unzip(Ret))
					end;
				_->
					[]
			end
	    end.

get_conprop(Oper,Params)->
	case Oper of
		"add"->
%% 			io:format("get_ConProp1:~p~n",[Params]),
			Class = proplists:get_value(class,Params),
			Key = list_to_atom(Class),			
			M = Key:new(),
			try
			M:set_property(?PAGE_PARAMS,Params),			
			Ret = M:get_conprop(),
%% 			io:format("get_ConProp2:~p~n",[Ret]),
			Ret
			catch
				_:Err->
					{error,Err}
			after
				M:delete()
			end;
		"edit"->
			Id = proplists:get_value(id,Params),
			case api_siteview:find_object(list_to_atom(Id)) of
				[]->
					[];
				[M2|_]->
					M2:get_conprop(Params)
			end;
		_->
			[]
	end.
  
%% @spec get_all_monitors()->[Monitor]
%% where
%%	Monitor = [{Key,Value}]
%%	Key = atom()
%%	Value = term()
%% @doc get all monitors,return a list of Monitor,each Monitor is a key-value tuple list
%%
get_all_monitors()->
	% SV = siteview:get_current_siteview(),
	% Monitors =SV:getMonitors(),
	Monitors = siteview:get_object_by_type(monitor),
	% F = fun(X)->
	%		{ok,{id,Id}} = X:get_property(id),
	%		{X:get_full_name(),atom_to_list(Id)}
	
	% end,
	% lists:map(F,Monitors)
	get_all_monitors(Monitors).
	
get_all_monitors([])->[];
get_all_monitors([M|T])->
	case M:get_property(id) of
		{ok,{id,Id}}->
			[{M:get_full_name(),atom_to_list(Id)}] ++ get_all_monitors(T);
		Err->
			?ERROR_LOG2("monitor get id error:~p,object:~p~n",[Err,M]),
			get_all_monitors(T)
	end.


%% @spec get_all_monitorsforjs()->[Monitor]
%% where
%%	Monitor = [{Key,Value}]
%%	Key = atom()
%%	Value = term()
%% @doc get all monitors,return a list of Monitor,each Monitor is a key-value tuple list
%%
get_all_monitorsforjs()->
	% SV = siteview:get_current_siteview(),
	% Monitors =SV:getMonitors(),
	Monitors = siteview:get_object_by_type(monitor),
	% F = fun(X)->
	%		{ok,{id,Id}} = X:get_property(id),
	%		{X:get_full_name(),atom_to_list(Id)}
	
	% end,
	% lists:map(F,Monitors)
	get_all_monitorsforjs(Monitors).
	
get_all_monitorsforjs([])->[];
get_all_monitorsforjs([M|T])->
	case M:get_property(id) of
		{ok,{id,Id}}->
			{_,{_,Type}}=M:get_property(class),
			{_,{_,Status}}=M:get_property(class),
			[{obj, [{"id", atom_to_list(Id)}, {"name", M:get_full_name()},{"type", atom_to_list(Type)}, {"statu", atom_to_list(Status)}]}] ++ get_all_monitorsforjs(T);
		Err->
			?ERROR_LOG2("monitor get id error:~p,object:~p~n",[Err,M]),
			get_all_monitorsforjs(T)
	end.

get_all_ids()->
	Monitors = siteview:get_object_by_type(monitor),
	get_all_ids(Monitors).
	
get_all_ids([])->[];
get_all_ids([M|T])->
	case M:get_property(id) of
		{ok,{id,Id}}->
			[Id] ++ get_all_ids(T);
		Err->
			?ERROR_LOG2("monitor get id error:~p~n",[Err]),
			get_all_ids(T)
	end.
	
%% @spec getStatePropertyObjects(Id) -> ({ok,[{Prop,Value}]} | {error,not_found_monitor})
%% where
%%	Id = atom()
%%	Prop = atom()
%%	Value = term()
%% @doc get monitor's state property and values,Id is monitor identify,Prop is state property,value is monitor value
%%
getStatePropertyObjects(Id) ->
	getStatePropertyObjects(Id,api_siteview:find_object(Id)).
	
getStatePropertyObjects(_,[])->{error,not_found_monitor};
getStatePropertyObjects(_,[O|_])->
	{ok,O:getStatePropertyObjects(O)}.
	
	
%% @spec browse(Type,Params)->Monitors
%% where
%%	Type = atom()
%%	Params = [{FilterKey,FilterValue}]
%%	Monitors = [Monitor]
%%	Monitor = [{Key,Val}]
%%	Key = atom()
%%	Val = term()
%%	FilterKey = string()
%%	FilterValue = string()
%% @doc filter all monitor with conditions
%%	<br>Type is 'all_type' or monitor type(example:'ping_monitor')</br>
%%	<br>FilterKey = ("filter_category" | "match_status" | "match_name" | "match_machine" | "match_id" | "match_group_name" | "match_group_ID")</br>
%%	<br>if FilterKey = "filter_category","FilterValue" can be:</br>
%%	<dl>
%%		<dt>"show_error_or_warning"</dt><dd></dd>
%%		<dt>"show_error"</dt><dd></dd>
%%		<dt>"show_warning"</dt><dd></dd>
%%		<dt>"show_ok"</dt><dd></dd>
%%		<dt>"show_nodata"</dt><dd></dd>
%%		<dt>"show_disabled"</dt><dd></dd>
%%		<dt>"hide_error_or_warning"</dt><dd></dd>
%%		<dt>"hide_error"</dt><dd></dd>
%%		<dt>"hide_warning"</dt><dd></dd>
%%		<dt>"hide_ok"</dt><dd></dd>
%%		<dt>"hide_nodata"</dt><dd></dd>
%%		<dt>"hide_disabled"</dt><dd></dd>
%%	</dl>
browse(Type,Params)->
	monitor_browse:browse(Type,Params).
	
%% @spec get_stat()->({ok,StatData} | undefined)
%% where
%%	StatData = {RunningCount,CurMinutesCount,MaxRunningCount,MaxRunningTime,MaxMinutesRun,MaxMinutesRunTime}
%%	RunningCount = integer()
%%	CurMinutesCount = integer()
%%	MaxRunningCount = integer()
%%	MaxRunningTime = integer()
%%	MaxMinutesRun = integer()
%%	MaxMinutesRunTime = integer()
%% @doc get stat data of monitor
%%	<br>RunningCount is count of running monitor</br>
%%	<br>CurMinutesCount is count of monitor run at current 1 minute</br>
%%	<br>MaxRunningCount is max value of RunningCount </br>
%%	<br>MaxRunningTime is time of MaxRunningCount Occur</br>
%%	<br>MaxMinutesRun is max value of CurMinutesCount</br>
%%	<br>MaxMinutesRunTime is time of MaxMinutesRun Occur</br>
%%	<br>MaxRunningTime and MaxMinutesRunTime is the elapsed time since 00:00 GMT, January 1, 1970 (zero hour)</br>
%%	<br>Time unit is Millisecond</br>
get_stat()->
	case siteview:get_monitor_scheduler() of
		{ok,S}->
			S:get_stat();
		_->
			undefined
	end.
	
%% @spec get_running()->({ok,RunData} | undefined)
%% where
%%	RunData = [{Time,{App,Id}}]
%%	Time = string()
%%	App = atom()
%%	Id = atom()
%% @doc get Running monitor
get_running()->
	case siteview:get_monitor_scheduler() of
		{ok,S}->
			case S:get_running() of
				{ok,Data}->
					App = dbcs_base:get_app(),
					F = fun(X)->
						case X of
							{_,{App,_}}->
								true;
							_->
								false
						end
					end,
					{ok,lists:filter(F,Data)};
				Else    ->
					Else
			end;
		_->
			undefined
	end.

%% @spec get_recent()->({ok,RecentData} | {error,Err})
%% where
%%	RunData = [{App,#monitorlog{}}]
%%	App = atom()
%% @doc get recent running monitor	
%%	<br>#monitorlog{} is defined in <a href="monitor.hrl">monitor.hrl</a></br>
get_recent()->
	SV = siteview:get_current_siteview(),
	case SV:get_attribute(recent_monitor) of
		{ok,{_,Recent}}->
			App = dbcs_base:get_app(),
			F = fun(X)->
				case X of
					{App,_}->
						true;
					_->
						false
				end
			end,
			{ok,lists:filter(F,Recent)};
		Err->
			{error,Err}
	end.
%	case siteview:get_monitor_scheduler() of
%		{ok,S}->
%			S:get_recent();
%		_->
%			undefined
%	end.

%% @spec get_log(Date)->{ok,Log} | {error,Reason}
%% where
%%	Date = {Y,M,D}
%%	Y = integer()
%%	M = integer()
%%	D = integer()
%%	Log = [#monitorlog{}]
%% @doc get monitor log by date
%%	<br>#monitorlog{} is defined in <a href="monitor.hrl">monitor.hrl</a></br>
get_log(Date) when is_tuple(Date) andalso size(Date)==3->
	{Y,M,D} = Date,
	DateStr = case date() of
			Date ->
				"";
			_->
				lists:flatten(lists:flatten(io_lib:format("~w-~w-~w",[Y,M,D])))
		end,
	case monitor_logger:q(DateStr) of
		{error,Reason}->
			{error,Reason};
		{_,Ret}->
			{ok,Ret}
	end;
get_log(Date) when is_list(Date)->
	[Y,M,D] = [list_to_integer(X)||X<-string:tokens(Date,"-")],
	get_log({Y,M,D});
get_log(_)->{error,parameter_error}.

%% @spec get_log(Date,Id)->{ok,Log} | {error,Reason}
%% where
%%	Date = {Y,M,D}
%%	Y = integer()
%%	M = integer()
%%	D = integer()
%%	Id = atom()
%%	Log = [#monitorlog{}]
%%	Reason = parameter_error | string()
%% @doc get monitor log by date and id.
%%	<br>#monitorlog{} is defined in <a href="monitor.hrl">monitor.hrl</a></br>
get_log(Date,Id)when is_tuple(Date) andalso is_atom(Id) andalso size(Date)==3->
	{Y,M,D} = Date,
	DateStr = case date() of
			Date ->
				"";
			_->
				lists:flatten(io_lib:format("~w-~w-~w",[Y,M,D]))
		end,
	case monitor_logger:q(DateStr,Id) of
		{error,Reason}->
			{error,Reason};
		{_,Ret}->
			{ok,Ret}
	end;
get_log(Date,Id)when is_list(Date) andalso is_atom(Id)->
	[Y,M,D] = [list_to_integer(X)||X<-string:tokens(Date,"-")],
	get_log({Y,M,D},Id);
get_log(_,_)->{error,parameter_error}.

%% @spec get_log(Id,StartDate,StartTime,EndDate,EndTime)-> {ok,Log} | {error,Reason}
%% where
%%	Id = atom()
%%	StartDate = {Year,Month,Day}
%%	StartTime = {Houre,Minute,Seconds}
%%	EndDate = {Year,Month,Day}
%%	EndTime = {Houre,Minute,Seconds}
%%	Log = [#monitorlog{}]
%% @doc get monitor log by start time endtime and id
%%	<br>#monitorlog{} is defined in <a href="monitor.hrl">monitor.hrl</a></br>
get_log(Id,StartDate,StartTime,EndDate,EndTime)
	when is_tuple(StartDate) andalso is_tuple(StartTime) andalso is_tuple(EndDate) andalso is_tuple(EndTime)->
	monitor_logger:q(Id,StartDate,StartTime,EndDate,EndTime);
get_log(Id,StartDate,StartTime,EndDate,EndTime)
	when is_list(StartDate) andalso is_list(StartTime) andalso is_list(EndDate) andalso is_list(EndTime)->
	[Y1,M1,D1] = [list_to_integer(X)||X<-string:tokens(StartDate,"-")],
	[Y2,M2,D2] = [list_to_integer(X)||X<-string:tokens(EndDate,"-")],
	[HH1,MM1,SS1] = [list_to_integer(X)||X<-string:tokens(StartTime,":")],
	[HH2,MM2,SS2] = [list_to_integer(X)||X<-string:tokens(EndTime,":")],
	monitor_logger:q(Id,{Y1,M1,D1},{HH1,MM1,SS1},{Y2,M2,D2},{HH2,MM2,SS2});
get_log(_,_,_,_,_)->{error,parameter_error}.


%% @spec disable_alert(Id,DisableDesc,NextSeconds)->({error,Reason} | {ok,Result})
%% where
%%		Id = atom()
%%		DisableDesc = string()
%%		NextSeconds = integer
%%		Result = atom()
%%		Reason = (not_found_monitor | save_monitor_error | parameter_error)
%% @doc disable monitor' alert
%% 
disable_alert(Id,DisableDesc,NextSeconds) when is_atom(Id) andalso is_integer(NextSeconds)->
	case api_siteview:find_object(Id) of
		[]->
			{error,not_found_monitor};
		[Monitor|_]->
			UntilTime = sv_datetime:next_time(NextSeconds),
			Monitor:set_property(?ALERT_DISABLED_DESCRIPTION,DisableDesc),
			Monitor:set_property(?ALERT_DISABLED,{true,{until,UntilTime}}),
			case Monitor:save_monitor() of
				{ok,_}->
					Monitor:runUpdate(Monitor,false,false),
					{ok,disable_monitor_alert_ok};
				_->
					{error,save_monitor_error}
			end
	end;
disable_alert(_,_,_)->{error,parameter_error}.


%% @spec disable_alert(MonitorId,DisableDesc,StartTime,EndTime)->({error,Reason} | {ok,Result})
%% where
%%		MonitorId = atom()
%%		DisableDesc = string()
%%		StartTime = tuple()
%%		EndTime = tuple()
%%		Reason = not_found_monitor | parameter_error | save_monitor_error
%% @doc disable monitor within a periodic time,the format of StartTime and EndTime is {{Y,M,D},{HH,MM,SS}} 
%%	<br>(example:{{2009,11,9},{12,11,23}})</br>
disable_alert(MonitorId,DisableDesc,{{Y,M,D},{HH,MM,SS}},{{Y2,M2,D2},{HH2,MM2,SS2}})->
	disable_alert(MonitorId,DisableDesc,{{Y,M,D},{HH,MM,SS}},{{Y2,M2,D2},{HH2,MM2,SS2}},?BASE_MODULE:find_object(MonitorId)).

disable_alert(_,_,_,_,[])->{error,not_found_monitor};
disable_alert(_,DisableDesc,{{Y,M,D},{HH,MM,SS}},{{Y2,M2,D2},{HH2,MM2,SS2}},[Monitor|_])->
	Monitor:set_property(?ALERT_DISABLED,{true,{period,{{Y,M,D},{HH,MM,SS}},{{Y2,M2,D2},{HH2,MM2,SS2}}}}),
	Monitor:set_property(?ALERT_DISABLED_DESCRIPTION,DisableDesc),
	case Monitor:save_monitor() of
		{ok,_}->
			Monitor:runUpdate(Monitor,false,false),
			{ok,disable_monitor_alert_ok};
		_->
			{error,save_monitor_error}
	end;
disable_alert(_,_,_,_,_)->{error,parameter_error}.



%% enable_alert(MonitorId)->{error,Reason} | {ok,Result}
%% where
%%	MonitorId  = atom()
%%	Reason = save_monitor_error | parameter_error | not_found_monitor
%%	Result = atom()
%% @doc enable monitor's alert.
enable_alert(MonitorId) when is_atom(MonitorId)->
	case api_siteview:find_object(MonitorId) of
		[]->
			{error,not_found_monitor};
		[Monitor|_]->
			Monitor:set_property(?ALERT_DISABLED_DESCRIPTION,""),
			Monitor:set_property(?ALERT_DISABLED,false),
			case Monitor:save_monitor() of
				{ok,_}->
					Monitor:runUpdate(Monitor,false,false),
					{ok,enable_monitor_alert_ok};
				_->
					{error,save_monitor_error}
			end
	end;
enable_alert(_) -> {error,parameter_error}.

%% get_hostname(MonitorId)->{error,Reason} | {ok,Result}
%% where
%%	MonitorId  = atom()
%%	Reason = parameter_error | not_found_monitor
%%	Result = atom()
%% @doc get host name of monitor
get_hostname(MonitorId)when is_atom(MonitorId)->
	case api_siteview:find_object(MonitorId) of
		[]->
			{error,not_found_monitor};
		[Monitor|_]->
			{ok,Monitor:getHostname()}
	end;
get_hostname(_) -> {error,parameter_error}.	

%% change_name(Id,Name)->{error,Reason} | {ok,Result}
%% where
%%	Id  = atom()
%%	Name = string()
%%	Reason = parameter_error | not_found_monitor
%%	Result = term()
%% @doc change name of monitor
change_name(Id,Name) when is_list(Name)->
	case api_siteview:find_object(Id) of
		[]->
			{error,not_found_monitor};
		[Monitor|_]->
			case dbcs_monitor:update_monitor([{id,Id},{name,Name}]) of
				{ok,_}->
					Monitor:set_property(name,Name),
					{ok,Name};
				Else->
					Else
			end
	end;
change_name(_,_)->{error,parameter_error}.
				
%% @spec query_log(StartDate,StartTime,EndDate,EndTime,Params)-> ({eof,Data} | {error,Reason})
%% where
%%	StartDate = {Year,Month,Day}
%% 	StartTime = {Hour,Minute,Seconds}
%%	EndData = {Year,Month,Day}
%% 	EndTime = {Hour,Minute,Seconds}
%%	Params = [{Field,Op,Val}]
%%	Field = id | name | category
%%	Op = '='
%%	Data = [#monitorlog{}]
%%	Reason = atom()
%% @doc query monitor log by Start time , end time and other conditions
%% <br></br>
%% <dl>
%%	<dt>#monitorlog{}</dt><dd>See <a href="monitor.hrl">monitor.hrl</a></dd>
%% </dl>
%%	
query_log(StartDate,StartTime,EndDate,EndTime,Params)  when is_tuple(StartDate) andalso is_tuple(EndDate) andalso is_tuple(StartTime) andalso is_tuple(EndTime)->
	monitor_logger:ql(StartDate,StartTime,EndDate,EndTime,Params);
query_log(_,_,_,_,_)->{error,parameter_error}.

%% @spec query_log(StartDate,StartTime,EndDate,EndTime,Params,From,Count)-> ({eof,Data} | {Left,Data} | {error,Reason})
%% where
%%	StartDate = {Year,Month,Day}
%% 	StartTime = {Hour,Minute,Seconds}
%%	EndData = {Year,Month,Day}
%% 	EndTime = {Hour,Minute,Seconds}
%%	Params = [{Field,Op,Val}]
%%	Field = id | name | category
%%	Op = '='
%%	Data = [#monitorlog{}]
%%	Reason = atom()
%%	From = integer()
%%	Count = integer()
%%	Left = integer()
%% @doc query monitor log by Start time , end time and other conditions
%% <br>return data limit by From and Count. </br>
%% <dl>
%%	<dt>#monitorlog{}</dt><dd>See <a href="monitor.hrl">monitor.hrl</a></dd>
%% </dl>
%%	
query_log(StartDate,StartTime,EndDate,EndTime,Params,From,Count) when is_integer(From) andalso is_integer(Count) andalso From > 0 andalso Count >0->
	case query_log(StartDate,StartTime,EndDate,EndTime,Params) of
		{eof,Data}->
			Len = length(Data),
			if
				From > Len +1 ->
					{eof,[]};
				From + Count > Len->
					{eof,lists:sublist(Data,From,Count)};
				true->
					{Len - From - Count + 1,lists:sublist(Data,From,Count)}
			end;
		Else->
			Else
	end;
query_log(_,_,_,_,_,_,_)->{error,parameter_error}.				
%% mq
getAvailableInstances(Parms)->
Mq=mqstatus_monitor:new(),
R = Mq:outgetAvailableInstances(Parms),
Mq:delete(),
R.

getclassifiers(Ids, Classifier)->
Temids=string:tokens(Ids, ","),
buildclassifier(Temids,[], Classifier).

buildclassifier([],R, Classifier)->
	R;
buildclassifier([H|E],R, Classifier)->
Id=list_to_atom(H),
M1=info(Id),
case M1 of
	{error,_}->
		buildclassifier(E, R, Classifier);
	_->
		Errorclassifier= proplists:get_value(Classifier,  M1,[]),
		buildclassifier(E, R++[{list_to_atom(H),Errorclassifier}], Classifier)
end.

runMonitor(Id)->
	case api_siteview:find_object(Id) of
		[]->
			{error,not_found_monitor};
		[M|_]->
			case M:update() of
				{ok,_}->
					M:runClassifiers(M);
				Else->
					throw(Else)
			end,
			F = fun(X)->
				case M:get_attribute(X) of
					{ok,Val}->
						Val;
					_->
						{X,not_found}
				end
			end,
			Ret = lists:map(F,M:getLogProperties(M)++[category]),
			{ok,Ret}
	end.

%% Return:({ok,Ids}|{error,Reason})	
%% Ids:[Id,Id,...]
get_monitorids_by_ip(Ip) ->
	case is_list(Ip) of
		true ->	
			Dbname = server_conf:get_db_node(),
			case machine:getOS(Ip) of
				1 -> Filter = lists:flatten(["my.machine=\\\\",Ip]);
				_ -> Filter = lists:flatten(["my.machine=",Ip])
			end,
			case db_ecc:get_data(Dbname,"monitor",Filter) of
				[] -> {error,nomonitor};
				Monitors ->
					F = fun(Monitor) ->
						{_,_App,Id,_,_,_,_,_,_,_,_,_,_,_,_Adv} = Monitor,
						Id
					end,
					Ids = lists:map(F,Monitors),
					{ok,Ids}
			end;
		false -> {error,ipwrong}
	end.

%% Return:({ok,Monitors_info}|{error,Reason})	
%% Monitors_info:[{ip,id,monitor_info},{ip,id,monitor_info},...]	
get_monitors_by_ip(all) ->
	Dbname = server_conf:get_db_node(),
	case db_ecc:get_data(Dbname,"machine",[]) of
		[] -> {error,nomachine};
		Machines ->
			F = fun(Machine) ->
				{_,_App,_Id,_,_,_,_,_,_,_,_,_,_,_,Adv} = Machine,
				{host,string,Ip} =  lists:keyfind(host,1,Adv),
				binary_to_list(Ip)
			end,
			Ips = lists:map(F,Machines),
			get_monitors_by_ips(Ips,[])
	end;
	
get_monitors_by_ip(Ip) ->
	case is_list(Ip) of
		true ->	
			Filter_win32 = lists:flatten(["my.machine=\\\\",Ip]),
			Filter_linux = lists:flatten(["my.machine=",Ip," | my.server=",Ip]),
			case get_monitors_by_ip(Ip,Filter_linux) of
				{error,nomonitor} -> get_monitors_by_ip(Ip,Filter_win32);
				{ok,Monitors_info} -> {ok,Monitors_info}
			end;
		false -> {error,invalidip}
	end.
	
get_monitors_by_ip(Ip,Filter) ->
	Dbname = server_conf:get_db_node(),
	case db_ecc:get_data(Dbname,"monitor",Filter) of
		[] -> {error,nomonitor};
		Monitors ->
			F = fun(Monitor) ->
				{_,_App,Id,_,_,_,_,_,_,_,_,_,_,_,_Adv} = Monitor,
				Id
			end,
			Ids = lists:map(F,Monitors),
			Monitors_info = [{Ip,Id,get_run_info(Id)}||Id <- Ids],
			{ok,Monitors_info}
	end.

get_monitors_by_ips([Ip|Tail],Result) ->
	case get_monitors_by_ip(Ip) of
		{error,nomonitor} -> get_monitors_by_ips(Tail,Result++[{Ip,none,nomonitor}]);
		{ok,Monitors_info} -> get_monitors_by_ips(Tail,Result++Monitors_info)
	end;
	
get_monitors_by_ips([],Result) ->
	{ok,Result}.


get_monitors_by_node(Ip,Node) ->
	
	case is_list(Ip) of
		true ->	
			Filter_win32 = lists:flatten(["my.machine=\\\\",Ip]),
			Filter_linux = lists:flatten(["my.machine=",Ip," | my.server=",Ip]),
			case get_monitors_by_ip(Ip,Filter_linux) of
				{error,nomonitor} -> get_monitors_by_ip(Ip,Filter_win32);
				{ok,Monitors_info} ->get_monitor_id(Monitors_info,Node)
			end;
		false -> {error,invalidip}
	end.
	
get_monitor_id([],Node) -> ok;	
get_monitor_id(Monitors_info,Node) ->
	[A|B]=Monitors_info,
	{_,_,[{_,Id},_,_,_,_,_,_]}=A,
	io:format("Monitorid:~p~n",[Id]),
	case Node of
		"default"->
			case api_monitor_proxy:get_mapping_by_monitor(Id) of
				{ok,Pm}->
					io:format("default PM:~p~n",[Pm]),
					[api_monitor_proxy:remove_proxy_mapping(X)||X<-Pm];
				_->
					ok
			end;
		Proxy->
			case api_monitor_proxy:get_mapping_by_monitor(Id) of
				{ok,Pm}->
					io:format("Proxy Proxy:~p~n",[Proxy]),
					[api_monitor_proxy:remove_proxy_mapping(X)||X<-Pm];
				_->
					ok
			end,
			api_monitor_proxy:add_proxy_mapping(Id,list_to_atom(Proxy))
	end,
	get_monitor_id(B,Node).	
	
	