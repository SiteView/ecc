%%
%% @doc device database module
%%
%%
-module(dbcs_device).
-compile(export_all).
-include("dbcs_common.hrl").
-define(Table,"device").

-define(DF_FLAG,df_flag).

base_property(id)->
	true;
base_property(_)->
	false.

%% Device:device 'a parameter，Attr：device parameter's attribute
%%
device_to_db({Device,Attr})->
	{value,{id,Id}} = lists:keysearch(id,1,Device),
	Advance = [dbcs_base:term2db(K,V)||{K,V}<- Device,base_property(K)=:=false],
	case [is_atom(Id)] of
		[true]->
			{content,list_to_atom(?Table), Id, <<"device">>,Attr,null,null,null,?Author,null,null,null,null,null,Advance};
		_->
			{}
	end.

db_to_device(DeviceData)->
	case DeviceData of
		{_,_,Id,_,_,_,_,_,_,_,_,_,_,_,Adv}->
			[{id,Id}] ++ [dbcs_base:db2term(K,T,V)||{K,T,V}<-Adv];
		_->
			[]
	end.

db_to_attribute(DeviceData)->
	case DeviceData of
		{_,_,_,_,null,_,_,_,_,_,_,_,_,_,_}->
			[];
		{_,_,_,_,Attr,_,_,_,_,_,_,_,_,_,_}->
			Attr;
		_->
			[]
	end.

%% @doc insert a new device information into database
%% @spec create_device(Device)->({error,Reason} | {ok,Result})
%% where
%%		Device = list()
%%		Result = tuple()
create_device(Device)when is_list(Device) ->
	NewData = case proplists:get_value(?DF_FLAG,Device) of
							undefined->
								Device ++ [{?DF_FLAG,true}];
							_->
								proplists:delete(?DF_FLAG,Device) ++ [{?DF_FLAG,true}]
						end,
	case proplists:get_value(id,NewData) of
		undefined->
			case db_ecc:insert_data(?DBName,?Table,device_to_db({[{id,dbcs_base:uuid()}] ++ NewData,[]})) of
				{ok,Ret}->
					{ok,db_to_device(Ret)};
				Else->
					Else
			end;
		_->
			case db_ecc:insert_data(?DBName,?Table,device_to_db({NewData,[]})) of
				{ok,Ret}->
					{ok,db_to_device(Ret)};
				Else->
					Else
			end
	end;
create_device(_)->{error,parameter_error}.


%% @doc query a device by id
%% @spec get_device(Id)->({error,Reason} | Device)
%% where
%%		Id = (atom() | string())
%%		Device = tuple() | {DataList,AttributeList}
get_device(Id)when is_atom(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=device & id="++atom_to_list(Id)),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_device};
				_->
					[Device|_] = Ret,
					db_to_device(Device)
			end
	end;
get_device(Id)when is_list(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=device & id="++Id),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_device};
				_->
					[Device|_] = Ret,
					db_to_device(Device)
			end
	end;
get_device(_)->
	{error,id_error}.

get_parameter_attribute(Id) when is_list(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=device & id="++Id),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_device};
				_->
					[Device|_] = Ret,
					db_to_attribute(Device)
			end
	end;
get_parameter_attribute(Id) when is_atom(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=device & id="++ atom_to_list(Id)),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_device};
				_->
					[Device|_] = Ret,
					db_to_attribute(Device)
			end
	end;
get_parameter_attribute(_)->{error,parameter_error}.


get_parameter_values_and_attribute(Id) when is_list(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=device & id="++Id),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_device};
				_->
					[Device|_] = Ret,
					{db_to_device(Device),db_to_attribute(Device)}
			end
	end;
get_parameter_values_and_attribute(Id) when is_atom(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=device & id="++ atom_to_list(Id)),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_device};
				_->
					[Device|_] = Ret,
					{db_to_device(Device),db_to_attribute(Device)}
			end
	end;
get_parameter_values_and_attribute(_)->{error,parameter_error}.

%% @doc update data device information,Device must contain 'id' attribute.
%% @spec update_device(Device)->({error,Reason} | {ok,Result})
%% where
%%		Device = list()
%%		Result = tuple()
update_device(Device)->
	{value,{id,Id}} = lists:keysearch(id,1,Device),
	{OldData,OldAttr} = get_parameter_values_and_attribute(Id),
	ND = lists:ukeymerge(1,lists:ukeysort(1,Device),lists:ukeysort(1,OldData)),
	case db_ecc:update_data(?DBName,?Table,"id=" ++ atom_to_list(Id),device_to_db({ND,OldAttr})) of
		{ok,Ret}->
			{ok,db_to_device(Ret)};
		Else->
			Else
	end.

update_parameter_attribute(Id,Attr)->
	{OldData,OldAttr} = get_parameter_values_and_attribute(Id),
	NA = lists:ukeymerge(1,lists:ukeysort(1,Attr),lists:ukeysort(1,OldAttr)),
	case db_ecc:update_data(?DBName,?Table,"id=" ++ atom_to_list(Id),device_to_db({OldData,NA})) of
		{ok,Ret}->
			{ok,db_to_attribute(Ret)};
		Else->
			Else
	end.

%% @doc query a device with custom conditions
%% @spec  get_device_match(Where)-> (Devices | {error,Reason})
%% where
%%		Where = string()
%%		Devices = list()
get_device_match(Where)->
	Ret = db_ecc:get_data(?DBName,?Table,Where),
	case Ret of
		{error,Reason}->
			{error,Reason};
		_->
			[db_to_device(X)||X <- Ret]
	end.


%% @doc get all data of a device
%% @spec get_all()->({error,Reason} | Devices)
%% where
%%		Devices = list()
get_all()->
	Ret = db_ecc:get_data(?DBName,?Table,""),
	case Ret of
		{error,_}->
			[];
		_->
			[db_to_device(X)||X <- Ret]
	end.

%% @doc remove a device
%% @spec remove_device(Id)->({error,Reason} | {ok,Result})
%% where
%%		Id = (atom() | string())
%%		Result = tuple()
remove_device(Id)when is_atom(Id)->
	db_ecc:delete_data(?DBName,?Table,"id="++atom_to_list(Id));
remove_device(Id)when is_list(Id)->
	db_ecc:delete_data(?DBName,?Table,"id="++Id);
remove_device(_)->
	{error,parameter_error}.

%% @doc get children of a device
%% @spec  get_childs(Id)->({error,Reason} | Childs)
%% where
%%		Id = (atom() | string())
%%		Childs = list()
get_childs(Id) when is_atom(Id)->
	get_device_match("my.parent='" ++ atom_to_list(Id)++"'") ++ dbcs_monitor:get_monitor_match("my.parent='" ++ atom_to_list(Id)++"'");
get_childs(Id) when is_list(Id)->
	get_device_match("my.parent='" ++ Id ++ "'") ++ dbcs_monitor:get_monitor_match("my.parent='" ++ Id ++"'");
get_childs(_)->
	{error,parameter_error}.


%% @doc get id for next object
%% @spec get_next_id(DID)->({error,Reason} | {ok,Id})
%% where
%%		DID = (atom() | string())
%%		Id = atom()
get_next_id(DID) when is_atom(DID);is_list(DID)->
	case get_device(DID) of
		{error,_}->
			{error,not_found_device};
		Ret->
			case lists:keysearch(nextid,1,Ret) of
				{value,{nextid,NextId}}->
					New = lists:map(fun({Key,Val})-> case Key of nextid-> {nextid,Val+1};_->{Key,Val} end end,Ret),
					update_device(New),
					{ok,list_to_atom(atom_to_list(DID)++ "." ++ integer_to_list(NextId))};
				_->
					update_device(Ret++[{nextid,2}]),
					{ok,list_to_atom(atom_to_list(DID)++ "." ++ integer_to_list(1))}
			end
	end;
get_next_id(_)->{error,parameter_error}.