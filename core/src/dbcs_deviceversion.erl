
-module(dbcs_deviceversion). 
-compile(export_all).
-include("dbcs_common.hrl").
-define(Table,"deviceversion").

base_property(id)->
	true;
base_property(_)->
	false.

deviceversion_to_db(Deviceversion)->
	{value,{id,Id}} = lists:keysearch(id,1,Deviceversion),
	Advance = [dbcs_base:term2db(K,V)||{K,V}<- Deviceversion,base_property(K)=:=false],
	case [is_atom(Id)] of
		[true]->
			{content,list_to_atom(?Table), Id, <<"deviceversion">>,null,null,null,null,?Author,null,null,null,null,null,Advance};
		_->
			{}
	end.

db_to_deviceversion(DeviceversionData)->
	case DeviceversionData of
		{_,_,Id,_,_,_,_,_,_,_,_,_,_,_,Adv}->
			[{id,Id}] ++ [dbcs_base:db2term(K,T,V)||{K,T,V}<-Adv];
		_->
			[]
	end.

%% @docNew version of a device
%% @spec create_deviceversion(deviceversion)->({error,Reason} | {ok,Result})
%% where
%%		deviceversion = list()
%%		Result = tuple()
create_deviceversion(Deviceversion)when is_list(Deviceversion) ->
	db_ecc:insert_data(?DBName,?Table,deviceversion_to_db(Deviceversion));
create_deviceversion(_)->{error,parameter_error}.


%% @doc Check the version of a device
%% @spec get_deviceversion(Id)->({error,Reason} | deviceversion)
%% where
%%		Id = (atom() | string())
%%		deviceversion = list()
get_deviceversion(Id)when is_atom(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=deviceversion & id="++atom_to_list(Id)),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_deviceversion};
				_->
					[Deviceversion|_] = Ret,
					db_to_deviceversion(Deviceversion)
			end
	end;
get_deviceversion(Id)when is_list(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=deviceversion & id="++Id),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_deviceversion};
				_->
					[Deviceversion|_] = Ret,
					db_to_deviceversion(Deviceversion)
			end
	end;
get_deviceversion(_)->
	{error,id_error}.


%% @doc Update the device version information
%% @spec update_deviceversion(deviceversion)->({error,Reason} | {ok,Result})
%% where
%%		deviceversion = list()
%%		Result = tuple()
update_deviceversion(D) when is_list(D)->
	case lists:keysearch(id,1,D) of
		{value,{id,Id}}->
			db_ecc:update_data(?DBName,?Table,"id=" ++ atom_to_list(Id),deviceversion_to_db(D));
		_->
			{error,not_found_id}
	end;
update_deviceversion(_)-> {error,parameter_error}.

%% @doc Conditions combined version of the device search for devices
%% @spec  get_deviceversion_match(Where)-> (deviceversions | {error,Reason})
%% where
%%		Where = string()
%%		deviceversions = list()
get_deviceversion_match(Where)->
	Ret = db_ecc:get_data(?DBName,?Table,Where),
	[db_to_deviceversion(X)||X <- Ret].


%% @doc get all versions of the device data
%% @spec get_all()->({error,Reason} | deviceversions)
%% where
%%		deviceversions = list()
get_all()->
	Ret = db_ecc:get_data(?DBName,?Table,""),
	[db_to_deviceversion(X)||X <- Ret].

%% @doc Remove a device version information
%% @spec remove_deviceversion(Id)->({error,Reason} | {ok,Result})
%% where
%%		Id = (atom() | string())
%%		Result = tuple()
remove_deviceversion(Id)when is_atom(Id)->
	db_ecc:delete_data(?DBName,?Table,"id="++atom_to_list(Id));
remove_deviceversion(Id)when is_list(Id)->
	db_ecc:delete_data(?DBName,?Table,"id="++Id);
remove_deviceversion(_)->
	{error,parameter_error}.
