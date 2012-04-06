%% 
%% @doc object base class
%% @version{1.0}
%% @copyright 2009 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>

-module(siteview_object,[BASEOBJ]).
-compile(export_all).
-extends(propertied_object).
-include("monitor.hrl").
-export([createObject/1,createObject/2,get_property_as_number/2,get_property_as_number/1,get_attribute_as_number/2,get_attribute_as_number/1]).
-export([set_owner/1,get_owner/0,getFullID/0,sleep/1]).

new()->
	Obj = propertied_object:new(null),
	{?MODULE,Obj}.

%% @spec createObject(Data)->Obj
%% where
%%	Data = [{atom(),term()}]
%%	Obj = instance()
%% @doc user Data to create a object instance,the object Class contains in Data
createObject(Data)->
	case is_list(Data) of
		false->
			{error,data_error};
		true->
			case lists:keysearch(class,1,Data) of
				{value,{class,Class}}->
					M = case Class of
							group->
								monitor_group;
							device->
								device;
							_->
								Class
						end,
					try 
						Obj=M:new(),
						Obj:init(Obj,Data),
						Obj
					catch
						_:_->{error,create_object_error}
					end;
				_->
					{error,not_found_class}
			end
	end.
	
%% @spec createObject(Parent,Data)->Obj
%% where
%%	Parent = atom()
%%	Data = [{atom(),term()}]
%% @doc create a object,and set it's parent
createObject(Parent,Data)->
	case is_list(Data) of
		false->
			{error,data_error};
		true->
			case lists:keysearch(class,1,Data) of
				{value,{class,Class}}->
					M = case Class of
							group->
								monitor_group;
							_->
								Class
						end,
					try 
						Obj=M:new(Parent),
						Obj:init(Obj,Data),
						Obj
					catch
						_:_->{error,create_class_error}
					end;
				_->
					{error,not_found_class}
			end
	end.

%% @spec get_property_as_number(Key,Default)->integer()
%% where
%%	Key = atom()
%%	Dafault=integer()
%% @doc get a property's value as number format,if the Key's value can not format as a number format,return Default
%%
get_property_as_number(Key,Default)->
	case THIS:get_property(Key) of
		{ok,{Key,Val}} when is_atom(Val)->
			case string:to_integer(atom_to_list(Val)) of
				{error,_}->
					Default;
				{Num,_}->
					Num
			end;
		{ok,{Key,Val}} when is_list(Val)->
			case string:to_integer(Val) of
				{error,_}->
					Default;
				{Num,_}->
					Num
			end;
		{ok,{Key,Val}} when is_number(Val)->
			Val;
		_->
			Default
	end.

%% @spec get_property_as_number(Key)->integer()
%% where
%%	Key = atom()
%% @doc get a property's value as number format,if the Key's value can not format as a number format,return 0
get_property_as_number(Key)->
	case THIS:get_property(Key) of
		{ok,{Key,Val}} when is_atom(Val)->
			case string:to_integer(atom_to_list(Val)) of
				{error,_}->
					0;
				{Num,_}->
					Num
			end;
		{ok,{Key,Val}} when is_list(Val)->
			case string:to_integer(Val) of
				{error,_}->
					0;
				{Num,_}->
					Num
			end;
		{ok,{Key,Val}} when is_number(Val)->
			Val;
		_->
			0
	end.

%% @spec get_attribute_as_number(Key,Default)->integer()
%% where
%%	Key = atom()
%%	Dafault=integer()
%% @doc get a attribute's value as number format,if the Key's value can not format as a number format,return Default
get_attribute_as_number(Key,Default)->
	case THIS:get_attribute(Key) of
		{ok,{Key,Val}} when is_atom(Val)->
			case string:to_integer(atom_to_list(Val)) of
				{error,_}->
					Default;
				{Num,_}->
					Num
			end;
		{ok,{Key,Val}} when is_list(Val)->
			case string:to_integer(Val) of
				{error,_}->
					Default;
				{Num,_}->
					Num
			end;
		{ok,{Key,Val}} when is_number(Val)->
			Val;
		_->
			Default
	end.

%% @spec get_attribute_as_number(Key)->integer()
%% where
%%	Key = atom()
%% @doc get a attribute's value as number format,if the Key's value can not format as a number format,return 0
get_attribute_as_number(Key)->
	case THIS:get_attribute(Key) of
		{ok,{Key,Val}} when is_atom(Val)->
			case string:to_integer(atom_to_list(Val)) of
				{error,_}->
					0;
				{Num,_}->
					Num
			end;
		{ok,{Key,Val}} when is_list(Val)->
			case string:to_integer(Val) of
				{error,_}->
					0;
				{Num,_}->
					Num
			end;
		{ok,{Key,Val}} when is_number(Val)->
			Val;
		_->
			0
	end.

%% @spec set_owner(Owner)->({ok,Result}|{error,Reason})
%% where
%%	Owner = atom()
%%	Result = term()
%%	Reason = atom()
%% @doc set the object's parent
set_owner(Owner)->
	THIS:set_attribute(parent,Owner).

%% @spec get_owner()->({ok,Result}|{error,Reason})
%% where
%%	Result = term()
%%	Reason = atom()
%% @doc get the object's parent
get_owner()->
	THIS:get_attribute(parent).
	
	
remove_owner()->
	THIS:remove_attribute(parent).


create_from_template(_)->ok.

get_group_path_id()->{ok,THIS:get_property(id)}.

%% @spec getFullID()->atom()
%% @doc get object's indentify
getFullID()->
	{ok,{id,Id}} = THIS:get_property(id),
	Id.
	%%case THIS:get_owner() of
	%%	{ok,{parent,Parent}}->
	%%		list_to_atom(atom_to_list(Parent:getFullID()) ++ ?ID_SEPARATOR ++ atom_to_list(Id));
	%%	_->
	%%		Id
	%%end.


get_schedule_settings()->ok.

%% @spec sleep(Val)->ok
%% where
%%	Val = integer
%% @doc 
sleep(Val)->
	receive
	after Val->
		ok
	end.
	
get_app()->
	case THIS:get_property(?APP) of
		{ok,{_,V}}->
			V;
		_->
			undefined
	end.

%% @spec getScalarValues(Params)->list()
%% where
%%	Params=[{string(),term()}]
%% @doc get scalar property's options
getScalarValues(_)->
	[].

%% @spec getCostInLicensePoints()->integer()
%% @doc get the license points this object const
getCostInLicensePoints()->0.