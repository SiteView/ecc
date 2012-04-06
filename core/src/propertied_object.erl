%% 
%% @doc property object module
%% @version{1.0}
%% @copyright 2009 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>
-module(propertied_object,[Tid]).
-compile(export_all).
-define(TIMEOUT,5000).
-include("monitor.hrl").

-export([set_attribute/2,get_attribute/1,get_attributes/0,remove_attributes/0,inc_attribute/1,remove_attribute_match/1]).
-export([add_properties/1,remove_properties/0,get_property/1,get_properties/0,set_property/2,remove_property/1,remove_property_match/1,inc_property/1]).
-export([init/2,delete/0]).

new(Tid)->
	%Id = ets:new(?MODULE,[set,protected]),
	Id = spawn(fun()->propertied_object_static:loop([],[]) end),
	%Id = spawn(fun()->propertied_object_static2:loop(dict:new(),dict:new()) end),
	%%io:format("module~p~n",[?MODULE]),
	{?MODULE,Id}.




%% @spec set_attribute(Name,Val)-> ({ok,Ret} | {error,Resean})
%% where
%%	Name = atom()
%%	Val = term()
%%	Ret = term()
%%	Resean = atom()
%% @doc set a attribute'val to Val
set_attribute(Name,Val)->
	Tid!{self(),{set_attribute,{Name,Val}}},
	receive
		{Tid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.

%% @spec get_attribute(Name)-> ({ok,{Name,Val}} | {error,Resean})
%% where
%% 	Name = atom()
%%	Val = term()
%%	Resean = atom()
%% @doc get a attribute's value
get_attribute(Name)->
	Tid!{self(),{get_attribute,Name}},
	receive
		{Tid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.

%% @spec get_attributes()-> ({ok,list()} | {error,Resean})
%% where
%% 	Resean = atom()	
%% @doc get all attributes
get_attributes()->
	Tid!{self(),get_attributes},
	receive
		{Tid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.

%% @spec remove_attributes()-> {ok,Ret} | {error,Resean}
%% where
%%	Ret = atom()
%%	Resean = atom()
%% @doc remove all attributes of this object
remove_attributes()->
	Tid!{self(),remove_attributes},
	receive
		{Tid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.

%% @spec inc_attribute(Name)-> ({ok,Ret} | {error,Reason})
%% where
%%	Ret = atom()
%%	Reason = atom()
%% @doc increase a attribute's value
inc_attribute(Name)->
	case THIS:get_attribute(Name) of
		{ok,{Name,Val}}->
			case THIS:set_attribute(Name,Val+1) of
				{ok,_}->
					{ok,inc_attribute};
				Else->
					{error,Else}
			end;
		_->
			{error,attribute_not_found}
	end.
	

add_attributes(Attributes)->
	Tid!{self(),{add_attributes,Attributes}},
	receive
		{Tid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.

%% @spec remove_attribute(Name)->({ok,Ret}|{error,Reason})
%% where
%%	Name = atom()
%%	Ret = term()
%%	Reason = atom()
%% @doc remove a attribute from this object,Name is the property's key
remove_attribute(Name)->
	Tid!{self(),{remove_attribute,Name}},
	receive
		{Tid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.

%% @spec remove_attribute_match(Reg)->({ok,Ret}|{error,Reason})
%% where
%%	Reg = string()
%%	Ret = term()
%%	Reason = atom()
%% @doc return a attribute which name match the Reg string
remove_attribute_match(Reg)->
	Tid!{self(),{remove_attribute_match,Reg}},
	receive
		{Tid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.

%% @spec add_properties(Properties)->({ok,Ret}|{error,Reason})
%% where
%%	Properties = [{atom(),term()}]
%%  Ret = atom()
%%	Reason = atom()
%% @doc add properties to this object,Properties is a key-value list contains the properties.
add_properties(Properties)->
	Tid!{self(),{add_properties,Properties}},
	receive
		{Tid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.

%% @spec remove_properties()->({ok,Ret}|{error,Reason})
%% where
%%	Ret = term()
%%	Reason = atom()
%% @doc remove all properties from the object
remove_properties()->
	Tid!{self(),remove_properties},
	receive
		{Tid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.

%% @spec get_property(Key)->({ok,[{Key,Value}]}|{error,Reason})
%% where
%%	Key = atom()
%%	Value = term()
%%	Reason = atom()
%% @doc get the object's property
get_property(Key)->
	Tid!{self(),{get_property,Key}},
	receive
		{Tid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.

%% @spec get_properties()->({ok,Ret}|{error,Reason})
%% where
%%	Ret = [{atom(),term()}]
%%	Reason=atom()
%% @doc get all properties of this object
get_properties()->
		Tid!{self(),get_properties},
	receive
		{Tid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.

%% @spec set_property(Key,Val)->({ok,Ret}|{error,Reason})
%% where
%%	Key = atom()
%%	Val = term()
%%	Ret = term()
%%	Reason = atom()
%% @doc set property's value to Val
set_property(Key,Val)->
	Tid!{self(),{set_property,{Key,Val}}},
	receive
		{Tid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.

%% @spec remove_property(Key)->({ok,Ret}|{error,Reason})
%% where
%%	Key = atom()
%%	Ret = atom()
%%	Reason = atom()
%% @doc remove a property
remove_property(Key)->
	Tid!{self(),{remove_property,Key}},
	receive
		{Tid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.

%% @spec remove_property_match(Reg)->({ok,Ret}|{error,Reason})
%% where
%%	Reg = string
%%	Ret = atom()
%%	Reason = atom()
%% @doc remove a property which is match Reg string
remove_property_match(Reg)->
	Tid!{self(),{remove_property_match,Reg}},
	receive
		{Tid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.
	
%% @spec inc_property(Key)-> ({ok,Ret} | {error,Reason})
%% where
%%	Key = atom()
%%	Ret = atom()
%%	Reason = atom()
%% @doc increase a property's value
inc_property(Key)->
	case THIS:get_property(Key) of
		{ok,{Key,Val}}->
			case THIS:set_property(Key,Val+1) of
				{ok,_}->
					{ok,inc_property};
				Else->
					{error,Else}
			end;
		_->
			{error,property_not_found}
	end.

%% @spec init(This,Data)->({ok,Ret}|{error,Reason})
%% where
%%	This = instance()
%%  Data = [{Key,Value}]
%%	Key = atom()
%%	Value = term()
%%	Ret = term()
%%	Reason = atom()
%% @doc add Data to object's property
init(This,Data)->
	This:remove_properties(),
	This:add_properties(Data).
	%%[ets:insert(Tid,X)||X<-Data].

get_tid()->Tid.

%% @spec delete()->({ok,Ret}|{error,Reason})
%% where
%%	Ret = atom()
%%	Reason = atom()
%% @doc delete object
delete()->
	case THIS:get_property(id) of
		{ok,{_,Id}}->
			App = case THIS:get_property(?APP) of {ok,{_,V}}->V;_->undefined end,
			% dbcs_base:set_app(App),
			catch(siteview:remove_object(App,Id));
		_->
			pass
	end,
	Tid!{self(),stop},
	receive
		{Tid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.
	%%ets:delete(Tid).