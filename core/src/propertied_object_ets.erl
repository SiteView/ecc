%% ---
%% propertied_object_ets
%%
%%---
-module(propertied_object_ets,[Tid]).
-compile(export_all).
-define(TIMEOUT,15000).

new(Tid)->
	PId = ets:new(?MODULE,[set,public]),
	AId = ets:new(?MODULE,[set,public]),
	{?MODULE,{PId,AId}}.




%%set_attribute(Name,Val)-> {ok,Ret} | {error,Resean}
%%
%%
set_attribute(Name,Val)->
	{_,AId} = Tid,
	case ets:insert(AId,{Name,Val}) of
		true ->
			{ok,set_attribute_ok};
		Err->
			{error,Err}
	end.
	

%%get_attribute(Name)-> {ok,{Name,Val}} | {error,Resean}
%%
%%
get_attribute(Name)->
	{_,AId} = Tid,
	case ets:lookup(AId,Name) of
		[] ->
			{error,not_found};
		[R|_]->
			{ok,R};
		Err->
			{error,Err}
	end.


%%get_attribute(Name)-> {ok,[]} | {error,Resean}
%%
%%
get_attributes()->
	{_,AId} = Tid,
	ets:tab2list(AId).

%%remove_attributes()-> {ok,Ret} | {error,Resean}
%%
%%
remove_attributes()->
	{_,AId} = Tid,
	case ets:delete_all_objects(AId) of
		true ->
			{ok,remove_attributes_ok};
		Err->
			{error,Err}
	end.

%%inc_attribute(Name)-> 
%%
%%
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

remove_attribute(Name)->
	{_,AId} = Tid,
	case ets:delete(AId,Name) of
		true ->
			{ok,remove_attributes_ok};
		_->
			{error,remove_attributes}
	end.

remove_attribute_match(Reg)->
	Atts = THIS:get_attributes(),
	[THIS:remove_attribute(N)||{N,_}<-Atts,nomatch=/=re:run(case is_list(N) of true-> N;_->lists:flatten(io_lib:format("~p",[N])) end,Reg)],
	{ok,remove_attribute_match_ok}.

add_attributes(Attributes)->
	{_,AId} = Tid,
	ets:delete_all_objects(AId),
	case ets:insert(AId,Attributes) of
		true ->
			{ok,add_attributes_ok};
		Err ->
			{error,Err}
	end.
	
add_properties(Properties)->
	{PId,_} = Tid,
	ets:delete_all_objects(PId),
	case ets:insert(PId,Properties) of
		true ->
			{ok,add_properties_ok};
		Err ->
			{error,Err}
	end.

remove_properties()->
	{PId,_} = Tid,
	case ets:delete_all_objects(PId) of
		true ->
			{ok,remove_properties_ok};
		_->
			{error,remove_properties_error}
	end.

get_property(Key)->
	{PId,_} = Tid,
	case ets:lookup(PId,Key) of
		[] ->
			{error,not_found};
		[R|_]->
			{ok,R};
		Err->
			{error,Err}
	end.


get_properties()->
	{PId,_} = Tid,
	ets:tab2list(PId).

set_property(Key,Val)->
	{PId,_} = Tid,
	case ets:insert(PId,{Key,Val}) of
		true ->
			{ok,set_property_ok};
		Err->
			{error,Err}
	end.

remove_property(Key)->
	{PId,_} = Tid,
	ets:delete(PId,Key).

remove_property_match(Reg)->
	{PId,_} = Tid,
	Atts = THIS:get_properties(),
	[THIS:remove_property(N)||{N,_}<-Atts,nomatch=/=re:run(case is_list(N) of true-> N;_->lists:flatten(io_lib:format("~p",[N])) end,Reg)],
	{ok,remove_property_match_ok}.


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



init(_,Data)->
	THIS:remove_properties(),
	THIS:add_properties(Data).

get_tid()->Tid.

delete()->
	{PId,AId} = Tid,
	case THIS:get_property(id) of
		{ok,{_,Id}}->
			catch(siteview:remove_object(Id));
		_->
			pass
	end,
	ets:delete(PId),
	ets:delete(AId).