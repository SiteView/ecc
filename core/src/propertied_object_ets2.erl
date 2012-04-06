%% ---
%% propertied_object_ets
%%
%%---
-module(propertied_object_ets2,[ID,TP,TA]).
-compile(export_all).
-define(TIMEOUT,15000).

-define(ETS_COUNT,100).
-include("monitor.hrl").

new(ID)->
	% Ref = make_ref(),
	% case lists:member(?MODULE,ets:all()) of
		% true->
			% ok;
		% _->
			% ets:new(?MODULE,[set,public,named_table]),
			% Ids = lists:seq(1,?ETS_COUNT),
			% F = fun(X)->
				% Name = list_to_atom(atom_to_list(?MODULE)++"_"++integer_to_list(X)),
				% ets:new(Name,[set,public,named_table])
			% end,
			% [ F(X) || X<-Ids]
	% end,
	% Ra = random:uniform(?ETS_COUNT),
	% Rn = list_to_atom(atom_to_list(?MODULE)++"_"++integer_to_list(Ra)),
	% ets:insert(?MODULE,{Ref,Rn}),
	{Tp,Ta} = propertied_object_server:new_object(),
	{?MODULE,make_ref(),Tp,Ta}.


%%set_attribute(Name,Val)-> {ok,Ret} | {error,Resean}
%%
%%
set_attribute(Name,Val)->
	case ets:insert(TA,{{ID,Name},Val}) of
		true ->
			{ok,set_attribute_ok};
		Err->
			{error,Err}
	end.

%%get_attribute(Name)-> {ok,{Name,Val}} | {error,Resean}
%%
%%
get_attribute(Name)->
	case ets:lookup(TA,{ID,Name}) of
		[]->
			{error,{Name,not_found}};
		[{_,Val}]->
			{ok,{Name,Val}}
	end.


%%get_attribute(Name)-> {ok,[]} | {error,Resean}
%%
%%
get_attributes()->
	case ets:match(TA,{{ID,'$1'},'$2'}) of
		[]->
			[];
		Ret->
			[list_to_tuple(X)||X<-Ret]
	end.

%%remove_attributes()-> {ok,Ret} | {error,Resean}
%%
%%
remove_attributes()->
	case ets:match_delete(TA,{{ID,'_'},'_'}) of
		true->
			{ok,remove_attributes_ok};
		Else->
			{error,Else}
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
	case ets:member(TA,{ID,Name}) of
		true->	
			case ets:match_delete(TA,{{ID,Name},'_'}) of
				true->
					{ok,{Name,removed}};
				Else->
					{error,Else}
			end;
		_->
			{error,{Name,not_found}}
	end.

remove_attribute_match(Reg)->
	Atts = THIS:get_attributes(),
	try
	[THIS:remove_attribute(N)||{N,_}<-Atts,nomatch=/=re:run(case is_list(N) of true-> N;_->lists:flatten(io_lib:format("~p",[N])) end,Reg)],
	{ok,{Reg,removed}}
	catch
		_:Err->
			{error,Err}
	end.

add_attributes(Attributes)->
	NewAttrs = [{{ID,K},V}||{K,V}<-Attributes],
	case ets:insert(TA,NewAttrs) of
		true->
			{ok,add_attributes_ok};
		Else->
			{error,Else}
	end.
	
add_properties(Properties)->
	NewProps = [{{ID,K},V}||{K,V}<-Properties],
	case ets:insert(TP,NewProps) of
		true->
			{ok,add_properties_ok};
		Else->
			{error,Else}
	end.

remove_properties()->
	case ets:match_delete(TP,{{ID,'_'},'_'}) of
		true->
			{ok,remove_properties_ok};
		Else->
			{error,Else}
	end.

get_property(Key)->
	case ets:lookup(TP,{ID,Key}) of
		[]->
			{error,{Key,not_found}};
		[{_,Val}]->
			{ok,{Key,Val}}
	end.


get_properties()->
	case ets:match(TP,{{ID,'$1'},'$2'}) of
		[]->
			[];
		Ret->
			[list_to_tuple(X)||X<-Ret]
	end.

set_property(Key,Val)->
	case ets:insert(TP,{{ID,Key},Val}) of
		true ->
			{ok,set_property_ok};
		Err->
			{error,Err}
	end.

remove_property(Key)->
	case ets:member(TP,{ID,Key}) of
		true->	
			case ets:match_delete(TP,{{ID,Key},'_'}) of
				true->
					{ok,{Key,removed}};
				Else->
					{error,Else}
			end;
		_->
			{error,{Key,not_found}}
	end.

remove_property_match(Reg)->
	Atts = THIS:get_properties(),
	try
	[THIS:remove_property(N)||{N,_}<-Atts,nomatch=/=re:run(case is_list(N) of true-> N;_->lists:flatten(io_lib:format("~p",[N])) end,Reg)],
	{ok,{Reg,removed}}
	catch
		_:Err->
			{error,Err}
	end.


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

get_tid()->
	{ID,TP,TA}.

init(_,Data)->
	THIS:remove_properties(),
	THIS:add_properties(Data).

delete()->
	case THIS:get_property(id) of
		{ok,{_,Id}}->
			App = case THIS:get_property(?APP) of {ok,{_,V}}->V;_->undefined end,
			catch(siteview:remove_object(App,Id));
		_->
			pass
	end,
	catch(ets:match_delete(TA,{{ID,'_'},'_'})),
	catch(ets:match_delete(TP,{{ID,'_'},'_'})),
	ok.