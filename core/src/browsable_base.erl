%% ---
%% browsable_base
%%
%%---
-module(browsable_base,[BASE]).
-extends(atomic_monitor).
-compile(export_all).
-include("monitor_template.hrl").

-define(MAX_COUNTER,10).

new()->
	Base = atomic_monitor:new(),
	Base:set_attribute(countersInError,0),
	{?MODULE,Base}.

getMaxCounter()->
	?MAX_COUNTER.

getBrowseData(_)->[].

verify(Params)->
	Errs =
	case proplists:get_value(browse,Params) of
		undefined->
			[{browse,"must select at least one browse counter"}];
		[]->
			[{browse,"must select at least one browse counter"}];
		_->
			[]
	end ++
	case BASE:verify(Params) of
		{error,Be}->
			Be;
		_->
			[]
	end,
	if
		length(Errs)>0->
			{error,Errs};
		true ->
			{ok,""}
	end.

getCostInLicensePoints()->
	{ok,{_,Counters}} = THIS:get_property(browse),
	length(Counters).

getLogProperties(This)->
	{ok,{_,Counters}} = THIS:get_property(browse),
	Temp = This:get_template_property(),
	[X#property.name || X<-Temp,X#property.state=:=true] ++ [X||{X,_}<- Counters].

getStatePropertyObjects()->
	[ #property{name=countersInError,title="counters in error",type=numeric,editable=true,configurable=false,state=true,order=100}].

get_template_property()->
	BASE:get_template_property() ++ 
	[#property{name=browse,title="Counters", description="Current selection of counters.",type=browsable,editable=true,order=100},
	 #property{name=countersInError,title="counters in error",type=numeric,editable=true,configurable=false,state=true,order=100}
	].
	%%++ lists:map(fun(X)->#property{name=list_to_atom(atom_to_list(browseName)++integer_to_list(X)),title="",type=text,editable=false,configurable=false} end,lists:seq(1,?MAX_COUNTER,1))
	%%++ lists:map(fun(X)->#property{name=list_to_atom(atom_to_list(browseNameid)++integer_to_list(X)),title="",type=text,editable=false,configurable=false} end,lists:seq(1,?MAX_COUNTER,1))
	%%++ lists:map(fun(X)->#property{name=list_to_atom(atom_to_list(browsableValue)++integer_to_list(X)),title="",type=text,editable=false,configurable=false} end,lists:seq(1,?MAX_COUNTER,1)).