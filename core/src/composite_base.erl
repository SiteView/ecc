%% ---
%% Composite Base
%%
%%---

%% @copyright 2008-2009 Dragonflow
%% @author Xianfang.Shi<Xianfang.Shi>, Jian Huang <jian.huang@dragonflow.com>
%% @version 1.0

%% @doc Composite Base
%%
%%This module provide base function for composite monitor:

-module(composite_base,[BASE]).
-extends(application_base).
-compile(export_all).
-include("monitor_template.hrl").
-include("monitor.hrl").

%% @spec new() -> Obj
%% @type Obj = term()
%% @doc create a new instance for compoiste base object
new()->
	Obj = application_base:new(),
	{?MODULE,Obj}.

%% @spec getScalarValues(Prop, Params) -> List
%% @type Prop = atom()
%% @type Params = [term()]
%% @type List = [Opt]
%% @type Opt = {ShowContent, Value}
%% @type ShowContent = string()
%% @type Value = string()
%% @doc getScalarValues is the function called by schedule to show a dropdown list box and its value.
%% 		in this function, all groups and all monitors are list in a listbox
getScalarValues(Prop,Params)->
	case Prop of
		item->
			SV = siteview:get_current_siteview(),
			Monitors =SV:getGroupsMonitors(true),
			F = fun(X)->
					{ok,{id,Id}} = X:get_property(id),
					{X:get_full_name(),atom_to_list(Id)}
				end,
			lists:map(F,Monitors);
		_->
			BASE:getScalarValues(Prop,Params)
	end.

%%update the attribute
updateProperties({MC,ME,MW,MN,GC,GE,GW,GN,SS})->
	%%set attribute
	THIS:set_attribute(itemsChecked,MC+GC),
	THIS:set_attribute(itemsInError,ME+GE),
	THIS:set_attribute(itemsInWarning,MW+GW),
	THIS:set_attribute(itemsInGood,MN+GN),
	
	%%format the stats string
	case MC+GC of 
		0->
			THIS:set_attribute(?STATE_STRING,"no items checked"),
			THIS:set_attribute(percentGood,0),
			THIS:set_attribute(percentError,0),
			THIS:set_attribute(percentWarning,0);
		_->
			S = if 
					GC>0-> 
						integer_to_list(GC) ++ " groups" ++
						if 
							MC>0->
								" and " ++ integer_to_list(MC) ++ " monitors";
							true->
								""
						end;
					true->
						if 
							MC>0->
								integer_to_list(MC) ++ " monitors";
							true->
								""
						end
				end,
			S1 = case S of
					""->
						"";
					_->
						S ++ " checked<br>"
				end,

			S2 = if
					ME+GE>0->
						integer_to_list(ME+GE) ++ " in error<br>";
					true->
						""
				end,
			S3 = if
					MW+GW>0->
						integer_to_list(MW+GW) ++ " in warning<br>";
					true->
						""
				end,
			S4 = if
					length(SS) > 0 ->
						"(" ++ SS ++ ")";
					true ->
						SS
				end,
			THIS:set_attribute(?STATE_STRING,integer_to_list(round((MN+GN)*100/(MC+GC))) ++"% OK<br>" ++ S1 ++ S2 ++ S3 ++ S4),
			THIS:set_attribute(percentGood,round((MN+GN)*100/(MC+GC))),
			THIS:set_attribute(percentError,round((ME+GE)*100/(MC+GC))),
			THIS:set_attribute(percentWarning,round((MW+GW)*100/(MC+GC)))
	end.

%% @spec verify(Params) -> {ok, []} | {error, Reason}
%% @type Params = [term()]
%% @type Reason = string()
%% @doc verify is the function called by schedule to verify the parameter of monitor inputed by user
%%	composite base verify listbox
verify(Params)->
	Errs = 
	case proplists:get_value(item,Params) of
		[]->
			[{item,"no monitor item selected."}];
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


%% @spec get_template_property() -> List
%% @type List = [term()]
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
%% 1.elements show on gui interface
%% 2.elements to evaluate the return value
get_template_property()->
	BASE:get_template_property() ++ 
	[
	 #property{name=item,title="Items",type=scalar,editable=true,order=1,listSize=8,multiple=true,description="select one or more groups and/or monitors that will be checked"},
	 #property{name=percentGood,title="% items OK",type=numeric,configurable=false,state=true},
	 #property{name=percentError,title="% items in error",type=numeric,configurable=false,state=true},
	 #property{name=percentWarning,title="% items in warning",type=numeric,configurable=false,state=true},
	 #property{name=itemsInError,title="items in error",type=numeric,configurable=false,state=true},
	 %%#property{name=nameItemsInError,title="name of the items in error",type=text,configurable=false,state=true},
	 %%#property{name=nameItemsInWarning,title="name of the items in warning",type=text,configurable=false,state=true},
	 #property{name=itemsInWarning,title="items in warning",type=numeric,configurable=false,state=true},
	 #property{name=itemsInGood,title="items OK",type=numeric,configurable=false,state=true},
	 #property{name=itemsChecked,title="items checked",type=numeric,configurable=false,state=true}

	].
	