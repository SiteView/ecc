%% ---
%% Composite Monitor
%%
%%---

%% @copyright 2008-2009 Dragonflow
%% @author Xianfang.Shi<Xianfang.Shi>, Jian Huang <jian.huang@dragonflow.com>
%% @version 1.0

%% @doc Composite monitor
%%
%%This module is to analyse the status of a group of monitors:
%%1. sum the number of error, warning and good status of select monitors or groups 
%%2. if checking all monitors in groups, the status pf all monitors that belong to the group will be summed seperatedly. Otherwise, the
%%		group is only summed as one entry.
%%3. if running all monitors setting is set, before sum the status number, all related monitor will be executed. Otherwise, only the storoed
%%		status is evaluated.

-module(composite_monitor,[BASE]).
-extends(composite_base).
-compile(export_all).


-include("monitor_template.hrl").
-include("monitor.hrl").

%% @spec new() -> Obj
%% @type Obj = term()
%% @doc create a new instance for compoiste monitor
new()->
	Obj = composite_base:new(),
	{?MODULE,Obj}.

%% @spec defaultTitle(Params) -> List
%% @type Params = [term()]
%% @type List = string()
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	case proplists:get_value(item,Params) of 
		undefined->
			BASE:defaultTitle(Params);
		List->
			Title = get_title(List, 0, 0),
			BASE:defaultTitle(Params) ++ ": " ++ Title
	end.

get_title([], Group, Monitor) ->
	integer_to_list(Group) ++ " Groups, " ++ integer_to_list(Monitor) ++ " Monitors";
get_title([Id|T], Group, Monitor) ->
	case api_siteview:find_object(list_to_atom(Id)) of
		%%not found, just return
		[]->
			get_title(T, Group, Monitor);
		%%found
		[M|_]->
			case M:get_property(?CLASS) of
				%%instance is a group
				{ok,{?CLASS,group}}->
					get_title(T, Group+1, Monitor);
				_->
					get_title(T, Group, Monitor+1)
			end
	end.		
	
	
%% @spec update() -> Result
%% @type Result = term()
%% @doc update is the run function called by schedule to test the  composite monitor
update()->
	%%get argument
	{ok,{_,Ids}} = THIS:get_property(item),
	{ok,{_,DeepCheck}} = THIS:get_property(deepCheck),
	{ok,{_,Delay}} = THIS:get_property(delay),
	{ok,{_,Check}} = THIS:get_property(checkSequentially),
	
	%%chech monitors
	%%MC - monitor total counts, ME - monitor total error, MW - monitor total warnings, MN - monitor total good
	%%GC - group total counts, GE - group total error, GW - group total warnings, GN - group total good
	{MC,ME,MW,MN,GC,GE,GW,GN,SS} = THIS:check_ids(Ids,DeepCheck,Delay,Check,{0,0,0,0,0,0,0,0,[]}),
	
	%%update the attribute
	THIS:updateProperties({MC,ME,MW,MN,GC,GE,GW,GN,SS}).

%%check monitor status
check_monitor(M, Check, {MC,ME,MW,MN,GC,GE,GW,GN,SS}) ->
	if
		%%monitor itself, not including it
		M =:= THIS ->
			{MC,ME,MW,MN,GC,GE,GW,GN,SS};
			
		true ->
			%%execute the monitor, if we have to execute monitor before getting state
			case Check of
				true ->
					M:monitorUpdate(M);
				_ ->
					ok
			end,
				
			%%sum the return status
			case M:get_attribute(?CATEGORY) of
				{ok,{_,error}}->
					{ok, {_, SS5}} = M:get_property(?NAME),
					{MC+1,ME+1,MW,MN,GC,GE,GW,GN,SS ++ "<br>" ++ SS5 ++ ": error"};
				{ok,{_,nodata}}->
					{ok, {_, SS5}} = M:get_property(?NAME),
					{MC+1,ME+1,MW,MN,GC,GE,GW,GN,SS ++ "<br>" ++ SS5 ++ ": nodata"};
				{ok,{_,warning}}->
					{MC+1,ME,MW+1,MN,GC,GE,GW,GN,SS};
				_->
					{MC+1,ME,MW,MN+1,GC,GE,GW,GN,SS}
			end
	end.
	
%%Real function to sum the status
check_ids([],_,_,_,{MC,ME,MW,MN,GC,GE,GW,GN,SS})->
	{MC,ME,MW,MN,GC,GE,GW,GN,SS};
check_ids([Id|T],DeepCheck,Delay,Check,{MC,ME,MW,MN,GC,GE,GW,GN,StateString})->
	if
		is_list(Id) ->
			%%find the instance of monitor or group
			case api_siteview:find_object(list_to_atom(Id)) of
				%%not found, just return
				[]->
					check_ids(T,DeepCheck,Delay,Check,{MC,ME,MW,MN,GC,GE,GW,GN,StateString});
				%%found
				[M|_]->
					case M:get_property(?CLASS) of
						%%instance is a group
						{ok,{?CLASS,group}}->
							%%call group sum function
							{C2,E2,W2,N2,C3,E3,W3,N3,SS4} = THIS:check_group(M,DeepCheck,Delay,Check,{MC,ME,MW,MN,GC,GE,GW,GN,StateString}),
							check_ids(T,DeepCheck,Delay,Check,{C2,E2,W2,N2,C3,E3,W3,N3,SS4});
						_->
							check_ids(T,DeepCheck,Delay,Check,check_monitor(M, Check, {MC,ME,MW,MN,GC,GE,GW,GN,StateString}))
					end
			end;
		true ->
			case Id:get_property(?CLASS) of
				%%instance is a group
				{ok,{?CLASS,group}}->
					%%call group sum function
					{C2,E2,W2,N2,C3,E3,W3,N3,SS4} = THIS:check_group(Id,DeepCheck,Delay,Check,{MC,ME,MW,MN,GC,GE,GW,GN,StateString}),
					check_ids(T,DeepCheck,Delay,Check,{C2,E2,W2,N2,C3,E3,W3,N3,SS4});
				_->
					check_ids(T,DeepCheck,Delay,Check,check_monitor(Id, Check, {MC,ME,MW,MN,GC,GE,GW,GN,StateString}))	
			end
	end.
	
%%group check function
check_group(G,DeepCheck,Delay,Check,{MC,ME,MW,MN,GC,GE,GW,GN,StateString})->
	case DeepCheck of
		%%all monitor in the group should be summed
		true->
			%%get the child list of the group
			Mons = G:get_childs(),
			%%check
			check_ids(Mons,DeepCheck,Delay,Check,{MC,ME,MW,MN,GC,GE,GW,GN,StateString});
		%%just evaluate the group as one entity 
		_->
			case G:get_attribute(?CATEGORY) of
				{ok,{_,error}}->
					{ok, {_, SS5}} = G:get_property(?NAME),
					{MC,ME,MW,MN,GC+1,GE+1,GW,GN,StateString++"<br>"++SS5++": error"};
				{ok,{_,nodata}}->
					{ok, {_, SS5}} = G:get_property(?NAME),
					{MC,ME,MW,MN,GC+1,GE+1,GW,GN,StateString++"<br>"++SS5++": nodata"};
				{ok,{_,warning}}->
					{MC,ME,MW,MN,GC+1,GE,GW+1,GN,StateString};
				_->
					{MC,ME,MW,MN,GC+1,GE,GW,GN+1,StateString}
			end
	end.
	
%% @spec get_classifier(error) -> List
%% @type List = [Tuple]
%% @type Tule = {status, Logic, Value}
%% @type Logic = '!=' | '==' | '>' | '<' | 'contain
%% @type Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{itemsInError,'>',0}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{itemsInError,'>',0}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{itemsInError,'==',0}]
	end.

%% @spec verify(Params) -> {ok, []} | {error, Reason}
%% @type Params = [term()]
%% @type Reason = string()
%% @doc verify is the function called by schedule to verify the parameter of monitor inputed by user
%%	composite monitor verify delay time
verify(Params)->
io:format("fffffffffffffffffffffffff~p~n",[Params]),
    Errs = 
    case proplists:get_value(item,Params) of
		undefined->
			[{item,"item is null"}];
		[[]]->
			[{item,"item is null"}]; 
		_->
			[]
	end ++ 
	case proplists:get_value(delay,Params) of
    ""->
	    [{delay,"Delay is missing."}];
    Delay->
		if
			not is_number(Delay) ->
				[{timeout,"Delay must be a number."}];
			true->
				if 
					Delay >= 0 ->
						[];
					Delay < 0 ->
					[{timeerror,"Delay must be greater than zero"}]
				end
					
		end
	end ++
	case BASE:verify(Params) of
		{error,E}->
			E;
		_->
			[]
	end,	

	if 
		length(Errs)>0 ->
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
	 #property{name=checkSequentially,title="Run Monitors",type=bool,editable=true,order=1,advance=true,description="Run each monitor before checking."},
	 #property{name=delay,title="Monitor Delay",type=numeric,editable=true,order=2,advance=true,default=5,description="If running each monitor, delay in seconds between monitors."},
	 #property{name=deepCheck,title="Check All Monitors in Group(s)",type=bool,editable=true,order=3,advance=true,description="By default, a group is counted as a single item when checking status.<br>If this box is checked, all of the monitors in selected groups (and their subgroups) are checked and counted towards the totals."}
	].
	