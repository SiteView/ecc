%% ---
%% rule
%%
%%---
-module(rule,[BASE]).
-compile(export_all).
-extends(siteview_object).

-include("monitor.hrl").

new()->
	Obj = siteview_object:new(),
	Obj:set_attribute(ruleGroup,2),
	Obj:set_attribute(stopOnMatch,false),
	Obj:set_attribute(isDefaultRule,false),
	Obj:set_attribute(includeFilter,[]),
	Obj:set_attribute(excludeFilter,[]),
	{?MODULE,Obj}.

init(This,Data)->
	BASE:init(This,Data),
	case proplists:get_value(id,Data) of
		undefined->
			pass;
		Id->
			%%io:format("~p~n",[Id]),
			% This:remove_attributes(),
			This:set_attribute(ruleGroup,2),
			dbcs_base:set_app(proplists:get_value(?APP,Data),true),
			siteview:set_object(Id,rule,element(1,This),This)
	end.
	

inc_alert_count(Id)->
	case THIS:get_attribute(Id) of
		{ok,{_,V}}->
			THIS:set_attribute(Id,V+1);
		_->
			THIS:set_attribute(Id,1)
	end.
	
get_alert_key(MId)->
	lists:flatten(io_lib:format("Alert-~p",[MId])).

doAction({group_all,Group,Monitor})->
	%~ {ok,{_,Id}} = Group:get_property(?ID),
	{ok,{_,Id}} = THIS:get_property(?ID),
	AId = THIS:get_alert_key(Id),
	THIS:inc_alert_count(AId),
	AlertCount = THIS:get_attribute_as_number(AId),
	if
		AlertCount == 1 ->
			THIS:doAction(Monitor);
		true->
			{error,do_nothing}
	end;
doAction({group_times,N,Group,Monitor})->
	%~ {ok,{_,Id}} = Group:get_property(?ID),
	{ok,{_,Id}} = THIS:get_property(?ID),
	AId = THIS:get_alert_key(Id),
	THIS:inc_alert_count(AId),
	AlertCount = THIS:get_attribute_as_number(AId),
	if
		N == AlertCount ->
			THIS:doAction(Monitor);
		true->
			{error,do_nothing}
	end;
doAction({always_times,N,Monitor})->
	%~ {ok,{_,Id}} = Monitor:get_property(?ID),
	{ok,{_,Id}} = THIS:get_property(?ID),
	AId = THIS:get_alert_key(Id),
	THIS:inc_alert_count(AId),
	AlertCount = THIS:get_attribute_as_number(AId),
	if
		AlertCount + 1 > N ->
			THIS:doAction(Monitor);
		true->
			{error,do_nothing}
	end;
doAction({select_times,N,S,Monitor})->
	%~ {ok,{_,Id}} = Monitor:get_property(?ID),
	{ok,{_,Id}} = THIS:get_property(?ID),
	AId = THIS:get_alert_key(Id),
	THIS:inc_alert_count(AId),
	AlertCount = THIS:get_attribute_as_number(AId),
	if
		N == AlertCount orelse ((AlertCount-N) rem S == 0) ->
			THIS:doAction(Monitor);
		true->
			{error,do_nothing}
	end;
doAction({match_times,N,Monitor})->
	%%{ok,{_,Id}} = Monitor:get_property(?ID),
	%%  The original is a monitor with ID KEY, the number of memories, now revised to Rule ID
	{ok,{_,Id}} = THIS:get_property(?ID),
	AId = THIS:get_alert_key(Id),
	THIS:inc_alert_count(AId),
	AlertCount = THIS:get_attribute_as_number(AId),
	%~ io:format("id:~p~n",[THIS:get_property(?ID)]),
	%~ io:format("match_times:~p~n",[{N,Id,AId,AlertCount}]), 
	if
		N == AlertCount ->
			THIS:doAction(Monitor);
		true->
			{error,do_nothing}
	end;
doAction(Monitor)->
        Monitor:set_attribute('alert',1),
	%~ io:format("doAction:~p~n",[Monitor:get_attribute('alert')]),
	ActParam = case THIS:get_property(action_param) of
					{ok,{action_param,Val}}->
						Val;
					_->
						[]
				end,
	case THIS:get_property(action) of 
		{ok,{action,Act}}->
			{ok,{_,Enabled}} = THIS:get_property(enabled),
			case THIS:get_property(disabled) of
				{ok,{_,true}}->
					{error,"disabled"};
				_->
					case THIS:check_enabled(Enabled) of
						false ->
							{error,"time_disabled"};
						_->
							Action = Act:new(Monitor,THIS),
							%Action:init(Action,ActParam),
							%Action:set_rule(THIS),
							%Action:set_monitor(Monitor),
							try
							(catch api_itsm:create_incident_alert(Monitor,THIS)),
							Ret = Action:trigger(Action),
							% io:format("rule:doAction:~p~n",[Ret]),
							THIS:set_alert_category(Monitor),
							Ret
							catch
								_:Err->
									{error,Err}
							% after
								% Action:delete()
							end
					end
			end;
		_->
			{error,"not found action"}
	end.

getStopOnMatch()->
	case THIS:get_attribute(stopOnMatch) of
		{ok,Val}->
			Val;
		_->
			{stopOnMatch,false}
	end.


setRuleGroup(1)->
	THIS:set_attribute(stopOnMatch,true),
	THIS:set_attribute(ruleGroup,1);
setRuleGroup(2)->
	THIS:set_attribute(stopOnMatch,false),
	THIS:set_attribute(ruleGroup,2);
setRuleGroup(Num)->
	THIS:set_attribute(ruleGroup,Num).


get_id()->
	{ok,{id,Id}} = THIS:get_property(id),
	Id.
%%getFullID()->
%%	BASE:getFullID().

%% @spec match(Obj)->(true | false)
%% @doc whether the monitor is match the rule
%%
match(Obj)->
	{ok,{_,NameMatch}} = THIS:get_property(name_match),
	{ok,{_,StatusMatch}} = THIS:get_property(status_match),
	{ok,{_,TypeMatch}} = THIS:get_property(type_match),
	{ok,{_,MonitorName}} = Obj:get_property(?NAME),
	{ok,{_,Status}} = Obj:get_attribute(?STATE_STRING),
	{ok,{_,Class}} = Obj:get_property(?CLASS),
	case TypeMatch of
		"any"->
			true;
		_->
			case list_to_atom(TypeMatch) of
				Class->
					true;
				_->
					false
			end
	end and
	case NameMatch of
		""->
			true;
		_->
			case re:run(MonitorName,NameMatch) of
				nomatch->
					false;
				_->
					true
			end
	end and
	case StatusMatch of
		""->
			true;
		_->
			case re:run(Status,StatusMatch) of
				nomatch->
					false;
				_->
					true
			end
	end.
	
%% @spec check_enabled(Enabled)->(true | false)
%% where
%%	Enabled = (true | {false,permanently} | {false,{next,From,N,Unit}} |  {false,{schedule,Ft,Fd,Tt,Td}} )
%%	From = string()
%%	N = integer()
%%	Unit = string()
%%	Ft = string()
%%	Fd = string()
%%	Tt = string()
%%	Td = string()
%% @doc check a alert is enable or disabled,From is a time when this action is set,format is {{Y,M,D},{HH,MM,SS}},N is a counter for Unit,
%% when N is 1 and Unit is "minutes",it means 1 minute.Unit can be "minutes","Ft and Tt is time string as "12:30:59",Fd and Td is date string as "2009-11-12".
%%
check_enabled(Enabled)->
	case Enabled of
		true ->
			true;
		{false,permanently}->
			false;
		{false,{next,From,C,"minutes"}}->
			F = sv_datetime:time(From) + C*60*1000,
			Now = sv_datetime:now(),
			if
				Now > F ->
					true;
				true ->
					false
			end;
		{false,{next,From,C,"hours"}}->
			F = sv_datetime:time(From) + C*60*60*1000,
			Now = sv_datetime:now(),
			if
				Now > F ->
					true;
				true ->
					false
			end;
		{false,{next,From,C,"days"}}->
			F = sv_datetime:time(From) + C*24*60*60*1000,
			Now = sv_datetime:now(),
			if
				Now > F ->
					true;
				true ->
					false
			end;
		{false,{schedule,Ft,Fd,Tt,Td}}->
			[HHF,MMF|_] = [list_to_integer(X)|| X<-string:tokens(Ft,":")],
			[YF,MF,DF|_] = [list_to_integer(X)|| X<-string:tokens(Fd,"/-")],
			[HHT,MMT|_] = [list_to_integer(X)|| X<-string:tokens(Tt,":")],
			[YT,MT,DT|_] = [list_to_integer(X)|| X<-string:tokens(Td,"/-")],
			From = sv_datetime:time({{YF,MF,DF},{HHF,MMF,0}}),
			To = sv_datetime:time({{YT,MT,DT},{HHT,MMT,0}}),
			Now = sv_datetime:now(),
			io:format("gggggggggggggggggggggggggggggggggggggggFt~p~n Fd~p~n Tt~p~n Td~p~n  From~p~n  To~p~n Now~p~n",[Ft,Fd,Tt,Td,From,To,Now]),
			if
				Now < From orelse Now > To ->
					true;
				true ->
					false
			end;
		Any->
			true
	end.
	
get_alert_category(Monitor)->
	{ok,{_,Id}} = Monitor:get_property(?ID),
	 case THIS:get_attribute(atom_to_list(Id) ++ "-category") of	
		{ok,{_,V}}->
			V;
		_->
			undefined
	end.
%~ oldhand increase for the five alarm code category has discarded use alert_level.
get_alert_alert_level(Monitor)->
	{ok,{_,Id}} = Monitor:get_property(?ID),	
	case THIS:get_attribute(atom_to_list(Id) ++ "-alert_level") of
		{ok,{_,V}}->
			V;
		_->
			undefined
	end.	
	
set_alert_category(Monitor)->
	{ok,{_,Id}} = Monitor:get_property(?ID),
	{ok,{_,Category}} = Monitor:get_attribute(?CATEGORY),
	THIS:set_attribute(atom_to_list(Id) ++ "-category",Category).
	
%% trouble recover
do_recover(Monitor)->
	% io:format("alert recover:~p~n",[Monitor:get_property(id)]),
	%~ io:format("alert recover:~p~n",[Monitor:get_attribute(alert)]),
	case Monitor:get_attribute(alert) of 
	      {ok,{_,1}}->
	        Monitor:set_attribute('alert',0),
		case THIS:get_property(action) of 
			{ok,{action,Act}}->
				{ok,{_,Enabled}} = THIS:get_property(enabled),
				case THIS:get_property(disabled) of
					{ok,{_,true}}->
						{error,"disabled"};
					_->
						case THIS:check_enabled(Enabled) of
							false ->
								{error,"time_disabled"};
							_->
								Action = Act:new(Monitor,THIS),
								
								try 
									Ret = Action:recover(Action),
									THIS:set_alert_category(Monitor),
									Ret
								catch
									_:Err->{error,Err}
								% after
								% Action:delete()
								end
								
						end
				end;
			_->
				{error,"not found action"}
		end;
		_ ->
		  {error,"normal"}
	end.
	