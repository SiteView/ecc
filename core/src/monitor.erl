%% 
%% @doc monitor base class
%% @author shixianfang<xianfang.shi@dragonflow.com>
%% @version {0.1}
%%
-module(monitor,[BASE]).
-compile(export_all).
-extends(siteview_object).
-include("monitor.hrl").
-include("monitor_template.hrl").
-include("classifierstring.hrl").

-define(Max,10).
-define(MBox,eccadmin).
-define(OfbizNode,server_conf:get_ofbiz_node()).

%% @spec new()->Object
%% @doc monitor construct class
%% 
new()->
	Obj = siteview_object:new(),
	Obj:set_attribute(?LAST_UPDATE,0),
	Obj:set_attribute(?LAST_CATEGORY,nodata),
	Obj:set_attribute(?CATEGORY,nodata),
	Obj:set_attribute(?STATE_STRING,""),
	Obj:set_attribute(?RULES,[]),
%% 	erlide_log:log("Create new monitor: " ++ atom_to_list(?MODULE)),
	{?MODULE,Obj}.


%% @spec get_parent()->{ok,{parent,Parent}} | {error,Resean}
%% @doc get parent of this object 
%%
get_parent() -> THIS:get_owner().


%% @spec set_parent(Val) -> (true | false)
%% @doc set parent of this object
%% 
%%
set_parent(Val)->
	THIS:set_owner(Val).

%% @spec verify(_)->({ok,Result}|{error,List})
%% @doc base function for subclass
%%
verify(_)-> {ok,""}.

%% @spec is_property_excluded(Prop)->(true|false)
%% @doc whether a property is excluded
%%
is_property_excluded(_)->false.

%% @spec isDisabled() -> true | false
%% @doc is monitor disabled
%%
isDisabled()->
	case THIS:whyDisabled() of
		""->
			false;
		_->
			true
	end.

%% @spec get_schedule()-> ({schedule,Sch} | {error,Reseaon})
%% @doc get this monitor's schedule 
%% Sch = term()
get_schedule()->
	schedule_manager:get_schedule(THIS).


%% @spec getReports()->[]
%% @doc base function for subclass
%%
getReports()->api_report:get_all().

%% @spec is_schedule_enabled()-> (true | false)
%% @doc is this monitor's schedule enabled
%%
is_schedule_enabled()->
	schedule_property:is_enabled(THIS:get_schedule()).
	


%% @spec save_monitor()->({ok,Result}|{error,Reason})
%% @doc save the monitor's data to database
%%
save_monitor()->
	%[{id,Id}|_]=THIS:get_attribute(id),
	M = THIS:get_properties(),
	dbcs_monitor:update_monitor(M).
	%%file:write_file(io_lib:format("monitor/~p",[Id]),io_lib:format("~p.",[M])).

%% @spec startMonitor(_)->ok.
%% @doc base function for subclass
%%
startMonitor(_)->
	ok.

%% @spec stopMonitor(_)->ok
%% @doc base function for subclass
%%
stopMonitor(_)->
	ok.

%% @spec default_title()->Title
%% where
%% 	Title = string()
%% @doc monitor's default title
%% 
defaultTitle(Params)->
	% case proplists:get_value(?NAME,Params) of
	%	""->
			case proplists:get_value(?CLASS,Params) of
				undefined->
					"error";
				Key->
					api_monitor_template:get_template_name(Key)
			end.
	%		end;
	%	Name->
	%		Name
	% end.


printableAlertEntry()->ok.

getReportsById(Ret,[],_)->
	Ret;
getReportsById(Ret,[H|E],Monitorid)->
	Report=H,
	{value,{_,Monitors}}=lists:keysearch(monitors, 1, Report),
	Mlist=string:tokens(Monitors,","),
	ISC=lists:any(fun(X)->if X=:=Monitorid ->true;true->false end end , Mlist),
	case ISC of
		true ->
		   {_,{_,Reportid}}=lists:keysearch(id, 1, Report),
	       {_,{_,ReportTitle}}=lists:keysearch(title, 1, Report),
			TRet=Ret++[[{id,Reportid},{title,ReportTitle}]];
		false->
			TRet=Ret
	end,
	getReportsById(TRet,E,Monitorid).
printTableReportEntry()->
	Reports=THIS:getReports(),
	{ok,{id,Id}} = THIS:get_property(id),
   ReportInfos=getReportsById([],Reports,atom_to_list(Id)).

printTableStatusEntry()->ok.

getCostInLicensePoints()->0.

signalMonitor()->ok.

%% @spec getScalarValues(Prop,_)-> [Terms]
%% @doc get scalar property's value 
%%
getScalarValues(Prop,_)->
	case Prop of
		?DEPENDS_CONDITION ->
			[{"good","good"},{"warning","warning"},{"error","error"}];
		?DEPENDS_ON ->
			SV = siteview:get_current_siteview(),
			Monitors =SV:getMonitors(),
			F = fun(X)->
					{ok,{id,Id}} = X:get_property(id),
					{X:get_full_name(),atom_to_list(Id)}
				end,
			[{"none","none"}]++lists:map(F,Monitors);
		_->
			[]
	end.

%% @spec resetCategoryProperties(S)->({ok,Result}|{error,Reason})
%% @doc reset category related properties
%%
resetCategoryProperties(S)->
	case S of
		good->
			THIS:set_attribute(goodCount,0),
			THIS:set_attribute(goodTimeSinceFirst,0);
		error->
			THIS:set_attribute(errorCount,0),
			THIS:set_attribute(errorTimeSinceFirst,0),
			THIS:set_attribute(warningCount,0);
		warning->
			THIS:set_attribute(warningTimeSinceFirst,0),
			THIS:set_attribute(errorCount,0),
			THIS:set_attribute(warningCount,0)
	end,
	THIS:remove_attribute_match("AlertCount"),
	THIS:remove_attribute_match("TimeSinceAlert").

%% @spec whyDisabled()->string()
%% @doc why monitor is disabled
%%
whyDisabled()->
	W = case THIS:get_owner() of
		{ok,{parent,P}}->
			P:whyDisabled();
		_->
			""
	end,
	case W of
		""->
			case THIS:get_property(?DISABLED) of
				{ok,{?DISABLED,true}}->
					"disabled " ++ case THIS:get_property(?DISABLED_DESCRIPTION) of
									{ok,{?DISABLED_DESCRIPTION,Desc}}->
										" " ++ Desc;
									_->
										""
									end;
				_->

					case THIS:get_property(?TIMED_DISABLE) of
						{ok,{?TIMED_DISABLE,{Start,End}}}->
							Now = sv_datetime:now(),
							Tms = sv_datetime:time(Start),
							Tme = sv_datetime:time(End),
							if
								(Now >Tms ) and (Now < Tme)->
									"timed disable" ++ " until " ++sv_datetime:now2str(Tme)++
									case THIS:get_property(?DISABLED_DESCRIPTION) of
															{ok,{?DISABLED_DESCRIPTION,Desc}}->
																" " ++ Desc;
															_->
																""
															end;
								true ->
									""
							end;
						_->
							case THIS:is_schedule_enabled() of
								true->
									"";
								false->
									"disabled by schedule"
							end
					end
			end;
		_->
			W
	end.

%% @spec get_classifier(_)->list()
%% @doc get classifier information
%%
get_classifier(_)->[].

%% @spec classifier(This,[R|T])-> (true | false)
%% @doc classify monitor's category
%%
% classifier(_,[])->false;
% classifier(This,[R|T])->
	% case This:classifier_item(This,R) of
		% false ->
			% This:classifier(This,T);
		% _->
			% true
	% end.
	
preclassifier(_,[],R)->R;
preclassifier(_,['and'],R)->R;
preclassifier(_,['or'],R)->R;
preclassifier(This,['and'|T],R)->
	preclassifier(This,T,R ++ ['and']);
preclassifier(This,['or'|T],R)->
	preclassifier(This,T,R ++ ['or']);
preclassifier(This,[I|T],R)->
	preclassifier(This,T,R++[This:classifier_item(This,I)]).
	
classifier(This,Conds)->
	Exp = preclassifier(This,Conds,[]),
	% io:format("Exp:~p~n",[Exp]),
	bool_exp:parse(Exp).

%% @spec str2num(Str)->(float|integer)
%% @doc convert string to float or integer
%%
str2num(Str)->
	case string:to_float(Str) of
		{F,[]}->
			F;
		_->
			case string:to_integer(Str) of
				{I,[]}->
					I;
				_->
					Str
			end
	end.

%% @spec num2str(Num)->string()
%% @doc convert number to string
%%
num2str(Num)->
	if
		is_float(Num)->
			float_to_list(Num);
		is_integer(Num)->
			integer_to_list(Num);
		true ->
			Num
	end.

%% @spec classifier_item(This,R)->(true|false)
%% @doc caculate a classifier item's value
%%
classifier_item(This,R)->
	case element(1,R) of
		always ->
			true;
		alwaysfalse ->
			false;
		P->
			%io:format("classifier_item:~p~n",[P]),
			case This:get_attribute(P) of
				{ok,{P,"n/a"}}->
					%io:format("value is n/a~n"),
					true;
				{ok,{P,V}}->
					case element(2,R) of
						'>'->
							if							
								(not is_number(V)) andalso (not is_number(element(3,R)))->
									str2num(V) > str2num(element(3,R));
								(not is_number(V))->
									str2num(V) > element(3,R);
								(not is_number(element(3,R)))->
									V >str2num(element(3,R));
								true ->
									V>element(3,R)
							end;
						'>='->
							if
								(not is_number(V)) andalso (not is_number(element(3,R)))->
									str2num(V) >= str2num(element(3,R));
								(not is_number(V))->
									str2num(V) >= element(3,R);
								(not is_number(element(3,R)))->
									V >=str2num(element(3,R));
								true ->
									V>=element(3,R)
							end;
							%V>=element(3,R);
						'<'->
							if
								(not is_number(V)) andalso (not is_number(element(3,R)))->
									str2num(V) < str2num(element(3,R));
								(not is_number(V))->
									str2num(V) < element(3,R);
								(not is_number(element(3,R)))->
									V <str2num(element(3,R));
								true ->
									V<element(3,R)
							end;
							%V < element(3,R);
						'<='->
							if
								(not is_number(V)) andalso (not is_number(element(3,R)))->
									str2num(V) =< str2num(element(3,R));
								(not is_number(V))->
									str2num(V) =< element(3,R);
								(not is_number(element(3,R)))->
									V =<str2num(element(3,R));
								true ->
									V=<element(3,R)
							end;
							%L =  element(3,R),
							%(V< L) or (V =:= L);
						'!='->
							if
								is_list(V) andalso is_list(element(3,R))->
									V=/= element(3,R);
								is_number(V) andalso is_number(element(3,R))->
									V=/= element(3,R);
								is_number(V) ->
									V=/=str2num(element(3,R));
								is_number(element(3,R))->
									str2num(V)=/= element(3,R);
								true ->
									V=/= element(3,R)
							end;
						'=='->
							if
								is_list(V) andalso is_list(element(3,R))->
									V== element(3,R);
								is_number(V) andalso is_number(element(3,R))->
									V== element(3,R);
								is_number(V) ->
									V==str2num(element(3,R));
								is_number(element(3,R))->
									str2num(V)== element(3,R);
								true ->
									V== element(3,R)
							end;
							%V =:= element(3,R);
						'contains'->
							if
								is_number(V) andalso is_number(element(3,R))->
									string:rstr(num2str(V),num2str(element(3,R)))>0;
								is_number(V)->
									string:rstr(num2str(V),element(3,R))>0;
								is_number(element(3,R))->
									string:rstr(V,num2str(element(3,R)))>0;
								true ->
									string:rstr(V,element(3,R)) > 0
							end;
						'!contains'->
							if
								is_number(V) andalso is_number(element(3,R))->
									string:rstr(num2str(V),num2str(element(3,R)))=<0;
								is_number(V)->
									string:rstr(num2str(V),element(3,R))=<0;
								is_number(element(3,R))->
									string:rstr(V,num2str(element(3,R)))=<0;
								true ->
									string:rstr(V,element(3,R)) =< 0
							end;
							%string:rstr(V,element(3,R)) =< 0;
						_->
							false
					end;
				_->
					false
			end
	end.

%% splitClassifier([], RetTime, Ret)->
%% 	{RetTime, Ret};
%% splitClassifier([H|T], RetTime, Ret)->	
%% 	case erlang:is_tuple(H) of
%% 		true ->
%% 		   case size(H) of
%% 				5->
%% 					splitClassifier(T, RetTime ++ [H], Ret);
%% 				_->
%% 					splitClassifier(T, RetTime, Ret ++ [H])
%% 				end;
%% 		_->
%% 			splitClassifier(T, RetTime, Ret)
%% 	end.
%% 
%% classifier_default(_, [], Operate, Ret)->
%%   Ret;
%% classifier_default(Default, [H|T], Operate, Ret)->
%% 	case erlang:is_tuple(H) of
%% 		true ->
%% 		   case size(H) of
%% 			3->
%% %% 				error
%% 				{Name, Operation, Value} = H,
%% 				case Default =:= H of
%% 					true->
%% 						case Operate of
%% 							'and'->
%% %% 								io:format("--------classifier_default-and------------------------------:~p~n", [H]),
%% 								classifier_default(Default, T, Operate ,{always});
%% 							'or'->
%% %% 								io:format("--------classifier_default--or-----------------------------:~p~n", [H]),
%% 								classifier_default(Default, T, Operate ,{alwaysfalse});
%% 							_->
%% %% 								io:format("--------classifier_default-???------------------------------:~p~n", [H]),
%% 								classifier_default(Default, T, Operate ,Ret)
%% 						end;
%% 					_->
%% 						classifier_default(Default, T, Operate ,Ret)
%% 				end;
%% 			4->
%% 				{Name, Operation, Value, Schedule} = H,
%% 				case ({Name, Operation, Value} =:= Default) and ((Schedule =:= "7x24") or (Schedule =:= " ")) of
%% 					true->
%% 						case Operate of
%% 							'and'->
%% %% 								io:format("--------classifier_default1-and------------------------------:~p~n", [H]),
%% 								classifier_default(Default, T, Operate ,{always});
%% 							'or'->
%% %% 								io:format("--------classifier_default2--or-----------------------------:~p~n", [H]),
%% 								classifier_default(Default, T, Operate ,{alwaysfalse});
%% 							_->
%% %% 								io:format("--------classifier_default3-???------------------------------:~p~n", [H]),
%% 								classifier_default(Default, T, Operate ,Ret)
%% 						end;
%% 					_->
%% 						classifier_default(Default, T, Operate ,Ret)
%% 				end;
%% 			_->
%% 				classifier_default(Default, T, Operate, Ret)
%% 		   end;
%% 		_->
%% %% 			io:format("--------classifier_default4-------------------------------:~p~n", [H]),
%% 			classifier_default(Default, T, H, Ret)
%%   	end.

compute_classifier(_, [], Operate, Ret)->
  Ret;
compute_classifier(Classifier, [H|T], Operate, Ret)->
	case erlang:is_tuple(H) of
		true ->
%% 		   case size(H) of
%% 			5->
%% 				{PropName, Operation, Value, StartTime, EndTime} = H,
				case Classifier =:= H of
					true->
						case Operate of
							'and'->
%% 								io:format("--------classifier_outoftime-and------------------------------:~p~n", [H]),
								compute_classifier(Classifier, T, Operate ,{always});
							'or'->
%% 								io:format("--------classifier_outoftime--or-----------------------------:~p~n", [H]),
								compute_classifier(Classifier, T, Operate ,{alwaysfalse});
							_->
%% 								io:format("--------classifier_outoftime--???-----------------------------:~p~n", [H]),
								compute_classifier(Classifier, T, Operate ,Ret)
						end;
					_->
						compute_classifier(Classifier, T, Operate ,Ret)
				end;
%% 			_->
%% 				classifier_outoftime(In, T, Operate, Ret)
%% 		   end;
		_->
%% 			io:format("--------classifier_outoftime4-------------------------------:~p~n", [H]),
			compute_classifier(Classifier, T, H, Ret)
  	end.

get_firstrelation(Classifier, Ret)->
	case erlang:length(Ret) > 2 of
		true->
			H = lists:nth(1, Ret),
			case erlang:is_tuple(H) of 
				true->
					case  Classifier=:= H of
						true->
							Operate= lists:nth(2, Ret),
							Operate;
						_->
							no
					end;
				_->
					no
			end;
		_->
			no
	end.

proc_valid_classifier([], RetSrc, Ret)->
	Ret;
proc_valid_classifier([H|T], RetSrc, Ret)->
	Relation = get_firstrelation(H, RetSrc),
%% 	io:format("get_firstrelation: ~p~n", [Relation]),
	TrueOrFalse = compute_classifier(H, RetSrc, Relation, {alwaysfalse}),
	F = fun(X,Acc1)->
		{X1, Value} = Acc1,
		case X1 =:= X of
			true->
				{Value, Acc1};
			_->
				{X, Acc1}
			end
	end,	
	{RetReplace, _} = lists:mapfoldl(F, {H, TrueOrFalse}, Ret),
	proc_valid_classifier(T, RetSrc, RetReplace).

get_default_firstrelation(Classifier, Ret)->
	case erlang:length(Ret) > 2 of
		true->
			H = lists:nth(1, Ret),
			case erlang:is_tuple(H) of 
				true->
					case (element(1, Classifier) =:= element(1, H)) and (element(4, Classifier) =:= element(4, H)) of
						true->
							Operate= lists:nth(2, Ret),
							Operate;
						_->
							no
					end;
				_->
					no
			end;
		_->
			no
	end.
	
compute_default_classifier(_, [], Operate, Ret)->
  Ret;
compute_default_classifier(Classifier, [H|T], Operate, Ret)->
	case erlang:is_tuple(H) of
		true ->
		   case size(H) of
			4->
%% 				{Name, Operation, Value, Scheudle} = H,
				case (element(1, Classifier) =:= element(1, H)) and (element(4, Classifier) =:= element(4, H)) of
					true->
						case Operate of
							'and'->
%% 								io:format("--------classifier_outoftime-and------------------------------:~p~n", [H]),
								compute_default_classifier(Classifier, T, Operate ,{always});
							'or'->
%% 								io:format("--------classifier_outoftime--or-----------------------------:~p~n", [H]),
								compute_default_classifier(Classifier, T, Operate ,{alwaysfalse});
							_->
%% 								io:format("--------classifier_outoftime--???-----------------------------:~p~n", [H]),
								compute_default_classifier(Classifier, T, Operate ,Ret)
						end;
					_->
						compute_default_classifier(Classifier, T, Operate ,Ret)
				end;
			_->
				compute_default_classifier(Classifier, T, Operate, Ret)
		   end;
		_->
%% 			io:format("--------classifier_outoftime4-------------------------------:~p~n", [H]),
			compute_default_classifier(Classifier, T, H, Ret)
  	end.

proc_default_classifier([], RetSrc, Ret)->
	Ret;
proc_default_classifier([H|T], RetSrc, Ret)->
	Relation = get_default_firstrelation(H, RetSrc),
%% 	io:format("get_default_firstrelation: ~p~n", [Relation]),	
	TrueOrFalse = compute_default_classifier(H, RetSrc, Relation, {alwaysfalse}),
	F = fun(X,Acc1)->
		{X1, Value} = Acc1,
%% 		case X1 =:= X of
		case erlang:is_tuple(X) of
			true ->
		   		case size(X) of
	 			  4->		
					case (element(1, X1) =:= element(1, X)) and (element(4, X1) =:= element(4, X)) of		
						true->
							{Value, Acc1};
						_->
							{X, Acc1}
					end;
				  _->
					  {X, Acc1}
				end;
			_->
				{X, Acc1}
		end
	end,	
	{RetReplace, _} = lists:mapfoldl(F, {H, TrueOrFalse}, Ret),
	proc_default_classifier(T, RetSrc, RetReplace).

confict_Classifier([], RetSrc, Ret)->
	Ret;
confict_Classifier([H|T], RetSrc, Ret)->
%% 	{Name, Operation, Value, Schedule} = H,
%% 	case schedule_property:is_enabled(Schedule) of
%% 		true->
%% 			ok;
%% 		_->
%% 			ok
%% 	end,
%% 	io:format("--------confict_Classifier--in time-----------------------------:~p~n", [H]),
	{Name, Operation, Value, Schedule} = H,
	
%% 	{{Year, Month, Day}, {Hour, Minutes, Seconds}} = calendar:local_time(),
%% 	case  (Hour >= StartTime) and (Hour =< EndTime) of
	case schedule_property:measurement_is_enabled(schedule_manager:get_measurement_schedule_filter(Schedule)) of
		true->
%% 			io:format("--------confict_Classifier--in time-----------------------------:~p~n", [H]),
%% 			case lists:keysearch(Name, 1, Ret) of
%% 				false ->
%% 					computeClassifier(T, RetSrc, Ret ++ [{Name, Operation, Value}]);
%% 				_ ->
%% 					computeClassifier(T, RetSrc, lists:keyreplace(Name, 1, Ret, {Name, Operation, Value}))
%% 			end;			
			case Schedule of
				"7x24"->
					Conditions= [{Name, Operation, Value, " "}];
%% 				" "->
%% 					Conditions= [{Name, Operation, Value}, {Name, Operation, Value, " "}, {Name, Operation, Value, "7x24"}];
				_->
					Conditions= [{Name, Operation, Value, " "}, {Name, Operation, Value, "7x24"}]
			end,
			RetReplace = proc_default_classifier(Conditions, RetSrc, Ret),
			confict_Classifier(T, RetSrc, RetReplace);
		_->	
%% 			io:format("--------confict_Classifier-out time In------------------------------:~p~n", [H]),
%% 			TrueOrFalse = classifier_outoftime(H, RetSrc, 'or', {alwaysfalse}),
%% 			F = fun(X,Acc1)->
%% 				{X1, T1} = Acc1,
%% 				case X1 =:= X of
%% 					true->
%% 						{T1, Acc1};
%% 					_->
%% 						{X, Acc1}
%% 				end
%% 			end,	
%% 			{RetReplace, _} = lists:mapfoldl(F, {H, TrueOrFalse}, Ret),
			Conditions = [H],
			RetReplace = proc_valid_classifier(Conditions, RetSrc, Ret),
%% 			io:format("--------confict_Classifier-out time------------------------------:~p~n", [RetReplace]),
			confict_Classifier(T, RetSrc, RetReplace)
	end.

time_Classifier([], RetSrc, Ret)->
	Ret;
time_Classifier([H|T], RetSrc, Ret)->
	{Name, Operation, Value, Schedule} = H,	
	F = fun(X,Acc1)->
		{X1, T1} = Acc1,
		case X1 =:= X of
			true->
				{T1, Acc1};
			_->
				{X, Acc1}
		end
	end,	
	{RetReplace, _} = lists:mapfoldl(F, {H, {Name, Operation, Value}}, Ret),
	time_Classifier(T, RetSrc, RetReplace).

%% @spec classifier_all(This,[C|T])->(true|false)
%% @doc caculate all classifier's value
%%
classifier_all(This,[])->This:set_attribute(?CATEGORY,good);
classifier_all(This,[C|T])->
	R = This:get_classifier(C),
%% 	R = lists:umerge(This:get_classifier(C)),
%% 	io:format("---------------classifier_all--------:~p~n", [R]),
	%%cxy 2011/04/22 Time + Measurement Filter
%% 	{RetTime, Ret} = splitClassifier(R, [], []),
	
	F = fun(X) ->
		case erlang:is_tuple(X) of
			true ->
		   		case size(X) of
					4->
						{Name, Operation, Value, Schedule} = X,
						case Schedule of
							" "->
%% 								io:format("---------------classifier_all kkkkk Schedule--------:~p~n", [X]),
								false;
							_->
								true
						end;
					_->
						false
				end;
			_->
				false
		   end
	end,
	RetTime = lists:filter(F, R),
%% 	RFilter = confict_Classifier(RetTime, R, R),
	Confict_Filter = confict_Classifier(RetTime, R, R),	
	RFilter = time_Classifier(RetTime, R, Confict_Filter),	
	case RetTime of
		[]->
			ok;
		_->
			io:format("---------------classifier_all-R-------:~p~n", [R]),
			io:format("---------------classifier_all-RetTime-------:~p~n", [RetTime]),
			io:format("---------------classifier_all-RFilter-------:~p~n", [RFilter])
	end,
%% 	io:format("---------------classifier_all--------:~p~n", [RFilter]),
	%%io:format("classifier_all:~p~n",[R]),
%% 	case THIS:classifier(This,R) of
	case THIS:classifier(This,RFilter) of
		true->
			THIS:set_attribute(?CATEGORY,C),
			ok;
		false->
			THIS:classifier_all(This,T)
	end.

%% @spec runClassifiers(This)->({ok,Result}|{error,Reason})
%% @doc run monitor's classifiers
%% 
runClassifiers(This)->	
	THIS:classifier_all(This,[error,warning,good]),
	% This:classifier_by_statestring(This),
	THIS:runClassifiers(This,This:get_attribute(?CATEGORY),This:get_attribute(?LAST_CATEGORY)).
	
classifier_by_statestring(This)->
	case This:get_attribute(?STATE_STRING) of
		{ok,{_,State}}->
			StateString = string:strip(State),
			F = fun(X)->re:run(StateString,X) =/= nomatch end,
			
			case lists:any(F,?NODATA_STRINGS) of
				true->
					This:set_attribute(?CATEGORY,?NO_DATA);
				_->
					case lists:any(F,?ERROR_STRINGS) of
						true->
							This:set_attribute(?CATEGORY,error);
						_->
							case lists:any(F,?WARNING_STRINGS) of
								true->
									This:set_attribute(?CATEGORY,warning);
								_->
									pass
							end
					end
			end;
		_->
			pass
	end.

%% @spec runClassifiers(This,{ok,{?CATEGORY,C}},{ok,{?LAST_CATEGORY,LC}})->({ok,Result}|{error,Reason})
%% @doc run monitor's classifiers
%%
runClassifiers(This,{ok,{?CATEGORY,C}},{ok,{?LAST_CATEGORY,LC}})->
	case C of
		LC->
			This:inc_attribute(list_to_atom(atom_to_list(C) ++ "Count"));
		_->
			This:set_attribute(errorCount,0),
			This:set_attribute(warningCount,0),
			This:set_attribute(normalCount,0),
			This:set_attribute(?LAST_CATEGORY,C),
			This:set_attribute(list_to_atom(atom_to_list(C) ++ "Count"),1)
	end,
	This:setParentCategory(This,C);
runClassifiers(_,_,_)->{error,object_state_wrong}.

%% @spec setParentCategory(This,Category)->({ok,Result}|{error,Reason})
%% @doc set parent's category
%%
setParentCategory(This,Category)->
	case This:get_owner() of
		{ok,{parent,Parent}}->
			case Parent:inc_attribute(list_to_atom(atom_to_list(Category) ++ "Count")) of
				{error,attribute_not_found}->
					Parent:set_attribute(list_to_atom(atom_to_list(Category) ++ "Count"),1);
				_->
					pass
			end,
			Childs = Parent:get_childs(),
			Stat = lists:map(fun(X)->
								case X:get_attribute(?CATEGORY) of
									{ok,{?CATEGORY,Categ}}->
										Categ;
									_->
										?ERROR_LOG2("no category:~p~n",[X:get_property(id)]),
										nodata
								end
							end,Childs),
			case lists:member(error,Stat) of
				true ->
					Parent:set_attribute(?CATEGORY,error);
				_->
					case lists:member(warning,Stat) of
						true ->
							Parent:set_attribute(errorCount,0),
							Parent:set_attribute(?CATEGORY,warning);							
						_->
							case lists:member(good,Stat) of
								true ->
									Parent:set_attribute(errorCount,0),
									Parent:set_attribute(warningCount,0),
									Parent:set_attribute(?CATEGORY,good);
								_->
									Parent:set_attribute(warningCount,0),
									Parent:set_attribute(errorCount,0),
									Parent:set_attribute(good,0),
									Parent:set_attribute(?CATEGORY,nodata)
							end
					end
			end,

			Parent:setParentCategory(Parent,Category);
		_->
			{error,not_found_parent}
	end.
		
%% @spec get_counter_attribute()->[]
%% @doc get counter attribute
%%
get_counter_attribute()->[].

%% @spec save_result(This,Category)->({ok,Result}|{error,Reason})
%% @doc save monitor result to log
%%
save_result(This,Category)->
	{ok,{id,Id}}= This:get_property(id),
	{ok,{?STATE_STRING,State}} =  This:get_attribute(?STATE_STRING),
	F = fun(X)->
			case This:get_attribute(X) of
				{ok,Val}->
					Val;
				_->
					{X,not_found}
			end
		end,
	R = lists:map(F,This:getLogProperties(This)),
	{ok,{_,Class}} = This:get_property(?CLASS),
	%%RS = [{monitorid,Id},{time,now()},{?CATEGORY,Category},{desc,State}] ++ R,
	Log = #monitorlog{id=Id,name=This:get_name(),time=erlang:localtime(),category=Category,desc=State,measurement=R,class=Class,groupname=This:get_parent_name()},
    %%for spark line, disable for now  
    %% SP = get_sparkline_data(This,R),
    %% This:set_attribute(sparkline,SP),
    %%io:format("log is:~p~n",[SP]),
	%%log into ofbiz entity store
%%	monitor_logger:log(Log),  
	Params = [{id,Id},{name,This:get_name()},{category,Category},{decription, State},{measurement,R},{time,erlang:localtime()}],
	io:format("dddddddddddddddddddddddddddddddddddddddddddddddddddddddddd~n~p~n",[Params]),
	{?MBox,?OfbizNode} ! {self(),"LogMonitor","monitorListLogger",Params},
	THIS:log_recent({This:get_app(),Log}).  %% update erlang kernel memory data store with the lastest value
%% 	monitor_logger:logdb({Id, #monitorlog{id=Id,name=This:get_name(),time=erlang:localtime(),category=Category,desc=State,measurement=R}}).

get_sparkline_data(This,R) ->
    Original_data = case This:get_attribute(sparkline) of
        {ok,{_,OV}} ->
            OV;
        _ ->
            []
    end,
    filter_sparkline_data(R,Original_data).

filter_sparkline_data([],_)->[];
filter_sparkline_data([{K,RawV}|R],Data) ->
    V = if
        is_number(RawV) ->
            integer_to_list(round(RawV));
        true ->
            "null"
    end,
    Value = proplists:get_value(K,Data,[])++[V],
    NV = if
        length(Value)>?Max ->
            lists:sublist(Value,length(Value)-?Max+1,length(Value));
        true ->
            Value
    end,
    [{K,NV}]++filter_sparkline_data(R,Data);
filter_sparkline_data([_|R],Data) ->
    filter_sparkline_data(R,Data).

log_recent({App,M})->
	SV = siteview:get_current_siteview(),
	case SV:get_attribute(recent_monitor) of
		{ok,{_,V}}->
			if
				length(V)<?MAX_RECENT_MONITOR->
					SV:set_attribute(recent_monitor,[{App,M}]++V);
				true->
					SV:set_attribute(recent_monitor,[{App,M}]++lists:sublist(V,?MAX_RECENT_MONITOR-1))
			end;
		_->
			SV:set_attribute(recent_monitor,[{App,M}])
	end.

%% @spec getParentActionRules()->List
%% @doc get parent's actions
%% 
getParentActionRules()->
	case THIS:get_owner() of
		{ok,{parent,Parent}}->
			Parent:getParentActionRules();
		_->
		[]
	end.

%% @spec getRules(Class)->List
%% @doc get monitor's rules
%%
getRules(Class)->
	case Class of
		2->
			P = THIS:getParentActionRules(),
			S1 = sets:new(),
			{S2,R2} = getRules(S1,P),
			{ok,{id,Id}}= THIS:get_property(id),
			% Rules = lists:map(fun(X)->proplists:get_value(id,X) end,dbcs_rule:get_monitor_rule(Id)),
			% {_,R3} = getRules(S2,find_rule(Rules)),
			{_,R3} = getRules(S2,find_monitor_rule(Id)),			
			R2++R3;
			% case THIS:get_attribute(?RULES) of
			%	{ok,{?RULES,Rules}}->
			%		{_,R3} = getRules(S2,find_rule(Rules)),
			%		R2++R3;
			%	_->
			%		R2
			% end;
		_->
			[]
	end.
	
find_monitor_rule(MId)->
	Rules = siteview:get_object_by_type(rule),
	lists:foldl(fun(X,R)->
				case X:get_property(target) of
					{ok,{_,Target}}-> 					         
					        %~ io:format("find_monitor_rule:~p~n",[{Target,atom_to_list(MId)}]),
						Comparetarget = "<" ++ atom_to_list(MId) ++ ">",
						Fun = fun(XX) ->
						          case XX of
							       {"<all>",_,_} -> true;
							       {Pid,_,_} when Pid =:= Comparetarget -> true;
							       _ -> false
							  end
						      end,
						case  lists:any(Fun,Target) of
						       true -> [X|R];
						       _ -> R
						end;
						%~ oldhand end
					        %~ case Target of						       
						      %~ {NewTarget,_,_} ->
							      %~ case re:run(Target,"<" ++ atom_to_list(MId) ++ ">") of
								%~ {match,_}->
									%~ [X|R];
								%~ _->
									%~ case re:run(Target,"<all>") of
										%~ {match,_}->
											%~ [X|R];
										%~ _->
											%~ R
									%~ end
							       %~ end;
						      %~ _ -> R						
						%~ end;						
					_->
						R
				end end,[],Rules).

%% @spec find_rule(List)->list()
%% @doc find rule by id list 
%%
find_rule([])->[];
find_rule([Id|T])->
	siteview:get_object(Id) ++ find_rule(T).

%% @spec getRules(S,L)->{S,R}
%% @doc get a set of rule
%% 
getRules(S,[])->{S,[]};
getRules(S,[R|T])->
	case sets:is_element(R:get_id(),S) of
		false->
			NS = sets:add_element(R:get_id(),S),
			{NNS,RS} = THIS:getRules(NS,T),
			{NNS, [R] ++ RS};
		_->
			{NNS,RS} = THIS:getRules(S,T),
			{NNS,RS}
	end;
getRules(S,_)->{S,[]}.

get_Rules(Category)->	
	{ok,{id,MId}}= THIS:get_property(id),
	Rules = siteview:get_object_by_type(rule),
        Fun = fun(XX,R) ->
	                case XX:get_property(target) of
				{ok,{_,Target}}-> 
								
					Comparetarget = "<" ++ atom_to_list(MId) ++ ">",
					Fun = fun(X) ->
						  case X of
						       {Pid,'=',Category} when Pid =:= Comparetarget -> true;
						       %~ {Pid,'=','ok'} when Pid =:= Comparetarget andalso Category =:= 'good' -> true;
						       {Pid,'<>',Category} when Pid =:= Comparetarget -> false;
						       {Pid,'<>',_} when Pid =:= Comparetarget -> true;
						       _ -> false
						  end
					      end,
					case lists:any(Fun,Target) of
					      true -> 
					           FilterFun = fun(X) -> X =/= 'and' end,   
						   AndFun = fun(X) -> compare_rules(X) end,						   
						   AnyFun = fun(X) -> 
							 FilterRules = lists:filter(FilterFun,X),
							 lists:all(AndFun,FilterRules)
							 end,
						   case lists:any(AnyFun,tokens(Target,'or')) of
						          true -> [XX|R];
							  _ -> R
						   end;					         
					      _ -> R
                                        end; 					
				_-> R
			end
             	   end,
	lists:foldl(Fun,[],Rules).

 
compare_rules({Pid,Op,Category}) -> 
    Pid1 = string:strip(Pid, right, $>),
    Pid2 = string:strip(Pid1, left, $<),
    
    case api_monitor:get_run_info(list_to_atom(Pid2)) of
          {error,_} -> false;
	  MonitorProplist -> 	       
	       case proplists:get_value(category,MonitorProplist) of
	           undefined -> false;
	           Category when Op =:= '=' -> true;
		   %~ 'good' when Op =:= '=' andalso Category =:= 'ok' -> true;
	           Category when Op =:= '<>' -> false;
	           _ when Op =:= '<>' -> true;
	           _ -> false
	      end
    end;
compare_rules(_) -> false.


tokens(S, Seps) ->
    tokens1(S, Seps, []).

tokens1([C|S], Seps, Toks) ->
    case C=:=Seps of
	true -> tokens1(S, Seps, Toks);
	false -> tokens2(S, Seps, Toks, [C])
    end;
tokens1([], _Seps, Toks) ->
    lists:reverse(Toks).

tokens2([C|S], Seps, Toks, Cs) ->
    case C=:=Seps of
	true -> tokens1(S, Seps, [lists:reverse(Cs)|Toks]);
	false -> tokens2(S, Seps, Toks, [C|Cs])
    end;
tokens2([], _Seps, Toks, Cs) ->
    lists:reverse([lists:reverse(Cs)|Toks]).
%%  oldhand end
%% @spec runRules(Class,Monitor,Category)->({ok,Msg}|{error,Reason})
%% @doc run rule 
%% 
runRules(Class,Monitor,Category)->
        {ok,{id,MId}}= THIS:get_property(id),
	{ok,{class,ClassName}}= THIS:get_property(class),
	{ok,{name,Name}}= THIS:get_property(name),                	
%% 	try 
%% 		 amqp_sample:send({MId,ClassName,Name,'monitor'})
%% 	catch
%% 		_:Err->io:format("MQ Send :~p~n",[{Err,MId}]),{error,Err}	
%% 	end,
	%~ Rules = getRules(Class),
	Rules = get_Rules(Category),	
	F = fun(X)->
			Lc = X:get_alert_alert_level(Monitor),
			%~ io:format("Rules:~p~n",[{X,Lc}]), 
			 
			%~ io:format("CATEGORY:~p~n",[{Lc,X:get_property(?CATEGORY)}]), 
			%~ io:format("Rules~p~n",[{X:get_property(?ALERT_LEVEL),Category,X:get_property(target)}]),
			%~ case X:get_property(?CATEGORY) of
				%~ {ok,{?CATEGORY,Category}}->
			case X:get_property(?ALERT_LEVEL) of
				{ok,{alert_level,Alert_level}}->  	
					case X:match(Monitor) of
						true ->
							M = case THIS:get_attribute(list_to_atom(atom_to_list(Category) ++ "Count")) of
									{ok,{_,V}}->V;
									_-> 0
								end,
							case X:get_property(condition) of
								{ok,{condition,{always,N}}}->
									% if
										% M+1 > N ->
											% X:doAction(Monitor),
											% THIS:incrementAlertProperties(X:getFullID(),Category);
										% true->
											% do_nothing
									% end;
									case X:doAction({always_times,N,Monitor}) of
										{ok,_}->
											THIS:incrementAlertProperties(X:getFullID(),Category);
										_->
											do_nothing
									end;
								{ok,{condition,{once,N}}}->
									% if
										% N =:= M->
											% X:doAction(Monitor),
											% THIS:incrementAlertProperties(X:getFullID(),Category);
										% true-> do_nothing
									% end;									
									case X:doAction({match_times,N,Monitor}) of
										{ok,_}->
											THIS:incrementAlertProperties(X:getFullID(),Category);
										_->
											do_nothing
									end;
								{ok,{condition,{select,{N,S}}}}->
									% if
										% M==N orelse ((M-N) rem S == 0) ->
											% X:doAction(Monitor),
											% THIS:incrementAlertProperties(X:getFullID(),Category);
										% true-> do_nothing
									% end;
									case X:doAction({select_times,N,S,Monitor}) of
										{ok,_}->
											THIS:incrementAlertProperties(X:getFullID(),Category);
										_->
											do_nothing
									end;
								{ok,{condition,{group,N}}}->
									
									{ok,{_,Target}} = X:get_property(target),
									R = [list_to_atom(Z)||Z<-string:tokens(Target,",><")],
									case Monitor:find_parent_in_list(R) of
										null->
											pass;
										Ppt->
											% Mp = 
											% case Ppt:get_attribute(list_to_atom(atom_to_list(Category) ++ "Count")) of
												% {ok,{_,V5}}->V5;
												% _-> 0
											% end,
											% if
												% Mp==N ->
													% X:doAction(Monitor),
													% Monitor:incrementAlertProperties(Monitor:getFullID(),Category);
												% true ->
													% pass
											% end
											case X:doAction({group_times,N,Ppt,Monitor}) of
												{ok,_}->
													THIS:incrementAlertProperties(X:getFullID(),Category);
												_->
													do_nothing
											end
									end;
									
								{ok,{condition,{all,[]}}}->
									{ok,{_,Target}} = X:get_property(target),
									R = [list_to_atom(Z)||Z<-string:tokens(Target,",><")],
									case Monitor:find_parent_in_list(R) of
										null->
											pass;
										Pp->
											{ok,{childs,Childs}} = Pp:get_attribute(childs),
											{Total,Count_Err,_,_} = Pp:get_group_monitors(Childs,{0,0,0,0}),
											io:format("Total:~p,Count_Err:~p~n",[Total,Count_Err]),
											if
												Total > 0 andalso Total == Count_Err ->
													% X:doAction(Monitor),
													% Monitor:incrementAlertProperties(Monitor:getFullID(),Category);
													case X:doAction({group_all,Pp,Monitor}) of
														{ok,_}->
															THIS:incrementAlertProperties(X:getFullID(),Category);
														_->
															do_nothing
													end;
												true ->
													pass
											end
									end;
								_->
									{error,error_condition}
							end;
						_->
							psss
					end;
				{ok,{?CATEGORY,Lc}} when Category==good->
					case X:match(Monitor) of
						true ->
							X:do_recover(Monitor);
						_->
							pass
					end;
				_->
					ok
			end
		end,
	lists:foreach(F,Rules),
	{ok,run_rules_ok}.

find_parent_in_list(List)->
	case THIS:get_owner() of
		{ok,{parent,Parent}}->
			{ok,{_,Id}} = Parent:get_property(?ID),
			case lists:member(Id,List) of
				true ->
					Parent;
				_->
					Parent:find_parent_in_list(List)
			end;
		_->
			null
	end.
				

%% @spec runActionRules(This,Category)->({ok,Msg}|{error,Reason})
%% @doc run action rule 
%% 
runActionRules(This,Category)->
	case This:isAlertTemporarilyDisabled() of
		true->
			{error,disabled};
		false->
			This:runRules(2,This,Category)
	end.

%% @spec isAlertTemporarilyDisabled()->(true|false)
%% @doc whether a alert is disabled
%%
isAlertTemporarilyDisabled()->
	case THIS:get_property(?ALERT_DISABLED) of
		{ok,{?ALERT_DISABLED,{true,{until,{Date,Time}}} }}->
			case compare_time({Date,Time}) of
				true->
					false;
				_->
					true
			end;
		{ok,{?ALERT_DISABLED,{true,{period,{From,To}}} }}->
			case compare_period_time(From,To) of
				true->
					false;
				_->
					true
			end;
		{ok,{?ALERT_DISABLED,{true,{always,_}} }}->
			true;
		_->
		 false
	end.

%% @spec compare_time({Date,Time})->(true|false)
%% @doc compare time with now()
%%
compare_time({Date,Time})->
	sv_datetime:localtime() > {Date,Time}.

%% @spec compare_period_time(From,To)->(true|false)
%% @doc compare period time with now()
%%
compare_period_time(From,To)->
	Now = sv_datetime:localtime(),
	if
		Now > From,Now < To->
			true;
		true->
			false
	end.

%% @spec incrementAlertProperties(Id,Category)->({ok,Result}|{error,Reson})
%% @doc increment alert related properties
%% 
incrementAlertProperties(Id,Category)->
	AlertCount = atom_to_list(Category) ++ "AlertCount" ++ atom_to_list(Id),
	TimeSinceAlert = atom_to_list(Category) ++ "TimeSinceAlert" ++ atom_to_list(Id),
	THIS:set_attribute(list_to_atom(TimeSinceAlert),sv_datetime:now()),
	case THIS:inc_attribute(list_to_atom(AlertCount)) of
		{ok,_}->
			{ok,increment_ok};
		_->
			THIS:set_attribute(list_to_atom(AlertCount),0),
			{ok,increment_ok}
	end.

%% @spec incrementProperty(S)->({ok,Result}|{error,Reson})
%% @doc increment property
%%
incrementProperty(S)->
	THIS:inc_attribute(S).

%% @spec init(This,Data)->{ok,Result}
%% @doc initial this monitor instance
%%
init(This,Data)->
	BASE:init(This,Data),
	% {ok,{?ID,Id}} = THIS:get_property(?ID),
	%% remove rule id from monitor's attribute,change to query it from database
	% RuleDatas = dbcs_rule:get_monitor_rule(Id),
	% F = fun(X)->
	%		AId = proplists:get_value(id,X),
	%		case siteview:get_object(AId) of
	%			[]->
	%				Obj = rule:new(),
	%				Obj:init(Obj,X),
	%				%%siteview:set_object(AId,rule,Obj),
	%				AId;
	%			_->
	%				AId
	%		end
	%	end,
	% Rules = lists:map(F,RuleDatas),
	% THIS:set_attribute(?RULES,Rules),
	{ok,""}.


%% @spec get_name()->string()
%% @doc get monitor's name
%%
get_name()->
	{ok,{id,Id}} = THIS:get_property(id),
	case THIS:get_property(name) of
		{ok,{name,Name}}->
			if 
				is_atom(Name)->
					atom_to_list(Name);
				true->
					Name
			end;
		_->
			atom_to_list(Id)
	end.

%% @spec get_full_name()->string()
%% @doc get monitor's full name
%%
get_full_name()->
	case THIS:get_owner() of
		{ok,{parent,Parent}}->
			Parent:get_full_name() ++ "\\" ++ THIS:get_name();
		_->
			THIS:get_name()
	end.

%% @spec get_parent_name()->string()
%% @doc get parent's name
%%
get_parent_name()->
		case THIS:get_owner() of
		{ok,{parent,Parent}}->
			Parent:get_name();
		_->
			""
	end.
%% @spec get_parent_full_name()->string()
%% @doc get parent's full name
%%
get_parent_full_name()->
		case THIS:get_owner() of
		{ok,{parent,Parent}}->
			Parent:get_full_name();
		_->
			""
	end.

%% @spec get_run_info()->list()
%% @doc get monitor's runing state
%%
get_run_info()->
	Category = case THIS:get_attribute(?CATEGORY) of
					{ok,Val}->
						[Val];
					_->
						[{?CATEGORY,nodata}]
				end,
	%%State =  case THIS:get_property(?CLASS) of
	%%			{ok,{?CLASS,group}}->
	%%				{ok,{childs,Childs}} = THIS:get_attribute(childs),
	%%				%%io:format("childs:~p~n",[Childs]),
	%%				{Total,Err,_,_} = THIS:get_group_monitors(Childs,{0,0,0,0}),
	%%				[{?STATE_STRING,io_lib:format("~p in group,~p in error",[Total,Err])}];
	%%			_->
	%%				case THIS:get_attribute(?STATE_STRING) of
	%%					{ok,V2}->
	%%						[V2];
	%%					_->
	%%						[{?STATE_STRING,""}]
	%%				end
	%%		end,
	Time = case THIS:get_attribute(?LAST_UPDATE) of
				{ok,{_,0}}->
					[{?LAST_UPDATE,""}];
				{ok,{_,LastUpdate}}->
					[{?LAST_UPDATE,sv_datetime:now2str(LastUpdate)}];
				_->
					[{?LAST_UPDATE,""}]
			end,

	Category ++ Time.

%% @spec get_group_monitors([],{Total,Err,Warning,Good})->{list(),list(),list()}
%% @doc get group's monitors status
%%
get_group_monitors([],{Total,Err,Warning,Good})->{Total,Err,Warning,Good};
get_group_monitors([Obj|T],{Total,Err,Warning,Good})->
	case Obj:get_property(?CLASS) of
		{ok,{?CLASS,group}}->
			Obj:get_group_monitors(Obj:get_childs() ++ T,{Total,Err,Warning,Good});
		_->
			case Obj:get_attribute(?CATEGORY) of
				{ok,{?CATEGORY,error}}->
					Obj:get_group_monitors(T,{Total+1,Err+1,Warning,Good});
				{ok,{?CATEGORY,warning}}->
					Obj:get_group_monitors(T,{Total+1,Err,Warning+1,Good});
				{ok,{?CATEGORY,good}}->
					Obj:get_group_monitors(T,{Total+1,Err,Warning,Good+1});
				{ok,{?CATEGORY,nodata}}->
					Obj:get_group_monitors(T,{Total+1,Err,Warning,Good});
				_->
					Obj:get_group_monitors(T,{Total,Err,Warning,Good})
			end
	end.

%% @spec getStateProperties(This,_)->list()
%% @doc get state properties
getStateProperties(_,_)->
	[].
	
%% @spec getStatePropertyObjects(This)->[{Prop,Value}]
%% where
%%	This = tuple()
%%	Prop = atom()
%%	Value = term()
%% @doc get state properties and which values
%% @end
%% add 2009-11-19
getStatePropertyObjects(This)->
	Props = This:get_template_property(),
	Sps = [P || P<-Props,P#property.type==counters orelse P#property.type == browsable] ++ This:getStateProperties(This,[]),
	getStatePropertyObjects(This,Sps).
	

getStatePropertyObjects(_,[])->[];
getStatePropertyObjects(This,[P=#property{type=counters}|T])->
	Counters = case This:get_property(P#property.name) of
		{ok,{_,V}}->
			V;
		_->
			[]
		end,
	F = fun(X)->
		case This:get_attribute(element(1,X)) of
			{ok,{_,V2}}->
				{element(1,X),V2};
			_->
				{element(1,X),"n/a"}
		end
	end,
	lists:map(F, Counters) ++getStatePropertyObjects(This,T);
getStatePropertyObjects(This,[P=#property{type=browsable}|T])->
	Counters = case This:get_property(P#property.name) of
		{ok,{_,V}}->
			V;
		_->
			[]
		end,
	F = fun(X)->
		case This:get_attribute(element(1,X)) of
			{ok,{_,V2}}->
				{element(1,X),V2};
			_->
				{element(1,X),"n/a"}
		end
	end,
	lists:map(F, Counters) ++getStatePropertyObjects(This,T);
getStatePropertyObjects(This,[P|T]) ->
	case This:get_attribute(P#property.name) of
		{ok,{_,V}}->
			[{P#property.name,V}] ++ getStatePropertyObjects(This,T);
		_->
			[{P#property.name,"n/a"}] ++ getStatePropertyObjects(This,T)
	end.
	
isRunning()->false.

getHostname()->"".


%% @spec getDependencies(Cond)->list()
%% @doc get dependecies of the monitor
%% 
getDependencies(Cond)->
	case THIS:get_property(?DEPENDS_ON) of
		{ok,{?DEPENDS_ON,DepVal}}->
			case api_siteview:find_object(DepVal) of
				[]->
					[];
				[M|_]->
					case THIS:get_property(?DEPENDS_CONDITION) of
						{ok,{?DEPENDS_CONDITION,DepCond}}->
							[{M,DepCond}];
						_->
							[]
					end
			end;
		_->
			[]
	end++
	case THIS:get_parent() of
		{ok,{parent,Parent}}->
			Parent:getDependencies(Cond);
		_->
			[]
	end.
	
get_status_info(This)->
	Category = case THIS:get_attribute(?CATEGORY) of
					{ok,Val}->
						[Val];
					_->
						[{?CATEGORY,nodata}]
				end,
	Time = case THIS:get_attribute(?LAST_UPDATE) of
				{ok,{_,LastUpdate}}->
					[{?LAST_UPDATE,LastUpdate}];
				_->
					[{?LAST_UPDATE,0}]
			end,

	Category ++ Time.	
	
%% @spec get_template_property()->[#property{}]
%% @doc get monitor's template property
%%
get_template_property()->
	[
	#property{name=?NAME,title="Title",type=text,editable=true,order=102,optional = true,default="",description="title that should appear in the Monitor table (optional)"},
	#property{name=?FREQUENCY,title="Update every",type=frequency,editable=true,default=600,description="amount of time between checks of a monitor"},
	#property{name=?DISABLED,title="Disable",type=bool,editable=true,advance=true,order =1,default=false,description="temporarily disable monitor sampling and alerting"},
	#property{name=?VERFIY_ERROR,title="Verify Error",type=bool,editable=true,advance=true,order=50,default=false,description="if the monitor detects an error, immediately perform the check again to verify the error."},
	#property{name=?ERROR_FREQUENCY,title="Update every (on errors)",type=frequency,editable=true,advance=true,default=0,order=60,description="the amount of time between checks whenever the status of the monitor is not ok; the Update value from above is used."},
	#property{name=?DEPENDS_ON,title="Depends On",type=scalar,editable=true,advance=true,default="none",description="Choose the monitor that this monitor depends on"},
	#property{name=?DEPENDS_CONDITION,title="Depends Condition",type=scalar,editable=true,default="good",advance=true,description="If OK, this monitor is only enabled if the Depends On monitor is OK."}
	].

