%% 
%% 
%% 		Useful Command Memo 
%% 
%% 		dbcs_baseline:clear_historigram_for_all_monitor_not_activate_baseline().
%% 		dbcs_baseline:reset_all_baseline_historigram().
%% 		dbcs_baseline:clear_all_baseline_data().
%% 		siteview:reset("localhost").
%% 		l(dbcs_baseline).
%% 		l(monitor).
%% 		dbcs_baseline:get_baseline('0.1.51').
%% 		dbcs_baseline:get_baseline_historigram('0.1.51').
%% 		dbcs_baseline:remove_baseline('0.1.51').
%% 		dbcs_baseline:showAll().
%% 		dbcs_baseline:set_baseline_historigram('0.1.51').
%% 
%% 
%% 		api_monitor:get_log({2010,5,28},'0.1.51').
%% 		fun(T)-> T:getLogProperties(T) end(lists:nth(1, api_monitor:find_object('0.1.51'))).
%% 		fun(T)-> T:get_template_property() end(lists:nth(1, api_monitor:find_object('0.1.51'))).
%% 		fun(T)-> dbcs_baseline:get_property_classifier(T) end(lists:nth(1, api_monitor:find_object('0.1.51'))).
%% 		fun(T)-> dbcs_baseline:filter_baseline_property(T) end(lists:nth(1, api_monitor:find_object('0.1.51'))).
%% 		fun(T)-> T:get_property(activate_baseline) end(lists:nth(1, api_monitor:find_object('0.1.51'))).
%% 
%% 
%%      default page, system generated URL:       http://localhost:8000/web/baseline_edit?id=0.1.51&returnUrl=/web/monitor_edit%3Fid%3D0.1.51
%%      debug page, manually input URL such as:   http://localhost:8000/web/baseline_edit?id=0.1.51
%%      debug page, manually input URL such as:   http://localhost:8000/web/baseline_edit?id=all
%% 
%% 


-module(dbcs_baseline).
-compile(export_all).
-define(Table,"baseline").

-define(Toomany,toomany).
-define(Proper,proper).
-define(Few,few).

-define(MaxDay,30).
-define(MinSample,2016).
-define(MinDay,14).

-define(Discard,2).
-define(MaxPercentile,8).

-define(UpOffSet,1.3).
-define(DownOffSet,0.7).

-define(UpIsBad,upIsBad).
-define(UpIsGood,upIsGood).

-define(DefaultErrorPercentile, 98).
-define(DefaultWarningPercentile, 93).
-define(DefaultGoodPercentile, 0). %% actually, it should be always good

-define(BaselinePrefix, "baseline_").
-define(HistorigramPrefix, "baseline_historigram_").

-include("dbcs_common.hrl").
-include("monitor.hrl").
-include("monitor_template.hrl").

	
getNumber(String)when is_list(String) ->
    case catch list_to_number(String) of
		{'EXIT', _} ->
			case catch list_to_number("0"++String) of
				{'EXIT', _} ->
					String;
				F->
					F
			end;
		I -> I
    end;
getNumber(V)->
	V.

list_to_number(String) ->
    case catch list_to_integer(String) of
		I when is_integer(I) -> I;
		_ -> list_to_float(String)
    end.


base_property(id)->
	true;
base_property(_)->
	false.

baseline_to_db(Baseline)->
	{value,{id,Id}} = lists:keysearch(id,1,Baseline),
	Advance = [dbcs_base:term2db(K,V)||{K,V}<- Baseline,base_property(K)=:=false],
	case [is_atom(Id)] of
		[true]->
			{content,list_to_atom(?Table), Id, <<"baseline">>,null,null,null,null,?Author,null,null,null,null,null,Advance};
		_->
			{}
	end.

db_to_baseline(BaselineData)->
	case BaselineData of
		{_,_,Id,_,_,_,_,_,_,_,_,_,_,_,Adv}->
			[{id,Id}] ++ [dbcs_base:db2term(K,T,V)||{K,T,V}<-Adv];
		_->
			[]
	end.


%% @spec create_baseline(baseline)->({error,Reason} | {ok,Result})
%% where
%%		baseline = list()
%%		Result = tuple()
create_baseline(Baseline)when is_list(Baseline) ->
	db_ecc:insert_data(?DBName,?Table,baseline_to_db(Baseline));
create_baseline(_)->{error,parameter_error}.

%% @spec update_baseline(baseline)->({error,Reason} | {ok,Result})
%% where
%%		baseline = list()
%%		Result = tuple()
update_baseline(Baseline)when is_list(Baseline) ->
	{value,{id,Id}} = lists:keysearch(id,1,Baseline),
	Ret= db_ecc:update_data(?DBName,?Table,"id=" ++ atom_to_list(Id),baseline_to_db(Baseline)),
%% 	io:format("update_baseline, ?DBName:~p  ?Table:~p  ~n~p~n",[?DBName,?Table,Ret]),	
	Ret;
update_baseline(_)->{error,parameter_error}.


%% @spec get_baseline(Id)->({error,Reason} | baseline)
%% where
%%		Id = (atom() | string())
%%		baseline = list()
get_baseline(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=baseline & id=" ++ get_baseline_id_list(Id)),
	parse_db_ecc_get_data(Ret).


%% @spec get_baseline_historigram(Id)->({error,Reason} | baseline)
%% where
%%		Id = (atom() | string())
%%		baseline = list()
get_baseline_historigram(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=baseline & id=" ++ get_baseline_historigram_id_list(Id)),
	parse_db_ecc_get_data(Ret).


parse_db_ecc_get_data(Ret)->
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_baseline};
				_->
					[Baseline|_] = Ret,
					db_to_baseline(Baseline)
			end
	end.


get_baseline_id_list(Mid)->
	case is_list(Mid) of
		true->
			?BaselinePrefix++Mid;
		_->
			?BaselinePrefix++atom_to_list(Mid)
	end.

get_baseline_id_atom(Mid)->
	Lmid= get_baseline_id_list(Mid),
	list_to_atom(Lmid).


get_baseline_historigram_id_list(Mid)->
	case is_list(Mid) of
		true->
			?HistorigramPrefix++Mid;
		_->
			?HistorigramPrefix++atom_to_list(Mid)
	end.

get_baseline_historigram_id_atom(Mid)->
	Lmid= get_baseline_historigram_id_list(Mid),
	list_to_atom(Lmid).




%% @spec  get_baseline_match(Where)-> (baselines | {error,Reason})
%% where
%%		Where = string()
%%		baselines = list()
get_baseline_match(Where)->
	Ret = db_ecc:get_data(?DBName,?Table,Where),
	[db_to_baseline(X)||X <- Ret].



%% @spec get_all()->({error,Reason} | baselines)
%% where
%%		baselines = list()
get_all()->
	Ret = db_ecc:get_data(?DBName,?Table,""),
	[db_to_baseline(X)||X <- Ret].


%% @spec remove_baseline(Id)->({error,Reason} | {ok,Result})
%% where
%%		Id = (atom() | string())
%%		Result = tuple() 
remove_baseline(Id)->
	db_ecc:delete_data(?DBName,?Table,"id="++get_baseline_id_list(Id)),
	db_ecc:delete_data(?DBName,?Table,"id="++get_baseline_historigram_id_list(Id)).


%% @spec clear_all_baseline_interim_data()->({error,Reason} | ok)
clear_all_baseline_data()->
	lists:foreach(
	  	fun(OneP)->
			catch begin 
					  {value,{id,Id}} = lists:keysearch(id,1,OneP),
					  Mid= case is_list(Id) of
							true->
								Id;
							_->
								atom_to_list(Id)
					  		end,
					  db_ecc:delete_data(?DBName,?Table,"id="++Mid)
			end
	  	end, get_all()).
	


%% @spec get_monitor_rule(Id)->({error,Reason} | Rules)
%% where
%%		Id		= (atom() | list())
%%		Rules	= list()
get_baseline_rule(Id)when is_atom(Id)->
	dbcs_rule:get_rule_match("my.parent='" ++ atom_to_list(Id)++"'");
get_baseline_rule(Id)when is_list(Id)->
	dbcs_rule:get_rule_match("my.parent='" ++ Id ++"'");
get_baseline_rule(_)-> {error,parameter_error}.


testA()->
	Id= test2,
	BaselineData= [{id,Id},{k1,v1},{k2,v2},{k3,v3}], 
	testB(Id,BaselineData),
	testB(test1),
	ok.
	
testB(Id)->
	BaselineData= [{id,Id},{k1,v1},{k2,v2}],
	testB(Id,BaselineData).
	
testB(Id,BaselineData)->
	io:format("&&&&&&&&&&&&&  create_baseline ~p : ~p ~n",[Id,create_baseline(BaselineData)]),
	io:format("&&&&&&&&&&&&&  update_baseline ~p : ~p ~n",[Id,update_baseline(BaselineData)]),	
	io:format("&&&&&&&&&&&&&  get_baseline ~p : ~p ~n",[Id,get_baseline(Id)]),
	io:format("&&&&&&&&&&&&&  baseline get_all : ~p ~n",[get_all()]),
	ok.	

test2()->
	Ret= save_baseline_update_data('0.1.2', v1, v1, v1, []),
	io:format("&&&&&&&&&&&&&  save_baseline_update_data : ~p ~n",[Ret]),
	Ret.

testRemoveSpeed()->
	F= fun(Index,Func)when Index>0->
			   remove_baseline('0.1.12000'), %% result: 3ms/per call 
			   Func(Index-1,Func);
		  (_Index,_Func)->
			   ok
	   end,
	F(10000,F).

testC()->
	testC('0.1.51').

testC(Id)->
	Mid= case is_list(Id) of
			true->
				list_to_atom(Id);
			_->
				Id
		end,		
	Monitor= lists:nth(1, api_monitor:find_object(Mid)),
	update_baseline_thresholds(Monitor).
	  	

getAllInfo(Display)->
	lists:foldl(
	 	fun(OneI,AccI)->
				Ret= 
				lists:foldl(
				  	fun(OneP,AccP)->
							catch begin
									  {N,V}= OneP,
									  
									  Detail_1=
									  if
										  N =:= initing_percent andalso V=/=null ->
											  display_format(Display,"~s",[V]),
											  V;
										  N =:= historigram andalso V=/=null ->
											  {IsEnough,Info}= check_monitor_historigram_is_enough(V),
											  _E= display_format(Display,"{~p, \"~p; ~s\" }~n",[N,IsEnough,Info]),
									  		  io_lib:format(" ~p; ~s~n~n",[IsEnough,Info]);
										  true ->
											  display_format(Display,"~p~n",[OneP]),
											  []
									  end,
									  
									  {A2,B2,Detail_2}=
									  case N=:=id of
										  true->
											  	Mid= case is_list(V) of
													true->
														V;
													_->
														atom_to_list(V)
					  							end,
												case 1=:= string:rstr(Mid, ?HistorigramPrefix) of
													true-> 
														{A,B,Detail}=AccP,
														Text= io_lib:format("historigram of ~p ;",[getback_monitor_id(Mid)]),
														{A,B+1,Detail++Text};
													false->
														case 1=:= string:rstr(Mid, ?BaselinePrefix) of	
															true->
																{A,B,Detail}=AccP,
																{A+1,B,Detail};
															_->
																AccP
														end
												end; 
										  _->
									   			AccP
									  end,
									  
									  {A2,B2,Detail_2++Detail_1}
							end
					end,AccI,OneI),
				display_format(Display,"~n",[]),
				Ret
		end, {0,0,[]}, get_all()).	

display_format(Display,Argu1,Argu2)->
	case Display of
		true->
			( catch io:format(Argu1,Argu2) );
		_->
			ok
	end.	
	

showAll()->
%% 	io:format("&&&&&&&&&&&&&  baseline showAll :~n ~p ~n",[get_all()]).
	io:format("&&&&&&&&&&&&&  baseline showAll :~n "),	
	clear_historigram_for_all_monitor_not_activate_baseline(),
	{Bcount,Hcount,_InfoDetail}=getAllInfo(true),
	io:format("&&& (DB/Table) ~p/~p , ~p/~p (Count of ~p/~p)~n ",[?DBName,?Table,Bcount,Hcount,?BaselinePrefix,?HistorigramPrefix]).

showMeasurement(This,Measurement=?NAME)->
	Attr = This:get_property(Measurement),
	io:format("Measurement--  ~p: ~p~n",[Measurement,Attr]),
	ok;
showMeasurement(This,Measurement)->
	Attr = This:get_attribute(Measurement),
	io:format("Measurement--  ~p: ~p~n",[Measurement,Attr]),
	ok.

showNameAndState(This)->
	catch begin
		{ok,Name}=  This:get_property(?NAME),
		{ok,State}= This:get_attribute(?STATE_STRING),
		io:format("Measurement-- ~p,~p~n",[Name,State]),
		ok	
	end.


%% call by atomic_monitor.erl 
try_save_baseline_update_data(This)->
	try_calculate_thresholds(This),
	(catch fun
	   		([])-> ok;
	   		(BaselineProperty)->
				try_save_baseline_update_data(This, This:getFullID(), BaselineProperty, This:getStatePropertyObjects(This), This:get_attribute(?CATEGORY))
			end(filter_baseline_property(This))).
	

try_save_baseline_update_data(This, Mid, TemplatePropertyNum, PropertyObj, Category)->
	Percentile= case catch get_dbcs_percentile(Mid) of
					{ok,P}->
						[{percentile,P}];
					_->
						build_dbcs_percentile(This)
%% 						[]
				end,	
	{_SaveB,Pobj,Time}= save_baseline_update_data(Mid, TemplatePropertyNum, PropertyObj, Category, Percentile),
	spawn(fun() -> update_monitor_historigram(This, Mid, Pobj, Time, TemplatePropertyNum) end),	
	
%% 	showNameAndState(This),  %% sometime needful
	
%% 	io:format("~n~n~n~n~nbaselineUpdate getFullID(): ~p~n~p~nsave:~p~n",[Mid,This,SaveB]),
%% 	getAll(),
	
%% 	showMeasurement(This,?NAME),
%% 	showMeasurement(This,?CATEGORY),
%% 	showMeasurement(This,?STATE_STRING),
%% 	io:format("LogProperties--  ~p~n",[This:getLogProperties(This)]),
%% 	io:format("StateProperties--  ~p~n",[This:getStatePropertyObjects(This)]),
%% 	io:format("filter_property_numerical --  ~p~n",[TemplatePropertyNum]),	
	
	ok.


save_baseline_update_data(Mid, TemplatePropertyNum, PropertyObj, Category, Percentile)->
	Pobj=simplify_propertyObj_by_templatePropertyNum(TemplatePropertyNum, PropertyObj), 
	Time= get_time_now(),
	BaselineData= [{id,  				get_baseline_id_atom(Mid)},
				   {templateBaseline,  	TemplatePropertyNum},
				   {measurementBaseline,Pobj},
				   {category,  			simplify_Category(Category)},
				   {updateTime,  		Time}]++Percentile,
    Ret= 
	case catch update_baseline(BaselineData) of
		{ok, _Ret} ->
			ok;
		{error,_} ->
			catch create_baseline(BaselineData);
		Wrong ->
			Wrong			  
	end,
	{Ret,Pobj,Time}.


get_dbcs_percentile(Mid)->
	OldData= get_baseline(Mid),
	{value,{percentile,P}} = lists:keysearch(percentile,1,OldData),
	true= P =/= null,
	{ok,P}.

get_time_now()->
	{_,_,MicroSec} = erlang:now(),	
    {{A,B,C},{X,Y,Z}} =  erlang:localtime(),
	{{A,B,C},{X,Y,Z+MicroSec/1000000}}.

simplify_propertyObj_by_templatePropertyNum(_TemplatePropertyNum,[])->
	[];
simplify_propertyObj_by_templatePropertyNum([],_PropertyObj)->
	[];
simplify_propertyObj_by_templatePropertyNum(TemplatePropertyNum,PropertyObj)->
	F= fun(PObject,Func)->
			   lists:foldl(
	  				fun(OneP,Acc) when is_list(OneP)->
							Acc++Func(OneP,Func);
					   (OneP,Acc) ->
							case catch begin 
										   	{PName,V}=OneP,
							 				IsTrue= lists:member(PName,TemplatePropertyNum),
											Number= getNumber(V),
											true= is_integer(Number) orelse is_float(Number),
											{IsTrue, {PName, Number} }
					   				end of
								{true,AP} ->
									[AP|Acc];
								_ ->
									Acc
							end
					end,[],PObject)
	   end,
	F(PropertyObj,F).	

simplify_Category(Category)->
	case catch begin {_,{?CATEGORY,C}}= Category,
					 {?CATEGORY,C}
			   end of
				{?CATEGORY,CC}->
					CC;
				_->
					error
	end.
						
  
filter_other_template_property(This,Property)->
	catch( 	(Property#property.baselinable =:= true) andalso  
			(Property#property.type =:= numeric) andalso 
			lists:keymember(Property#property.name, 1, get_property_classifier(This) )
	).	

simplest_filter_other_template_property_old(Name)when is_atom(Name) ->
	Name =/= countersInError andalso Name =/= contersInError andalso Name =/= oidIndex andalso Name =/= index 
	andalso Name =/= status andalso Name =/= pStatus andalso Name =/= port andalso Name =/= pPort ;
simplest_filter_other_template_property_old(_Name) ->
	true.
	
filter_other_template_property_old(This,Name)->
	case simplest_filter_other_template_property_old(Name) of
		false->
			false;
		true->
			lists:keymember(Name, 1, get_property_classifier(This))
	end.			
			

this_get_classifier(This,C)->
	Classifier= This:get_classifier(C),
	if 
		is_list(Classifier)->
			lists:foldr(
	  			fun(OneP,Acc)->
					case catch begin
						   {_ClassifierName,_Operation,_V} = OneP,
						   {check_ok,OneP}					   						   						   
					end of 
					{check_ok, ClassifierP} ->
						[ClassifierP|Acc];
					_ ->
						Acc
					end
				end,[],Classifier);
		true->
			Classifier
	end.	


%% [{percentFull,'>',90}, {percentFull,'>',80}, {freeSpace,'>=',8000}]
get_property_classifier(This)->
	get_property_classifier(This,[error,warning,good],[]).
get_property_classifier(_This,[],R)->
	lists:foldl(
	  	fun(OneP,Acc)->
			case catch begin
						   {ClassifierName,Operation,_V} = OneP,
						   true= ClassifierName=/='N/A',
						   true= ClassifierName=/='N/a',						   
						   true= ClassifierName=/='n/a',
						   true= ClassifierName=/='n/A',
						   true= Operation=/='',
						   {check_ok,OneP}					   						   						   
					   end of 
				{check_ok, Classifier} ->
					[Classifier|Acc];
				_ ->
					Acc
			end
		end,[],R);
get_property_classifier(This,[C|T],R)->
	Classifier= this_get_classifier(This, C),
	NewR=
	if 
		is_list(Classifier)->
			R++Classifier;
		true->
			R
	end,
	get_property_classifier(This, T, NewR).	

is_activate_baseline(This)->
	{ok,{activate_baseline,true}}=:=(catch This:get_property(activate_baseline)).

filter_baseline_property(This)->
	case is_activate_baseline(This) of
		false ->
			case This:get_attribute(baseline_removed) of
				{ok,{_,true}}->
					[];
				_->
					catch remove_baseline(This:getFullID()),
					This:set_attribute(baseline_removed,true),
					[]
			end;
		_ ->		
			lists:foldl(
	  			fun(OneP,Acc)->
					case catch filter_other_template_property(This,OneP) of
						true ->
							[OneP#property.name|Acc];
						_ ->
							Acc
					end
				end,[],get_template_and_dynamic_property(This))
	end.	

is_baseline_property(This, Propety)->
	case is_activate_baseline(This) of
		false -> 
			{false,false};
		_ ->		
			lists:foldl(
				fun(OneP,{IsTrue,P})->
					case IsTrue of
						true->
							{true,P};
						_->
							case catch OneP#property.name =:= Propety andalso filter_other_template_property(This, OneP) of
								true ->
									{true,OneP};
								_ ->
									{false,false}
							end
					end
				end,{false,false},get_template_and_dynamic_property(This))
	end.
	

get_template_and_dynamic_property(This)->
	Template= This:get_template_property(),
	T= Template ++ 
	case catch begin
				   {Class,_}= This, 
				   {getok,get_dynamic_property(Class, Template, This)} 
			   end of
		{getok,DynamicProperty} ->
			DynamicProperty;
		_ ->
			[]
	end,
	T.


get_dynamic_property(browsa_cpu_utilization,Template, This)->
	[build_property_upIsBad(Name) || Name <- filter_classifier_not_in_template(Template, This) ];
get_dynamic_property(apache_monitor, _Template, _This)->
	get_dynamic_property_old(apache_monitor); 
get_dynamic_property(browsableNTCounter_monitor, Template, This)->
	lists:foldl(
	  	fun(OneP,Acc)->
				case contain_word_no_case(OneP,"Free") of
					true-> [build_property_upIsGood(OneP)|Acc];
					_ ->   [build_property_upIsBad(OneP)|Acc]
				end
		end,[],filter_classifier_not_in_template(Template, This));
get_dynamic_property(_, Template, This)->
	lists:foldl(
	  	fun(OneP,Acc)->
				case contain_word_no_case(OneP,"Free")=:=true orelse contain_word_no_case(OneP,"idle")=:=true of
					true-> [build_property_upIsGood(OneP)|Acc];
					_ ->   [build_property_upIsBad(OneP)|Acc]
				end
		end,[],filter_classifier_not_in_template(Template, This)).


contain_word_no_case(Word, SubWord)->
	0=/=string:rstr(string:to_upper(to_list(Word)),string:to_upper(to_list(SubWord))).

to_list(W)when is_atom(W)->
	atom_to_list(W);
to_list(W)when is_integer(W)->
	integer_to_list(W);
to_list(W)when is_float(W)->
	float_to_list(W);
to_list(W)when is_list(W)->
	W;
to_list(W)->
	[Z]= io_lib:format("~p",[W]),
	Z.

filter_classifier_not_in_template(Template, This)->
	lists:foldl(
	  	fun(OneP,Acc)->
			case catch begin
						   {ClassifierName, Operation, _} = OneP,
						   true= case Operation of
										'>' -> true;
										'>=' -> true;
										'<' -> true;
										'<=' -> true;
										'==' -> true;
										'!=' -> true;
										_ -> false
								end,						   
						   {lists:keymember(ClassifierName, 2, Template), ClassifierName}
					   end of 
				{false, Name} ->
					[Name|Acc];
				_ ->
					Acc
			end
		end,[],get_property_classifier(This)).


get_template_and_dynamic_property_old(This)->
	T= This:get_template_property()++
	case catch begin
				   {Class,_}= This, 
				   {getok,get_dynamic_property_old(Class)} 
			   end of
		{getok,DynamicProperty} ->
			DynamicProperty;
		_ ->
			[]
	end,
	T.
	

get_dynamic_property_old(browsa_cpu_utilization)->
	F= fun(Index,Acc,Func)when Index>0->
			   N= "core"++integer_to_list(Index)++"#",
			   NewAcc= [build_property_upIsBad(N)|Acc], 
			   Func(Index-1, NewAcc, Func);
		  (_Index,Acc,_Func)->
			   Acc
	   end,
	[build_property_upIsBad("utilization")|F(128,[],F)];
get_dynamic_property_old(apache_monitor)->
	L1=['Total Accesses','Total kBytes','CPULoad','Uptime','ReqPerSec','BytesPerSec','BytesPerReq','BusyWorkers',
	   'Total accesses','Total Traffic','CPU Usage','CPU load','requests/sec','B/second','B/request'],
	R1= lists:foldl(
		  fun(OneP,Acc)->
				  [build_property_upIsBad(OneP)|Acc]
		  end,[],L1),
	L2=['IdleWorkers','idle workers'],
	R2= lists:foldl(
		  fun(OneP,Acc)->
				  [build_property_upIsGood(OneP)|Acc]
		  end,[],L2),	
	R1++R2;
get_dynamic_property_old(_)->
	[].
	


build_property_upIsBad(Name)->
	#property{name=Name,type=numeric,upIsBad=true,baselinable=true}.

build_property_upIsGood(Name)->
	#property{name=Name,type=numeric,upIsBad=false,baselinable=true}.


%% call by monitor.erl  func: classifier_item(This,InR)
%% R,P,V :{percentFull,'>',90}  percentFull  20 
get_baseline_classifier_value(This,R,P,V)->
	case catch begin
			{true,Porperty}= is_baseline_property(This,P),						   
			{ok,{baseline_thresholds,BslThreshold}}= This:get_attribute(baseline_thresholds),
			true= BslThreshold=/=[],
			{value,{P,UpT,DownT}}= lists:keysearch(P, 1, BslThreshold),
			{BaselineTd,UpIsBadOrGood}= choose_baseline_threshold(Porperty,UpT,DownT),
			Td= get_threshold_by_comparing_with_static(BaselineTd,R,UpIsBadOrGood),
						
			{P, Operator, OperatorValue}= R,	 						
			BaselineR1= {P, Operator, Td},
			io:format("classifier--  BaselineR:~p (~p); runtime v:~p  hist:{~p,~p} ~n",[BaselineR1,OperatorValue,V,UpT,DownT]),					
			{got_baseline_R, BaselineR1}
			end of
		{got_baseline_R, BaselineR} ->
			BaselineR;
		_ ->
			R
	end.


get_threshold_by_comparing_with_static(BaselineThreshold,R,UpIsBadOrGood)->
	StaticThreshold= case catch begin 
									V1= element(3,R),
									V= str2num(V1),
									{getValue,V}
								end of
						 {getValue,Value}->
							 Value;
						 _ ->
							 []
					  end,
	case StaticThreshold=:=[] of
		true-> 
%%			without static threshold
			case UpIsBadOrGood=:=?UpIsBad of
				true -> BaselineThreshold*?UpOffSet;
				_ ->	BaselineThreshold*?DownOffSet	
			end;				
		_->
%% 			with static threshold
			get_more_extreme_value(BaselineThreshold,StaticThreshold,UpIsBadOrGood)
	end.
			

get_more_extreme_value(BaselineThreshold,StaticThreshold,UpIsBadOrGood)->
	case UpIsBadOrGood=:=?UpIsBad of
		true->
			case StaticThreshold>=BaselineThreshold of
				true -> StaticThreshold;
				_ -> BaselineThreshold*?UpOffSet			
			end;
		_->
			case StaticThreshold=<BaselineThreshold of
				true -> StaticThreshold;
				_ -> BaselineThreshold*?DownOffSet			
			end 
	end.
			 
  
%% "2"  2
%% ""	[]  and true=[]>2
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

choose_baseline_threshold(Property,UpT,DownT)->
	UpIsBadOrGood= 
		case catch Property#property.upIsBad =:= false of
			true-> ?UpIsGood;
			_-> ?UpIsBad
		end,
	case UpIsBadOrGood=:=?UpIsBad of
		true -> {getBigger(UpT,DownT),UpIsBadOrGood};
		_ ->    {getSmaller(UpT,DownT),UpIsBadOrGood}
	end.
			

getBigger(UpT,DownT)when UpT>=DownT->
	UpT;
getBigger(_UpT,DownT)->
	DownT.

getSmaller(UpT,DownT)when UpT>=DownT->
	DownT;
getSmaller(UpT,_DownT)->
	UpT.

			
%% call by monitor.erl  func: runClassifiers(This)
set_baseline_thresholds(This)->
	case catch begin
			true= ([]=/= filter_baseline_property(This)),			
			Mid= This:getFullID(),
			Dbcs= get_baseline_historigram(Mid),
			{value,{historigram,Historigram}} = lists:keysearch(historigram,1,Dbcs),				
			{IsEnough1, Info1}= check_monitor_historigram_is_enough(Historigram),
			true= IsEnough1=/=?Few,
			Threshold1= get_monitor_threshold(Historigram),
			{IsEnough1=/=?Few, Mid, IsEnough1, Info1, Threshold1}
			end of
		{true, Id, IsEnough, Info, Threshold} ->
			io:format("runClassifiers--  historigram of ~p ; ~p; ~s~n",[Id,IsEnough,Info]),
			io:format("runClassifiers--  set Threshold:~p ~n",[Threshold]),	
			catch This:set_attribute(baseline_thresholds,Threshold),
			Threshold;
		_ ->
			[]
	end.
	

get_monitor_threshold(Historigram)->
	lists:foldl(
		fun(OneP,Acc)->
			case catch begin
						{PName1,Values}= OneP,
						Length= length(Values),
						Nth= round( (?MaxPercentile + ?Discard/2) * Length / 100 ),
						true= ((Length - Nth)>0 andalso Length>0 andalso Nth>0),
						Values2= lists:sort(Values),	
						{ret,PName1,lists:nth(Nth, Values2),lists:nth(Length - Nth, Values2)}
					end of
				{ret,PName,{UpThreshold,_UpTime},{DownThreshold,_DownTime}} ->
					[{PName,UpThreshold,DownThreshold}|Acc];
				_Exception ->
%% 					{PName, exception, io_lib:format("Exception happend:~p",[_Exception])},
					io:format("Exception get_monitor_threshold:~p",[_Exception]),
					Acc
			end
		end, [], Historigram).



reset_all_baseline_historigram()->
	clear_historigram_for_all_monitor_not_activate_baseline(),	
	lists:foreach(
	  	fun(Id)->
			set_baseline_historigram(Id)				
		end, api_monitor:get_all_ids()).

clear_historigram_for_all_monitor_not_activate_baseline()->
	lists:foreach(
	  	fun(OneP)->
			catch begin 
					  {value,{id,Id}} = lists:keysearch(id,1,OneP),
					  Tid= case is_list(Id) of
							true->
								Id;
							_->
								atom_to_list(Id)
					  		end,
					  Mid= getback_monitor_id(Tid),
					  Monitors= api_monitor:find_object(Mid),
					  case Monitors of
						  []->
							db_ecc:delete_data(?DBName,?Table,"id="++Tid);
						  _->
					  		lists:foreach(
								fun(OneM)->
									catch begin 
								   		OneId= OneM:getFullID(),
										true= OneId=:=list_to_atom(Mid),
										BP= filter_baseline_property(OneM),
										true= BP=:=[],
										{ok,deleted}= db_ecc:delete_data(?DBName,?Table,"id="++Tid),
										io:format("Deleted dbcs DBName:Table:Id ,  ~p : ~p : ~p~n",[?DBName,?Table,Tid])			
							   		end
								end,Monitors)
					  end
			end
	  	end, get_all()),
	ok.

getback_monitor_id(Mid)->
	case 1=:= string:rstr(Mid, ?HistorigramPrefix) of
		true-> 
			string:substr(Mid, length(?HistorigramPrefix)+1);
		false->
			case 1=:= string:rstr(Mid, ?BaselinePrefix) of	
				true->
					string:substr(Mid, length(?BaselinePrefix)+1);
				_->
					Mid
			end
	end.

set_baseline_historigram(Mid)->
	Monitors= lists:foldl(
				fun(OneP,Acc)->
					case catch set_baseline_historigram(OneP, Mid) of
						{ok,Id,MonitorObj,BaselineP} ->
							[{Id,MonitorObj,BaselineP}|Acc];						
						_ ->
							Acc
					end
				end,[],api_monitor:find_object(Mid)),	
	Monitors.

set_baseline_historigram(This, Mid)->
  	OneId= This:getFullID(),
	true= OneId=:=Mid,
	BP= filter_baseline_property(This),
	true= BP=/=[],
	{{Y,M,D},_}= erlang:localtime(),
	Days= calendar:date_to_gregorian_days(Y,M,D),
	NowSeconds= calendar:datetime_to_gregorian_seconds(erlang:localtime()),
	build_monitor_historigram({NowSeconds,NowSeconds},Mid,This,BP,-1,Days,[]),
	{ok,OneId,This,BP}.
	

build_monitor_historigram({StartTime,Lasttime},Mid,Monitor,BaselineP,-1,Days,_Historigram)->
	io:format("start baseline initting for ~p ... ~n",[Mid]),
	db_ecc:delete_data(?DBName,?Table,"id="++get_baseline_historigram_id_list(Mid)),	
	catch save_initing_percent(Mid,0),
	build_monitor_historigram({StartTime,Lasttime},Mid,Monitor,BaselineP,0,Days,[{X,[]} || X <- BaselineP ]);
build_monitor_historigram({StartTime,Lasttime},Mid,Monitor,BaselineP,Dayindex,Days,Historigram)->
	Date= calendar:gregorian_days_to_date(Days-Dayindex),
	NewHist= add_new_Historigram(Mid, BaselineP, Historigram, api_monitor:get_log(Date,Mid)),
	NewLasttime= show_build_process({StartTime,Lasttime},Dayindex,Mid,false),
	{IsEnough,Info}= check_monitor_historigram_is_enough(NewHist),	
	case IsEnough=:=?Toomany orelse Dayindex>=?MaxDay of
		 true ->
%%		[{percentFull,values.. },{freeSpace,values.. }]	
			db_ecc:delete_data(?DBName,?Table,"id="++get_baseline_historigram_id_list(Mid)),			 
			save_monitor_historigram(Mid, NewHist), 
%% 			io:format("~p~n",[NewHist]),
			show_build_process({StartTime,Lasttime},Dayindex,Mid,true),			
			io:format("inited historigram of ~p ; ~p ; ~s~n",[Mid,IsEnough,Info]),
			ok;
		_ ->
    		build_monitor_historigram({StartTime,NewLasttime},Mid,Monitor,BaselineP,Dayindex+1,Days,NewHist)
	end. 

show_build_process({StartTime,Lasttime},Dayindex,Mid,IsEnd)->
	Now= calendar:datetime_to_gregorian_seconds(erlang:localtime()),
	BetweenTime= Now-Lasttime,
%% 	io:format("BetweenTime:~p~n",[BetweenTime]),
	case BetweenTime>5 orelse IsEnd=:=true of
		true->
			Percent= case IsEnd of
						 true->
							 100;
						 _->
							 P= round(100*Dayindex/?MaxDay),
							 _E= (catch save_initing_percent(Mid,P)),
							 P
					 end,
			{_Days,TotalTime}= calendar:seconds_to_daystime(Now-StartTime),
			io:format("~p%, run{Hr,Min,Sec}:~p, dayIndex ~p, baseline initting for ~p ... ~n",[Percent,TotalTime,Dayindex,Mid]),
			Now;
		_->
			Lasttime
	end.
	

save_initing_percent(Mid,Percent)->
	Data= "Baseline initting, " ++ io_lib:format("~p",[Percent]) ++ "%" ++io_lib:format("~n~n",[]),
	BaselineData= [{id, get_baseline_historigram_id_atom(Mid)},
				   {initing_percent, Data}],
    case catch update_baseline(BaselineData) of
		{ok, _Ret} ->
			ok;
		{error,_} ->
			catch create_baseline(BaselineData);
		Wrong ->
			Wrong			  
	end.	
	


%% [{percentFull, [{20,time},{21,time}...] }] 
add_new_Historigram(Mid, BaselineP, Historigram, {ok,Logs} )->
%%         -record(monitorlog,{id,name,time,category,desc,measurement}). 	  
%% OneLog: {monitorlog,'0.1.12',"Disk Space:this server(C)",{{2010,5,6},{10,15,23}},good,"20% full<br>83527MB free<br>104856MB total",[{percentFull,20},{freeSpace,83527}]},
%% AccHis: [{percentFull, [] }]
	lists:foldr(
				fun(OneLog,AccHis)->
					case catch begin 
						true= Mid=:=OneLog#monitorlog.id,
						Flog= add_measurements(BaselineP, AccHis, OneLog#monitorlog.measurement, OneLog#monitorlog.time, head),
						{filter_onelog_ok,Flog}
						end of	
					{filter_onelog_ok,NewAccHis}->
						NewAccHis;
					_ ->
						AccHis
					end
				end, Historigram, Logs);	
add_new_Historigram(_Mid, _BaselineP, Historigram, _)->
	Historigram.


add_measurements(BaselineP, Historigram, Measurements, Time, Direction)->
	Historigram2=
		lists:foldr(
			fun(OneH,AccH)->
				case catch begin
								{PName,_Value}= OneH,
								lists:member(PName,BaselineP)
							end of
					true ->
						[OneH|AccH];
					_ ->
						AccH
				end
			end, [], Historigram),
	
	lists:foldl(
			fun(OneM,AccM)->
				case catch begin
								{PName,Value}= OneM,
								true= lists:member(PName,BaselineP),
								Number= getNumber(Value),
								true= is_integer(Number) orelse is_float(Number),
								add_one_value(AccM,PName,{Number, Time}, Direction)
							end of
					{add_onevalue_ok,NewAccM} ->
						NewAccM;
					_ ->
						AccM
				end
			end, Historigram2, Measurements).


add_one_value(AccM,PName,{Value,Time}, Direction)->
	AccM2= case lists:keysearch(PName, 1, AccM) of
				false ->
		   			AccM++[{PName,[]}];
	   			_->
		   			AccM
			end,
	NewAccM= 
	lists:foldl(
		fun(OneP,Acc)->
				{Name,Values}= OneP,
				case Name=:=PName of
					true ->
						fun
						   	(tail)->
								[{Name, Values++[{Value,Time}] }|Acc];
							(_)->
								[{Name, [{Value,Time}|Values]  }|Acc]
						end(Direction);
					_ ->
						[OneP|Acc]
				end
		end, [], AccM2),
	{add_onevalue_ok, NewAccM}.


%% 	?Toomany
%% 	?Proper
%% 	?Few
check_monitor_historigram_is_enough(Historigram)->
	lists:foldl(
		fun(OneP,Acc)->
			case Acc of
				{IsProper, AccInfo} ->
					case catch begin
								{PName,Values}= OneP,
								Length= length(Values),
								Info0= AccInfo ++ io_lib:format("~nMeasurement:~p,", [PName]),
								Info1= io_lib:format("~p logs,", [Length]),

								if
									Length<10 ->
										{ret,{?Few, Info0++Info1}};
									true->
										{_,Date1}= {_,{{Y1,M1,D1},_}}= lists:last(Values),
										{_,Date2}= {_,{{Y2,M2,D2},_}}= lists:nth(1, Values),										
										Days= abs(calendar:date_to_gregorian_days(Y1,M1,D1)-calendar:date_to_gregorian_days(Y2,M2,D2)),
										Info2= io_lib:format("~p days,from ~p to ~p.", [Days,Date1,Date2]),
										if 
											Days>?MaxDay ->
												{ret,{?Toomany, Info0++Info1++Info2}};
											Days>?MinDay andalso Length>=(?MinSample/4) ->
												{ret,{?Proper, Info0++Info1++Info2}};	
											Length>=?MinSample ->
												{ret,{?Proper, Info0++Info1++Info2}};											
											true ->
												{ret,{?Few, Info0++Info1++Info2}}
										end
								end
						end of
						{ret,{ThisProper,ThisInfo}} ->
							{is_proper(IsProper,ThisProper),ThisInfo};
						_Exception ->
							{IsProper, AccInfo ++ io_lib:format("Exception happend:~p",[_Exception])}
					end;
				Lot ->
					Lot
			end
		end, {?Proper,""}, Historigram).
  
is_proper(A, B)when A=:=?Few orelse B=:=?Few ->
	?Few;
is_proper(A, B)when A=:=?Toomany orelse B=:=?Toomany ->
	?Toomany;
is_proper(_A, _B)->
	?Proper.


save_monitor_historigram(Mid, Historigram)->
	BaselineData= [{id,  		 get_baseline_historigram_id_atom(Mid)},
				   {historigram, Historigram}],
    case catch update_baseline(BaselineData) of
		{ok, _Ret} ->
			ok;
		{error,_} ->
			catch create_baseline(BaselineData);
		Wrong ->
			Wrong			  
	end.
	

update_monitor_historigram(This, Mid, Pobj, Time, BaselineP)->
	E= (catch begin
				Historigram= ensure_get_baseline_historigram(This, Mid),
				Historigram2= try_setHistorigram_when_addMeasurment(This, Mid, BaselineP, Historigram),						
				{{A,B,C},{X,Y,Z}}= Time,
				Time2= {{A,B,C},{X,Y,round(Z)}},
				NewHist= add_measurements(BaselineP, Historigram2, Pobj, Time2, tail),
				{IsEnough1, _Info1}= check_monitor_historigram_is_enough(NewHist),
				ProperHist= reduce_proper_historigram(IsEnough1, NewHist, Mid),
				{_IsEnough, _Info}= check_monitor_historigram_is_enough(ProperHist),
				save_monitor_historigram(Mid, ProperHist),
				update_baseline_thresholds(This),
%% 				io:format("updateBaseline-- of ~p ; ~p; ~s~n",[Mid,_IsEnough,_Info]),
			  	ok
		end),
	E.


reduce_proper_historigram(?Toomany, Historigram, Id)->
	BeginDay= lists:foldl(
		fun(OneP,DayFlag)->
					case catch begin
								{_PName,Values}= OneP,
								{_,_Date1}= {_,{{Y1,M1,D1},_}}= lists:last(Values),
								Day1= calendar:date_to_gregorian_days(Y1,M1,D1),										
								BeginDay1= Day1-?MaxDay,	
								{BeginDay1>=DayFlag,BeginDay1}
						end of
						{true,Day} ->
							Day;
						_ ->
							DayFlag
					end
		end, calendar:date_to_gregorian_days(1,1,1), Historigram),
	lists:foldr(
		fun(OneM,Acc)->
			case catch begin
						{Name1,MValues}= OneM,   
						Measurment1= 
						case catch cut_historigram(BeginDay,MValues,0) of
							{cutok,MValues2,Count} ->
								io:format("updateBaseline-- cut out ~p logs of ~p : ~p ~n",[Count,Id,Name1]),
								MValues2;
							_ ->
								MValues
						end,
						{cutok,Name1,Measurment1}
				end of
				{cutok,Name,Measurment}->
					[{Name,Measurment}|Acc];
				_ ->
					[OneM|Acc]
			end
		end, [], Historigram);
reduce_proper_historigram(_, Historigram, _)->
	Historigram.

cut_historigram(BeginDay,Measurment,Count)when BeginDay<10000 ->
	{cutok,Measurment,Count};
cut_historigram(BeginDay,[{_V,{{Y,M,D},_}}|T]=Measurment,Count)->
	Day= calendar:date_to_gregorian_days(Y,M,D),
	if
		Day<BeginDay ->
			cut_historigram(BeginDay,T,Count+1);
		true->
			{cutok,Measurment,Count}
	end;
cut_historigram(_BeginDay,Measurment,Count)->
	{cutok,Measurment,Count}.


ensure_get_baseline_historigram(This, Mid)->
	case try_get_baseline_historigram(Mid) of
		false->
			set_baseline_historigram(This, Mid),
			try_get_baseline_historigram(Mid);
		Hist->
			Hist
	end.
	
try_get_baseline_historigram(Mid)->
	case catch begin
				Dbcs= get_baseline_historigram(Mid),
				{value,{historigram,Historigram}} = lists:keysearch(historigram,1,Dbcs),
				true= Historigram =/= null,
				{got,Historigram}
			end of
		{got,Hist}->
			Hist;
		_->
			false
	end.	
	
		
try_setHistorigram_when_addMeasurment(This, Mid, BaselineP, Historigram)->
	IsNeed= lists:foldl(
			fun(PName,Acc)->
				case Acc of
					true->
						true;
					_->
						case lists:keysearch(PName, 1, Historigram) of
							false ->
								true;
							_ ->
								Acc
						end
				end
			end, false, BaselineP),
	if 
		IsNeed ->
			set_baseline_historigram(This, Mid),
			ensure_get_baseline_historigram(This, Mid);
	   	true ->
			Historigram   
	end.
	

%% call by web_monitor_edit.erl
%% Classifier: {percentFull,'>',90}
print_image(Id,Classifier,ImageSrc)->
	case catch print_image_2(Id,Classifier,ImageSrc) of
		{true, Src}->
			Src;
		_->
			""
	end.
  
print_image_2([Id],{Classifier,_,_},ImageSrc)->
	Mid= case is_list(Id) of
			true->
				list_to_atom(Id);
			_->
				Id
	  		end,
	Monitors= api_monitor:find_object(Mid),
	This= lists:nth(1, Monitors),
	OneId= This:getFullID(),
	true= OneId=:=Mid,
	{IsB1,_}= is_baseline_property(This, Classifier),
	IsB=if
			IsB1->
				IsB1;
			is_atom(Classifier)->
				IsB1;
			true->
				CA= list_to_atom(Classifier),
			 	{IsB2,_}= is_baseline_property(This, CA),
				IsB2				
		end,
	{IsB, ImageSrc}.  


match_property(Id,{Classifier,O,N})->
	if
		is_atom(Classifier)->
			{Classifier,O,N};
		true->
			case catch match_property_2(Id,{Classifier,O,N}) of
				{true, Clf}->
					Clf;
				_->
					{Classifier,O,N}
			end
	end.	
  
match_property_2(Id,{Classifier,O,N})->
	Mid= case is_list(Id) of
			true->
				list_to_atom(Id);
			_->
				Id
	  		end,
	Monitors= api_monitor:find_object(Mid),
	This= lists:nth(1, Monitors),
	OneId= This:getFullID(),
	true= OneId=:=Mid,
	{IsB1,_}= is_baseline_property(This, Classifier),
	Cf= if
			IsB1->
				{Classifier,O,N};
			true->
				CA= list_to_atom(Classifier),
			 	{true,_}= is_baseline_property(This, CA),
				{CA,O,N}			
		end,	
	{true, Cf}.
	
	

%% call by web_baseline_edit.erl  
get_baseline_info(Id,Category,Index,Classifier)->
%% 	io:format("get_baseline_info:~n~p~n  ~p~n  ~p~n  ~p~n  ",[Id,Category,Index,Classifier]),
	case catch begin
			Mid= case is_list(Id) of
					true->
						list_to_atom(Id);
					_->
						Id
	  			end,
			Dbcs= get_baseline_historigram(Mid),
			
			{IsEnough, Info, Data, Percentile}=
			case Mid of
				all->
					{_Bcount,Hcount,InfoDetail_2}=getAllInfo(false),
					InfoDetail_1= io_lib:format("      DB:   ~p~n Table:   ~p~nCount:   ~p~n",[?DBName,?Table,Hcount]),
					{?Few, InfoDetail_1, InfoDetail_2, ""};
				_->			
					case try_get_initing_percent(Dbcs) of
						{initing_percent,InitData}->
							{?Few, [], InitData, ""};
						_->	
							get_baseline_info_filter_historigram(Mid,Category,Index,Classifier,Dbcs)
					end
			end,
			
			{true, IsEnough=:=?Few, Info, Data, Percentile}
			end of
		{true, IsFew, DebugInfo, HtmlData, PercentileText} ->
			{IsFew, DebugInfo, HtmlData, PercentileText};
		_ ->
			{?Few, [], [], ""}
	end.

try_get_initing_percent(Dbcs)->
	case catch begin
				{value,{initing_percent,Historigram}} = lists:keysearch(initing_percent,1,Dbcs),
				{initing_percent,Historigram}
			end of
		{initing_percent,Info}->
			{initing_percent,Info};
		_->
			false
	end.

get_baseline_info_filter_historigram(Id,Category,Index,Classifier,Dbcs)->
	ClassifierExac= match_property(Id, Classifier),
	catch try_delete_useless_measurment(Id),
	{value,{historigram,Historigram}} = lists:keysearch(historigram,1,Dbcs),
	{IsEnough1, Info1}= check_monitor_historigram_is_enough(Historigram),
	Info2= io_lib:format("historigram of ~p ; ~p; ~s~n",[atom_to_list(Id),IsEnough1,Info1])
			++ get_percentile_dbcs_info(Id),
%% 	io:format("~s",[Info2]),					
	case IsEnough1=:=?Few of
			true->
				{IsEnough1, Info2, "Not enough samples to calculate baseline", ""};
			_->	
				build_classifier_info(IsEnough1, Info2, Historigram,ClassifierExac,Id,Category,Index)
	end.

build_classifier_info(IsEnough, Info, Historigram,_Classifier,Id, all_baseline_classifier, _Index)->
	case catch build_classifier_info_2(Historigram,Id) of
			{all_baseline_classifier, RawText}->
				{IsEnough, Info++RawText, [], ""};
			_->
				{IsEnough, Info, [], ""}
	end;
build_classifier_info(IsEnough, Info, Historigram,Classifier,Id,Category,Index)->
	case catch build_percentile_map_data(Historigram,Classifier) of
			{mapdata,MapData,RawData}->
				RawText= io_lib:format("~n~n~p~n",[RawData]),
				{IsEnough, Info++RawText, MapData, get_measurment_percentile(Id,Category,Index) };
			_->
				{IsEnough, Info, [], ""}
	end.

build_classifier_info_2(Historigram,Id)->
	{ok,Percentile}= get_dbcs_percentile(Id),
	RetInfo=
	lists:foldl(
	  fun(OneP,Acc)->
			case catch begin
						   	{_, Classifier, PercentileValue}= OneP,
							case catch build_percentile_map_data(Historigram,{Classifier,'>',PercentileValue}) of
								{mapdata,MapData,RawData}->
									RawText= io_lib:format("~n~n~n~p~n~p~n~p~n~p~n",[OneP,RawData,OneP,MapData]),
									{ok, RawText};
								_->
									{ok, ""}
							end						   
					end of
				{ok,OneData}->
					Acc++OneData;
				_->
					Acc
			end
	  end, "", Percentile ),
	{all_baseline_classifier, RetInfo}.
	
build_percentile_map_data(Historigram,{PName,_,_})->
	{StartNth,EndNth,Values}= get_arithmetic_threshold_arguments(Historigram, PName),
	RawData= accmulate_percentile_map_data(Values, 0, StartNth, EndNth, []),
	[{InitPercentile,InitValue}|_]= RawData,
	MapData= getEightData(RawData, {InitPercentile,InitValue}, {InitPercentile,InitValue}, 1, []),
    {mapdata,MapData,RawData}.	
	

accmulate_percentile_map_data(_Values,Percentile,_StartNth,_EndNth, Ret) when Percentile>100 ->
	[{_,V}|T]= Ret,
	lists:reverse([{100,V}|T]);
accmulate_percentile_map_data(Values,Percentile,StartNth,EndNth, Ret)->
	NewRet=	case catch arithmetic_threshold_by_percentile(Values,Percentile,StartNth,EndNth) of
				{ok,Va}->
					[{Percentile,Va}|Ret];
				_->
					Ret
			end,
	accmulate_percentile_map_data(Values,Percentile+1,StartNth,EndNth,NewRet).


getEightData([{Percentile,V}], {InitPercentile,_}, _, _, Ret)->
	Ret++[{InitPercentile,Percentile,V}]
	++[{100,110,V*1.1}]++[{110,120,V*1.2}]++[{120,130,V*1.3}];
getEightData([{Percentile,V}|T], {InitPercentile,InitValue}, {Lastpercentile,LastValue}, Index, Ret)->
	case InitValue=/=V andalso Percentile>=(Index*20+1) of
		true->
			getEightData(T, {Lastpercentile,V}, {Percentile,V}, Index+1, Ret++[{InitPercentile,Lastpercentile,LastValue}]);
		_->
			getEightData(T, {InitPercentile,InitValue}, {Percentile,V}, Index, Ret)
	end.
		
	

%%  GWE_Classifier: {good_classifier,[{freeSpace,'<=',86}]}
%%  GWE: good waring error 
update_classifier_value(Id, GWE_Classifier)->
	Mid= case is_list(Id) of
			true->
				list_to_atom(Id);
			_->
				Id
		end,
	OldMonitorData= dbcs_monitor:get_monitor(Mid),
	{Key,_}= GWE_Classifier,
	MonitorData= lists:keyreplace(Key, 1, OldMonitorData, GWE_Classifier),
	ok= case dbcs_monitor:update_monitor(MonitorData) of
			{error,_} ->
				error;
			_->
				ok
		end,
	Monitor= lists:nth(1, api_monitor:find_object(Mid)),
	true= Mid=:= Monitor:getFullID(),
	Monitor:init(Monitor,MonitorData),
	ok.


%% call by web_baseline_edit.erl  
%%  "0.1.16",  error_classifier,   0,  {pFreeSpace, '<', 90},   10
set_measurment_percentile(Id,Category,Index,Classifier,Percentile)->
	case catch set_measurment_percentile_2(Id,Category,Index,Classifier,Percentile) of
		{ok,Data}->
			io:format("set baseline percentile --Id:~p ~p~n", [Id,Data]),
			catch begin
				Mid= case is_list(Id) of
					true->
						list_to_atom(Id);
					_->
						Id
				end,					  
				This= lists:nth(1, api_monitor:find_object(Mid) ),
				update_baseline_thresholds(This)
			end,
			ok;
		_Exception->
			io:format("set baseline percentile exception--(Id,Category,Index,Classifier,Percentile):~n  (~p  ~p  ~p  ~p  ~p)~n  ~p~n", [Id,Category,Index,Classifier,Percentile,_Exception])
	end.	

set_measurment_percentile_2(Id,Category,Index,Classifier,Value)->
	{Key,OldData}= get_percentile_dbcs_data(Id,Category,Index),	
	{PName,_,_}= match_property(Id, Classifier),
	{PercentileData, DBCS_Data}= 
	case lists:keysearch(percentile, 1, OldData) of
			{value,{percentile,PData}}->
				case PData of
					null->
						{[], OldData};
					_->
						{PData, OldData}
				end;
			_->
				{[], {percentile,[]} }
	end,
	F= fun(I,Acc,Func)when I>0->
			   NewAcc= lists:keydelete(Key, 1, Acc), 
			   Func(I-1, NewAcc, Func);
		  (_Index,Acc,_Func)->
			   Acc
	   end,
	NewPercentileData= F(length(PercentileData),PercentileData,F) ++ [{Key,PName,Value}], 		
	NewData= lists:keyreplace(percentile, 1, DBCS_Data, {percentile,NewPercentileData} ),
	update_baseline(NewData),
	{ok,NewPercentileData}.
	
try_delete_useless_measurment(Id)->
	{ok,Percentile}= get_dbcs_percentile(Id),
	This= lists:nth(1, api_monitor:find_object(Id)),
	delete_useless_measurment(Percentile,This,Id,[{error,error_classifier},{warning,warning_classifier},{good,good_classifier}]),
	ok.
	
delete_useless_measurment(ToDeletePercentile,_This,Mid,[])->
	lists:foreach(
	   	fun(OneP)->
				catch begin
						  {{Classifier,Index},_,_}= OneP,
						  delete_measurment_percentile(Mid,Classifier,Index)
				end
		end, ToDeletePercentile);
delete_useless_measurment(ToDeletePercentile,This,Mid,[{C1,C2}|T])->
	{ToDelete,_}=
	lists:foldl(
	  fun(OneP,{Acc,Index})->
			case catch begin
						   	{Classifier, Operation, _}= OneP, 	
							true= Classifier=/='N/A',
						   	true= Classifier=/='N/a',						   
						   	true= Classifier=/='n/a',
						   	true= Classifier=/='n/A',
						   	true= Operation=/='',
							Key= {C2,Index},
							{value,{Key,Classifier, _}}= lists:keysearch(Key,1,Acc),							
							Deleted= lists:keydelete(Key,1,Acc),
							{ok, Deleted}						   
					end of
				{ok,NewAcc}->
					{NewAcc,Index+1};
				_->
					{Acc,Index+1}
			end
	  end, {ToDeletePercentile,0}, this_get_classifier(This, C1) ),	
	delete_useless_measurment(ToDelete,This,Mid,T).

	
delete_measurment_percentile(Id,Category,Index)->
	case catch delete_measurment_percentile_2(Id,Category,Index) of
		{ok,_Data}->
			io:format("delete baseline percentile --Id:~p {~p , ~p} ~n", [Id,Category,Index]),
			ok;
		_Exception->
			io:format("delete baseline percentile exception--(Id,Category,Index):~n  (~p  ~p  ~p)~n  ~p~n", [Id,Category,Index,_Exception])
	end.		
	
						 
delete_measurment_percentile_2(Id,Category,Index)->
	{Key,OldData}= get_percentile_dbcs_data(Id,Category,Index),	
	{PercentileData, DBCS_Data}= 
	case lists:keysearch(percentile, 1, OldData) of
			{value,{percentile,PData}}->
				case PData of
					null->
						{[], OldData};
					_->
						{PData, OldData}
				end;		
			_->
				{[], {percentile,[]} }
	end,
	F= fun(I,Acc,Func)when I>0->
			   NewAcc= lists:keydelete(Key, 1, Acc), 
			   Func(I-1, NewAcc, Func);
		  (_Index,Acc,_Func)->
			   Acc
	   end,
	NewPercentileData= F(length(PercentileData),PercentileData,F), 	
	NewData= lists:keyreplace(percentile, 1, DBCS_Data, {percentile,NewPercentileData} ),
	update_baseline(NewData),
	{ok,NewPercentileData}.


get_measurment_percentile(Id,Category,Index)->
	case catch get_measurment_percentile_2(Id,Category,Index) of
		{ok,Percentile}->
			Percentile;
		_Exception->
			io:format("get baseline percentile exception--(Id,Category,Index):~n  (~p  ~p  ~p)~n  ~p~n", [Id,Category,Index,_Exception]),
			""
	end.
	
get_measurment_percentile_2(Id,Category,Index)->	
	{Key,Data}= get_percentile_dbcs_data(Id,Category,Index),
	{value,{percentile,PData}}= lists:keysearch(percentile, 1, Data),
	{value,{Key,_,Value}}= lists:keysearch(Key, 1, PData),
	Text= integer_to_list(Value), 
	{ok,Text}.
	
get_percentile_dbcs_data(Id,Category,Index)->
	Mid= case is_list(Id) of
			true->
				list_to_atom(Id);
			_->
				Id
		end,
	Key= {Category,Index},
	Data= get_baseline(Mid),
	{Key,Data}.
	

get_percentile_dbcs_info(Id)->
	case catch get_percentile_dbcs_info_2(Id) of
		{ok, Info}->
			Info;
		_->
			"\npercentile data:\n"
	end.
	
get_percentile_dbcs_info_2(Id)->	
	Mid= case is_list(Id) of
			true->
				list_to_atom(Id);
			_->
				Id
		end,	
	{ok,PData}= get_dbcs_percentile(Mid),
	Text= io_lib:format("~npercentile data:~n ~p~n",[PData]), 
	{ok,Text}.

get_default_percentile()->
	{get_default_percentile(error),
	 get_default_percentile(warning),
	 get_default_percentile(good)}.

get_default_percentile(Category)->
	fun(error)->	?DefaultErrorPercentile;
	   (warning)->	?DefaultWarningPercentile;
	   (good)->		?DefaultGoodPercentile
	end(Category).


build_dbcs_percentile(This)->
	Data= build_dbcs_percentile(This,[{error,error_classifier},{warning,warning_classifier},{good,good_classifier}], []),
	[{percentile,Data}].


build_dbcs_percentile(_This,[],Ret)->
	Ret;
build_dbcs_percentile(This,[{C1,C2}|T],Ret)->
	{Data,_I}=
	lists:foldl(
	  fun(OneP,{Acc,Index})->
			case catch begin
						   	{Classifier,_,_}= OneP,
							{true,_Porperty}= is_baseline_property(This, Classifier),
							Value= get_default_percentile(C1),
							{ok, {{C2,Index},Value}}						   
					end of
				{ok,OnePercentile}->
					{Acc++[OnePercentile],Index+1};
				_->
					{Acc,Index+1}
			end
	  end, {[],0}, this_get_classifier(This, C1) ),
	build_dbcs_percentile(This,T,Ret++Data).
		

%% call by atomic_monitor.erl
try_calculate_thresholds(This)->
	spawn(fun() -> update_baseline_thresholds(This) end).


%% to build GWE_Classifier: {good_classifier,[{freeSpace,'<=',86}]} 
update_baseline_thresholds(This)->
	case catch begin
			true= ([]=/= filter_baseline_property(This)),			
			Mid= This:getFullID(),
			Percentile= try_get_percentile_classifier(This, Mid),
			Dbcs= get_baseline_historigram(Mid),
			{value,{historigram,Historigram}} = lists:keysearch(historigram,1,Dbcs),				
			{IsEnough1, _Info1}= check_monitor_historigram_is_enough(Historigram),
			true= IsEnough1=/=?Few,
			Data= calculate_the_monitor_threshold(Historigram, Percentile),
			ShowData= combine_GWE_Classifier(Mid, Data),
			{IsEnough1=/=?Few, Mid, ShowData}
			end of
		{true, _Id, Threshold} ->
%% 			io:format("update baseline thresholds id:~p ~n~p ~n",[_Id,Threshold]),
			Threshold;
		_ ->
			[]
	end,
	ok.
	

try_get_percentile_classifier(This, Mid)->
	Percentile= case catch get_dbcs_percentile(Mid) of
					{ok,P}-> P;
					_->	[]
				end,
	Data= get_percentile_classifier(Percentile,This,Mid,[{error,error_classifier},{warning,warning_classifier},{good,good_classifier}], []),
	
%% 	only baselined property
%% 	[{{error_classifier, 0}, 102, {percentFull, '>', 90}}, 
%% 	{{warning_classifier, 0}, 99, {freeSpace, '<', 8000}}]	
	Data.

get_percentile_classifier(_Percentile,_This,_Mid,[],Ret)->
	Ret;
get_percentile_classifier(Percentile,This,Mid,[{C1,C2}|T],Ret)->
	{Data,_I}=
	lists:foldl(
	  fun(OneP,{Acc,Index})->
			case catch begin
						   	{Classifier,_,_}= OneP,
							{true,_Porperty}= is_baseline_property(This, Classifier),
							Value= ensure_get_percentile(Mid,Percentile,C1,C2,Index,OneP),
							{ok, {{C2,Index},Value,OneP}}						   
					end of
				{ok,OnePercentile}->
					{Acc++[OnePercentile],Index+1};
				_->
					{Acc,Index+1}
			end
	  end, {[],0}, this_get_classifier(This, C1) ),
	get_percentile_classifier(Percentile,This,Mid,T,Ret++Data).

ensure_get_percentile(Mid,Percentile,C1,C2,Index,Classifier)->
	Key= {C2,Index},	
	case catch begin
				{Pname,_,_}= Classifier,
				{value,{Key,Pname,Value}}= lists:keysearch(Key,1,Percentile),
				true= is_integer(Value),
				true= Value>=0 andalso Value=<130,
				{ok,Value}
			end of
				{ok,V}->
					V;
				_->
					DefaultValue= get_default_percentile(C1),					
					catch io:format("repair baseline percentile to ~p of id:~p, ~p ~p ~n",[DefaultValue,Mid,Key,Classifier]),
					catch set_measurment_percentile_2(Mid,C2,Index,Classifier,DefaultValue),
					DefaultValue
	end.


calculate_the_monitor_threshold(Historigram, Percentile)->
	lists:foldl(
	  fun(OneP,Acc)->
			case catch begin
						   	{{Category,Index},PercentileValue,{Classifier,Operation,_}}= OneP,
							{ok, V}= get_classifier_threshold(Historigram, Classifier, PercentileValue),
							{ok, { {Category, Index}, {Classifier,Operation,V}}}						   
					end of
				{ok,OneData}->
					Acc++[OneData];
				_->
					Acc
			end
	  end, [], Percentile ).
	

get_classifier_threshold(Historigram, Property, PercentileValue)->
	{StartNth,EndNth,Values}= get_arithmetic_threshold_arguments(Historigram, Property), 
	arithmetic_threshold_by_percentile(Values,PercentileValue,StartNth,EndNth).

get_arithmetic_threshold_arguments(Historigram, Property)->
	{value,{Property, Values}}= lists:keysearch(Property,1,Historigram),
	Length= length(Values),
	StartNth= round( (?Discard/2) * Length / 100 ),
	EndNth=  round( (100- ?Discard/2) * Length / 100 ),
	true= (Length>0 andalso EndNth>0 andalso EndNth=<Length andalso StartNth>0),
	Values2= lists:sort(Values),
	{StartNth,EndNth,Values2}.
  

arithmetic_threshold_by_percentile(Values,PercentileValue,StartNth,EndNth)->
	ThresholdValue=
	if
		PercentileValue >=100 ->
			{Threshold,_}= lists:nth(EndNth, Values),
			Threshold * PercentileValue /100;
		PercentileValue =< 0 ->
			{Threshold,_}= lists:nth(StartNth, Values),
			Threshold;
		true->
			Nth= round((EndNth-StartNth)*PercentileValue/100)+StartNth,
			{Threshold,_}= lists:nth(Nth, Values),		
			Threshold
	end,
	true= is_integer(ThresholdValue) orelse is_float(ThresholdValue),
	{ok, simplify_number(ThresholdValue)}.


simplify_number(V) when is_integer(V)->
	V;
simplify_number(V)->
	I= round(V),
  	D= abs(I - V),
	D2= abs(V*0.01),
	if 
		D<D2->
			I;
		true->
			V
	end.

combine_GWE_Classifier(Id, ThresholdData)->
	MonitorData= dbcs_monitor:get_monitor(Id),	
	{E,W,G}= get_monitor_GWE_Classifier(ThresholdData,MonitorData,{error_classifier,warning_classifier,good_classifier}),
	E2= {error_classifier,E},
	W2= {warning_classifier,W},
	G2= {good_classifier,G},
	update_classifier_value(Id, E2),
	update_classifier_value(Id, W2),
	update_classifier_value(Id, G2),	
	{E2,W2,G2}.
		
get_monitor_GWE_Classifier(ThresholdData,DbcsData,{E,W,G})->
	Get= fun(MonitorData,Category)->
				case catch begin
				 		{value,{Category, Data}}= lists:keysearch(Category,1,MonitorData),
						true= Data =/= null,
						true= is_list(Data),
						{ok,Data}
				     end of
					 	{ok,CData}-> CData;
					 	_->[]
				end			
		 end,
	Comb=fun(CategoryData,Category,TData)->
				 {CombData,_}= 
				 lists:foldl(
				  	  fun(OneP,{Acc,Index})->
						  case catch begin
									Key= {Category,Index},
				 					{value,{Key, P}}= lists:keysearch(Key,1,TData),
									case OneP of
										{Classifier,Operation,_}->
											{_,_,Value}= P,
											{ok,{Classifier,Operation,Value}};
										_->
											other_data
									end
				     			end of
							  		other_data ->
										{Acc++[OneP], Index};
					 				{ok,NewP}-> 
										{Acc++[NewP], Index+1};
					 				_->
										{Acc++[OneP], Index+1}
						  end
				  	  end, {[],0}, CategoryData),
				 CombData
		 end,
	{Comb(Get(DbcsData,E),E,ThresholdData),
	 Comb(Get(DbcsData,W),W,ThresholdData),
	 Comb(Get(DbcsData,G),G,ThresholdData)}.
	

	
	
	