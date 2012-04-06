%% @copyright 2008-2009 Dragonflow
%% @author Shaohua.Wang, Jian.huang<jian.huang@dragonflow.com> 
%% @version 1.0

%% @doc bandwidth monitor
%% This module is to monitor running monitor.

-module(bandwidth_monitor, [BASE]).
-extends(atomic_monitor).
-compile(export_all).

-include("monitor.hrl").
-include("monitor_template.hrl").
-define(ERROR_VALUE, -999.0).
-define(EMPTY, "").
-define(DEBUG_INFO, debug_info).


%% @spec new() -> Obj
%% @type Obj = term()
%% @doc create a new instance for bandwidth_monitor.
new() ->	
	Base = atomic_monitor:new(),
	Base:set_attribute(?DEBUG_INFO, "new"),
	Base:set_attribute(result, 0.0),
	Base:set_attribute(status, "good"),
	Base:set_attribute(result_state, ""),
	
    {?MODULE,Base}.

%% @spec defaultTitle(Params) -> List
%% @type Params = [term()]
%% @type List = string()
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	BASE:defaultTitle(Params) ++ ": " ++ case proplists:get_value(operation,Params) of undefined->"";V->V end.
	
%% @spec update() -> Result
%% @type Result = term()
%% @doc update is the run function called by schedule to monitor performance.
update() ->
	debug_info("update begin..."),
	%%Initialize the status value, those values are for temporary use 
	initialize_stats(),

	THIS:set_attribute(result, -999.0),
	THIS:set_attribute(?STATE_STRING, ""),
	THIS:set_attribute(status, "nodata"),
	THIS:set_attribute(result_state, ""),

	%%get property
	{ok, {_, MonitorIds}} = THIS:get_property(items),
	Monitors = get_mulitple_values(MonitorIds, []),
	{ok, {_, D}} = THIS:get_property(delay),
	Delay = D * 1000,
	{ok, {_, Static}} = THIS:get_property(static),
	Constant = string:strip(Static),
	{ok, {_, IsRunMonitor}} = THIS:get_property(run_monitor),
	
	if
		length(Monitors) >= 2 ->
		%%if we have to run monitor before evaluate
		case IsRunMonitor of
			true ->
				%%run the monitor
				check_sequentially(Monitors, Delay, 1);
			_ ->
				%%update the status, directly
				update_stats(Monitors, 1)
		end,

		%%get result from update
		{ok, {_, Operate}} = THIS:get_property(operation),	
		{ok, {_, First}} = THIS:get_attribute(value_1),
		{ok, {_, Second}} = THIS:get_attribute(value_2),
		{ok, {_, Name1}} = THIS:get_attribute(name_1),
		{ok, {_, Name2}} = THIS:get_attribute(name_2),
		case Operate of
			%%add operation
			"Add" ->
				debug_info("Perform Add Operation"),
				case (First /= ?ERROR_VALUE andalso Second /= ?ERROR_VALUE) of		%%  is the initial value equal -999 ?! 
					true ->
						Addition = First + Second, 
						Result_State_Add = Name1 ++ " = " ++ number_string1(First) ++ ", <br>" ++
											Name2 ++ " = " ++ number_string1(Second) ++ ", <br>" ++
											"Add Result= " ++ number_string1(Addition),	
						THIS:set_attribute(result, Addition),
						THIS:set_attribute(status, "good"),
						THIS:set_attribute(result_state, Result_State_Add);
					false ->
						THIS:set_attribute(status, "n/a"),
						THIS:set_attribute(result_state, " Add Result= n/a")
				end;		
			%%multiply operation
			"Multiply" ->
				debug_info("Perform Multiply Operation"),
				case (First /= ?ERROR_VALUE andalso Second /= ?ERROR_VALUE) of
					true ->
						Multiplication = First * Second,
						Result_State_Multiply = Name1 ++ " = " ++ number_string1(First) ++ ", <br>" ++
												Name2 ++ " = " ++ number_string1(Second) ++ ", <br>" ++
												"Multiply Result= " ++ number_string1(Multiplication),
						THIS:set_attribute(result, Multiplication),
						THIS:set_attribute(status, "good"),
						THIS:set_attribute(result_state, Result_State_Multiply);
					false ->
						THIS:set_attribute(status, "n/a"),
						THIS:set_attribute(result_state, " Multiply Result= n/a")
				end;
			%%substract  1 operation
			"Substract12" ->
				debug_info("Perform Subtract12 Operation"),
				case (First /= ?ERROR_VALUE andalso Second /= ?ERROR_VALUE) of
					true ->
						Substract12 = First - Second,
						Result_State_Sub12 = Name1 ++ " = " ++ number_string1(First) ++ ", <br>" ++
											 Name2 ++ " = " ++ number_string1(Second) ++ ", <br>" ++
											 "Substract Result= " ++ number_string1(Substract12),
					
						THIS:set_attribute(result, Substract12),
						THIS:set_attribute(status, "good"),
						THIS:set_attribute(result_state, Result_State_Sub12);
					false ->
						THIS:set_attribute(status, "n/a"),
						THIS:set_attribute(result_state, " Subtract Result= n/a")
				end;
			%%substract 2 operation
			"Substract21" ->
				debug_info("Perform Subtract21 Operation"),
				case (First /= ?ERROR_VALUE andalso Second /= ?ERROR_VALUE) of
					true ->
						Substract21 = Second - First,
						Result_State_Sub21 = Name1 ++ " = " ++ number_string1(First) ++ ", <br>" ++
											 Name2 ++ " = " ++ number_string1(Second) ++ ", <br>" ++
											 "Substract Result= " ++ number_string1(Substract21),								
						THIS:set_attribute(result, Substract21),
						THIS:set_attribute(status, "good"),
						THIS:set_attribute(result_state, Result_State_Sub21);
					false ->
						THIS:set_attribute(status, "n/a"),
						THIS:set_attribute(result_state, " Subtract Result= n/a")
				end;
			%%divide  1 operation
			"Divide12" ->
				debug_info("Perform Divide12 Operation"),
				case (First /= ?ERROR_VALUE andalso Second /= ?ERROR_VALUE) of
					true ->
						Division12 = First/Second, 
						Result_State_Div12 = Name1 ++ " = " ++ number_string1(First) ++ ", <br>" ++
											 Name2 ++ " = " ++ number_string1(Second) ++ ", <br>" ++ 
											 "Divide Result= " ++ number_string1(Division12),
						THIS:set_attribute(result, Division12),
						THIS:set_attribute(status, "good"),
						THIS:set_attribute(result_state, Result_State_Div12);
					false ->
						THIS:set_attribute(status, "n/a"),
						THIS:set_attribute(result_state, " Divide Result= n/a")
				end;
			%%substract  2 operation
			"Divide21" ->
				debug_info("Perform Divide21 Operation"),
				case (First /= ?ERROR_VALUE andalso Second /= ?ERROR_VALUE) of
					true ->
						Division21 = Second/First,
						Result_State_Div21 = Name1 ++ " = " ++ number_string1(First) ++ ", <br>" ++
											 Name2 ++ " = " ++ number_string1(Second) ++ ", <br>" ++
											 "Divide Result= " ++ number_string1(Division21),					
						THIS:set_attribute(result, Division21),
						THIS:set_attribute(status, "good"),
						THIS:set_attribute(result_state, Result_State_Div21);
					false ->
						THIS:set_attribute(status, "n/a"),
						THIS:set_attribute(result_state, " Divide Result= n/a")
				end	
		end,
	
		{ok, {_, Status_Flag}} = THIS:get_attribute(status),
		Flag = (Status_Flag =/= "n/a" andalso length(Constant) > 0),
	
		%%perform the constant operation
		case Flag of
			true ->
				debug_info("Perform Static Operation"),
			
				%%get operation
				Operator = string:substr(Constant, 1, 1),
				%%get constant
				Operand_Str = string:sub_string(Constant, 2),
			
				%%get the result
				{ok, {_, Total}} = THIS:get_attribute(result),
				if 
					length(Operand_Str) > 0  ->
						Double = number_util:get_value(Operand_Str),
						if
							is_list(Double) ->
								THIS:set_attribute(status, "n/a"),
								THIS:set_attribute(result, ?ERROR_VALUE),
								error;
							(Double /= 0.0 andalso Double /= 0 andalso Total /= ?ERROR_VALUE) ->
								case Operator of
									"+" ->
									
										THIS:set_attribute(result, round((Total + Double)*10)/10);
									"-" ->
										THIS:set_attribute(result, round((Total - Double)*10)/10);
									"*" ->
										THIS:set_attribute(result, round((Total * Double)*10)/10);
									"/" -> 
										THIS:set_attribute(result, round((Total / Double)*10)/10);
									_ ->
										%% Static text is error,  the text must begin with one of [+,-,*,/].
										THIS:set_attribute(status, "n/a"),
										THIS:set_attribute(result, ?ERROR_VALUE),
										error									
								end,
								set_static_state();							
							true ->
								nothing
						end;
					true ->
						nothing
				end;
			_ ->
				nothing
		end,
	
		%%set status
		{ok, {_, State_Flag}} = THIS:get_attribute(status),
		case State_Flag of
			"good" ->
				set_good_status();
			_ ->
				THIS:set_attribute(result, ?ERROR_VALUE),
					{ok, {_, Static_State}} = THIS:get_attribute(result_state),
				THIS:set_attribute(?STATE_STRING, Static_State)
		end,
	
		true;
	
		true ->
		THIS:set_attribute(?STATE_STRING, "Two monitors must be selected"),
		THIS:set_attribute(status, "error")
	end.


%% @spec set_static_state() -> ok
%% 
%% @doc set_static_state is the function that set state after upate function being run.
set_static_state() ->
	debug_info("set static state"),
	{ok, {_, R}} = THIS:get_attribute(result_state),
	RS = string:sub_string(R, 1, string:str(R, "Result=")-1),
	{ok, {_, Val}} = THIS:get_attribute(result),
	Static_State = RS ++ "Result= " ++ number_string1(Val),
	THIS:set_attribute(result_state, Static_State),
	ok.


%% @spec set_good_status() -> ok
%% 
%% @doc set_good_status is the function that set state after upate function being run.
set_good_status() ->
	debug_info("set good status"),
	{ok, {_, Val}} = THIS:get_attribute(result),
	Result = number_string1(Val),
	{ok, {_, Label}} = THIS:get_property(value_label),
	{ok, {_, Result_State}} = THIS:get_attribute(result_state),
	STATES = case length(Label) of
				 N when (N > 0) ->
					 Result_State ++ "<br>" ++ Label ++ " = " ++ Result;
				 _ ->
					 Result_State
			 end,
	THIS:set_attribute(result_state, STATES),
	THIS:set_attribute(?STATE_STRING, STATES),
	ok.
	
%% @spec get_classifier(error) -> List
%% @type List = [Tuple]
%% @type Tule = {status, Logic, Value}
%% @type Logic = '!=' | '==' | '>' | '<' | 'contain
%% @type Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error) ->
	case THIS:get_property(error_classifier) of
		{ok, {error_classifier, Classifier}} ->
			Classifier;
		_ ->
			[{status, '==', "error"}]
	end;
get_classifier(warning) ->
	case THIS:get_property(warning_classifier) of
		{ok, {warning_classifier, Classifier}} ->
			Classifier;
		_ ->
			[{status, '==', "nodata"}]
	end;
get_classifier(good) -> 
	case THIS:get_property(good_classifier) of
		{ok, {good_classifier, Classifier}} ->
			Classifier;
		_ ->
			[{status, '==', "good"}]
	end.




%% @spec get_template_property() -> List
%% @type List = [term()]
%% @doc get_template_property is the function called by schedule to show user the GUI interface to input data.
get_template_property() ->
	BASE:get_template_property() ++ 
		[
		 #property{name=items, title="Items", type=scalar, multiple=true, listSize=3, order=1, description="Select two SNMP, Script, or Database monitors to be checked and their results calculated." },
		 #property{name=operation, title="Operation", type=scalar, order=2, description="Select the operation to be performed on the results of the above monitors. "},
		 #property{name=run_monitor, title="Run Monitors", type=bool, advance=true, order=3, description="Run each monitor before performing calculation."},
		 #property{name=delay, title="Monitor Delay", type=numeric, advance=true, order=4, description="If running each monitor, delay in seconds between monitor runs.",baselinable=true},
		 #property{name=static, title="Constant", type=text, advance=true, order=5, description="Operate a constant on the Operation result. For example entering *8 will multiply the Operation result by 8."},
		 #property{name=value_label, title="Result Label", type=text, advance=true, order=6, description="Optional label for the result of the formula calculation."},
		 #property{name=result, title="Result", type=numeric, configurable=false, state=true},
		 #property{name=status,title="Status", type=text, configurable=false,state=true}
		].


%% @spec getScalarValues(Prop, Params) -> List
%% @type Prop = atom()
%% @type Params = [term()]
%% @type List = [Opt]
%% @type Opt = {ShowContent, Value}
%% @type ShowContent = string()
%% @type Value = string()
%% @doc getScalarValues is the function called by schedule to show a dropdown list box and its value.
getScalarValues(Prop,Params) ->
	case Prop of
		%%monitor list
		items -> 		
			%%get the active monitor lisr
			SV = siteview:get_current_siteview(),
			List = SV:getMonitors(),
			Pred = fun(X) -> 
						   Monitor_Class = X:get_property(?CLASS),
							%%only evaluate the snmp, script, database and ntcounter monitors
						   case Monitor_Class of
							   {ok, {_, snmp_monitor}} -> true;
							   {ok, {_, script_monitor}} -> true;
							   {ok, {_, database_monitor}} -> true;
							   {ok, {_, ntcounter_monitor}} -> true;
							   _ -> false
						   end
				   end,
			%%get filtered monitor list	   
			Monitors = lists:filter(Pred, List),

			%%format the monitor list used for monitor
			Fun = fun(X) ->
						  {ok, {id, Id}} = X:get_property(id),						 
						  {X:get_full_name(), atom_to_list(Id)}
				  end,
			lists:map(Fun, Monitors);		
			
		%%return value calculate method	
		operation -> 		 
			[
			 {"Add", "Add"}, 
			 {"Multiply", "Multiply"}, 
			 {"Substract 1 from 2", "Substract12"}, 
			 {"Substract 2 from 1", "Substract21"}, 
			 {"Divide 1 by 2", "Divide12"},
			 {"Divide 2 by 1", "Divide21"}
			];		
		_ ->
			BASE:getScalarValues(Prop, Params)
	end.


%% @spec verify(Params) -> {ok, []} | {error, Reason}
%% @type Params = [term()]
%% @type Reason = string()
%% @doc verifying data input by user is correct or not.
verify(Params) ->
	Items = proplists:get_value(items,Params),
	Errs = 
		case length(Items) of		
			Len when (Len =< 0) ->
				[{items, "no monitors selected"}];
			_ ->
				[]
		end ++ 
			case proplists:get_value(delay, Params) of
				""->
					[{delay, "delay time missing."}];
				V->
					if
						not is_number(V) ->
							[{delay, "delay time must be a number."}];
						true->
							[]
					end
		end ++ 
			case proplists:get_value(static, Params) of
				"" ->
					[];
				Const  ->
					case regexp:match(Const,"^[(\\+|\\-|\\*|\\/)](\\+|-)?[0-9]+(\\.[0-9]+((E|e)(\\+|-)?[0-9]+)?)?$") of						
						nomatch ->
							[{static, "input format error, for example: *8 "}];
						_ -> 
							[]
					end
		end ++ 
			case BASE:verify(Params) of
				{error, E} ->
					E;
				_ ->
					[]
		end,
	if 
		length(Errs) >0 ->
			{error, Errs};
		true ->
			{ok, ""}
	end.
	

%% @spec check_sequentially(List, Delay, Nth) - ok
%% @type List = [term()]
%% @type Delay = Nth = integer()
%% @doc check_sequentially is the function called by update function to execute monitor sequentially.
check_sequentially([], _, _) ->
	ok;
check_sequentially([H|Monitors], Delay, Nth) -> 
	debug_info("check sequentially " ++ lists:concat([Nth])),
	
	%%this property is temprary not exsit 
	Start_Up_Time = THIS:get_property_as_number("composite_start_up_time", 500),
	Check_Delay = THIS:get_property_as_number("composite_check_delay", 500),
	
	%%run monitor
	H:runUpdate(H, true, false),
	THIS:set_attribute(?STATE_STRING, "Running monitor: " ++ H:get_name() ++ ", <br>" ++ "checking..." ++ "<br>"),
	
	%%sleep to wait for monitor run over
	THIS:sleep(Start_Up_Time),
	
	%%still running? wait for it to end
	{ok, {_, IsRunning}} = H:get_attribute(running),
	running_loop(IsRunning, H, Check_Delay),

	%%update the monitors state
	update_single_state(H, Nth),
	
	%%if delay value are set, sleep
	if
		(Delay > 0) ->
			THIS:sleep(Delay);
		true ->
			ok
	end,
	check_sequentially(Monitors, Delay, Nth + 1).

%% @spec running_loop(Bool, Monitor, Delay) -> ok
%% @type Bool = boolean()
%% @type Monitor = term()
%% @type Delay = integer()
%% @doc running_loop is the function to run monitor till it stoped.
running_loop(Bool, Monitor, Delay) ->
	case Bool of
		true ->
			THIS:sleep(Delay),
			{ok, {_, IsRunning}} = Monitor:get_attribute(running),
			running_loop(IsRunning, Monitor, Delay);
		false ->
			ok
	end.


%% @spec update_stats(List, Nth) -> ok
%% @type List = [term()]
%% @type Nth = integer()
%% @doc update_stats is the function that save states into this monitor.
update_stats([], _) ->
	ok;
update_stats([M|Monitors], Nth) ->
	debug_info("update stats --- " ++ lists:concat([Nth])),
	%%call single monitor update
	update_single_state(M, Nth),

	%%only two values needed to formula
	if
		Nth < 2 ->
			update_stats(Monitors, Nth + 1);
		true ->
			update_stats([], Nth + 1)
	end.
	

%% @spec update_single_state(Monitor, Nth) -> ok
%% @type Monitor = term()
%% @type Nth = integer()
%% @doc update_single_state is the function that save single state into this monitor.
update_single_state(Monitor, Nth) ->
	debug_info("update single state " ++ lists:concat([Nth])),
	%%get monitor name
	Name = Monitor:get_name(),
	{ok, {_, Catagory}} = Monitor:get_attribute(?CATEGORY),
	case Catagory of
		error ->
			Monitor:set_attribute(?MEASUREMENT, ?ERROR_VALUE);
		_ ->
			ok
	end,
	
	%%monitor still running, wait for it to end
	case Monitor:get_attribute(running) of
		{ok, {_, Bool}} ->
			running_loop(Bool, Monitor, 500);
		_ ->
			ok
	end,
	
	%%set the measurement  
	Monitor_Class = Monitor:get_property(?CLASS),
	%%only evaluate the snmp, script, database and ntcounter monitors
	%%get the mesurement differently
	case Monitor_Class of
		{ok, {_, snmp_monitor}} -> 
			case Monitor:get_attribute(snmpValue) of
				{ok, {_, VV}} 	-> VV;
				_		-> VV = -999.0
			end;
		{ok, {_, script_monitor}} ->
			case Monitor:get_attribute(value) of
				{ok, {_, VV}}	-> VV;
				_		-> VV = -999.0
			end;
		{ok, {_, database_monitor}} -> 
			case Monitor:get_attribute(column_1th) of
				{ok, {_, VV}}	-> VV;
				_		-> VV = -999.0
			end;
		{ok, {_, ntcounter_monitor}} ->
			case Monitor:get_attribute(values) of
				{ok, {_, VVVV}} ->
					KV = tuple_to_list(VVVV),
					if
						length(KV) > 0 ->
							VV = lists:nth(1, KV);
						true ->
							VV = -999.0
					end;
				_		-> VV = -999.0
			end
	end,
	
	%%change the value to integer or float
	VVV = number_util:get_value(VV),
	%% io:format("~n****************~n Monitor_Class = ~p~n VV = ~p~n",[Monitor_Class,VVV]),
	if
		is_list(VVV) ->
			V = -999.0;
		true ->
			V = VVV
	end,
	
	%%set the attribute
	case Nth of 
		1 ->
			THIS:set_attribute(name_1, Name),
			THIS:set_attribute(value_1, V);
		2 ->
			THIS:set_attribute(name_2, Name),
			THIS:set_attribute(value_2, V);
		_ ->
			done
	end.
		


%% @spec initialize_stats() -> ok
%%
%% @doc initialize is the function initializing the states of this monitor.
initialize_stats() ->
	THIS:set_attribute(value_1, 0.0),
	THIS:set_attribute(value_2, 0.0),
	THIS:set_attribute(name_1, ""),
	THIS:set_attribute(name_2, ""),
	ok.

	
%% @spec get_multiple_values(Ids, Monitors) -> NewMonitors
%% @type Ids = Monitors = NewMonitors = [term()]
%% @doc get_multiple_values is the function to get monitors which id in Ids list.
get_mulitple_values([], Monitors) ->
	debug_info("get multiple values ok. "),
	Monitors;
get_mulitple_values([H|Ids], Monitors) ->
	[M] = api_siteview:find_object(H),
	New_Monitors = Monitors ++ [M],
	get_mulitple_values(Ids, New_Monitors).
	

%% @spec number_string(Val) -> String
%% @type Val = numeric()
%% @type String = string()
%% @doc number_stirng is the function that convert numeric value into stirng value.
number_string1(Val) ->
	S = case Val of
			Val when (erlang:is_integer(Val)) ->
				lists:flatten(io_lib:format("~.1f", [Val * 1.0]));
			Val when (erlang:is_float(Val)) ->
				lists:flatten(io_lib:format("~.1f", [Val]));
			Val when (erlang:is_list(Val)) ->
				Val;
			_ ->
				"0.0"
		end,
	debug_info("number to string1: " ++ S),
	S.

%% @spec debug_info(Info) -> ok
%% @type Info = term()
%% @doc debug_info is the function to save debug info. 
debug_info(Info) ->
	{ok, {_, Old}} = THIS:get_attribute(?DEBUG_INFO),
	New = Old ++ ";\n" ++ Info,
	THIS:set_attribute(?DEBUG_INFO, New),
	ok.

%% @spec pirnt_debug_info() -> ok.
%% @doc print debug info in this monitor.
print_debug_info() ->
	{ok, {_, Info}} = THIS:get_attribute(?DEBUG_INFO),
	io:format("~s~n", [Info]).



