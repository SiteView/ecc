%%
%% snmp_trap_base
%%
-module(snmp_trap_base,[BASE]).
-extends(atomic_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").
-include_lib("snmp/include/snmp_types.hrl").

-export([new/0, getScalarValues/2, update/0, handle_trap/2, match_trap/2, is_string_var/1, get_template_property/0]).

%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for snmp trap base
new()->
	Obj = atomic_monitor:new(),
	Obj:set_attribute(lineCount,0),
	Obj:set_attribute(matchCount,0),
	Obj:set_attribute(lastAlertsPerMinute,0),
	Obj:set_attribute(lastLinesPerMinute,0),
	Obj:set_attribute(lastMeasure,sv_datetime:now()),
    %%%%%%%%%%%%new add
    Obj:set_attribute(myLineCount,0),
	Obj:set_attribute(myMatchCount,0),
	{?MODULE,Obj}.


runOwnRules()->
	case THIS:get_property(alerting) of
		{ok,{_,"each"}}->
			true;
		_->
			false
	end.

%% @spec getScalarValues(Property,Params) -> ValueList
%% Property = atom()
%% Params = [{PropertyName,PropertyValue}]
%% PropertyName = atom()
%% PropertyValue = string()
%% ValueList = [{Scalarname,Scalarvalue}]
%% Scalarname = string()
%% Scalarvalue = string()
%% @doc get scalar properties value.
getScalarValues(Prop,Params)->
	case Prop of
		alerting->
			[{"for each SNMP Trap matched","each"},{"once, after all SNMP Traps have been checked","once"}];
		_->
			BASE:getScalarValues(Prop,Params)
	end.

%% @spec update() -> Result
%% Result = term()
%% @doc update is the run function called by schedule to test the snmp trap information
update()->
	%%{ok,{_,Lines}} = THIS:get_attribute(lineCount),
	%%{ok,{_,Matchs}} = THIS:get_attribute(matchCount),
    {ok,{_,MyLines}} = THIS:get_attribute(myLineCount),
	{ok,{_,MyMatchs}} = THIS:get_attribute(myMatchCount),
	{ok,{_,LastMeasure}} = THIS:get_attribute(lastMeasure),
    io:format("*** update ***"
	      "~n   lineCount: ~p"
	      "~n   matchCount:     ~p"
          "~n   lastMeasure:     ~p"
	      "~n", [MyLines, MyMatchs,LastMeasure]),
    THIS:set_attribute(lineCount,MyLines),
    THIS:set_attribute(matchCount,MyMatchs),
    Lines = MyLines,
	Matchs = MyMatchs,
    
	Now = sv_datetime:now(),
	Inter = (Now -LastMeasure)/60000,
	THIS:set_attribute(lastAlertsPerMinute,Matchs/Inter),
	THIS:set_attribute(lastAlertsPerMinute,Lines/Inter),
	Stat = integer_to_list(Matchs) ++ " matchs<br>" ++ lists:flatten(io_lib:format("~.3f",[Matchs/Inter])) ++ " matches/min<br>"
			++ lists:flatten(io_lib:format("~.3f",[Lines/Inter])) ++ " trap entries/min",
	THIS:set_attribute(?STATE_STRING,Stat),
	%%THIS:set_attribute(lineCount,0),
	%%THIS:set_attribute(matchCount,0),
    THIS:set_attribute(myLineCount,0),
	THIS:set_attribute(myMatchCount,0),
	THIS:set_attribute(lastMeasure,sv_datetime:now()),
	case THIS:runOwnRules() of
		true ->
			THIS:set_attribute(?CATEGORY,?GOOD_CATEGORY);
		_->
			ok
	end,
	ok.

%% @spec handle_trap(This,Trap) -> Result
%% This = module
%% Trap = tuple
%% Result = term()
%% @doc separate Varbinds info from snmp message and match it
handle_trap(This,Trap)->
	
    {ok,{_,Lines}} = THIS:get_attribute(myLineCount),
    THIS:set_attribute(myLineCount,Lines +1),
	{Addr,Port,Info} = Trap,
	case Info of
		{Enteprise, Generic, Spec, Timestamp, Varbinds} ->
			This:forwardAlerts(Varbinds);
		{ErrorStatus, ErrorIndex, Varbinds} ->
			This:forwardAlerts(Varbinds);
		_->
			lists:flatten(io:format("unknow trap:~p~n",[Trap]))
	end,
	ok.
    
save_log(Trap)-> 
    %%io:format("**********ok~n"),
	{Addr,Port,Info} = Trap,
	case Info of
		{Enteprise, Generic, Spec, Timestamp, Varbinds} ->
            Infos = "from=" ++ to_IpString(snmp_trap_data:getlocalAddr()) ++ ":162" ++ "\toid="
                        ++ " n/a" ++ "\ttrap="
                        ++ any_to_list(Generic)
                        ++ "\tspecific="
                        ++ any_to_list(Spec)
                        ++ "\ttraptime=" ++ any_to_list(Timestamp)
                        ++ "\tcommunity=" ++ proplists:get_value(community, Varbinds)
                        ++ "\tagent=" ++ "n/a" ++ to_IpString(Addr) ++ ":" ++ integer_to_list(Port)
                        ++ "\tversion= " ++ erlang:atom_to_list(proplists:get_value(version, Varbinds)),
            TrapInfo = build_trapInfo(Infos,Varbinds),
            snmp_trap_data:log(TrapInfo);
		{ErrorStatus, ErrorIndex, Varbinds} ->
			%%io:format("**********ok1~n"),
            Infos = "from=" ++ to_IpString(snmp_trap_data:getlocalAddr()) ++ ":162" ++ "\toid="
                        ++ " n/a" ++ "\ttrap= n/a" ++ "\tspecific= n/a"
                        ++ "\ttraptime=" ++ "n/a" ++ "\tcommunity="
                        ++ proplists:get_value(community, Varbinds) ++ "\tagent=" ++ to_IpString(Addr) ++ ":" ++ integer_to_list(Port)
                        ++ "\tversion= " ++ erlang:atom_to_list(proplists:get_value(version, Varbinds)),
            %%io:format("**********ok2~n"),
            TrapInfo = build_trapInfo(Infos,Varbinds),
            %%io:format("**********ok3     Info = ~s~n", [TrapInfo]),
            
            snmp_trap_data:log(TrapInfo);

		_->
			lists:flatten(io:format("unknow trap:~p~n",[Trap]))
	end,
	ok.




%%***********************Expression match*************************

any_to_list(AnyType) ->
    if
        is_tuple(AnyType) == true ->
            tuple_to_list(AnyType);
        is_integer(AnyType) == true ->
            integer_to_list(AnyType);
        is_float(AnyType) == true ->
            float_to_list(AnyType);
        is_list(AnyType) == true ->
            AnyType;
        is_atom(AnyType) ->
            atom_to_list(AnyType);
        is_binary(AnyType) ->
            binary_to_list(AnyType);
        is_bitstring(AnyType) ->
            io:format("is bitstring~n"),
            bitstring_to_list(AnyType);
        is_boolean(AnyType) ->
            atom_to_list(AnyType);
        is_pid(AnyType) ->
            pid_to_list(AnyType);
        is_port(AnyType) ->
            erlang:port_to_list(AnyType);
        %%is_record(AnyType) ->
        %%    AnyType;
        %%is_reference(AnyType) ->
        %%    AnyType;
        true ->
            AnyType
    end.


build_trapInfo(Info,[]) -> Info;
build_trapInfo(Info, [Vb=#varbind{}|T]) ->
    Infos = Info ++ "\tvar" ++ integer_to_list(Vb#varbind.org_index) ++ "=" ++ vb_to_snmpstring(Vb),
    build_trapInfo(Infos, T);
build_trapInfo(Info, [_|T]) -> build_trapInfo(Info, T).


vb_to_snmpstring(Vb=#varbind{}) ->
    case Vb#varbind.variabletype of
        'INTEGER' -> 
            M = integer_to_list(Vb#varbind.value);
            %%io:format("INTEGER:~p~n",[M]);
        'OCTET STRING' ->
            M = Vb#varbind.value;
            %%io:format("OCTET STRING:~p~n",[M]);
        'NULL' ->
            M = "NULL";
        'OBJECT IDENTIFIER' ->
            M = to_OidString(Vb#varbind.value);
            %%io:format("OBJECT IDENTIFIER:~p~n",[M]);
        'IpAddress' ->
            M = to_OidString(Vb#varbind.value);
            %%io:format("IpAddress:~p~n",[M]);
        'Counter32' ->
            M = integer_to_list(Vb#varbind.value);
            %%io:format("Counter32:~p~n",[M]);
        'Unsigned32' ->
            M = integer_to_list(Vb#varbind.value);
            %%io:format("Unsigned32:~p~n",[M]);
        'TimeTicks' ->
            M = integer_to_list(Vb#varbind.value);
            %%io:format("TimeTicks:~p~n",[M]);
        'Opaque' ->
            M = Vb#varbind.value;
            %%io:format("Opaque:~p~n",[M]);
        'Counter64' ->
            M = integer_to_list(Vb#varbind.value);
            %%io:format("Counter64:~p~n",[M]);
        _ -> 
            M = Vb#varbind.value
            
    end,
    M.


matchExpression([],_) -> error;
matchExpression([Vb=#varbind{}|T], Match) ->
    M = vb_to_snmpstring(Vb),
    case (catch regexp:match(M, Match)) of
        {match,_,_} ->
            ok;
        _ ->
            matchExpression(T, Match)
    end;
matchExpression([_|T],Match)->
	matchExpression(T,Match).

%%to_IpOrOidString(T) ->
%%     Temp = lists:map(fun(X) -> integer_to_list(X) end, T),
%%     string:join(Temp, ".").
     
to_IpString(T) ->
    to_OidString(tuple_to_list(T)).

to_OidString(T) ->
    Temp = lists:map(fun(X) -> integer_to_list(X) end, T),
    string:join(Temp, ".").

    
    
%%***********************Expression match*************************


forwardAlerts(M) ->
    {ok,{_,Match}} = THIS:get_property(match),
    %%io:format("~nMatchContent:~p~n",[Match]),
    IsMatch = matchExpression(M, Match),
    if
        IsMatch == ok -> 
    %%如果匹配上就执行下一句
            {ok,{_,Macth}}= THIS:get_attribute(myMatchCount),
            THIS:set_attribute(myMatchCount,Macth+1),
            THIS:set_attribute(value,""),
            THIS:set_attribute(value2,""),
            THIS:set_attribute(value3,""),
            THIS:set_attribute(value4,""),
            match_trap(M, 1),
            case THIS:runOwnRules() of
                true ->
                    THIS:set_attribute(?CATEGORY,?ERROR_CATEGORY),
                    THIS:runActionRules(THIS,?ERROR_CATEGORY),
                    THIS:incrementProperty(atom_to_list(?ERROR_CATEGORY) ++ "Count");
                _->
                    do_nothing
            end;
        true ->
            do_nothing
    end.

%% @spec match_trap(Varbinds,I) -> Result
%% Varbinds = [record(varbind, {oid, variabletype, value, org_index})]
%% I = integer()
%% Result = term()
%% @doc match Varbinds
match_trap([],_)->ok;
match_trap([Vb=#varbind{}|T],I)->
    case I of
        1->
            THIS:set_attribute(value,Vb#varbind.value);
        2->
            THIS:set_attribute(value2,Vb#varbind.value);
        3->
            THIS:set_attribute(value3,Vb#varbind.value);
        4->
            THIS:set_attribute(value4,Vb#varbind.value);
        _ ->
            ok
    end,
    THIS:match_trap(T,I+1);
    
match_trap([_|T],I)->
	match_trap(T,I).


%% @spec is_string_var(Vb) -> Result
%% Vb = [record(varbind, {oid, variabletype, value, org_index})]
%% Result = bool()
%% @doc judge a Varbind's type
is_string_var(Vb)->
	case Vb#varbind.variabletype of
		'OCTET STRING'->
			true;
		'IpAddress'->
			true;
		'OBJECT IDENTIFIER'->
			true;
		_->
			false
	end.


%% @spec get_template_property()-> Result
%% Result = [Property]
%% Property = record(varbind, {oid, variabletype, value, org_index})
%% @doc get monitor template of snmp trap monitor
get_template_property()->
	BASE:get_template_property() ++ 
	[
    
	#property{name=match,title="Content Match", description="by default, all SNMP Traps received will be matched.  Optionally, enter the text to match in an SNMP Trap",type=text,editable=true,order=1},
	#property{name=alerting,title="Run Alerts", type=scalar,editable=true,order=1,description="How alerts for this monitor are triggered by the following options:  <table width=100% border=0><TR><TD>(1) For <b>'for each SNMP Trap entry matched and report status'</b> the monitor triggers alerts for every matching SNMP trap entry found based on the <b>Error If</b> and <b>Warning If</b> thresholds defined for the monitor. </TD></TR>\n<TR><TD>(2) For <b>'once, after all SNMP Traps have been checked'</b>, the traps received since the last monitor run are checked and alerts are triggered based on the <b>Error If</b> and <b>Warning If</b> thresholds defined for the monitor. </TD></TR></table>\n"},
	#property{name=lineCount,title="lines", type=numeric,state=true,configurable=false},
	#property{name=matchCount,title="matches", type=numeric,state=true,configurable=false},
	#property{name=lastAlertsPerMinute,title="matches/min", type=numeric,state=true,configurable=false},
	#property{name=lastLinesPerMinute,title="lines/min", type=numeric,state=true,configurable=false},
	#property{name=value,title="value", type=numeric,state=true,configurable=false},
	#property{name=value2,title="value2", type=numeric,state=true,configurable=false},
	#property{name=value3,title="value3", type=numeric,state=true,configurable=false},
	#property{name=value4,title="value4", type=numeric,state=true,configurable=false}
    
	
	].