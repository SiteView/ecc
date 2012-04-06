%% ---
%% health_unixserver_monitor
%%
%%---
-module(health_unixserver_monitor,[BASE]).
-extends(multiContent_base).
-compile(export_all).
-include("monitor_template.hrl").
-include("monitor.hrl").

-define(MAX_COUNTERS,30).
-define(HEALTHPATH, "templates.health/").
-define(ETSNAME, health_unixserver_ets).

new()->
	Obj = multiContent_base:new(),
	{?MODULE,Obj}.


update()->
	Ret = THIS:runScript(),
	{ok,{counters,Counters}} = THIS:get_property(counters),
	if
		is_list(Ret)->
			THIS:set_attribute(?STATE_STRING,THIS:parse_value(Counters,Ret)),
			THIS:set_attribute(status,"ok");
		true->
			THIS:set_attribute(?STATE_STRING,snmp_ex2_manager:any_to_list(Ret)),
			THIS:set_attribute(status,"error")
	end.
	

parse_value([],_)->"";
parse_value([{K,_}|T],Data)->
	Val = case proplists:get_value(K,Data) of
		undefined->
			THIS:inc_attribute(countersInError),
			undefined;
		V->
			V
		end,
	THIS:set_attribute(K,Val),
	%%io_lib:format("~p=~p<br>",[K,Val]) ++ THIS:parse_value(T,Data).
    snmp_ex2_manager:any_to_list(K) ++ " = " ++ snmp_ex2_manager:any_to_list(Val) ++ "<br>" ++ THIS:parse_value(T,Data).

getCountersContent()->
	case THIS:get_property(counters) of
		{ok,{_,V}}->
			V;
		_->
			[]
	end.



getCostInLicensePoints()->0.

%% Read non-designated application filters...
getCmdResult_Windows(CmdExeReStr) ->
    case string:tokens(CmdExeReStr, "\r\n") of
        Result ->
            buildCounters(Result, 1, Result);
        _ ->
            []
    end.
    
getCmdResult_Unix(CmdExeReStr) ->
    case string:tokens(CmdExeReStr, "\n") of
        Result ->
            buildCounters(Result, 1, Result);
        _ ->
            []
    end.
buildCounters([], I, Result) ->
    [];
buildCounters([CmdRe|T], I, Result) ->
    case (I rem 2) of
        1 ->
            Length = string:len(Result),
            Value =
            if
                Length >= I + 1 ->
                    lists:nth(I + 1, Result);
                true ->
                    []
            end,
            [{CmdRe,Value}] ++
            buildCounters(T, I + 1, Result);
        _ ->
            buildCounters(T, I + 1, Result)
    end.

runScript()->
	{{_,Input},{_,Output}}=erlang:statistics(io),
    DefaultValue =
	[{"memory",proplists:get_value(total,erlang:memory())},{"process_count",erlang:system_info(process_count)},
	{"run_queue",erlang:statistics(run_queue)},{"io_input",Input},{"io_output",Output}
	],
    OsType =
    case os:type() of
        {Osfamily, _} ->
            Osfamily;
        Osfamily ->
            Osfamily
    end,
    ResultStr =
    if
        OsType =:= win32 ->
            getCmdResult_Windows(os:cmd("templates.health\\uberscript.bat"));
        OsType =:= unix ->
            getCmdResult_Unix(os:cmd("templates.health/health_siteview_server.sh"));
        true ->
            []
    end,
    FinalResult = DefaultValue ++ ResultStr,
    FinalResult.

getDefaultCounters()->
	THIS:getAvailableCounters(THIS).

%% *******new method******************
%% The existence of the ets table this mib_node
is_exist_mib_node() ->
    lists:member(?ETSNAME, ets:all()).

make_a_set() ->
    ets:new(?ETSNAME, [public,named_table]). 
    
    
get_script_counters(Item) ->
    case ets:lookup(?ETSNAME, Item) of
        undefined ->
            {error, undefine_item};
        [{Item, Value}] ->
            {ok, Value};
        Other ->
            {error, Other}
    end.
set_script_counters(Item, Value) ->
    ets:insert(?ETSNAME,{Item, Value}).
    
%% init
health_unixserver() -> 
    case is_exist_mib_node() of
        false ->
            make_a_set(),
            set_script_counters(health_unix_script, runScript()),
            case get_script_counters(health_unix_script) of
                {ok, Value} ->
                    Value;
                _ ->
                    []
            end;
        _ ->
            case get_script_counters(health_unix_script) of
                {ok, Value} ->
                    Value;
                _ ->
                    []
            end
    end.
%% ****************************************

getAvailableCounters(This)->
    Counters = health_unixserver(),
    
    %%SptOut =
    %%case This:get_attribute(scriptOut) of
    %%    {ok,{_,ScriptOut}} ->
    %%        ScriptOut;
    %%    Other ->
    %%        io:format("Other = ~p~n", [Other]),
    %%        []
    %%end,
    %%io:format("SptOut = ~p~n", [SptOut]),
    
    %%OutCounters =
    %%case string:len(SptOut) of
    %%    0 ->
    %%        Out = This:runScript(),
    %%        This:set_attribute(scriptOut, Out),
    %%        Out;
    %%    _ ->
    %%        SptOut
    %%end,
    %%io:format("Counters = ~p~n", [OutCounters]),
	[{X,X}||{X,_}<-Counters].

get_classifier(error)->
	Count = THIS:getMaxCounters(),
	Cls = case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'!=',"ok"}]
	end,
	if 
		length(Cls) < Count ->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,Count - length(Cls)));
		true ->
			Cls
	end;
get_classifier(warning)->
	Count = THIS:getMaxCounters(),
	Cls = case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'!=',"ok"}]
	end,

	if 
		length(Cls) < Count ->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,Count - length(Cls)));
		true ->
			Cls
	end;
get_classifier(good)->
	Count = THIS:getMaxCounters(),
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'==',"ok"}]
	end,
	if 
		length(Cls) < Count ->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,Count - length(Cls)));
		true ->
			Cls
	end.

get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=status,title="status",configurable=false,state=true},
	#property{name=counters,title="Counters", description="Current selection of counters.",type=counters,editable=true,order=1,default=THIS:getDefaultCounters()}
	].
