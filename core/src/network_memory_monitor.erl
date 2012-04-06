%%
%% network_memory_monitor
%%
%%
-module(network_memory_monitor,[BASE]).
-extends(atomic_monitor).
-compile(export_all).
-export([new/0, defaultTitle/1, update/0, check_match/1, do_measure/5, do_snmp_v2/5, do_snmp_v3/7, getScalarValues/2, get_classifier/1, verify/1, get_template_property/0]).
-define(SNMP_PORT,161).
-include("monitor_template.hrl").
-include("monitor.hrl").
-include_lib("snmp/include/snmp_types.hrl").

-define(SNMP_OID,"templates.nnm/memoryConf.dat").			
  

-define(SCALES,[{"no scaling","no"},
				{"TimeTicks to minutes","6000"},
				{"TimeTicks to hours","360000"},
				{"seconds to minutes","60"},
				{"seconds to hours","3600"},
				{"bytes to bits","0.125"},
				{"bits to bytes","8"},
				{"bytes to kilobytes","1024"},
				{"bytes to megabytes","1048576"},
				{"divide by 10","10"},
				{"divide by 100","100"},
				{"divide by 1000","1000"},
				{"divide by 10000","10000"},
				{"divide by 100000","100000"}
				]).


%% @spec new() -> Obj
%% where
%% Obj = term()
%% @doc create a new instance for snmp monitor
new()->
	Base = atomic_monitor:new(),
	Base:set_attribute(snmpValue,0),
	Base:set_attribute(matchValue,""),
	Base:set_attribute(content,""),
	Base:set_attribute(snmpMeasurement,0),
	Base:set_attribute(baseMeasurement,0),
	Base:set_attribute(baseValue,0),
	Base:set_attribute(status,"ok"),
	Base:set_attribute(snmpTime,0),
	{?MODULE,Base}.

%% @spec defaultTitle(Params) -> Obj
%% Obj = string()
%% @doc create a title for snmp monitor
defaultTitle(Params)->
	BASE:defaultTitle(Params) ++ ":" ++
	proplists:get_value(host,Params) ++ " " ++
	case file:consult(filename:nativename(?SNMP_OID)) of
        {ok, Config} ->
			case lists:keysearch(proplists:get_value(oid,Params),2,Config) of
				{value,{Desc,_}}->
					Desc;
				_->
					proplists:get_value(oid,Params)
			end ++
			case proplists:get_value(oidIndex,Params) of
				"0"->
					"";
				OidIndex ->
					"[" ++ integer_to_list(OidIndex) ++ "]"
			end;
        _ ->
         {error, 'read_error'}
    end.
	
	


%% @spec update() -> Result
%% Result = term()
%% @doc update is the run function called by schedule to test the snmp information
update()->
	%%io:format("network_memory_monitor run..."),
	{ok,{host,Host}} = THIS:get_property(host),
	{ok,{community,Comm}} = THIS:get_property(community),
	{ok,{oid,OId}} = THIS:get_property(oid),
	io:format("Memory_monitor_OId:~p~n",[OId]),
	{ok,{timeout,Timeout}} = THIS:get_property(timeout),
	{ok,{_,LastTime}} = THIS:get_attribute(snmpTime),
	{ok,{_,LastVal}} = THIS:get_attribute(snmpMeasurement),
	{ok,{_,LastBase}} = THIS:get_attribute(baseMeasurement),
	{ok,{_,OidIndex}} = THIS:get_property(oidIndex),


	H = 
	case ip_utils:check_ip(Host) of
		{error,_}->
			if
				length(Host) =<0->
					THIS:set_attribute(?NO_DATA,true),
					THIS:set_attribute(?CATEGORY,error),
					THIS:set_attribute(status,"error"),
					THIS:set_attribute(?STATE_STRING,"Host Name is empty."),
					"";
				true ->
					case inet:gethostbyname(Host) of
						{error,_}->
							THIS:set_attribute(?NO_DATA,true),
							THIS:set_attribute(?CATEGORY,error),
							THIS:set_attribute(status,"error"),
							THIS:set_attribute(?STATE_STRING,"Host Name invalid."),
							"";
						{ok,{_,_,_,_,_,[Ip|_]}}->
							Ip;
						_->
							THIS:set_attribute(?NO_DATA,true),
							THIS:set_attribute(?CATEGORY,error),
							THIS:set_attribute(status,"error"),
							THIS:set_attribute(?STATE_STRING,"Host Name invalid.")
					end
			end;
		_->
			list_to_tuple([list_to_integer(X) || X <- string:tokens(Host,".")])
	end,
	
	case H of
		""->
			{error,host_name_empty};
		_->

			O = [list_to_integer(X) || X <- string:tokens(OId,".")],

			{ok,{snmpversion,Ver}} = THIS:get_property(snmpversion),
			{ok,{snmpv3user,User}} = THIS:get_property(snmpv3user),
			{ok,{snmpv3passwd,Passwd}} = THIS:get_property(snmpv3passwd),
			{ok,{_,MeasureDelta}} = THIS:get_property(measureDelta),

			snmp_ex2_manager:start_link(),
            
            %%Session0 = snmp_session:new(H,161,Ver,Comm,"",User,Passwd,"","","",Timeout*1000,[]),
            %%Engine = Session0:get_engineid_fromdev(),
            %%io:format("Engineseee = ~p~n", [Engine]),
            
			Session = snmp_session:new(H,161,Ver,Comm,"",User,Passwd,"","","",Timeout*1000),
            case Session:test_snmp() of
                true ->
                    Err1 = case Ver of
                        "v1"->
                            do_snmp_v2(H,Comm,O++[OidIndex],Timeout, Session);
                        "v2"->
                            do_snmp_v2(H,Comm,O++[OidIndex],Timeout, Session);
                        "v3"->
                            do_snmp_v3(H,Comm,O++[OidIndex],Timeout,User,Passwd,Session)
                    end,
                    THIS:set_attribute(snmpTime,sv_datetime:now()),
                    Err2 = THIS:check_match(Err1),
                    Err3 = THIS:do_measure(Err2,LastVal,LastTime,LastBase,Session),
                    Err5=
                    if
                        MeasureDelta==true andalso length(Err3) =<0 andalso LastTime==0 ->
                            {ok,{_,LastTime2}} = THIS:get_attribute(snmpTime),
                            {ok,{_,LastVal2}} = THIS:get_attribute(snmpMeasurement),
                            {ok,{_,LastBase2}} = THIS:get_attribute(baseMeasurement),
                            platform:sleep(2000),
                            Err4=case Ver of
                                    "v1"->
                                        do_snmp_v2(H,Comm,O++[OidIndex],Timeout, Session);
                                    "v2"->
                                        do_snmp_v2(H,Comm,O++[OidIndex],Timeout, Session);
                                    "v3"->
                                        do_snmp_v3(H,Comm,O++[OidIndex],Timeout,User,Passwd,Session)
                                end,
                            THIS:set_attribute(snmpTime,sv_datetime:now()),
                            THIS:do_measure(Err4,LastVal2,LastTime2,LastBase2,Session);
                        true ->
                            Err3
                    end,
                    if
                        length(Err5)>0->
                            THIS:set_attribute(?STATE_STRING,Err5);
                        true ->
                            {ok,{_,SnVal}} = THIS:get_attribute(snmpValue),
                            {ok,{_,Mes}} = THIS:get_attribute(?MEASUREMENT),
                            {ok,{_,MesDesc}} = THIS:get_property(measurementDesc),
                            {ok,{_,Units}} = THIS:get_property(units),
                            Disp = if
                                        is_number(SnVal)->
                                            Mes;
                                        true->
                                            SnVal
                                    end,
                            case Units of
                                ""->
                                    %%THIS:set_attribute(?STATE_STRING,lists:flatten(io_lib:format("~s=~p",[MesDesc,Disp])));
                                    THIS:set_attribute(?STATE_STRING,snmp_ex2_manager:any_to_list(MesDesc) ++ "=" ++ snmp_ex2_manager:any_to_list(Disp));
                                _->
                                    %%THIS:set_attribute(?STATE_STRING,lists:flatten(io_lib:format("~s=~p ~s",[MesDesc,Disp,Units])))
                                    THIS:set_attribute(?STATE_STRING,snmp_ex2_manager:any_to_list(MesDesc) ++ "=" ++ snmp_ex2_manager:any_to_list(Disp) ++ " " ++ snmp_ex2_manager:any_to_list(Units))
                            end
                    end;
                _ ->
                    THIS:set_attribute(?NO_DATA,true),
                    THIS:set_attribute(?CATEGORY,error),
                    THIS:set_attribute(status,"error"),
                    THIS:set_attribute(?STATE_STRING,"Application Server not available on host")
            end
		end.

%% @spec check_match(Err) -> Result
%% Result = string()
%% @doc match snmp info with match contents
check_match("")->
	case THIS:get_property(content) of
		{ok,{_,""}}->
			THIS:set_attribute(matchValue,""),
			"";
		{ok,{_,Match}}->
			{ok,{_,SnmpVal}} = THIS:get_attribute(snmpValue),
			Temp = if
						is_integer(SnmpVal) ->
							integer_to_list(SnmpVal);
						is_float(SnmpVal)->
							float_to_list(SnmpVal);
						true ->
							SnmpVal
					end,
			case regexp:match(Temp,Match) of
				nomatch->
					THIS:set_attribute(matchValue,""),
					THIS:set_attribute(status,"error"),
					lists:flatten(io_lib:format("content match error<br>~p",[SnmpVal]));
				{match,S,E}->
					THIS:set_attribute(?STATE_STRING,lists:flatten(io_lib:format("matched <br>~p",[SnmpVal]))),
					THIS:set_attribute(matchValue,string:substr(Temp,S,E)),
					""
			end;
		_->
			THIS:set_attribute(matchValue,""),
			""
	end;
check_match(Err)->Err.

%% @spec do_measure(Err2,LastVal,LastTime,LastBase,Session) -> Result
%% Result = string()
%% @doc measure snmp info with LastVal, LastTime, LastBase, Session
do_measure("",LastVal,LastTime,LastBase,Sess)->
	Now = sv_datetime:now(),
	%{ok,{_,GaugeMax}} = THIS:get_property(gaugeMax),
	GaugeMax = case THIS:get_property(gaugeMax) of
				{ok,{_,GMax}}->
					GMax;
				_->
					0
				end,
	{ok,{_,MeasureDelta}} = THIS:get_property(measureDelta),
	{ok,{_,PercentageBase}} = THIS:get_property(percentageBase),
	{ok,{_,PercentageDelta}} = THIS:get_property(percentageDelta),
	{ok,{_,MeasureRate}} = THIS:get_property(measureRate),
	{ok,{_,Scale}} = THIS:get_property(scale),
	
	Mes = if
			GaugeMax > 0 ->
				THIS:getMeasurement(THIS,snmpValue,GaugeMax);
			true ->
				THIS:getMeasurement(THIS,snmpValue)
		end,
	THIS:set_attribute(snmpMeasurement,Mes),

	NMes = if 
			MeasureDelta == true ->                         %%if MeasureDelta is true, MeasureRate is a effective
				NMesd = (Mes - LastVal),
				if
					MeasureRate == true ->
						NMesd *1000/(Now - LastTime);
					true ->
						NMesd
				end;
			true ->
				Mes
		end,
	Mes2 = case PercentageBase of
				"no"->
					NMes;
				_->
					O = [list_to_integer(X) || X <- string:tokens(PercentageBase,".")],
					Base = case Sess:g(O) of
								{ok,{noError,_,[Vb|_]},_}->
									snmp_ex2_manager:getOctetString(Vb#varbind.value);
								_->
									1
							end,
					THIS:set_attribute(baseValue,Base),
					BaseMeasure = THIS:getMeasurement(THIS,baseValue),
					THIS:set_attribute(baseMeasurement,BaseMeasure),
					if 
						PercentageDelta =:= true ->
							BasePre = (BaseMeasure-LastBase),
							case BasePre of
								0->
									NMes;
								_->
									NMes/ BasePre
							end;
						BaseMeasure =/= 0 ->
							NMes/BaseMeasure;
						true ->
							NMes
					end
			end,
	Mes3 = case Scale of
			"no"->
				Mes2;
			_->
				case string:to_float(Scale) of
					{error,_}->
						case string:to_integer(Scale) of
							{error,_}->
								Mes2;
							{V1,_}->
								Mes2/V1;
							_->
								Mes2
						end;
					{V2,_}->
						Mes2/V2
				end
		end,
	if
		is_float(Mes3)->
			THIS:set_attribute(?MEASUREMENT,round(Mes3*100)/100);
		true ->
			THIS:set_attribute(?MEASUREMENT,Mes3)
	end,
	"";
do_measure(Err,_,_,_,_)->Err.

%% @spec do_snmp_v2(Host,Community,Oid,Timeout, Session) -> Result
%% Result = string()
%% @doc get snmp info by from device by Host, Community, Oid and so on in snmp version2
do_snmp_v2(Host,Community,Oid,Timeout, Session)->

	%%Val = case lists:member({Host,?SNMP_PORT},snmp_ex2_manager:which_agents()) of
	%%	true->
	%%		snmp_ex2_manager:update_agent(Host,timeout,Timeout),
	%%		snmp_ex2_manager:update_agent(Host,community,Community),
	%%		snmp_ex2_manager:sync_get(Host,[Oid]);
	%%	_->
	%%		snmp_ex2_manager:agent(Host,[{community,Community},{timeout,Timeout}]),
	%%		snmp_ex2_manager:sync_get(Host,[Oid])
	%%end,
    Val = Session:g(Oid),
	case Val of
		{ok,{noError,_,[M|_]},_}->
			case M#varbind.value of
				noSuchObject->
					THIS:set_attribute(status,"error"),
					THIS:set_attribute(snmpValue,M#varbind.value),
					"error-noSuchObject";
				noSuchInstance->
					THIS:set_attribute(status,"error"),
					THIS:set_attribute(snmpValue,M#varbind.value),
					"error-noSuchInstance";
				endOfMibView->
					THIS:set_attribute(status,"error"),
					THIS:set_attribute(snmpValue,M#varbind.value),
					"error-endOfMibView";
				'NULL'->
					THIS:set_attribute(status,"error"),
					THIS:set_attribute(snmpValue,M#varbind.value),
					"error-NULL";
				_->
					THIS:set_attribute(status,"ok"),
					THIS:set_attribute(snmpValue,snmp_ex2_manager:getOctetString(element(4,M))),
					""
			end;
		{ok,{Reason,_,_},_}->
			THIS:set_attribute(status,"error"),
			THIS:set_attribute(snmpValue,""),
			lists:flatten(io_lib:format("error-~p",[Reason]));
		{error,Reason}->
			THIS:set_attribute(status,"error"),
			THIS:set_attribute(snmpValue,""),
			THIS:set_attribute(?NO_DATA,true),
			lists:flatten(io_lib:format("error-~p",[Reason]));
		Ret->
			THIS:set_attribute(status,"nodata"),
			THIS:set_attribute(?NO_DATA,true),
			lists:flatten(io_lib:format("unknow return-~p",[Ret]))
	end.


to_IpString(T) ->
    to_OidString(tuple_to_list(T)).

to_OidString(T) ->
    Temp = lists:map(fun(X) -> integer_to_list(X) end, T),
    string:join(Temp, ".").

%% @spec do_snmp_v3(Host,Community,Oid,Timeout,User,Passwd, Session) -> Result
%% Result = string()
%% @doc get snmp info by from device by Host, Community, Oid, User, Password and so on in snmp version3
do_snmp_v3(Host,_,Oid,Timeout,User,Passwd,Session)->
	Val = Session:g(Oid),
	case Val of
		{ok,{noError,_,[M|_]},_}->
			case M#varbind.value of
				noSuchObject->
					THIS:set_attribute(status,"error"),
					THIS:set_attribute(snmpValue,M#varbind.value),
					"error-noSuchObject";
				noSuchInstance->
					THIS:set_attribute(status,"error"),
					THIS:set_attribute(snmpValue,M#varbind.value),
					"error-noSuchInstance";
				endOfMibView->
					THIS:set_attribute(status,"error"),
					THIS:set_attribute(snmpValue,M#varbind.value),
					"error-endOfMibView";
				'NULL'->
					THIS:set_attribute(status,"error"),
					THIS:set_attribute(snmpValue,M#varbind.value),
					"error-NULL";
				_->
					THIS:set_attribute(status,"ok"),
					THIS:set_attribute(snmpValue,snmp_ex2_manager:getOctetString(element(4,M))),
					""
			end;
		{ok,{Reason,_,_},_}->
			THIS:set_attribute(status,"ok"),
			THIS:set_attribute(snmpValue,""),
			lists:flatten(io_lib:format("error-~p",[Reason]));
		{error,Reason}->
			THIS:set_attribute(status,"error"),
			THIS:set_attribute(snmpValue,""),
			THIS:set_attribute(?NO_DATA,true),
			lists:flatten(io_lib:format("error-~p",[Reason]));
		Ret->
			THIS:set_attribute(status,"nodata"),
			THIS:set_attribute(?NO_DATA,true),
			lists:flatten(io_lib:format("unknow return-~p",[Ret]))
	end.

%% @spec getScalarValues(Prop,Params)-> Result
%% Result = list()
%% @doc get version list, oid list, percentageBase list, scale list for ui's combobox
getScalarValues(Prop,Params)->
	Path = filename:nativename(?SNMP_OID),	
	case file:consult(Path) of
        {ok, Config} ->
			case Prop of
				snmpversion->
					[{"V1","v1"},{"V2","v2"},{"V3","v3"}];
				oid->
					Config;
				percentageBase->
					[{"no percentage base","no"}] ++ Config;
				scale->
					?SCALES;
				_->
					BASE:getScalarValues(Prop,Params)
			end;
        _ ->
         {error, 'read_error'}
    end.


%% @spec get_classifier(error) -> List
%% List = [Tuple]
%% Tule = {status, Logic, Value}
%% Logic = '!=' | '==' | '>' | '<' | 'contain'
%% Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'!=',"ok"}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'!=',"ok"}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'==',"ok"}]
	end.

%% @spec verify(Params)-> Result
%% Params = [Tuple]
%% Result = list()
%% @doc verify data of ui, ex. host,percentageBase,oid,oidIndex,timeout,scale and son
verify(Params)->
	Errs =
	case proplists:get_value(host,Params) of
		""->
			[{host,"Host Name  missing"}];
		Host->
			case string:rstr(Host," ") of
				0->
					[];
				_->
					[{host,"no spaces are allowed"}]
			end
	end ++
    case proplists:get_value(oid,Params) of
        undefined ->
            [{oid,"no oid or oid is empty"}];
        "" ->
            [{oid,"oid is empty"}];
        _ ->
            []
    end ++
	case proplists:get_value(percentageBase,Params) of
		"no"->
			[];
		PercBase ->
			case regexp:match(PercBase,"^[0-9|\.]+$") of
				nomatch->
					[{percentageBase,"must be a number or an object id"}];
				_->
					[]
			end
	end ++
	case proplists:get_value(oid,Params) of
		""->
			[{oid,"object id missing"}];
		Oid->
			case regexp:match(Oid,"^[0-9|\.]+$") of
				nomatch->
					[{oid,"object ids must be of the form 1.3.6.1.2.1"}];
				_->
					[]
			end
	end ++
	case proplists:get_value(oidIndex,Params) of
		""->
			[{oidIndex,"Index missing"}];
		OidIndex when not is_number(OidIndex)->
			[{oidIndex,"index must be a number"}];
		_->
			[]
	end ++
	case proplists:get_value(timeout,Params) of
		""->
			[{timeout,"timeout missing."}];
		V->
			if
				not is_number(V) ->
					[{timeout,"timeout must be a number."}];
				true->
					[]
			end
	end ++
	case proplists:get_value(scale,Params) of
		""->
			[{scale,"Scaling missing"}];
		"no"->
			[];
		Scale->
			case regexp:match(Scale,"^[0-9|\.]+$") of
				nomatch->
					[{scale,"Scaling must be a number"}];
				_->
					[]
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

getHostname()->
	case THIS:get_property(host) of
		{ok,{_,V}}->
			V;
		_->
			BASE:getHostname()
	end.
	
%% @spec get_template_property()-> Result
%% Result = [Record]
%% Record = property
%% @doc get monitor template of snmp monitor
get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=status,title="status",configurable=false,state=true},
	#property{name=snmpValue,title="value",configurable=false,state=true},
	#property{name=matchValue,title="content match",configurable=false,state=true},

	#property{name=host,title="Host Name",description="the IP address or host name from which the snmp value will be retrieved",type=text,editable=true,order=1},
	#property{name=oid,title="Object ID",default="1.3.6.1.4.1.9.2.1.58",description="the object ID of the main SNMP object to be queried",type=scalar,editable=true,order=2,allowother=true},
	#property{name=oidIndex,title="Index",description="the index of the main SNMP object - for non-table object IDs, this is 0",type=numeric,editable=true,order=2},
	#property{name=community,title="Community",description="the community the SNMP object",type=text,editable=true,order=3,default="public"},


	#property{name=timeout,title="Timeout",description="the total time, in seconds, to wait for a successful reply",type=numeric,editable=true,advance=true,default=5,order=1,baselinable=true},
	#property{name=snmpversion,title="SNMP Version",description="the version of the mib that the oid belongs to",type=scalar,editable=true,advance=true,default="v1",order=2},
	#property{name=scale,title="Scaling",description="divide the value by a scaling factor",type=scalar,allowother=true,editable=true,advance=true,order=3,default="no"},

	#property{name=content,title="Match Content",advance=true,order=4,description="optional, match against SNMP value."},
	#property{name=units,title="Units",advance=true,order=5,description="optional units string to append when displaying the value of this counter."},
	#property{name=measurementDesc,title="Measurement Label",advance=true,order=5,description="optional label for the status string and measurements column of reports."},
	#property{name=measureDelta,type=bool,title="Measure as Delta",advance=true,order=5,description="when selected, the measurement reported is the difference between the current value and the previous value."},
	#property{name=measureRate,type=bool,title="Measure as Rate per Second",advance=true,order=5,description="when selected, the measurement reported is divided by the number of seconds since the last measurement."},
	#property{name=percentageBase,type=scalar,title="Percentage Base",advance=true,allowother=true,default="no",order=5,description="optional, the measurement will be divided by this value to calculate a percentage, enter a number or SNMP object ID,  if an object ID if entered the Index from above will be used"},
	#property{name=percentageDelta,type=bool,title="Measure Base as Delta",advance=true,order=5,description="optional, calculate the Percentage Base as the difference between the current base and the previous base.  use this option when an SNMP object ID is used for Percentage Base and the object is not a fixed value"},
	%%#property{name=gaugeMax,type=numeric,title="Gauge Maximum",advance=true,order=5,description="optional, enter a maximum value for this object ID.  the maximum is calculate to create the gauge display"},

	#property{name=snmpv3user,title="SNMP V3 Username",description="Username to be used for authentication for SNMP V3",type=text,editable=true,advance=true,order=5},
	#property{name=snmpv3passwd,title="SNMP V3 Password",description="Password to be used for authentication for SNMP V3",type=text,editable=true,advance=true,order=5}

	].