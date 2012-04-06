%% 
%% @doc license tools
%% @version{1.0}
%% @copyright 2009 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>
-module(lc_util).

-define(TRIAL_DAYS,10).
-define(FINAL_DAYS,999999999).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([getLicensedPoints/0,generateLicenseKeyX/2,getLicensedByEquipment/0,getLicensedByCPE/0,
		 getLicensedPoints/1,getLicenseSummary/0,getLicenseForSystemActivation/0,
		 wouldExceedLimit/1,getLicenseForXKey/0,getLicenseKey/0,valid_license/0,
		 isMonitorTypeAllowed/1,getCostInLicensePoints/0,check_trial/0,getDaysRemaining/0,page_limit/0,
         getMachineCode/0, takeEffectLicense/0, setLicense/1, isFinalLicense/0
		 ]).

-define(ALTBL,[
			   {48,51},
			   {49,50},
			   {50,53},
			   {51,49},
			   {52,57},
			   {53,56},
			   {54,48},
			   {55,52},
			   {56,54},
			   {57,55}
			  ]).

-define(MLTBL,[
			   {port_monitor,1},
			   {memory_monitor,2},
			   {diskspace_monitor,3},
			   {url_monitor,4},
			   {url_content_monitor,5},
			   {url_list_monitor,6},
			   {tr069_newrocksiprelayver1_0monitor,7},
			   {checkpoint_monitor,8},
			   {apache_monitor,9},
			   {cisco_monitor,10},
			   {network_bandwidth_monitor,11},
			   {dynamo_monitor,12},
			   {composite_monitor,13},
			   {iplanet_server_monitor,14},
			   {web_service_monitor,15},
			   {file_monitor,16},
			   {database_monitor,17},
			   {health_unixserver_monitor,18},
			   {ntcounter_monitor,19},
			   {snmp_monitor,20},
			   {browsable_snmp_monitor,21},
			   {snmp_trap_monitor,22},
			   {f5_monitor,23},
			   {sybase_monitor,24},
			   {dhcp_monitor,25},
			   {telnet_monitor,26},
			   {dns_monitor,27},
			   {link_monitor,28},
			   {asp_monitor,29},
			   {database_counter_monitor,30},
			   {iisserver_monitor,31},
			   {sqlserver_monitor,32},
			   {ping_monitor,33},
			   {web_server_monitor,34},
			   {oracle_jdbc_monitor,35},
			   {cpu_utilization_monitor,36},
			   {bandwidth_monitor,37},
			   {service_monitor,38},
			   {script_monitor,39},
			   {weblogic6x_monitor,40},
			   {ldap_monitor,41},
			   {mail_monitor,42},
			   {weblogic5x_monitor,43},
			   {directory_monitor,44},
			   {url_sequence_monitor,45},
			   {ftp_monitor,46},
			   {adperformance_monitor,47},
			   {browsableNTCounter_monitor,48},
			   {complus_monitor,49},
			   {network_monitor,50},
			   {websphere_servlet_monitor,51},
			   {healthserverload_monitor,52},
			   {ipmi_monitor,53},
			   {vmwareperformance_monitor,54},
			   {logMonitor,55}
			   ]).

%%
%% API Functions
%%

%% @spec generateLicenseKey(StartDate,Days,Points,Ver)->string()
%% where
%%	StartDate = string()
%%	Days = integer()
%%	Points = integer()
%%	Ver = string()
%% @doc generate a license key from a start date,days,points and version.
%% generateLicenseKey(StartDate,Days,Points,Ver)->
	%% S1 = Ver ++ lists:flatten(io_lib:format("~8.8.0s",[StartDate]))
		%% ++ lists:flatten(io_lib:format("~4.4.0w", [Days]))
		%% ++ lists:flatten(io_lib:format("~6.6.0w", [Points])),

	%% S2 = exchg(S1),
	%% "EL" ++ tt(S2).
	
valid_license()->
    OriginalLic = getLicenseKey(),
    %io:format("OriginalLic: ~p~n", [OriginalLic]),
	S = decode_license(OriginalLic),
	check_license(S).
	
check_license(Str)->
    %%io:format("Str: ~p~n", [Str]),
    Length = length(Str),
    if
        Length > 0 ->
            case lists:keymember('ProductName', 1, Str) of
                true ->
                    true;
                _ ->
                    false
            end;
            %%io:format("Str: ~p~n", [Str]),
            %%case license:isvalidlicense("",Str,0,"false") of
            %%    {ok, match} ->
            %%        true;
            %%    OV ->
            %%        io:format("OV: ~p~n", [OV]),
            %%        false
            %%end;
        true ->
            false
    end.
	

%% 这个不清楚是什么意思
generateLicenseKeyX(Type,Ver)->
	case proplists:get_value(Type, ?MLTBL) of
		undefined->
			"type error";
		Ti->
			S = lists:flatten(io_lib:format("~2.2.0s",[Ver]))
					++ lists:flatten(io_lib:format("~4.4.0w",[Ti])),
			S
	end.

%% @spec getLicensedPoints()->integer()
%% @doc get licensed points
%%
getLicensedPoints()->
    OriginalLic = getLicenseKey(),
	S = decode_license(OriginalLic),
	getLicensedPoints(S).

%% @spec getLicensedPoints(Lc)->integer()
%% where
%%	Lc = string()
%% @doc get licensed points
getLicensedPoints(Lc)->
	case check_license(Lc) of
		true->
			%%S2 = exchg(S1),
            Points = 
            case lists:keysearch('Points', 1, Lc) of
                {value, {'Points', P}} ->
                    P;
                _ ->
                    0
            end,
            Devices =
            case lists:keysearch('Devices', 1, Lc) of
                {value, {'Devices', D}} ->
                    D;
                _ ->
                    0
            end,
			Points + Devices;
		_->
			-1
	end.
	
getLicensedByEquipment()->
    OriginalLic = getLicenseKey(),
	S = decode_license(OriginalLic),
	getLicensedByEquipment(S).
	
getLicensedByEquipment(Lc) ->
	case check_license(Lc) of
		true->
			Devices =
            case lists:keysearch('Devices', 1, Lc) of
                {value, {'Devices', D}} ->
                    D;
                _ ->
                    -1
            end;
		_->
			-1
	end.

getLicensedByCPE() ->
    OriginalLic = getLicenseKey(),
	S = decode_license(OriginalLic),
	getLicensedByCPE(S).

getLicensedByCPE(Lc) ->
	case check_license(Lc) of
		true->
			Devices =
            case lists:keysearch('Devices', 1, Lc) of
                {value, {'Devices', D}} ->
                    D;
                _ ->
                    -1
            end;
		_->
			-1
	end.

getDaysRemaining()->
    OriginalLic = getLicenseKey(),
	S = decode_license(OriginalLic),
	getDaysRemaining(S).

getDaysRemaining(Lc)->
	case check_license(Lc) of
		true->
			Date = 
                case lists:keysearch('StartDate', 1, Lc) of
                    {value, {'StartDate', D}} ->
                        D;
                    _ ->
                        "19010101"
                end,
			Days = 
                case lists:keysearch('Delaydays', 1, Lc) of
                    {value, {'Delaydays', De}} ->
                        De;
                    _ ->
                        -1
                end,
			%% io:format("getDaysRemaining:S1 = ~p S2 = ~p Date = ~p Days = ~p~n",[S1,S2,Date,Days]),
            Ds =
			case	(list_to_integer(string:left(Date,4)) > 0) and 
				(list_to_integer(string:substr(Date,5,2)) > 0) and 
				(list_to_integer(string:right(Date,2)) > 0) of
				true ->
					D1 = calendar:date_to_gregorian_days(list_to_integer(string:left(Date,4)),
														 list_to_integer(string:substr(Date,5,2)),
														 list_to_integer(string:right(Date,2))),
					D2 = calendar:date_to_gregorian_days(date()),
					Days - (D2 - D1);
				false -> -1
			end,
            case isFinalLicense(Lc) of
                true ->
                    ?FINAL_DAYS;
                _ ->
                    Ds
            end;
		_->
			case preferences:get(master_config,install_date) of
				{ok,[{_,Date}|_]}->
					Day1 = calendar:date_to_gregorian_days(Date),
					Day2 = calendar:date_to_gregorian_days(date()),
					?TRIAL_DAYS -(Day2 - Day1)  ;
				_->
					-1
			end
	end.
	
isMonitorTypeAllowed(Type)->
	S = getLicenseForXKey(),
	isMonitorTypeAllowed(Type,S).

isMonitorTypeAllowed(_Type,_S)->
	true.

decode_license(Lic) when erlang:is_binary(Lic) ->
	case license:decrypt2data(Lic) of
        {ok, L} ->
            L;
        _ ->
            ""
    end;
decode_license(Lic) ->
    "".

getLicenseKey()->
    lc_server:getLicense().
    
takeEffectLicense()->
    lc_server:takeEffectLicense().
    
getMachineCode() ->
    lc_server:getMachineCode().
    
setLicense(LicenseStr) ->
    lc_server:setLicense(LicenseStr).

getLicenseForXKey()->
	case preferences:get(general,license_for_x) of
		{ok,[{_,V}]}->
			V;
		_->
			""
	end.
	
getLicenseForSystemActivation() ->
	case preferences:get(general,system_activation) of
		{ok,[{_,V}]} ->
			V;
		_ ->	""
	end.
    
isFinalLicense() ->
    OriginalLic = getLicenseKey(),
	S = decode_license(OriginalLic),
    isFinalLicense(S).

isFinalLicense(Lic) ->
    case Lic of
        Lstr when erlang:is_list(Lstr) ->
            case lists:keysearch('Isfinal', 1, Lstr) of
                {value,{'Isfinal',VIsfinal}} ->
                    case VIsfinal of
                        0 ->
                            false;
                        _ ->
                            true
                    end;
                _->
                    false
            end;    
        _ ->
            false
    end.

%% @spec getLicenseSummary()->string()
%% @doc get license's summary information
%%
getLicenseSummary()->
    OriginalLic = getLicenseKey(),
	S = decode_license(OriginalLic),
	if
		length(S) =< 0 ->
			"no license";
		true->
			case check_license(S) of
				true->
					Point = getLicensedPoints(S),
					Used = siteview:totalPointsUsed(),
					Days = getDaysRemaining(S),
					if
						Days < 0 ->
							lists:flatten(io_lib:format("license point:~w,used:~w,License is Expired",[Point,Used]));
						true ->
                            case isFinalLicense(S) of
                                true ->
                                    lists:flatten(io_lib:format("license point:~w,used:~w,License is effective all along",[Point,Used]));
                                _ ->
                                    lists:flatten(io_lib:format("license point:~w,used:~w,License Expired after ~w days",[Point,Used,Days]))
                            end
					end;
				_->
					"invalid license"
			end
	end.

getCostInLicensePoints()->
	siteview:totalPointsUsed().

%% @spec wouldExceedLimit(Li)->(true | false)
%% where
%%	Li = integer()
%% @doc is limit to excute? 
wouldExceedLimit(Li)->
	Cp = getCostInLicensePoints(),
	Pts = getLicensedPoints(),
	if
		Pts < 0 ->
			not check_trial();
		true ->
			if
				Cp + Li > Pts ->
					true;
				true ->
					false
			end
	end.
    
%页面部分对licence的判断
page_limit()->
	Days = lc_util:getDaysRemaining(),
    Cp = getCostInLicensePoints(),
	Pts = getLicensedPoints(),
	if
		Pts < 0 ->
			not check_trial();
		true ->
			if
				(Days<0) ->
					true;
				true ->
					false
			end
	end.

check_trial()->
	case preferences:get(master_config,install_date) of
		{ok,[{_,Date}|_]}->
			Day1 = calendar:date_to_gregorian_days(Date),
			Day2 = calendar:date_to_gregorian_days(date()),
			(Day2 - Day1) =< ?TRIAL_DAYS;
		_->
			false
	end.
%%
%% Local Functions
%%
%%
tt([]) -> [];
tt([A|T])->
	case proplists:get_value(A,?ALTBL) of
		undefined->
			[48] ++ tt(T);
		V->
			[V] ++ tt(T)
	end.

tr([])->[];
tr([A|T])->
	case lists:keysearch(A,2,?ALTBL) of
		{value,{V,_}}->
			[V] ++ tr(T);
		_->
			[48] ++ tr(T)
	end.

exchg(S)->
	T1 = list_to_tuple(S),
	T2 = T1,
	T3 = setelement(1,T2,element(3,T1)),
	T4 = setelement(3,T3,element(1,T1)),
	T5 = setelement(5,T4,element(7,T1)),
	T6 = setelement(7,T5,element(5,T1)),
	T7 = setelement(13,T6,element(17,T1)),
	T8 = setelement(17,T7,element(13,T1)),
	T9 = setelement(19,T8,element(20,T1)),
	T10 = setelement(20,T9,element(19,T1)),
	tuple_to_list(T10).
