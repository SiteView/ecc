-module(snmptrap,[BASE,Monitor,Rule]).
-extends(action).
-compile(export_all).
-export([new/2, execute/0, get_text/2, ipstr_to_erip/1, oidstr_to_eroid/1, parseOutKeyIfExists/3, increment/1, trans_type/1, build_varbinds/2, sendTrapAlert/2, gettrapname_byid/1, getalertid/0]).

-include("monitor.hrl").
-include("monitor_template.hrl").
-include("alert.hrl").
-include("snmp_preferences.hrl").
-include_lib("snmp/include/snmp_types.hrl").
-include("snmp_ecc.hrl").

-define(PRODUCT_NAME,"elecc").
-define(TIMEOUT,5).
-define(LOG_TYPE,"Snmp trap alert sent").

%% @spec new(Monitor,Rule) -> Obj
%% where
%% Obj = term()
%% @doc create a new instance for snmptrap alert
new(Monitor,Rule)->
	Obj = action:new(Monitor,Rule),
	% Obj:set_monitor(Monitor),
	% Obj:set_rule(Rule),
	% Obj:set_attribute(runType,2),
	{?MODULE,Obj,Monitor,Rule}.
 
%% @spec execute() -> Obj
%% where
%% Obj = term()
%% @doc execute snmptrap alert
execute()->
	Msg = ?PRODUCT_NAME ++ " Alert, " ++ 
	case Monitor:get_attribute(?CATEGORY) of 
		{ok,{_,Category}} -> 
			atom_to_list(Category);
		_->
			""
	end ++ "," ++
	case Monitor:get_property(?NAME) of
		{ok,{_,Name}}->
			Name;
		_->
			""
	end ++ "," ++
	case Monitor:get_attribute(?STATE_STRING) of
		{ok,{_,State}}->
			State;
		_->
			""
	end,
	{ok,{_,Params}} = Rule:get_property(action_param), 
	{ok,{_,Enabled}} = Rule:get_property(enabled),
	case Rule:get_property(disabled) of
		{ok,{_,true}}->
			{error,"disabled"};
		_->
			case THIS:check_enabled(Enabled) of
				false ->
					{error,"time_disabled"};
				_->
					sendTrapAlert(Params,Msg)
			end
	end.

check_schedule(Shedule)->
	Day = calendar:day_of_the_week(date()),
	case lists:keysearch(Day,1,Shedule) of
		{value,{_,Flag,St,Et}}-> % "12:12","24:24"
			[HHS,MMS|_] = case string:tokens(St,":") of
								[]->
									["0","0"];
								[T1]->
									[T1,"0"];
								[T2,T3|_]->
									[T2,T3]
							end,
			[HHE,MME|_] = case string:tokens(Et,":") of
								[]->
									["24","60"];
								[T4]->
									[T4,"60"];
								[T5,T6|_]->
									[T5,T6]
							end,
			Sth = case string:to_integer(HHS) of
					{error,_}->
						0;
					{V1,_}->
						V1
				end,
			Stm = case string:to_integer(MMS) of
					{error,_}->
						0;
					{V2,_}->
						V2
				end,
			Seh = case string:to_integer(HHE) of
					{error,_}->
						24;
					{V3,_}->
						V3
				end,
			Sem = case string:to_integer(MME) of
					{error,_}->
						60;
					{V4,_}->
						V4
				end,
			Sts = sv_datetime:time({Sth,Stm,0}),
			Ste = sv_datetime:time({Seh,Sem,0}),
			Now = sv_datetime:time(time()),
			case Flag of
				"enable" ->
					if
						Now >= Sts andalso Now =< Ste ->
							true;
						true ->
							false
					end;
				_->
					if
						Now < Sts orelse Now > Ste ->
							true;
						true ->
							false
					end
			end;
		_->
			true
	end.

verify(Params)->
	{ok,""}.

%% @spec get_text(Key,Data) -> Obj
%% Key = string()
%% Data = list()
%% Obj = string()
%% @doc get value from a property lists by specify a key
get_text(Key,Data)->
	case proplists:get_value(Key,Data) of
		undefined->
			"";
		V->
			V
	end.

%% @spec ipstr_to_erip(IpStr) -> Obj
%% IpStr = string()
%% Obj = tuple()
%% @doc transform a ip string to a ip address of erlang
ipstr_to_erip(IpStr) ->
    FS = string:tokens(IpStr, "."),
    Ip_list = lists:map(fun(X) -> erlang:list_to_integer(X) end, FS),
    erlang:list_to_tuple(Ip_list).

%% @spec oidstr_to_eroid(OidStr) -> Obj
%% OidStr = string()
%% Obj = list()
%% @doc transform a oid string to a oid of erlang
oidstr_to_eroid(OidStr) -> 
    FS = string:tokens(OidStr, "."),
    Oid_list = lists:map(fun(X) -> erlang:list_to_integer(X) end, FS).

%% @spec parseOutKeyIfExists(S, S1, S2) -> Obj
%% S = string()
%% S1 = string()
%% S2 = string()
%% Obj = list()
%% @doc parse template
parseOutKeyIfExists(S, S1, S2) ->
    I = string:str(S1, S),
    if
        I > 0 ->
            J = string:str(S1, "]"),
            if 
                J > 0 ->
                    As1 = string:strip(string:substr(S1, I + string:len(S), (J - (I + string:len(S))))),
                    As2 = string:strip(string:substr(S1, 1, I - 1) ++ string:substr(S1, J + 1)),
                    [As2, As1];
                true ->
                    [S1, S2]
            end;
        true ->
            [S1, S2]
    end.
    
%%getSetting(S, St, Params) ->
%%    S0 =
%%    case St of
%%        [] ->
%%            [];
%%        _ ->
%%            case get_text(S, St) of
%%                [] ->
%%                    []
%%                Value ->
%%                    Value
%%            end
%%    end,
%%    case string:len(S0) of
%%        0 ->
%%            _ ->
            
%%    end,

%% @spec increment(S) -> Obj
%% S = string()
%% Obj = string()
%% @doc transform S to integer, after add 1 and to string
increment(S) ->
    L1 =
    try erlang:list_to_integer(S) of
        L ->
            L
    catch
        _:_ ->
            0
    end,
    L2 = erlang:integer_to_list(L1 + 1),
    L2.

cycarray([], S15, S16, Vbs, Sk13, I) ->
    {Sk13, Vbs};
cycarray([H|T], S15, S16, Vbs, Sk13, I) ->
    [As0, As1] = parseOutKeyIfExists("[Command:", H, Sk13),
    S17 = As0,
    {S13, IsSet} = 
    if
        As1 =:= "SET" ->
            {As1, true};
        true ->
            {Sk13, false}
    end,
    if
        IsSet =:= true ->
            cycarray(T, S15, S16, Vbs, S13, I);
        true ->
            [As20, As21] = parseOutKeyIfExists("[OID:", S17, S15),
            SS17 = As20,
            SS15 = As21,
            [As30, As31] = parseOutKeyIfExists("[Type:", SS17, S16),
            S1_17 = As30,
            S1_16 = As31,
            S2_17 = textutils:removeChars(S1_17,"\r"),
            J = string:rstr(SS15, "."),
            S2_15 =
            if
                J =/= 0 ->
                    S18 = string:substr(SS15, 1, J),
                    S19 = string:substr(SS15, J + 1),
                    S1_15 = S18 ++ increment(S19),
                    S1_15;
                true ->
                    SS15
            end,
            cycarray(T, S2_15, S1_16, Vbs ++ [{I, {S2_15, S1_16, S2_17}}], S13, I + 1)
    end.

%% @spec trans_type(Type) -> Obj
%% Type = string()
%% Obj = atom()
%% @doc transform a varbind type string to erlang know's type
trans_type(Type) ->
    case Type of
        "STRING" ->
            'OCTET STRING';
        _ ->
            'OCTET STRING'
    end.

sendSNMPTrap(S, S1, S2, S3, S4, S5, S6, Array, S7, S8, S9, S10, S11) ->
    S1_9 =
    case string:str(S9, ".") of
        1 ->
            string:substr(S9, 2);
        _ ->
            S9
    end,
    S1_9_len = string:len(S1_9),
    S1_9_index = string:rstr(S1_9, "."),
    S2_9 =
    if
        S1_9_index =:= S1_9_len ->
            S1_9 ++ ".";
        true ->
            S1_9
    end.

get_notifyName(Ge) ->
    case Ge of
        "0" ->
            'coldStart';
		"1" ->
            'warmStart';
		%%"2" ->
        %%    'linkDown';
		%%"3" ->
        %%    'linkUp';
        _ ->
            []
    end.

%% @spec build_varbinds(S1, S9) -> Obj
%% S1 = [{Index,{Oid,Type,Value}}|T]
%% S9 = string()
%% Obj = list()
%% @doc build a varbinds by some data and snmp prefix
build_varbinds([], S9) ->
    [];
build_varbinds([{Index,{Oid,Type,Value}}|T], S9) ->
    S13 =
    case lists:nth(1, Oid) of
        "." ->
            string:substr(Oid, 2);
        _ ->
            Oid
    end,
    Lens = string:len(S9),
    S1_13 =
    if
        Lens > 0 ->
            S1_9 =
            case lists:nth(1, S9) of
                "." ->
                    string:substr(S9, 2);
                _ ->
                    S9
            end,
            I = string:rstr(S1_9, "."),
            Len9 = string:len(S1_9),
            if
                I =/= Len9 ->
                    S1_9 ++ ".";
                true ->
                    S1_9
            end,
            S1_9 ++ S13;
        true ->
            ?SYSTEM_OID ++ S13
    end,
    build_varbinds(T, S9) ++
    [{oidstr_to_eroid(S1_13), #asn1_type{bertype = trans_type(Type)}, Value}];
build_varbinds([H|T], S9) ->
    build_varbinds(T, S9).
    
build_contents([], S9) ->
    [];
build_contents([{Index,{Oid,Type,Value}}|T], S9) ->
    Value ++ "\n" ++
        build_contents(T, S9);
build_contents([H|T], S9) ->
    build_contents(T, S9).

%% @spec sendTrapAlert(Params,Content) -> Obj
%% Params = list()
%% Content = string()
%% Obj = term()
%% @doc send trap alert
sendTrapAlert(Params,Content)->
	Msg = THIS:createMessage("templates.snmp",Params#snmptrap_alert.template),
    [S3, S4] = parseOutKeyIfExists("[Agent Host:", Msg, ""),
    [S33, S5] = parseOutKeyIfExists("[Specific:", S3, ""),
	(catch sendTrap(Params#snmptrap_alert.trapto,Params,S33, S4, S5, Content, "")).

sendDetailTrap(Id, Snmp_host, Snmp_objectid, Snmp_objectid_other, Snmp_community, Snmp_generic, Snmp_specific, Snmp_trap_version, _Snmp_prefix, S,S1,S2) -> 
io:format("rule:doAction:~p~n",[_Snmp_prefix]),
    Snmp_prefix = [],
    Server_er = ipstr_to_erip(Snmp_host),  
    Session = snmp_session:new(Server_er,162,Snmp_trap_version,Snmp_community,[],[],[],[],[],[],?TIMEOUT),
    SS1 =
    case string:len(S1) of
        0 ->
            S14 = Session:getLocalServerStr(),
            case textutils:onlyChars(S14,"0123456789.") of
                true ->
                    S14;
                _ ->
                    S1
            end;
        _ ->
            S1
    end,
    SLen = string:len(string:strip(S)),
    {S13, Vbs} =
    if
        (SLen > 0) ->
            S15 = "1.0",
            S16 = "",
            SS16 =
            if
                S16 =:= "" ->
                    "STRING";
                true ->
                    S16
            end,
            Array1 = string:tokens(S, "\n"),
            cycarray(Array1, S15, SS16, [], [], 1);
        true ->
            {[], []}
    end,
    %%Varbinds = [{oidstr_to_eroid(Snmp_objectid), #asn1_type{bertype = 'OCTET STRING'}, S2}],
    Varbinds = build_varbinds(Vbs, Snmp_prefix),
    Contents = build_contents(Vbs, Snmp_prefix),
    %%{ok,{_,Id}} = Rule:get_property(id), 
    Spec =
    case string:to_integer(Snmp_specific) of
        {error, _} ->
            case string:to_float(Snmp_specific) of
                {error, _} ->
                    0;
                {Float, _} ->
                    Float;
                _ ->
                    0
            end;
        {Int, _} ->
            Int;
        _ ->
            case string:to_float(Snmp_specific) of
                {error, _} ->
                    0;
                {Float, _} ->
                    Float;
                _ ->
                    0
            end
    end,
    SOId =
    case Snmp_objectid of
        "0" ->
            Snmp_objectid_other;
        _ ->
            Snmp_objectid
    end,
    SSOId =
    case string:str(SOId, ".") of
        1 ->
            string:substr(SOId, 2);
        _ ->
            SOId
    end,
    Lens = string:len(Snmp_prefix),
    S1_OId =
    if
        Lens > 0 ->
            S1_9 =
            case lists:nth(1, Snmp_prefix) of
                "." ->
                    string:substr(Snmp_prefix, 2);
                _ ->
                    Snmp_prefix
            end,
            I = string:rstr(S1_9, "."),
            Len9 = string:len(S1_9),
            if
                I =/= Len9 ->
                    S1_9 ++ ".";
                true ->
                    S1_9
            end,
            S1_9 ++ SSOId;
        true ->
            ?SYSTEM_OID ++ SSOId
    end,
    TrapNames = gettrapname_byid(Id),
    %%io:format("TrapNames = ~p~n", [TrapNames]),
    Trap = #trap
	{trapname = TrapNames, 
	 enterpriseoid = oidstr_to_eroid(S1_OId),  
	 specificcode = Spec, 
	 %%oidobjects = {oid, asn1_type}, 
     oidobjects = [],
	 description = undefined
	},
    Isgeneric =
    case Snmp_generic of
        "6" ->
            false;
        "2" ->
            false;
        "3" ->
            false;
        _ ->
            true
    end,
    NotifyName = 
    case Isgeneric of
        false ->
            Trap#trap.trapname;
        _ ->
            get_notifyName(Snmp_generic)
    end,
    #trap_obj{isgeneric = Isgeneric, varbinds = Varbinds, trap = Trap, session = Session, msgprefix = Snmp_prefix, notifyname = NotifyName, contents = _Snmp_prefix++Contents, agentaddress = SS1}.

%% 获取trapname
%% @spec gettrapname_byid(AlertId) -> Obj
%% AlertId = atom()
%% Obj = atom()
%% @doc get trapname by alert id
gettrapname_byid(AlertId) ->
    AlertIdStr = erlang:atom_to_list(AlertId),
    TrapName = "snmp_trap_alert" ++ AlertIdStr,
    erlang:list_to_atom(TrapName).

%% @spec getalertid() -> Obj
%% Obj = atom()
%% @doc get alert id
getalertid() ->
    {ok,{_,Id}} = Rule:get_property(id), 
    case erlang:is_list(Id) of
        true ->
            erlang:list_to_atom(Id);
        _ ->
            case erlang:is_atom(Id) of
                true ->
                    Id;
                _ ->
                    Id
                    
            end
    end.

sendTrap([],_,S,S1,S2, Title, Msg)->{ok,Msg};
sendTrap(["default"|T],Params,S,S1,S2, Title, Msg)->
    St = case api_preferences:get_all(snmp) of
			{ok,Ret}->
				Ret;
			_->
				[]
		end,
    Snmp_host = get_text(?SNMP_HOST,St),
    Snmp_objectid = get_text(?SNMP_OBJECTID,St),
    Snmp_objectid_other = get_text(?SNMP_OBJECTID_OTHER,St),
    Snmp_community = get_text(?SNMP_COMMUNITY,St),
    Snmp_generic = get_text(?SNMP_GENERIC,St),
    Snmp_specific = get_text(?SNMP_SPECIFIC,St),
    Snmp_trap_version = get_text(?SNMP_TRAP_VERSION,St),
    Snmp_prefix = Params#snmptrap_alert.msgprefix,
    %%开始配置
    Id = getalertid(),
    TrapObj = sendDetailTrap(Id, Snmp_host, Snmp_objectid, Snmp_objectid_other, Snmp_community, Snmp_generic, Snmp_specific, Snmp_trap_version, Snmp_prefix, S,S1,S2),
    Session = TrapObj#trap_obj.session,
    Varbinds = TrapObj#trap_obj.varbinds,
    Trap = TrapObj#trap_obj.trap,
    TrapName = TrapObj#trap_obj.notifyname,
    Contents = TrapObj#trap_obj.contents,
    Rets = 
	case Session:send_trap([], TrapName, [], [], Varbinds, TrapObj) of
		{ok, Result}->
			THIS:logAlert(?LOG_TYPE, Snmp_host,Title ++ "\n", Contents,"ok"),
			{ok,Snmp_host,Contents, snmptrap};
		{error,Result} when is_binary(Result)->
			THIS:logAlert(?LOG_TYPE, Snmp_host, binary_to_list(Result),"","fail"),
			{error,Snmp_host,binary_to_list(Result), snmptrap};
		{error,Result}->
			THIS:logAlert(?LOG_TYPE, Snmp_host,Result,"","fail"),
			{error,Snmp_host,Result, snmptrap}
	end,
	sendTrap(T,Params,S,S1,S2, Title, [Rets] ++ Msg);
sendTrap([M|T],Params,S,S1,S2, Title, Msg)->
	io:format("sendTrap"),
	Rets = 
	case preferences:get(additional_snmp_settings,list_to_atom(M)) of
		{ok,[{_,Am}|_]}->
			%%CheckShedule = THIS:check_schedule(Am#additional_snmp_settings.schedule),
			if 
				Am#additional_snmp_settings.disable =/= "true" ->%%andalso CheckShedule ->
                    Snmp_host = Am#additional_snmp_settings.snmp_host,
                    Snmp_objectid = Am#additional_snmp_settings.snmp_objectid,
                    Snmp_objectid_other = Am#additional_snmp_settings.snmp_objectid_other,
                    Snmp_community = Am#additional_snmp_settings.snmp_community,
                    Snmp_generic = Am#additional_snmp_settings.snmp_generic,
                    Snmp_specific = Am#additional_snmp_settings.snmp_specific,
                    Snmp_trap_version = Am#additional_snmp_settings.snmp_trap_version,
                    Snmp_prefix = Params#snmptrap_alert.msgprefix,  
                    %% 开始配置
                    Id = getalertid(),
                    TrapObj = sendDetailTrap(Id, Snmp_host, Snmp_objectid, Snmp_objectid_other, Snmp_community, Snmp_generic, Snmp_specific, Snmp_trap_version, Snmp_prefix, S,S1,S2),
                    Session = TrapObj#trap_obj.session,
                    Varbinds = TrapObj#trap_obj.varbinds,
                    Trap = TrapObj#trap_obj.trap,
                    TrapName = TrapObj#trap_obj.notifyname,
                    Contents = TrapObj#trap_obj.contents,
					case Session:send_trap([], TrapName, [], [], Varbinds, TrapObj) of
						{ok, Result}->
							io:format("OK_TRIP,~p~n",[Contents]),
							THIS:logAlert(?LOG_TYPE, Snmp_host,Title ++ "\n",Contents,"ok"),
							{ok,Snmp_host,Contents, snmptrap};
						{error,Result} when is_binary(Result)->
							io:format("ERROR1_TRIP,~p~n",[Contents]),
							THIS:logAlert(?LOG_TYPE, Snmp_host,binary_to_list(Result),Contents,"fail"),
							{error,Snmp_host,binary_to_list(Result), snmptrap};
						{error,Result} ->
							io:format("ERROR2_TRIP,~p~n",[Contents]),
							THIS:logAlert(?LOG_TYPE, Snmp_host,Result,Contents,"fail"),
							{error,Snmp_host,Result, snmptrap, snmptrap}
					end;
				true ->
					{error,M,"disabled or schedule", snmptrap}
			end;
		_->
			{error,M,"get additional_snmp_settings error"}
	end,
	sendTrap(T,Params,S,S1,S2, Title, [Rets] ++ Msg).

getLocalServer() ->
    case inet:gethostname() of
        {ok, Hostname} ->
            case inet:getaddr(Hostname, inet) of
                {ok, Address} ->
                    Address;
                _ ->
                    {127,0,0,1}
            end;
        Other ->
            {127,0,0,1}
    end.
    
test_additionPres(Am, SnmpMessage, Params, Id) ->
    Snmp_host = Am#additional_snmp_settings.snmp_host,
    Snmp_objectid = Am#additional_snmp_settings.snmp_objectid,
    Snmp_objectid_other = Am#additional_snmp_settings.snmp_objectid_other,
    Snmp_community = Am#additional_snmp_settings.snmp_community,
    Snmp_generic = Am#additional_snmp_settings.snmp_generic,
    Snmp_specific = Am#additional_snmp_settings.snmp_specific,
    Snmp_trap_version = Am#additional_snmp_settings.snmp_trap_version,
    Snmp_prefix = Params#snmptrap_alert.msgprefix,  
    %% 开始配置
    TrapObj = sendDetailTrap(Id, Snmp_host, Snmp_objectid, Snmp_objectid_other, Snmp_community, Snmp_generic, Snmp_specific, Snmp_trap_version, Snmp_prefix, SnmpMessage,[],[]),
    Session = TrapObj#trap_obj.session,
    Varbinds = TrapObj#trap_obj.varbinds,
    Trap = TrapObj#trap_obj.trap,
    TrapName = TrapObj#trap_obj.notifyname,
    Contents = TrapObj#trap_obj.contents,
	case Session:send_trap([], TrapName, [], [], Varbinds, TrapObj) of
		{ok, Result}->
			{ok,"successful"};
        {error,Result} when is_binary(Result)->
			{error,binary_to_list(Result)};
		{error,Result} ->
			{error,Result}
	end.
%%getLocalServerStr() ->
%%    LocalServer = getLocalServer(),
%%    to_IpString(LocalServer).

get_addtional_mail([])->[];
get_addtional_mail([N|T])->
	case preferences:get(additional_email_settings,list_to_atom(N)) of
		{ok,[]}->
			get_addtional_mail(T);
		{ok,Rm}->
			[Y||{_,Y}<-Rm] ++ get_addtional_mail(T);
		_->
			get_addtional_mail(T)
	end.

getScalarValues(Prop,Params)->
	case Prop of
		to->
			case preferences:all(additional_snmp_settings) of
				{ok,Traps}->
					[{Y#additional_snmp_settings.name,atom_to_list(X)}||{X,Y}<-Traps]++[{"Default","default"}];
				_->
					[]
			end;
		template->
			[{filename:basename(X),filename:basename(X)}||X<-filelib:wildcard("templates.snmp/*")];
		_->
			BASE:getScalarValues(Prop,Params)
	end.
    
get_template_property()->
	%BASE:get_template_property() ++ 
	[
	%%#property{name=to,title="To",type=scalar,multiple = true,allowother = true,listSize=5,description="either choose one or more e-mail setting(s), or enter the e-mail address of the person to send the mail to, separate multiple addresses with commas (example: " ++ ?SUPPORT_MAIL ++ "," ++ ?SALES_MAIL ++ ")"},
	%%#{name=template,title="Template",type=scalar,description="choose which template to use for formatting the contents of the message.  If you are sending mail to a pager, choose one of the \"short\" templates."}
	].