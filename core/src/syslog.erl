-module(syslog,[BASE,Monitor,Rule]).
-extends(action).
-compile(export_all).

-include("monitor.hrl").
-include("monitor_template.hrl").
-include("alert.hrl").
-include("syslog.hrl").

-define(PRODUCT_NAME,"elecc").
-define(TIMEOUT,5).
-define(LOG_TYPE,"Syslog alert sent").

new(Monitor,Rule)->
	Obj = action:new(Monitor,Rule),
	% Obj:set_monitor(Monitor),
	% Obj:set_rule(Rule),
	% Obj:set_attribute(runType,2),
	{?MODULE,Obj,Monitor,Rule}.

verify(Params)->
	{ok,""}.
    
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
					sendSyslogAlert(Params,Msg)
			end
	end.

recover(This)->
    {ok,{_,Params}} = Rule:get_property(action_param),
    Title = ?PRODUCT_NAME ++ " Alert,recover " ++ 
	case Monitor:get_property(?NAME) of
		{ok,{_,Name}}->
			Name;
		_->
			"Unkown"
	end,
    Msg = THIS:createMessage("templates.syslog","Recover"),
    sendSyslog(Params, Title, Msg).
    

sendSyslogAlert(Params,Content)->
	Msg = THIS:createMessage("templates.syslog",Params#syslog_alert.template),
	(catch sendSyslog(Params, Content, Msg)).
    
sendSyslog(Params, Title, Message)->
    Session = syslog_session:new(),
    [Host] = Params#syslog_alert.host,
    [Port] = Params#syslog_alert.port,
    [Facility] = Params#syslog_alert.facility,
    [Program] = Params#syslog_alert.program,
    [Level] = Params#syslog_alert.level,
    Contents = Message,
    %%¿ªÊ¼ÅäÖÃ
    ErHost = ipstr_to_erip(Host),
    ErPort = hoststr_to_erhost(Port),
    ErFacility = facilitystr_to_erfacility(Facility),
    ErProgram = programstr_to_erprogram(Program),
    ErLevel = levelstr_to_erlevel(Level),
    Rets = 
	case Session:send_sysloginfo(ErHost, ErPort, ErFacility, ErProgram, ErLevel, Message) of
		{ok, Result}->
			THIS:logAlert(?LOG_TYPE, Host, Title ++ "\n", Contents,"ok"),
			{ok,Host,Contents, syslog};
		{error,Result} when is_binary(Result)->
			% THIS:logAlert(?LOG_TYPE, Host,binary_to_list(Result),"","fail"),
			THIS:logAlert(?LOG_TYPE, Host,binary_to_list(Result),Contents,"fail"),
			{error,Host,binary_to_list(Result), syslog};
		{error,Result}->
			% THIS:logAlert(?LOG_TYPE, Host, "send alert fail","","fail"),
			THIS:logAlert(?LOG_TYPE, Host,binary_to_list(Result),Contents,"fail"),
			{error,Host,Result, syslog}
	end,
    {ok,[Rets]}.

hoststr_to_erhost(PortStr) ->
    case string:to_integer(PortStr) of
        {error, Reason} ->
            514;
        {Int, _} ->
            Int
    end.

levelstr_to_erlevel(Level) ->
    case string:to_integer(Level) of
        {error, Reason} ->
            0;
        {Int, _} ->
            Int
    end.

facilitystr_to_erfacility(FacilityStr) ->
    case string:to_integer(FacilityStr) of
        {error, Reason} ->
            0;
        {Int, _} ->
            Int
    end.
    
programstr_to_erprogram(Program) ->
    try erlang:list_to_atom(Program) of
        P ->
            P
    catch
        _:_ ->
            []
    end.

ipstr_to_erip(IpStr) ->
    FS = string:tokens(IpStr, "."),
    Ip_list = lists:map(fun(X) -> erlang:list_to_integer(X) end, FS),
    erlang:list_to_tuple(Ip_list).
    
getScalarValues(Prop,Params)->
	case Prop of
        ?SYSLOG_FACILITY ->
            ?SYSLOG_FACILITYS;
        ?SYSLOG_LEVEL ->
            ?SYSLOG_LEVELS;
		%%to->
		%%	case preferences:all(additional_snmp_settings) of
		%%		{ok,Traps}->
		%%			[{Y#additional_snmp_settings.name,atom_to_list(X)}||{X,Y}<-Traps]++[{"Default","default"}];
		%%		_->
		%%			[]
		%%	end;
		template->
			[{filename:basename(X),filename:basename(X)}||X<-filelib:wildcard("templates.snmp/*")];
		_->
			BASE:getScalarValues(Prop,Params)
	end.
    
get_template_property()->
	[
	].