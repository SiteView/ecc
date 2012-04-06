%%
%% snmp_monitor
%%
%%
-module(olt_device_subinterface_state_monitor,[BASE]).
-extends(atomic_monitor).
-compile(export_all).
-export([new/0, defaultTitle/1, update/0, do_snmp_v2/4, do_snmp_v3/6, getScalarValues/2, get_classifier/1, verify/1, get_template_property/0]).
-define(SNMP_PORT,161).
-include("monitor_template.hrl").
-include("monitor.hrl").
-include_lib("snmp/include/snmp_types.hrl").




%% @spec new() -> Obj
%% where
%% Obj = term()
%% @doc create a new instance for snmp monitor
new()-> 
	Base = atomic_monitor:new(),
	Base:set_attribute(content,""),
	Base:set_attribute(count,0),
	{?MODULE,Base}.

%% @spec defaultTitle(Params) -> Obj
%% Obj = string()
%% @doc create a title for snmp monitor
defaultTitle(Params)->
	BASE:defaultTitle(Params) ++ ":" ++
	proplists:get_value(host,Params).
	

%% @spec update() -> Result
%% Result = term()
%% @doc update is the run function called by schedule to test the snmp information
update()->
	%%io:format("snmp_monitor run..."),
	{ok,{host,Host}} = THIS:get_property(host),
	{ok,{community,Comm}} = THIS:get_property(community),
	{ok,{timeout,Timeout}} = THIS:get_property(timeout),

	H = 
	case ip_utils:check_ip(Host) of
		{error,_}->
			if
				length(Host) =<0->
					THIS:set_attribute(count,0),
					THIS:set_attribute(?NO_DATA,true),
					THIS:set_attribute(?CATEGORY,error),
					THIS:set_attribute(?STATE_STRING,"Host Name is empty."),
					"";
				true ->
					case inet:gethostbyname(Host) of
						{error,_}->
							THIS:set_attribute(count,0),
							THIS:set_attribute(?NO_DATA,true),
							THIS:set_attribute(?CATEGORY,error),
							THIS:set_attribute(?STATE_STRING,"Host Name invalid."),
							"";
						{ok,{_,_,_,_,_,[Ip|_]}}->
							Ip;
						_->
							THIS:set_attribute(count,0),
							THIS:set_attribute(?NO_DATA,true),
							THIS:set_attribute(?CATEGORY,error),
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
			{ok,{snmpversion,Ver}} = THIS:get_property(snmpversion),
			{ok,{snmpv3user,User}} = THIS:get_property(snmpv3user),
			{ok,{snmpv3passwd,Passwd}} = THIS:get_property(snmpv3passwd),

			Session = snmp_session:new(H,161,Ver,Comm,"",User,Passwd,"","","",Timeout*1000),
			   
			Err1 = case Ver of
				"v1"->
				    do_snmp_v2(H,Comm,Timeout, Session);
				"v2"->
				    do_snmp_v2(H,Comm,Timeout, Session);
				"v3"->
				    do_snmp_v3(H,Comm,Timeout,User,Passwd,Session)
			end
	end.



%% @spec do_snmp_v2(Host,Community,Oid,Timeout, Session) -> Result
%% Result = string()
%% @doc get snmp info by from device by Host, Community, Oid and so on in snmp version2
do_snmp_v2(Host,Community,Timeout, Session)->	
	InterfaceStautOid = [1,3,6,1,4,1,2011,6,128,1,1,2,46,1,15],
	DescOid = [1,3,6,1,4,1,2011,6,128,1,1,2,43,1,9],		
	
	InterfaceStautOidResult = Session:get_table_col(InterfaceStautOid),
	
	OLTDESC = get_oltdesc(InterfaceStautOidResult,[],DescOid,Session,0),
	
	{Des,Count} = OLTDESC,

	Return = "offLine_Onu_Conut:" ++ integer_to_list(Count) ++ "<br>" ++ "offLine_Onu_describe:" ++ Des,
	THIS:set_attribute(count,Count),
	THIS:set_attribute(?NO_DATA,true),
	THIS:set_attribute(?CATEGORY,good),
	THIS:set_attribute(?STATE_STRING,Return).


    
 
get_value_col([Priv|Next],OltCol)->
	OltType = Priv#varbind.value,
	io:format("OltType = ~p~n",[OltType]),
	if
	     OltType =/= 1 ->
                    get_value_col(Next,[Priv] ++ OltCol);
	     true -> 
		     get_value_col(Next,OltCol)
	end;
		
get_value_col([],OltCol)->
	OltCol. 




get_oltdesc([H|T],Value,DescOid,Session,Count) ->
    case H#varbind.value of 
	1 -> %% interface status is normal
	    get_oltdesc(T,Value,DescOid,Session,Count);
	    
	_->  %% get interface status isn't normal
	    Storage_typeoid = H#varbind.oid,
%%-------------yi.duan-----------get last 2 oid-------------------
	    Len = length(Storage_typeoid),
	    Index = lists:sublist(Storage_typeoid, Len-1, 2),
%%-------------yi.duan------------------------------	
	    DescOid1 = DescOid++Index,
	    Descs =	{oltdesc, format_result(Session:gt(DescOid1))},
	    {oltdesc, [Desc]} = Descs,
	    OltDesc = Desc#varbind.value,
	    get_oltdesc(T,Value ++"<br>"++ [OltDesc],DescOid,Session,Count+1)
    end;

get_oltdesc([],Value,_DescOid,Session,Count) ->
	{Value,Count}.



format_result(Value) ->
	case Value of
		{ok,{noError,_,[Vb|_]},_}->
			case Vb#varbind.variabletype of
				'NULL'-> [];
				_->
					io:format("Vb= ~p~n",[Vb]),
					[Vb]
			end;
		_->
			[]
	end.


to_IpString(T) ->
    to_OidString(tuple_to_list(T)).

to_OidString(T) ->
    Temp = lists:map(fun(X) -> integer_to_list(X) end, T),
    string:join(Temp, ".").

%% @spec do_snmp_v3(Host,Community,Oid,Timeout,User,Passwd, Session) -> Result
%% Result = string()
%% @doc get snmp info by from device by Host, Community, Oid, User, Password and so on in snmp version3
do_snmp_v3(Host,_,Timeout,User,Passwd,Session)->

	InterfaceStautOid = [1,3,6,1,4,1,2011,6,128,1,1,2,46,1,15],
	DescOid = [1,3,6,1,4,1,2011,6,128,1,1,2,43,1,9],	
	
	InterfaceStautOidResult = Session:get_table_col(InterfaceStautOid),
	
	OLTDESC = get_oltdesc(InterfaceStautOidResult,[],DescOid,Session,0),
	
	{Des,Count} = OLTDESC,

	Return = "offLine_Onu_Conut:" ++ integer_to_list(Count) ++ "<br>" ++ "offLine_Onu_describe:" ++ Des,
	THIS:set_attribute(count,Count),
	THIS:set_attribute(?NO_DATA,true),
	THIS:set_attribute(?CATEGORY,good),
	THIS:set_attribute(status,"good"),
	THIS:set_attribute(?STATE_STRING,Return).
	    
	

%% @spec getScalarValues(Prop,Params)-> Result
%% Result = list()
%% @doc get version list, oid list, percentageBase list, scale list for ui's combobox
getScalarValues(Prop,Params)->
	case Prop of
		snmpversion->
			[{"V1","v1"},{"V2","v2"},{"V3","v3"}];
		_->
			BASE:getScalarValues(Prop,Params)
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
			[{count,'>',200}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{count,'>',100}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{count,'<',100}]
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


%% @spec get_template_property()-> Result
%% Result = [Record]
%% Record = property
%% @doc get monitor template of snmp monitor
get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=host,title="Host Name",description="the IP address or host name from which the snmp value will be retrieved",type=text,editable=true,order=1},
	#property{name=community,title="Community",description="the community the SNMP object",type=text,editable=true,order=3,default="public"},

	#property{name=timeout,title="Timeout",description="the total time, in seconds, to wait for a successful reply",type=numeric,editable=true,advance=true,default=5,order=1,baselinable=true},
	#property{name=snmpversion,title="SNMP Version",description="the version of the mib that the oid belongs to",type=scalar,editable=true,advance=true,default="v1",order=2},

	#property{name=snmpv3user,title="SNMP V3 Username",description="Username to be used for authentication for SNMP V3",type=text,editable=true,advance=true,order=5},
	#property{name=snmpv3passwd,title="SNMP V3 Password",description="Password to be used for authentication for SNMP V3",type=text,editable=true,advance=true,order=5},

	%~ #property{name=count,title="count",configurable=false,state=true}
	
	 #property{name=count,title="count",type=numeric,order=1,configurable=false,state=true,baselinable=true}
	].