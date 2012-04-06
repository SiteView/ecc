%% ---
%% browsable_snmp_base
%%
%%---
-module(browsable_snmp_base_text,[BASE]).
-extends(browsable_base).
-compile(export_all).

-export([new/0, update/0, obtainSession/0, obtainSession/1, getBrowseData/1, get_counter_oids/1, oid2string/1, find_counter_name/1, set_counter_val/1, verify/1, defaultTitle/1, getScalarValues/2, get_template_property/0]).

-include("monitor_template.hrl").
-include_lib("snmp/include/snmp_types.hrl").
-include("monitor.hrl").

%% @spec new() -> Obj
%% where
%% Obj = term()
%% @doc create a new instance for browsable_snmp_base
new()->
	Base = browsable_base:new(),
	Base:set_attribute(lastMeasurementTime,0),
	{?MODULE,Base}.

%% @spec update() -> Result
%% Result = term()
%% @doc update is the run function called by schedule to test the snmp information
update()-> 
	Session = THIS:obtainSession(),
    case Session:test_snmp() of
        true ->
            Browse = case THIS:get_property(browse) of
                            {ok,{_,V}}->
                                V;
                            _->
                                []
                        end,
            Oids = get_counter_oids(Browse),
            case Session of
                null->
                    THIS:set_attribute(?NO_DATA,true),
                    THIS:set_attribute(?CATEGORY,error),
                    THIS:set_attribute(countersInError,"n/a"); %+1±£Ö¤
                _->
		
                Oids = get_counter_oids(Browse),

		%%io:format("snmp oids:~p~n",[Oids]),

                Ret = Session:get(Oids),

                F = fun({_,Y})->
                            case Y of
                                {ok,{noError,_,[Ob|_]},_}->
                                    case Ob#varbind.value of
                                        noSuchObject->
                                            false;
                                        noSuchInstance->
                                            false;
                                        {timeout,_}->
                                            false;
                                        _->
                                            true
                                    end;
                                _->
                                    false
                            end
                        end,
        
        %%io:format("*************browsable_snmp_base data = ~p~n", [Ret]),

                {_,ErrVal} = lists:splitwith(F, Ret),
		
                THIS:set_attribute(countersInError,length(ErrVal)),
		
                RetArray = set_counter_val(Ret),

                THIS:set_attribute(?STATE_STRING,RetArray)
            end;
        _ ->
            THIS:set_attribute(?NO_DATA,true),
			THIS:set_attribute(?CATEGORY,error),
            THIS:set_attribute(countersInError,1),
			THIS:set_attribute(status,"error"),
			THIS:set_attribute(?STATE_STRING,"Application Server not available on host")
    end.


isMultiThreshold()->true.

%% @spec obtainSession() -> Result
%% Result = object
%% @doc create snmpsession and set it
obtainSession()->
	{ok,{_,Server}} = THIS:get_property(server),
	{ok,{_,Ver}} = THIS:get_property(snmpversion),
	{ok,{_,Community}} = THIS:get_property(community),
	{ok,{_,AuthType}} = THIS:get_property(snmpv3authtype),
	{ok,{_,User}} = THIS:get_property(snmpv3username),
	{ok,{_,Passwd}} = THIS:get_property(snmpv3authpassword),
	{ok,{_,PrivPasswd}} = THIS:get_property(snmpv3privpassword),
	{ok,{_,EngineID}} = THIS:get_property(contextEngineID),
	{ok,{_,ContextName}} = THIS:get_property(contextName),
	{ok,{_,Timeout}} = THIS:get_property(timeout),
	case Server of
		""->
			THIS:set_attribute(?STATE_STRING,"Server name is empty"),
			null;
		_->
			S = case ip_utils:check_ip(Server) of
					{error,_}->
						if
							length(Server) =<0->
								THIS:set_attribute(?NO_DATA,true),
								THIS:set_attribute(?CATEGORY,error),
								THIS:set_attribute(?STATE_STRING,"Server Name is empty."),
								"";
							true ->
								case inet:gethostbyname(Server) of
									{error,_}->
										THIS:set_attribute(?NO_DATA,true),
										THIS:set_attribute(?CATEGORY,error),
										THIS:set_attribute(?STATE_STRING,"Server Name invalid."),
										"";
									{ok,{_,_,_,_,_,[Ip|_]}}->
										Ip;
									_->
										THIS:set_attribute(?NO_DATA,true),
										THIS:set_attribute(?CATEGORY,error),
										THIS:set_attribute(?STATE_STRING,"Server Name invalid."),
										""
								end
						end;
					_->
						list_to_tuple([list_to_integer(X) || X <- string:tokens(Server,".")])
				end,
			%S = list_to_tuple([list_to_integer(X) || X <- string:tokens(Server,".")]),
			case S of
				""->
					null;
				_->
					Session = snmp_session:new(S,161,Ver,Community,AuthType,
												User,Passwd,PrivPasswd,
												EngineID,ContextName,Timeout*1000),
					Session
			end
	end.
%	case Server of
%		""->
%			THIS:set_attribute(?STATE_STRING,"host name is empty"),
%			null;
%		_->
%		S = list_to_tuple([list_to_integer(X) || X <- string:tokens(Server,".")]),
%
%		Session = snmp_session:new(S,161,Ver,Community,AuthType,
%									User,Passwd,PrivPasswd,
%									EngineID,ContextName,Timeout*1000),
%		Session

	%end.

%% @spec obtainSession(Params) -> Result
%% Result = object
%% Params = list()
%% @doc create snmpsession and set it by specify parameter
obtainSession(Params)->
	Server = proplists:get_value(server,Params),
	Ver = proplists:get_value(snmpversion,Params),
	Community = proplists:get_value(community,Params),
	AuthType = proplists:get_value(snmpv3authtype,Params),
	User = proplists:get_value(snmpv3username,Params),
	Passwd = proplists:get_value(snmpv3authpassword,Params),
	PrivPasswd = proplists:get_value(snmpv3privpassword,Params),
	EngineID = proplists:get_value(contextEngineID,Params),
	ContextName = proplists:get_value(contextName,Params),
	Timeout = case proplists:get_value(timeout,Params) of
					undefined->
						5;
					"0"->
						5;
					Time->
						case string:to_integer(Time) of
							{error,_}->
								5;
							{Val,_}->
								Val
						end
				end,
	case Server of
		""->
			THIS:set_attribute(?STATE_STRING,"host name is empty"),
			null;
		_->
			S = case ip_utils:check_ip(Server) of
					{error,_}->
						if
							length(Server) =<0->
								THIS:set_attribute(?NO_DATA,true),
								THIS:set_attribute(?CATEGORY,error),
								THIS:set_attribute(?STATE_STRING,"Host Name is empty."),
								"";
							true ->
								case inet:gethostbyname(Server) of
									{error,_}->
										THIS:set_attribute(?NO_DATA,true),
										THIS:set_attribute(?CATEGORY,error),
										THIS:set_attribute(?STATE_STRING,"Host Name invalid."),
										"";
									{ok,{_,_,_,_,_,[Ip|_]}}->
										Ip;
									_->
										THIS:set_attribute(?NO_DATA,true),
										THIS:set_attribute(?CATEGORY,error),
										THIS:set_attribute(?STATE_STRING,"Host Name invalid."),
										""
								end
						end;
					_->
						list_to_tuple([list_to_integer(X) || X <- string:tokens(Server,".")])
				end,
			%S = list_to_tuple([list_to_integer(X) || X <- string:tokens(Server,".")]),
			case S of
				""->
					null;
				_->
					Session = snmp_session:new(S,161,Ver,Community,AuthType,
												User,Passwd,PrivPasswd,
												EngineID,ContextName,Timeout*1000),
					Session
			end
	end.

%% @spec getBrowseData(Params)-> Obj
%% Params = [Tuple]
%% Obj = list()
%% @doc get counters of specified mibfile
getBrowseData(Params)->
	case proplists:get_value(mibfile,Params) of
		undefined->
			[];
		Mibfile->
			browsable_mib:get_mid_oids(Mibfile,Params)
	end.

%% @spec get_counter_oids(Browse)-> Obj
%% Browse = list()
%% Obj = list()
%% @doc get counters by Browse counters
get_counter_oids([])->[];
get_counter_oids([{K,_}|T])->
	[[list_to_integer(X) || X <- string:tokens(K,".")]] ++ get_counter_oids(T).


get_counter(_,[])->[];
get_counter(I,[{K,_}|T])->
	Key = atom_to_list(K),
	case string:rstr(Key,?BROWSE_COUNTER) of
		1->
			[_,Id,_] = string:tokens(Key,?BROWSE_SPLIT),
			[[list_to_integer(X) || X <- string:tokens(Id,".")]] ++ get_counter(I+1,T);
		_->
			 get_counter(I,T)
	end.

%% @spec oid2string(Oids)-> Obj
%% Obj = string()
%% @doc switch oid list to oid string (eg. [1,3,3,1] -> "1.3.3.1")
oid2string([])->"";
oid2string([I|T])->
	case T of
		[]->
			integer_to_list(I);
		_->
			integer_to_list(I) ++ "." ++ oid2string(T)
	end.

%% @spec find_counter_name(C)-> Obj
%% C = atom()
%% Obj = string()
%% @doc get counter name
find_counter_name(C)->
	find_counter_name(C,THIS:get_properties()).

find_counter_name(_,[])->'unknow_name';
find_counter_name(C,[{K,V}|T])->
	Key = K,
	case  Key of
		C->	
			V;
		_->
			find_counter_name(C,T)
	end.

get_counter(I)->
	Count = THIS:getMaxCounter(),
	if
		I < Count, I =:= Count ->
			{ok,{_,V}} = THIS:get_property(list_to_atom(atom_to_list(browseNameid) ++ integer_to_list(I))),
			case V of
				''->
					[];
				_->
					[[list_to_integer(X) || X <- string:tokens(atom_to_list(V),".")]] ++ get_counter(I+1)
			end;
		true->
			[]
	end.

%% Inside look at Counters by OidStr Name
getfullname_in_counters(OidStr) ->
    ok.

%% @spec set_counter_val(V)-> Obj
%% V = list()
%% Obj = string()
%% @doc set counter's value to attribute and return value string
set_counter_val([])->[];
set_counter_val([C|T])->
	Browse = case THIS:get_property(browse) of
				{ok,{_,V}}->
					V;
				_->
					[]
			end,
	Name = THIS:find_counter_name(oid2string(element(1,C)),Browse),
	Id = oid2string(element(1,C)),
	case C of 
		{_,{ok,{noError,_,[M|_]},_}}->
            V1 = snmp_ex2_manager:getOctetString(M#varbind.value),
			THIS:set_attribute(Id,V1),
            THIS:set_attribute(Name,V1),
            %%io:format("~p=~p<br>",[list_to_atom_filter(Name),list_to_atom_filter(V1)]),
			%%io_lib:format("~p=~p<br>",[Name,V1]) ++ set_counter_val(T);
            snmp_ex2_manager:any_to_list(Name) ++ " = " ++ snmp_ex2_manager:any_to_list(V1) ++ "<br>" ++ set_counter_val(T);
		{_,{ok,{Reason,_,_},_}}->
			THIS:set_attribute(Id,""),
            THIS:set_attribute(Name,""),
			%%io_lib:format("~p=~p<br>",[Name,Reason]) ++ set_counter_val(T);
            snmp_ex2_manager:any_to_list(Name) ++ " = " ++ snmp_ex2_manager:any_to_list(Reason) ++ "<br>" ++ set_counter_val(T);
		{_,{error,Reason}}->
			THIS:set_attribute(Id,""),
            THIS:set_attribute(Name,""),
			%%io_lib:format("~p=~p<br>",[Name,Reason]) ++ set_counter_val(T);
            snmp_ex2_manager:any_to_list(Name) ++ " = " ++ snmp_ex2_manager:any_to_list(Reason) ++ "<br>" ++ set_counter_val(T);
		{_,_}->
			
			THIS:set_attribute(Id,""),
            THIS:set_attribute(Name,""),
			%%io_lib:format("~p=~p<br>",[Name,"unknow return"]) ++ set_counter_val(T)
            snmp_ex2_manager:any_to_list(Name) ++ " = " ++ "unknow return" ++ "<br>" ++ set_counter_val(T)
	end.



%% @spec verify(Params)-> Result
%% Params = [Tuple]
%% Result = list()
%% @doc verify data of ui, ex. host,percentageBase,oid,oidIndex,timeout,scale and son
verify(Params)->
	AuthType = proplists:get_value(snmpv3authtype,Params),
	Ver =proplists:get_value(snmpversion,Params),
	Errs=
	case proplists:get_value(server,Params) of
		Server when Server==undefined orelse Server==""->
			[{server,"The device name/IP address cannot be blank."}];
		_->
			[]
	end ++
	case proplists:get_value(community,Params) of
		Com when Com==undefined orelse Com==""->
			case proplists:get_value(snmpversion,Params) of
				Ver when Ver=="v1" orelse Ver=="v2"->
					[{community,"No community string given (version " ++ Ver ++ " requires a community string)."}];
				_->
					[]
			end;
		_->
			[]
	end ++
	case proplists:get_value(snmpv3username,Params) of
		Usr when Usr == undefined orelse Usr==""->
			case proplists:get_value(snmpversion,Params) of
				"v3"->
					[{snmpv3username,"User name cannot be blank for version 3"}];
				_->
					[]
			end;
		_->
			[]
	end ++
	case proplists:get_value(snmpv3authpassword,Params) of
		AuthPasswd when length(AuthPasswd) < 8 andalso AuthType=/= "None" andalso Ver=="v3" ->
				[{snmpv3authpassword,"The authentication password must be greater than 8 characters."}];
		_->
			[]
	end ++
	case proplists:get_value(snmpv3privpassword,Params) of
		PrivPass when length(PrivPass) >0 andalso length(PrivPass) < 8 andalso Ver=="v3"->
			[{snmpv3privpassword,"The privacy password must be either blank (no privacy) or greater than 8 characters (DES privacy)"}];
		_->
			[]
	end ++
	case proplists:get_value(contextEngineID,Params) of
		""->
			[];
		ContextEngineID when Ver=="v3"->
			case regexp:match(ContextEngineID,"^[0-9|a-f|A-F]+$") of
				nomatch->
					[{contextEngineID,"The Context Engine ID must be a string of hexidecimal digits (0-9, A-F) or blank, if no Context Engine ID should be transmitted"}];
				_->
					[]
			end;
		_->
			[]
	end ++
	case proplists:get_value(timeout,Params) of
		""->
			[{timeout,"timeout missing"}];
		Timeout when not is_number(Timeout)->
			[{timeout,"timeout must be a number"}];
		_->
			[]
	end ++
	case proplists:get_value(port,Params) of
		""->
			[{port,"Port missing"}];
		Timeout when not is_number(Timeout)->
			[{port,"Port must be a number"}];
		_->
			[]
	end ++
	case BASE:verify(Params) of
		{error,Be}->
			Be;
		_->
			[]
	end,
	if
		length(Errs)>0->
			{error,Errs};
		true ->
			{ok,""}
	end.

%% @spec defaultTitle(Params) -> Obj
%% Obj = string()
%% @doc create a title for snmp monitor
defaultTitle(Params)->
	BASE:defaultTitle(Params) ++ ":" ++ proplists:get_value(server,Params).
	
%% @spec getScalarValues(Prop,Params)-> Result
%% Result = list()
%% @doc get version list, oid list, percentageBase list, scale list for ui's combobox
getScalarValues(Prop,Params)->
	case Prop of
		snmpversion->
			[{"V1","v1"},{"V2","v2"},{"V3","v3"}];
		snmpv3authtype->
			[{"MD5","MD5"},{"SHA","SHA"},{"NoAuthentication","None"}];
		_->
			BASE:getScalarValues(Prop,Params)
	end.
	
getHostname()->
	case THIS:get_property(server) of
		{ok,{_,V}}->
			V;
		_->
			BASE:getHostname()
	end.

%% @spec get_template_property()-> Result
%% Result = [Record]
%% Record = property
%% @doc get monitor template of browsable_snmp_base
get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=server,title="Server", description="the IP address or host name of the server",type=text,editable=true,order=1},
	#property{name=mibfile,title="MIB File", description="the MIB file from which this monitor should read OIDs",type=scalar,editable=true,order=2},
	#property{name=snmpversion,title="SNMP Version", description=" the version of SNMP to use when requesting data from  the server",type=scalar,editable=true,order=3},
	#property{name=community,title="V1/V2 Community", description="the community for the SNMP agent for SNMP V1/V2 only",type=text,editable=true,order=4,default=public},
	#property{name=snmpv3authtype,title="SNMP V3 Authentication Type", description="Type of authentication to be used for SNMP V3 only",type=scalar,editable=true,order=5},
	#property{name=snmpv3username,title="SNMP V3 Username", description="Username to be used for authentication for SNMP V3 only",type=text,editable=true,order=6},
	#property{name=snmpv3authpassword,title="SNMP V3 Authentication Password", description="Password to be used for authentication for SNMP V3 only",type=password,editable=true,order=7},
	#property{name=snmpv3privpassword,title="SNMP V3 Privacy Password", description="SNMP V3 Privacy Password",type=password,editable=true,order=8},
	#property{name=contextEngineID,title="SNMP V3 Context Engine ID", description="a hexidecimal string representing the Context Engine ID to use for this connection for SNMP V3 only",type=text,editable=true,order=9},
	#property{name=contextName,title="SNMP V3 Context Name", description="the Context Name to use for this connection for SNMP V3 only",type=text,editable=true,order=10},
	#property{name=timeout,title="Timeout", description="the time in seconds to wait for all requests to complete (including retries)",type=numeric,editable=true,advance=true,order=10,default=5,baselinable=true},
	#property{name=port,title="Port", description="the port number on which to query the SNMP agent",type=numeric,editable=true,advance=true,order=12,default=161}
	].
