%% ---
%% checkpoint_monitor
%%
%%---
-module(checkpoint_monitor,[BASE]).
-extends(snmp_base).
-compile(export_all).
-include("monitor_template.hrl").
-include("monitor.hrl").
-include_lib("snmp/include/snmp_types.hrl").

-export([new/0, defaultTitle/1, update/0, getSNMPData2/3, getSNMPData/3, find_counter_name/2, set_counter_val/1, getCountersContent/0, getTemplateFile/0, getDefaultCounters/0, getAppServerTestOID/0, get_classifier/1, verify/1, getLogProperties/1, getCostInLicensePoints/0, get_template_property/0]).

-define(TEMPLATE_FILE,"counters.cp").

%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for checkpoint monitor
new()->
	Obj = snmp_base:new(),
	{?MODULE,Obj}.

%% @spec defaultTitle(Params) -> Obj
%% Obj = string()
%% @doc create a title for snmp monitor
defaultTitle(Params)->
	BASE:defaultTitle(Params) ++ ":" ++ proplists:get_value(host,Params).

    

%% @spec update() -> Result
%% Result = term()
%% @doc update is the run function called by schedule to test the checkpoint monitor info
update()->
	Oids = THIS:getCounters(THIS,THIS:getCountersContent()),
    %%io:format("*******Oids = ~p~n", [Oids]),
    %% ^\\d+$
	{ok,{_,Hosts}} = THIS:get_property(host),
    %%If the format is host: port, host and ip then isolated
    IsZero = (string:rstr(Hosts, ":") =:= 0),
    [Host, Port] =
    if
         IsZero ->
            [Hosts, THIS:getDefaultPort()];
         true ->
            [Ho, PortStr] = string:tokens(Hosts, ":"),
            Po =
            case string:to_integer(PortStr) of
                {error, _} ->
                    THIS:getDefaultPort();
                {P, _} ->
                    P;
                _ ->
                    THIS:getDefaultPort()
            end,
            [Ho, Po]
    end,
    
    %%io:format("****Host = ~p~n,Port = ~p~n", [Host, Port]),
    
	case ip_utils:check_ip(Host) of
		{error,_}->
			if
				length(Host) =<0->
					THIS:set_attribute(?NO_DATA,true),
					THIS:set_attribute(?CATEGORY,error),
					THIS:set_attribute(status,"error"),
					THIS:set_attribute(?STATE_STRING,"Host Name is empty.");
				true ->
					case inet:gethostbyname(Host) of
						{error,_}->
							THIS:set_attribute(?NO_DATA,true),
							THIS:set_attribute(?CATEGORY,error),
							THIS:set_attribute(status,"error"),
							THIS:set_attribute(?STATE_STRING,"Host Name invalid.");
						{ok,{_,_,_,_,_,[Ip|_]}}->
							THIS:getSNMPData2(Ip,Port,Oids);
						_->
							THIS:set_attribute(?NO_DATA,true),
							THIS:set_attribute(?CATEGORY,error),
							THIS:set_attribute(status,"error"),
							THIS:set_attribute(?STATE_STRING,"Host Name invalid.")
					end
			end;
		_->
			THIS:getSNMPData(Host,Port,Oids)
	end,
	ok.
	
%% @spec getSNMPData2(Ip, Port, OIds) -> Result
%% Result = term()
%% @doc get selected some checkpoint firewall-1 info by Ip
getSNMPData2(Ip,Port,OIds)->
	{ok,{_,Timeout}} = THIS:get_property(timeout),
	{ok,{_,Comm}} = THIS:get_property(community),
	{ok,{_,Index}} = THIS:get_property(index),

	Ids = [[list_to_integer(X) || X <- string:tokens(K,".")]|| {K,_} <- OIds],

	Session = snmp_session:new(Ip,Port,"v1",Comm,"","","","","","",Timeout*1000),
    %%io:format("****IP2 = ~p~n,Port2 = ~p~n", [Ip, Port]),
    
	case Session:g(THIS:getAppServerTestOID()) of
		{ok,_,_}->

			Ret = Session:get(Ids,Index),
			
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
							
			{_,ErrVal} = lists:splitwith(F, Ret),
			%io:format("getSNMPData:~p~n,~p~n",[Ret,ErrVal]),
			
			THIS:set_attribute(countersInError,length(ErrVal)),
			
			RetArray = set_counter_val(Ret),

			if 
					length(ErrVal) > 0 ->
						THIS:set_attribute(status,"error");
					true ->
						THIS:set_attribute(status,"ok")
				end,

			THIS:set_attribute(?STATE_STRING,RetArray);
		{error,{timeout,_}}->
			THIS:set_attribute(?CATEGORY,?NO_DATA),
			THIS:set_attribute(status,"error"),
			THIS:set_attribute(?STATE_STRING,"connect timeout");
		_->
			THIS:set_attribute(?CATEGORY,?NO_DATA),
			THIS:set_attribute(status,"error"),
			THIS:set_attribute(?STATE_STRING,"Application Server not available on host")
	end.

%% @spec getSNMPData(Ip, Port, OIds) -> Result
%% Result = term()
%% @doc get selected some checkpoint firewall-1 info by Host
getSNMPData(Host,Port,OIds)->
	%io:format("checkpoint getSNMPData:~p~n",[Host]),
	case Host of
		""->
			THIS:set_attribute(?NO_DATA,true),
			THIS:set_attribute(?CATEGORY,error),
			THIS:set_attribute(?STATE_STRING,"host name is empty");
	
		_->

		H = list_to_tuple([list_to_integer(X)||X<-string:tokens(Host,".")]),
		
		{ok,{_,Timeout}} = THIS:get_property(timeout),
		{ok,{_,Comm}} = THIS:get_property(community),
        {ok,{_,Index}} = THIS:get_property(index),

		Ids = [[list_to_integer(X) || X <- string:tokens(K,".")]|| {K,_} <- OIds],

	
		Session = snmp_session:new(H,Port,"v1",Comm,"","","","","","",Timeout*1000),
        
        %%io:format("****Host1 = ~p~n,Port1 = ~p~n", [H, Port]),
    
		case Session:g(THIS:getAppServerTestOID()) of
			{ok,_,_}->
				Ret = Session:get(Ids,Index),
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
							
				{_,ErrVal} = lists:splitwith(F, Ret),
				
				THIS:set_attribute(countersInError,length(ErrVal)),
				
				RetArray = set_counter_val(Ret),
				if 
					length(ErrVal) > 0 ->
						THIS:set_attribute(status,"error");
					true ->
						THIS:set_attribute(status,"ok")
				end,

				THIS:set_attribute(?STATE_STRING,RetArray);
			{error,{timeout,_}}->
				THIS:set_attribute(?CATEGORY,?NO_DATA),
				THIS:set_attribute(status,"error"),
				THIS:set_attribute(?STATE_STRING,"connect timeout");
			Other->
                io:format("*************Ret = ~p~n", [Other]),
				THIS:set_attribute(?CATEGORY,?NO_DATA),
				THIS:set_attribute(status,"error"),
				THIS:set_attribute(?STATE_STRING,"Application Server not available on host")
		end
	end.

%% @spec find_counter_name(C, Counters) -> Result
%% Result = atom()
%% @doc get name by oid
find_counter_name(_,[])->'unknow_name';
find_counter_name(C,[{K,V}|T])->
    if 
        is_atom(K) =:= true ->
            Key = atom_to_list(K);
        is_list(K) =:= true ->
            Key = K;
        true ->
            Key = K
    end,
	case  Key of
		C->	
			V;
		_->
			find_counter_name(C,T)
	end.

%% @spec set_counter_val(Ret) -> Result
%% Result = list()
%% @doc set value into attribute, build string of name and value
set_counter_val([])->[];
set_counter_val([C|T])->
	Counters = case THIS:get_property(counters) of
				{ok,{_,V}}->
					V;
				_->
					[]
			end,
	Name = THIS:find_counter_name(THIS:oid2string(element(1,C)),Counters),
	case C of 
		{_,{ok,{noError,_,[M|_]},_}}->
			THIS:set_attribute(Name,M#varbind.value),
			%%io_lib:format("~p=~p<br>",[Name,M#varbind.value]) ++ set_counter_val(T);
            snmp_ex2_manager:any_to_list(Name) ++ "=" ++ snmp_ex2_manager:any_to_list(M#varbind.value) ++ "<br>" ++ set_counter_val(T);
		{_,{ok,{Reason,_,_},_}}->
			THIS:set_attribute(Name,""),
			%%io_lib:format("~p=~p<br>",[Name,Reason]) ++ set_counter_val(T);
            snmp_ex2_manager:any_to_list(Name) ++ "=" ++ snmp_ex2_manager:any_to_list(Reason) ++ "<br>" ++ set_counter_val(T);
		{_,{error,Reason}}->
            %%io:format("Oid = ~p~n", [Name]),
			THIS:set_attribute(Name,""),
            snmp_ex2_manager:any_to_list(Name) ++ "=" ++ snmp_ex2_manager:any_to_list(Reason) ++ "<br>" ++ set_counter_val(T);
			%%io_lib:format("~p=~p<br>",[Name,Reason]) ++ set_counter_val(T);
		{_,_}->
			THIS:set_attribute(Name,""),
			%%io_lib:format("~p=~p<br>",[Name,"unknow return"]) ++ set_counter_val(T)
            snmp_ex2_manager:any_to_list(Name) ++ "=" ++ "unknow return" ++ "<br>" ++ set_counter_val(T)
	end.

%% @spec getCountersContent() -> Result
%% Result = list()
%% @doc get checkpoint counters of selected
getCountersContent()->
	case THIS:get_property(counters) of
		{ok,{_,V}}->
			V;
		_->
			[]
	end.

%% @spec getTemplateFile() -> Result
%% Result = string()
%% @doc get checkpoint firewall templete file name
getTemplateFile()->
	?TEMPLATE_FILE.

%% @spec getDefaultCounters() -> Result
%% Result = list()
%% @doc get default checkpoint firewall counters
getDefaultCounters()->
	THIS:getSNMPCounters(THIS,"",false).

%% @spec getAppServerTestOID() -> Result
%% Result = list()
%% @doc get oid for test application server
getAppServerTestOID()->
	[1,3,6,1,4,1,2620,1,1,7].

%% @spec get_classifier(error) -> List
%% List = [Tuple]
%% Tule = {status, Logic, Value}
%% Logic = '!=' | '==' | '>' | '<' | 'contain'
%% Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
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

%% @spec verify(Params)-> Result
%% Params = [Tuple]
%% Result = list()
%% @doc verify data of ui, ex. host,percentageBase,oid,oidIndex,timeout,scale and son
verify(Params)->
	Errs=
	case proplists:get_value(host,Params) of
		""->
			[{host,"Host Name missing"}];
		Host->
			case string:rstr(Host," ") of
				0->
					[];
				_->
					[{host,"no spaces are allowed"}]
			end
	end ++
	case proplists:get_value(counters,Params) of
		[]->
			[{counters,"Counters missing"}];
		_->
			[]
	end ++
	case proplists:get_value(index,Params) of
		""->
			[{index,"Index missing"}];
		Index when not is_number(Index)->
			[{index,"Index must be a number"}];
		_->
			[]
	end ++
	case proplists:get_value(timeout,Params) of
		""->
			[{index,"Timeout missing"}];
		Timeout when not is_number(Timeout)->
			[{index,"Timeout must be a number"}];
		X->
			case X > 0 of
			true ->
				[];
			_ ->
				[{index,"Timeout must be greater than 0"}]
			end
	end ++
	case proplists:get_value(community,Params) of
		[]->
			[{community,"Community missing"}];
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

%%Each update when they are called
%% @spec getLogProperties(This) -> Result
%% Result = list()
%% @doc get log info of this checkpoint monitor
getLogProperties(This)->
    %%io:format("***********LogProperty~n"),
	{ok,{_,Counters}} = THIS:get_property(counters),
	Temp = This:get_template_property(),
	[X#property.name || X<-Temp,X#property.state=:=true] ++ [X||{X,_}<- Counters].


%%Edit Save time call
%% @spec getCostInLicensePoints() -> Result
%% Result = integer()
%% @doc get counters of selected for calculate license points
getCostInLicensePoints()->
    %%io:format("***********License~n"),
	{ok,{_,Counters}} = THIS:get_property(counters),
	length(Counters).

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
%% @doc get monitor template of checkpoint monitor
get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=status,title="status",configurable=false,state=true},
	#property{name=host,title="Host Name", description="the IP address or host name of the CheckPoint Server to be monitored.",type=text,editable=true,order=1},
	#property{name=counters,title="Counters", description="Current selection of counters.",type=counters,editable=true,order=1,default=THIS:getDefaultCounters()},
	#property{name=index,title="Index", description="the index of the SNMP object - for non-table object IDs, this is 0",type=numeric,editable=true,default=0},
	#property{name=community,title="Community", description="Community for the SNMP object",type=text,editable=true,default='public'},
	#property{name=timeout,title="Timeout", description="the total time, in seconds, to wait for a successful reply",type=numeric,editable=true,default=5,baselinable=true}
	].
