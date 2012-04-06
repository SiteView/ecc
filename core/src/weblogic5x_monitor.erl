%% ---
%% checkpoint_monitor
%%
%%---
-module(weblogic5x_monitor,[BASE]).
-extends(snmp_base).
-compile(export_all).
-include("monitor_template.hrl").
-include("monitor.hrl").
-include_lib("snmp/include/snmp_types.hrl").

-export([new/0, defaultTitle/1, update/0, getSNMPData2/3, getSNMPData/3, find_counter_name/2, set_counter_val/1, getCountersContent/0, getTemplateFile/0, getDefaultCounters/0, getAppServerTestOID/0, get_classifier/1, verify/1, get_template_property/0]).

-define(TEMPLATE_FILE,"counters.beawl").

%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for weblogic5.x monitor
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
%% @doc update is the run function called by schedule to test the weblogic5.x monitor info
update()->
	Oids = THIS:getCounters(THIS,THIS:getCountersContent()),

	{ok,{_,Hosts}} = THIS:get_property(host),
    
    %%如果格式是host:port，那么分离出host和ip
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
%% @doc get selected some weblogic5.x info by Ip
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
%% @doc get selected some weblogic5.x info by Host
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
			_->
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
	Key = K,
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
			%%lists:flatten(io_lib:format("~p=~p<br>",[Name,M#varbind.value])) ++ set_counter_val(T);
            snmp_ex2_manager:any_to_list(Name) ++ " = " ++ snmp_ex2_manager:any_to_list(M#varbind.value) ++ "<br>" ++ set_counter_val(T);
		{_,{ok,{Reason,_,_},_}}->
			THIS:set_attribute(Name,""),
			%%lists:flatten(io_lib:format("~p=~p<br>",[Name,Reason])) ++ set_counter_val(T);
            snmp_ex2_manager:any_to_list(Name) ++ " = " ++ snmp_ex2_manager:any_to_list(Reason) ++ "<br>" ++ set_counter_val(T);
		{_,{error,Reason}}->
			THIS:set_attribute(Name,""),
			%%lists:flatten(io_lib:format("~p=~p<br>",[Name,Reason])) ++ set_counter_val(T);
            snmp_ex2_manager:any_to_list(Name) ++ " = " ++ snmp_ex2_manager:any_to_list(Reason) ++ "<br>" ++ set_counter_val(T);
		{_,_}->
			THIS:set_attribute(Name,""),
			%%lists:flatten(io_lib:format("~p=~p<br>",[Name,"unknow return"])) ++ set_counter_val(T)
            snmp_ex2_manager:any_to_list(Name) ++ " = " ++ "unknow return" ++ "<br>" ++ set_counter_val(T)
	end.

%% @spec getCountersContent() -> Result
%% Result = list()
%% @doc get weblogic5.x counters of selected
getCountersContent()->
	case THIS:get_property(counters) of
		{ok,{_,V}}->
			V;
		_->
			[]
	end.

%% @spec getTemplateFile() -> Result
%% Result = string()
%% @doc get weblogic5.x templete file name
getTemplateFile()->
	?TEMPLATE_FILE.

%% @spec getDefaultCounters() -> Result
%% Result = list()
%% @doc get default weblogic5.x counters
getDefaultCounters()->
	THIS:getSNMPCounters(THIS,"",false).

%% @spec getAppServerTestOID() -> Result
%% Result = list()
%% @doc get oid for test application server
getAppServerTestOID()->
	[1,3,6,1,4,1,140,600,20,1,20].

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
		_->
			[]
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
%% @doc get monitor template of weblogic5.x monitor
get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=status,title="status",configurable=false,state=true},
	#property{name=host,title="Host Name", description="the IP address or host name of the Weblogic Server to be monitored.",type=text,editable=true,order=1},
	#property{name=index,title="Index", description="the index of the SNMP object - for non-table object IDs, this is 0",type=numeric,editable=true,default=0},
	#property{name=community,title="Community", description="Community for the SNMP object",type=text,editable=true,default='public'},
	#property{name=counters,title="Counters", description="Current selection of counters.",type=counters,editable=true,default=THIS:getDefaultCounters()},
	#property{name=timeout,title="Timeout", description="the total time, in seconds, to wait for a successful reply",type=numeric,editable=true,default=5,baselinable=true}
	].
