%% ---
%% checkpoint_monitor
%%
%%---
-module(dynamo_monitor,[BASE]).
-extends(snmp_base).
-compile(export_all).
-include("monitor_template.hrl").
-include("monitor.hrl").
-include_lib("snmp/include/snmp_types.hrl").

-define(TEMPLATE_FILE,"counters.atgdyn").

new()->
	Obj = snmp_base:new(),
	{?MODULE,Obj}.

defaultTitle(Params)->
	BASE:defaultTitle(Params) ++ ":" ++ proplists:get_value(host,Params).

update()->
	Oids = THIS:getCounters(THIS,THIS:getCountersContent()),

	{ok,{_,Host}} = THIS:get_property(host),
	%%io:format("checkpoint oids:~p~n",[Host]),

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
							THIS:getSNMPData2(Ip,THIS:getDefaultPort(),Oids);
						_->
							THIS:set_attribute(?NO_DATA,true),
							THIS:set_attribute(?CATEGORY,error),
							THIS:set_attribute(status,"error"),
							THIS:set_attribute(?STATE_STRING,"Host Name invalid.")
					end
			end;
		_->
			THIS:getSNMPData(Host,THIS:getDefaultPort(),Oids)
	end,
	ok.

getSNMPData2(Ip,Port,OIds)->
	{ok,{_,Timeout}} = THIS:get_property(timeout),
	{ok,{_,Comm}} = THIS:get_property(community),
	{ok,{_,Index}} = THIS:get_property(index),

	Ids = [[list_to_integer(X) || X <- string:tokens(K,".")]|| {K,_} <- OIds],


	Session = snmp_session:new(Ip,Port,"v1",Comm,"","","","","","",Timeout*1000),
	
	case Session:g(THIS:getAppServerTestOID()) of
		{ok,_,_}->

			Ret = Session:get(Ids,Index),
			
			F = fun({_,Y})->
					case Y of
						{ok,{noError,_,[Ob|_]},_}->
							case Ob#varbind.value of
								noSuchObject->
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
	end.

getSNMPData(Host,Port,OIds)->
	%io:format("checkpoint getSNMPData:~p~n",[Host]),
	case ip_utils:check_ip(Host) of
		{error,_}->
			THIS:set_attribute(?NO_DATA,true),
			THIS:set_attribute(?CATEGORY,error),
			THIS:set_attribute(status,"error"),
			THIS:set_attribute(?STATE_STRING,"Host Name invalid");
	
		_->

		H = list_to_tuple([list_to_integer(X)||X<-string:tokens(Host,".")]),
		
		{ok,{_,Timeout}} = THIS:get_property(timeout),
		{ok,{_,Comm}} = THIS:get_property(community),
		{ok,{_,Index}} = THIS:get_property(index),

		Ids = [[list_to_integer(X) || X <- string:tokens(K,".")]|| {K,_} <- OIds],

	
		Session = snmp_session:new(H,Port,"v1",Comm,"","","","","","",Timeout*1000),
		
		case Session:g(THIS:getAppServerTestOID()) of
			{ok,_,_}->

				Ret = Session:get(Ids,Index),
				
				F = fun({_,Y})->
						case Y of
							{ok,{noError,_,[Ob|_]},_}->
								case Ob#varbind.value of
									noSuchObject->
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


find_counter_name(_,[])->'unknow_name';
find_counter_name(C,[{K,V}|T])->
	Key = K,
	case  Key of
		C->	
			V;
		_->
			find_counter_name(C,T)
	end.

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
			lists:flatten(io_lib:format("~p=~p<br>",[Name,M#varbind.value])) ++ set_counter_val(T);
		{_,{ok,{Reason,_,_},_}}->
			THIS:set_attribute(Name,""),
			lists:flatten(io_lib:format("~p=~p<br>",[Name,Reason])) ++ set_counter_val(T);
		{_,{error,Reason}}->
			THIS:set_attribute(Name,""),
			lists:flatten(io_lib:format("~p=~p<br>",[Name,Reason])) ++ set_counter_val(T);
		{_,_}->
			THIS:set_attribute(Name,""),
			lists:flatten(io_lib:format("~p=~p<br>",[Name,"unknow return"])) ++ set_counter_val(T)
	end.

getCountersContent()->
	case THIS:get_property(counters) of
		{ok,{_,V}}->
			V;
		_->
			[]
	end.

getTemplateFile()->
	?TEMPLATE_FILE.

getDefaultCounters()->
	THIS:getSNMPCounters(THIS,"",false).

getAppServerTestOID()->
	[1,3,6,1,4,1,2725,1,1,4].

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
	case THIS:get_property(server) of
		{ok,{_,V}}->
			V;
		_->
			BASE:getHostname()
	end.

get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=status,title="status",configurable=false,state=true},
	#property{name=counters,title="Counters", description="Current selection of counters.",type=counters,editable=true,order=1,default=THIS:getDefaultCounters()},
	#property{name=host,title="Host Name", description="the IP address or host name of the CheckPoint Server to be monitored.",type=text,editable=true},
	#property{name=index,title="Index", description="the index of the SNMP object - for non-table object IDs, this is 0",type=numeric,editable=true,default=0},
	#property{name=community,title="Community", description="Community for the SNMP object",type=text,editable=true,default='public'},
	#property{name=timeout,title="Timeout", description="the total time, in seconds, to wait for a successful reply",type=numeric,editable=true,advance=true,order=10,default=5,baselinable=true}
	].
