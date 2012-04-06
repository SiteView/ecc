%% ---
%% network_bandwidth_monitor
%%
%%---
-module(network_bandwidth_monitor,[BASE]).
-extends(browsable_snmp_base).
-compile(export_all).
-export([new/0, getBrowseData/1, phy2colon/1, colon2phy/1, update/0, setDevTypeAttr/1, reInitializeDevice/0, setIfValue/4, getStateProperties/2, buildClassifer/2, getDevClassify/1, getIfDevTypeName/1, getClassifyByBrowse/1, getIfCounterName/2, oid2dots/1, inc_count_error/0, get_classifier/1, getScalarValues/2, verify/1, get_template_property/0]).

-include("monitor.hrl").
-include("monitor_template.hrl").
-include_lib("snmp/include/snmp_types.hrl").

%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for network_bandwidth monitor
new()->
	Base = browsable_snmp_base:new(),
	Base:set_attribute(pLastRunTime,0),
	{?MODULE,Base}.

%% @spec getBrowseData(Params)-> Obj
%% Params = [Tuple]
%% Obj = list()
%% @doc get counters of network_bandwidth_monitor's oids
getBrowseData(Params)->
    Session = THIS:obtainSession(Params),
    THIS:set_attribute(session, Session),
	Index = 
		case proplists:get_value(indexingMethod,Params) of
			undefined->
				0;
			Num->
				list_to_integer(Num)
		end,
	F = iftable:new(Session),
	Ret = F:getActiveInterfaces(1),

	SSSS = case Index of
		0->
			[{oid2dots(X#varbind.oid),X#varbind.value}||X<-Ret];
		1->
			F1 = fun(X)->
				N = lists:last(X#varbind.oid),
				Addr = case F:getPhysicalAddress(N) of
						error->
							"error";
						Val->
							THIS:phy2colon(Val#varbind.value)
					end,
				{Addr,X#varbind.value}
				end,
			lists:map(F1,Ret);
		2->
			[{integer_to_list(lists:last(X#varbind.oid)),X#varbind.value}||X<-Ret];
		3->
			F2 = fun(X)->
				N = lists:last(X#varbind.oid),
				Name = case F:getInterfaceName(N) of
						error->
							"error";
						Val->
							case Val#varbind.value of
								noSuchObject->
									"error";
								'NULL'->
									"error";
								V1->
									V1
							end 
								
					end,
				{Name,X#varbind.value}
				end,
			lists:map(F2,Ret);
		_->
			[{oid2dots(X#varbind.oid),X#varbind.value}||X<-Ret]
	end,
    io:format("~nSSSS:~p",[SSSS]),
    SSSS  .

%% @spec phy2colon(Vb)-> Obj
%% Obj = list()
%% @doc get physic address string
phy2colon([])->"NULL";
phy2colon([P|T])->
	case length(T) of
		0->
			integer_to_list(P);
		_->
			integer_to_list(P) ++ ":" ++ phy2colon(T)
	end.

%% @spec colon2phy(ColonStr)-> Obj
%% Obj = list()
%% @doc switch oid string to oid list
colon2phy("NULL")->[];
colon2phy(ColonStr)->
	string:tokens(ColonStr).

%% @spec update() -> Result
%% Result = term()
%% @doc update is the run function called by schedule to test the network bandwidth monitor info
update()->
	{ok,{pLastRunTime,LastRunTime}} = THIS:get_attribute(pLastRunTime),
    THIS:set_attribute(pLastRunTime, sv_datetime:now()),
    Sess = THIS:obtainSession(),
    case Sess:test_snmp() of
        true ->
            THIS:set_attribute(session, Sess),
            case THIS:get_property(deviceType) of
                {ok, {_, DType}} ->
                    if 
                        DType =/= "NO_DEVICE" ->
                            %%io:format("*********DType = ~p~n", [DType]),
                            reInitializeDevice();
                        true ->
                            THIS:set_attribute(device, [])
                
                    end;
                _->
                    []
            end,
    %%*********************************************************
    %%A piece of code to illustrate the above two problems, 1. Parametric module object, declared in a method, then the method will release the object after the end of all content, including the establishment of the ets table; (2) If the method before the end of this object is assigned to process all of the saved memory block, then this module will copy all the contents, if there ets table, then will not copy, ets table with the same name may be the reason, the original end of the method will still be released after¬
    %%*********************************************************
            F = iftable:new(Sess),
            IdxMethod = case THIS:get_property(indexingMethod) of
                            {ok,{_,V1}}->
                                V1;
                            _->
                                0
                        end,
            Ifs = case THIS:get_property(browse) of
                        {ok,{_,V2}}->
                            V2;
                        _->
                            []
                    end,
            THIS:set_attribute(countersInError,0),
            State_string = THIS:setIfValue(Ifs,IdxMethod,F,LastRunTime),
            THIS:set_attribute(?STATE_STRING,State_string),
			
            case THIS:get_attribute(device) of
                {ok,{_,Device}}->
                    if
                        Device =/= [] ->
                            %%io:format("*********Device = ~p~n", [Device]),
                            Ids = Device:refreshMetrics(),
                            THIS:set_attribute(devMetrics, Ids),
                            if
                                Ids =:= [] ->
                                    [];
                                true ->
                                    setDevTypeAttr(Ids)
                            end;
                        true ->
                            []
                    end;
                _->
                    []
            end;
        _ ->
            THIS:set_attribute(?NO_DATA,true),
			THIS:set_attribute(?CATEGORY,error),
			THIS:set_attribute(status,"error"),
			THIS:set_attribute(?STATE_STRING,"Application Server not available on host")
    end.

%% @spec setDevTypeAttr(Ids)-> Obj
%% Ids = list()
%% Obj = list()
%% @doc set attribute with device type key and value
setDevTypeAttr([]) ->
    [];
setDevTypeAttr([{Dis, Value}|T]) ->
    DisAtom = list_to_atom(Dis),
    THIS:set_attribute(DisAtom, Value),
    setDevTypeAttr(T);
setDevTypeAttr([_H|T]) ->
    setDevTypeAttr(T).

%% @spec reInitializeDevice()-> Obj
%% Obj = term
%% @doc onitialize device object newly
reInitializeDevice() ->
    case THIS:get_property(deviceType) of
        {ok, {_, DType}} ->
            ok;
        _->
            DType = []
    end,
    case THIS:get_attribute(session) of
		{ok,{_,Session}}->
			ok;
		_->
			Session = []
	end,
    case THIS:get_attribute(maxRTDataWindow) of
		{ok,{_,MaxRTDataWindow}}->
			ok;
		_->
			MaxRTDataWindow = []
	end,
    Device = network_bandwidth_config:getDeviceInstance(DType,Session,MaxRTDataWindow, THIS),
    THIS:set_attribute(device, Device).
    

%% @spec setIfValue(Ifs, Idx, F, LastRunTime)-> Obj
%% Ifs = list()
%% Idx = integer
%% F = object
%% Obj = list()
%% @doc get value of some oid(InDiscards,OutDiscards,InErrors,OutErrors,InOctets,InOctets,OutOctets,InUCastPackets,OutUCastPackets,Speed,OutQLen,OperStatus)
setIfValue([],_,_,_)->"";
setIfValue([If|T],Idx,F,LastRunTime)->
	New_LastRunTime = sv_datetime:now(),
	RunTime = round((New_LastRunTime-LastRunTime)/1000),
	Row = case Idx of
			'0'->
				F:getRowFromDescription(element(2,If));
			'1'->
				F:getRowFromPhysicalAddress(element(1,If));
			'2'->
				list_to_integer(element(1,If));
			'3'->
				F:getRowFromName(THIS:colon2phy(element(1,If)));
			_->
				F:getRowFromDescription(element(2,If))
		end,
	case F:getInDiscards(Row) of            %%Selected number of lost packets, even if no error occurred
		error->
			THIS:inc_count_error(),
			THIS:set_attribute(THIS:getIfCounterName(If,inDiscards),error);
		V1->
			THIS:set_attribute(THIS:getIfCounterName(If,inDiscards),V1#varbind.value)
	end,
    case F:getOutDiscards(Row) of            %%Selected number of lost packets, even if no error occurred
		error->
			THIS:inc_count_error(),
			THIS:set_attribute(THIS:getIfCounterName(If,outDiscards),error);
		V1s->
			THIS:set_attribute(THIS:getIfCounterName(If,outDiscards),V1s#varbind.value)
	end,
	case F:getInErrors(Row) of              %%The number of error packets (the upper layer protocol organization)
		error->
			THIS:inc_count_error(),
			THIS:set_attribute(THIS:getIfCounterName(If,inErrors),error);
		V2->
			THIS:set_attribute(THIS:getIfCounterName(If,inErrors),V2#varbind.value)
	end,
    case F:getOutErrors(Row) of              %%The number of error packets (the upper layer protocol organization)
		error->
			THIS:inc_count_error(),
			THIS:set_attribute(THIS:getIfCounterName(If,outErrors),error);
		V2s->
			THIS:set_attribute(THIS:getIfCounterName(If,outErrors),V2s#varbind.value)
	end,
	Inkbps = 
		case F:getInOctets(Row) of              %%Octet group number
			error->
				THIS:inc_count_error(),
				THIS:set_attribute(THIS:getIfCounterName(If,inOctets),error),
				"0.00";
			V3->
				Value3 = 
					case THIS:get_attribute(THIS:getIfCounterName(If,inOctets)) of
						{ok,{_,InOctets}} -> InOctets;
						_ -> 0
					end,
				THIS:set_attribute(THIS:getIfCounterName(If,inOctets),V3#varbind.value),
				case LastRunTime of
					0 ->
						"0.00";
					_ ->
						[Inbps2] = io_lib:format("~.2f",[(V3#varbind.value-Value3)/RunTime/1024/8]),
						Inbps2
				end
				
		end,
   Outkbps = 
	   case F:getOutOctets(Row) of              %%Octet group number
			error->
				THIS:inc_count_error(),
				THIS:set_attribute(THIS:getIfCounterName(If,outOctets),error),
				"0.00";
			V3s->
				Value3s = 
					case THIS:get_attribute(THIS:getIfCounterName(If,outOctets)) of
						{ok,{_,OutOctets}} -> OutOctets;
						_ -> 0
					end,
				THIS:set_attribute(THIS:getIfCounterName(If,outOctets),V3s#varbind.value),
				case LastRunTime of
					0 ->
						"0.00";
					_ ->
						[Outbps2] = io_lib:format("~.2f",[(V3s#varbind.value-Value3s)/RunTime/1024/8]),
						Outbps2
				end
		end,
	case F:getInUCastPackets(Row) of        %%Number of packets (not including broadcast and multicast address)
		error->
			THIS:inc_count_error(),
			THIS:set_attribute(THIS:getIfCounterName(If,inUCastPackets),error);
		V4->
			THIS:set_attribute(THIS:getIfCounterName(If,inUCastPackets),V4#varbind.value)
	end,
    case F:getOutUCastPackets(Row) of        %%Number of packets (not including broadcast and multicast address)
		error->
			THIS:inc_count_error(),
			THIS:set_attribute(THIS:getIfCounterName(If,outUCastPackets),error);
		V4s->
			THIS:set_attribute(THIS:getIfCounterName(If,outUCastPackets),V4s#varbind.value)
	end,
    case F:getSpeed(Row) of              %%Octet group number
		error->
			THIS:inc_count_error(),
			THIS:set_attribute(THIS:getIfCounterName(If,speed),error);
		V5->
			THIS:set_attribute(THIS:getIfCounterName(If,speed),V5#varbind.value)
	end,
    case F:getOutQLen(Row) of              %%Octet group number
		error->
			THIS:inc_count_error(),
			THIS:set_attribute(THIS:getIfCounterName(If,outQLen),error);
		V6->
			THIS:set_attribute(THIS:getIfCounterName(If,outQLen),V6#varbind.value)
	end,
	State = 
		case F:getOperStatus(Row) of            %%Current operating state of the device interface
			error->
				element(2,If)++":error";
			Ops->
				case Ops#varbind.value of
					1->
						element(2,If)++":up";
					_->
						element(2,If)++":down"
				end
		end,
	String = lists:flatten([State,",Inkbps=",Inkbps,",Outkbps=",Outkbps,"<br>"]),
	String++THIS:setIfValue(T,Idx,F,LastRunTime).

%% @spec getStateProperties(This,Params)-> Obj
%% This = object
%% Params = [Tuple]
%% Obj = list()
%% @doc do something when after update or edit
getStateProperties(This,Params)->
    buildClassifer(This, Params).

%% @spec buildClassifer(This, Params)-> Obj
%% This = object
%% Params = [Tuple]
%% Obj = list()
%% @doc build Classifer by runtime data
buildClassifer(This, _Params) ->
    Ifs = case THIS:get_property(browse) of
				{ok,{_,V2}}->
					V2;
				_->
					[]
	    end,
    Temp = This:get_template_property(),
	T = [X || X<-Temp,X#property.state=:=true],
    
    Cls =
    if 
        Ifs =/= [] ->
            getClassifyByBrowse(Ifs);
        Ifs =:= [] ->
            case get('sv_classfier') of
                undefined ->
                    [];
                Values ->
                    Values
            end;
        true ->
            []
    end,
    put('sv_classfier', Cls),
    
    %%Special type of device parameters
    DevIfs = case THIS:get_attribute(devMetrics) of
				{ok,{_,V3}}->
					V3;
				_->
					[]
	    end,
    DevCls =
    if 
        DevIfs =/= [] ->
            getDevClassify(DevIfs);
        DevIfs =:= [] ->
            case get('sv_sndev_classfier') of
                undefined ->
                    [];
                DevValues ->
                    DevValues
            end;
        true ->
            []
    end,
    put('sv_sndev_classfier', DevCls),
    %%io:format("*************sv_sndev_classfier: ~p~n", [DevCls]),
    %%The final structure threshold
    Classifier = T ++ Cls ++ DevCls,
    %%io:format("*************Classifier: ~p~n", [Classifier]),
    Classifier.


%%Device type has been specified threshold parameters
%% @spec getDevClassify(Ifs)-> Obj
%% Ifs = list()
%% Obj = list()
%% @doc build device type Classifer
getDevClassify([]) ->
    [];
getDevClassify([If|T]) ->
    [#property{name=THIS:getIfDevTypeName(If),title=atom_to_list(THIS:getIfDevTypeName(If)),type=text,state=true,configurable=false}
     ] ++
     getDevClassify(T).
     
%% @spec getIfDevTypeName(If)-> Obj
%% If = tuple()
%% Obj = atom()
%% @doc switch device type name to atom
getIfDevTypeName(If) ->
    list_to_atom(element(1,If)).

%%Interface parameters of the threshold to be
%% @spec getClassifyByBrowse(Ifs)-> Obj
%% Ifs = list()
%% Obj = list()
%% @doc get classfier by browse counters
getClassifyByBrowse([]) ->
    [];
getClassifyByBrowse([If|T]) ->
    
    [#property{name=THIS:getIfCounterName(If,inDiscards),title=atom_to_list(THIS:getIfCounterName(If,inDiscards)),type=text,state=true,configurable=false},
     #property{name=THIS:getIfCounterName(If,outDiscards),title=atom_to_list(THIS:getIfCounterName(If,outDiscards)),type=text,state=true,configurable=false},
     #property{name=THIS:getIfCounterName(If,inErrors),title=atom_to_list(THIS:getIfCounterName(If,inErrors)),type=text,state=true,configurable=false},
     #property{name=THIS:getIfCounterName(If,outErrors),title=atom_to_list(THIS:getIfCounterName(If,outErrors)),type=text,state=true,configurable=false},
     #property{name=THIS:getIfCounterName(If,inOctets),title=atom_to_list(THIS:getIfCounterName(If,inOctets)),type=text,state=true,configurable=false},
     #property{name=THIS:getIfCounterName(If,outOctets),title=atom_to_list(THIS:getIfCounterName(If,outOctets)),type=text,state=true,configurable=false},
     #property{name=THIS:getIfCounterName(If,inUCastPackets),title=atom_to_list(THIS:getIfCounterName(If,inUCastPackets)),type=text,state=true,configurable=false},
     #property{name=THIS:getIfCounterName(If,outUCastPackets),title=atom_to_list(THIS:getIfCounterName(If,outUCastPackets)),type=text,state=true,configurable=false},
     #property{name=THIS:getIfCounterName(If,speed),title=atom_to_list(THIS:getIfCounterName(If,speed)),type=text,state=true,configurable=false},
     #property{name=THIS:getIfCounterName(If,outQLen),title=atom_to_list(THIS:getIfCounterName(If,outQLen)),type=text,state=true,configurable=false}] ++
     getClassifyByBrowse(T).


%% @spec getIfCounterName(If,Measure)-> Obj
%% If = tuple()
%% Measure = atom()
%% @doc build browse counters string and switch to atom
getIfCounterName(If,Measure)->
	list_to_atom(element(2,If)++":"++ atom_to_list(Measure)). 

%% @spec oid2dots(Oids)-> Obj
%% Oids = list()
%% Obj = list()
%% @doc switch oid list to oid string
oid2dots([])->"";
oid2dots([H|T])->
	case T of
		[]->
			integer_to_list(H);
		_->
			integer_to_list(H) ++ "." ++ oid2dots(T)
	end.

%% @spec inc_count_error()-> Obj
%% Obj = term()
%% @doc count error
inc_count_error()->
	case THIS:get_attribute(countersInError) of
		{ok,{_,C}}->
			THIS:set_attribute(countersInError,C+1);
		_->
			THIS:set_attribute(countersInError,1)
	end.

%% @spec get_classifier(error) -> List
%% List = [Tuple]
%% Tule = {status, Logic, Value}
%% Logic = '!=' | '==' | '>' | '<' | 'contain'
%% Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error)->
	
	Cls = case THIS:get_property(error_classifier) of
				{ok,{error_classifier,Classifier}}->
					Classifier;
				_->
					[{countersInError,'>',0}]
			end,
	if 
		length(Cls) < 10 ->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end;
get_classifier(warning)->
	Cls =case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'>',0}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end;
get_classifier(good)->
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'==',0}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end.

%% @spec getScalarValues(Prop,Params)-> Result
%% Result = list()
%% @doc get version list, oid list, percentageBase list, scale list for ui's combobox
getScalarValues(Prop,Params)->
	case Prop of
		duplexState->
			[{"Full-Duplex","full-duplex"},{"Half-Duplex","half-duplex"}];
		deviceType->
            DevType =
            case network_bandwidth_config:getIDsandDisplayNames() of
                {ok, IdToDis} ->
                    IdToDis;
                _ ->
                    []
            end,
			[{"Do not monitor device-specific metrics","NO_DEVICE"}] ++ DevType;
            %%[{"Do not monitor device-specific metrics","NO_DEVICE"}];
            
		indexingMethod->
			[{"Indexed by ifTable Row Number","2"},{"Indexed by Interface Name","3"},{"Indexed by Physical Address","1"},{"Indexed by Description","0"}];
		_->
			BASE:getScalarValues(Prop,Params)
	end.

%% @spec verify(Params)-> Result
%% Params = [Tuple]
%% Result = list()
%% @doc verify data of ui, ex. host,percentageBase,oid,oidIndex,timeout,scale and son
verify(Params)->
	Errs=
	case proplists:get_value(maxRTDataWindow,Params) of
		""->
			[{maxRTDataWindow,"Real-Time Data Time Window missing"}];
		MaxRTDataWindow when not is_number(MaxRTDataWindow) orelse MaxRTDataWindow<1 orelse MaxRTDataWindow > 24 ->
			[{maxRTDataWindow,"The Real-Time Data Time Window must be a number between 1 and 24"}];
		_->
			[]
	end ++
	case proplists:get_value(maxRTDataVerticalAxis,Params) of
		""->
			[];
		MaxRTDataVerticalAxis when not is_number(MaxRTDataVerticalAxis) orelse MaxRTDataVerticalAxis < 0 ->
			[{maxRTDataVerticalAxis,"The Real-Time Data Vertical Axis must be a number greater than 0"}];
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
		true->
			{ok,""}
	end.

%% @spec get_template_property()-> Result
%% Result = [Record]
%% Record = property
%% @doc get monitor template of network_bandwidth_monitor monitor
get_template_property()->
	Tp = BASE:get_template_property(),
	[X||X<-Tp,X#property.name=/=mibfile]++
    %%buildClassifer(THIS) ++
	[
	#property{name=deviceType,title="Device Type", description="by specifying a device type, you will enable the Network Bandwidth monitor to watch certain device-specific metrics",type=scalar,editable=true,advance=true,order=1},
	#property{name=duplexState,title="Duplex or Half-Duplex", description="the duplex state to use when calculating percent bandwidth utilized for all selected interfaces on this device",type=scalar,editable=true,advance=true,order=3},
	#property{name=indexingMethod,title="Interface Index", description="specify how SiteView should keep track of the interfaces it is monitoring.  Some network devices do not persist the index used to identify a particular interface between reboots.  The interface description or physical address will usually remain the same, however.",type=scalar,editable=true,advance=true,order=4},
	#property{name=showRTTraffic,title="Show Bytes In/Out", description="display a graph for bytes in/out along with percent bandwidth utilized on the  Real-Time Metrics page",type=bool,editable=true,advance=true,order=5},
	#property{name=maxRTDataWindow,title="Real-Time Data Time Window", description="the number of hours for which real-time graph data should be stored",type=numeric,editable=true,advance=true,default=24,order=6},
	#property{name=maxRTDataVerticalAxis,title="Real-Time Data Vertical Axis", description="the maximum value on the vertical axis for real-time graphs (leave blank to have this automatically calculated)",type=numeric,editable=true,advance=true,order=7}
	].
	
getLogProperties(This)->
	Ifs = case THIS:get_property(browse) of
				{ok,{_,V2}}->
					V2;
				_->
					[]
	    end,
    Temp = This:get_template_property() ++ getClassifyByBrowse(Ifs),
	[X#property.name || X<-Temp,X#property.state=:=true].