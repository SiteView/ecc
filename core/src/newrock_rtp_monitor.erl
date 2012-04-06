-module(newrock_rtp_monitor,[BASE]).
-extends(browsable_base).
-compile(export_all).
-include("tt.hrl").
-include("monitor.hrl").
-include("monitor_template.hrl").

new() ->
    Obj= browsable_base:new(),
    Obj:set_attribute(number,0),
	{?MODULE,Obj}.
    
update() ->
    {ok,{_,DeviceS}} = THIS:get_property(cpe),
    {ok,{_,Browse}} = THIS:get_property(browse),
    {ok,{_,Number}} = THIS:get_attribute(number), 
    Id = make_id(DeviceS),
    %% profile 
    CpeDevice= dbcs_tr069:get_deviceById(Id),
    io:format("CpeDevice = ~p~n", [CpeDevice]),
    
    %%
	ParametersT = get_parameters(Browse,[],CpeDevice),
    if length(ParametersT) == 0 ->
        Parameters = [
                    "Device.Services.VoiceService.1.VoiceProfile.1.Line.1.Stats.PacketsSent",
                    "Device.Services.VoiceService.1.VoiceProfile.1.Line.1.Stats.PacketsReceived",
                    "Device.Services.VoiceService.1.VoiceProfile.1.Line.1.Stats.BytesSent",
                    "Device.Services.VoiceService.1.VoiceProfile.1.Line.1.Stats.BytesReceived",
                    "Device.Services.VoiceService.1.VoiceProfile.1.Line.1.Stats.PacketsLost"];
    true ->
        Parameters = ParametersT   
    end, 
    io:format("Parameters = ~p~n", [Parameters]),
    case api_tr069:getParameterValue(Id,Parameters) of
    [] -> 
        State = set_parameter(Number,Browse,[]),     
        THIS:set_attribute(?CATEGORY,nodata),
        THIS:set_attribute(?STATE_STRING,State), 
	    THIS:set_attribute(?NO_DATA,true);
    ValueList ->             
        State = set_parameter(Number,Browse,ValueList),
        THIS:set_attribute(number,1), 
        THIS:set_attribute(?STATE_STRING,State)     
    end.

set_parameter(Number,Browse,ValueList) ->
    case [ element(4,X)||X<-ValueList,string:str(element(3,X),"PacketsSent")>0] of
    [] ->
        %%String1 = "Sent  Packets = n/a <br>", 
        %%THIS:set_attribute(packetssent,"n/a"); 
        String1=[];
    PacketsSentNumList ->
        case Number of
        0 ->
            String1 = "Sent  Packets = 0 <br>", 
            THIS:set_attribute(packetssent,0);               
        _ -> 
            String1 = "Sent  Packets = " ++ integer_to_list(add(PacketsSentNumList)) ++ "<br>", 
            THIS:set_attribute(packetssent,add(PacketsSentNumList))
        end            
    end,
    case [ element(4,X)||X<-ValueList,string:str(element(3,X),"PacketsReceived")>0] of
    [] ->
        %%String2 = "Received  Packets = n/a <br>", 
        %%THIS:set_attribute(packetsreceived,"n/a"); 
        String2=[];
    PacketsReceivedNumList ->
        case Number of 
        0 ->
            String2 = "Received  Packets = 0 <br>",   
            THIS:set_attribute(packetsreceived,0);        
        _ ->
            String2 = "Received  Packets = " ++  integer_to_list(add(PacketsReceivedNumList)) ++ "<br>",   
            THIS:set_attribute(packetsreceived,add(PacketsReceivedNumList))
        end            
    end,
    case [ element(4,X)||X<-ValueList,string:str(element(3,X),"BytesSent")>0] of
    [] ->
        %%String3 = "Sent  Bytes = n/a <br>",  
        %%THIS:set_attribute(bytessent,"n/a"); 
        String3=[];
    BytesSentNumList ->
        case Number of
        0 ->
            String3 = "Sent  Bytes = 0 <br>",   
            THIS:set_attribute(bytessent,0);
        _ -> 
            String3 = "Sent  Bytes = " ++  integer_to_list(add(BytesSentNumList)) ++ "<br>",   
            THIS:set_attribute(bytessent,add(BytesSentNumList))
        end            
    end,
    case [ element(4,X)||X<-ValueList,string:str(element(3,X),"BytesReceived")>0] of
    [] -> 
        %%String4 = "Received  Bytes = n/a <br>",   
        %%THIS:set_attribute(bytesreceived,"n/a"); 
        String4=[];
    BytesReceivedNumList ->
        case Number of
        0 ->
            String4 = "Received  Bytes = 0 <br>",   
            THIS:set_attribute(bytesreceived,0);
        _ ->  
            String4 = "Received  Bytes = " ++  integer_to_list(add(BytesReceivedNumList)) ++ "<br>",   
            THIS:set_attribute(bytesreceived,add(BytesReceivedNumList))
        end            
    end,
    case [ element(4,X)||X<-ValueList,string:str(element(3,X),"PacketsLost")>0] of
    [] ->
        %%String5 = "Lost  Packets = n/a <br>",   
        %%THIS:set_attribute(packetslost,"n/a"); 
        String5=[];
    PacketsLostNumList ->
        case Number of
        0 ->  
            String5 = "Lost  Packets = 0 <br>",   
            THIS:set_attribute(packetslost,0); 
        _ ->
            String5 = "Lost  Packets = " ++  integer_to_list(add(PacketsLostNumList)) ++ "<br>",   
            THIS:set_attribute(packetslost,add(PacketsLostNumList))
        end            
    end,    
    set_parameter_t(Browse,length(Browse),ValueList,String1 ++ String2 ++ String3 ++ String4 ++ String5).
set_parameter_t(_B,0,_V,R) -> R;
set_parameter_t([{Para,_}|B],Len,V,Res) ->
    Num = string:substr(Para,5,1),
    Str = string:substr(Para,6),  
    case [ element(4,X)||X<-V,string:str(element(3,X),Num++".Stats."++Str)>0] of
    [] ->
        THIS:set_attribute(Para,"n/a"); 
    [Value] ->
        THIS:set_attribute(Para,list_to_integer(Value)) 
    end,
    set_parameter_t(B,Len-1,V,Res). 
    
    
    %case lists:keysearch(A,3,V) of
    %{value,{_,_,_,Value}} ->
    %    THIS:set_attribute(A,Value),
    %    set_parameter_t(B,Len-1,V,A++"="++Value++"<br>"++Res);
    %_ ->
    %    THIS:set_attribute(A,"n/a"),
    %    set_parameter_t(B,Len-1,V,A++"=n/a"++"<br>"++Res)      
    %end.
    
add(List) ->
    add_t(List,length(List),0).
add_t(_L,0,Value) -> Value;
add_t([A|B],Len,V) ->
    add_t(B,Len-1,list_to_integer(A)+V). 
 
get_parameters([{Para,_}|B],R,CpeDevice) ->
    %%Num = string:substr(Para,5,1),
    Num = textutils:getNumFromStr(Para, 5),
    ParamBegin = 5 + string:len(Num),
    Index = string:str(Para,"ISDN"),
    if Index > 0 ->
        Content = string:substr(Para,ParamBegin),  
        ParasList = 
        case CpeDevice#tr069_device.profile of
                "Device:1.0[](baseline:1), VoiceService:1.0[1](voiceservice:1,tgprofile:1)" ->
                    ["Device.Services.VoiceService.1.VoiceProfile.1.Isdn."++Num++".Stats."++Content];
                "Device:1.0[](baseline.ini:1), VoiceService:1.0[1](voiceservice.ini:1,agprofile.ini:1)" ->
                    ["Device.Services.VoiceService.1.VoiceProfile.1.Line."++Num++".Stats."++Content]
        end;
        %ParasList = replace(List,Num);
    true ->
        ParasList = [] 
    end, 
    get_parameters(B,R++ ParasList,CpeDevice);   
get_parameters([],Res,CpeDevice) -> Res.    


replace(ParaList,Num) ->
    replace_t(ParaList,length(ParaList),Num,[]).  
replace_t(_P,0,_N,List) -> List;
replace_t([A|B],Len,Num,L) ->
    Index = string:str(A,"{"),
    H = string:substr(A,1,Index-1),
    E =  string:substr(A,Index+3),
    replace_t(B,Len-1,Num,[H++Num++E|L]). 
   
get_template_property() ->
    Paras = THIS:get_property(?PAGE_PARAMS),
    io:format("ParamsTemplate = ~p~n", [Paras]),
    Property =
    case Paras of
        {ok,{'_PAGE_PARAMS',Params}} ->
            CPE = proplists:get_value(cpe,Params),
            case CPE of
                [] ->
                    [];
                VCPE when erlang:is_list(VCPE) ->
                    Id = make_id(CPE),
                    io:format("Id:~p~n",[Id]),
                    CpeDevice= dbcs_tr069:get_deviceById(Id),
                    io:format("CpeDevice:~p~n",[CpeDevice]),
                    case CpeDevice#tr069_device.profile of 
                        "Device:1.0[](baseline:1), VoiceService:1.0[1](voiceservice:1,tgprofile:1)" ->
                            [];
                        "Device:1.0[](baseline.ini:1), VoiceService:1.0[1](voiceservice.ini:1,agprofile.ini:1)" ->
                            [#property{name=line,title="Choose Line",default="cn",description="the line of trunk or user",type=scalar,editable=true,order=2,allowother=false}];
                        _ ->
                            []
                    end;
                _ ->
                    []
            end;
        _ ->
            []
    end,
    io:format("Property = ~p~n", [Property]),
    BASE:get_template_property() ++ 
	[
        #property{name=cpe, title="CPE", type=cpe, order=1,description=""}
	] ++
    Property.

getStateProperties(This,Params) ->
   %%[
   %%     #property{name=packetssent,title="Sent Packets",type=numeric,state=true,configurable=false},
   %%     #property{name=packetsreceived,title="Received Packets",type=numeric,state=true,configurable=false},
   %%     #property{name=bytessent,title="Sent Bytes",type=numeric,state=true,configurable=false},
   %%     #property{name=bytesreceived,title="Received Bytes",type=numeric,state=true,configurable=false},
   %%     #property{name=packetslost,title="Lost Packets",type=numeric,state=true,configurable=false}
    %%].
   [].


defaultTitle(Params)->
	CPE= proplists:get_value(cpe,Params),
	if
		length(CPE)>0->
			BASE:defaultTitle(Params) ++":" ++ CPE;
		true ->
			BASE:defaultTitle(Params)
	end.


verify(Params) ->
    Errs = 
    case BASE:verify(Params) of
		{error,Be}->
			Be;
		_->
			[]
	end,   
	if length(Errs)>0 ->
	    {error,Errs};
    true ->
	    {ok,""}
	end.

getBrowseData(Params) -> 
 	CPE = proplists:get_value(cpe,Params),
    case CPE of
    [] ->
        [];
    _ ->         
        Id = make_id(CPE),
        CpeDevice= dbcs_tr069:get_deviceById(Id),
        case CpeDevice#tr069_device.profile of 
        "Device:1.0[](baseline:1), VoiceService:1.0[1](voiceservice:1,tgprofile:1)" ->
            case api_tr069:getParameterValue(Id,["Device.Services.VoiceService.1.Capabilities.MaxLineCount"]) of
            [] ->                
                [];
            [ValueList] ->
                Isdn = ValueList#'cwmp:ParameterValueStruct'.'Value',
                make_isdn(list_to_integer(Isdn))
            end;
        "Device:1.0[](baseline.ini:1), VoiceService:1.0[1](voiceservice.ini:1,agprofile.ini:1)" ->
            case proplists:get_value(line,Params) of
                [] ->
                    [];
                VLine when erlang:is_list(VLine) ->
                    VLine,
                    io:format("VLine = ~p~n", [VLine]),
                    Isdn = get_linenumber(VLine),
                    io:format("Isdn = ~p~n", [Isdn]),
                    ISDN = make_cur_line(erlang:list_to_integer(Isdn));
                _ ->
                    []
            end;
        _ ->
            [{"Line1PacketsSent","Line1PacketsSent"},{"Line1PacketsReceived","Line1PacketsReceived"},{"Line1BytesSent","Line1BytesSent"},{"Line1BytesReceived","Line1BytesReceived"},{"Line1PacketsLost","Line1PacketsLost"}]
        end
    end.    
    
%% @spec getScalarValues(Prop,Params)-> Result
%% Result = list()
%% @doc get version list, oid list, percentageBase list, scale list for ui's combobox
getScalarValues(Prop,Params)->
	case Prop of
        line ->
            io:format("begin~n"),
            CPE = proplists:get_value(cpe,Params),
            %%io:format("CPE = ~p~n", [CPE]),
            case CPE of
                [] ->
                    [];
                VCPE when erlang:is_list(VCPE) ->
                    Id = make_id(CPE),
                    io:format("Id:~p~n",[Id]),
                    CpeDevice= dbcs_tr069:get_deviceById(Id),
                    io:format("CpeDevice:~p~n",[CpeDevice]),
                    case CpeDevice#tr069_device.profile of 
                        "Device:1.0[](baseline:1), VoiceService:1.0[1](voiceservice:1,tgprofile:1)" ->
                            [];
                        "Device:1.0[](baseline.ini:1), VoiceService:1.0[1](voiceservice.ini:1,agprofile.ini:1)" ->
                            ValueList = [],
                            io:format("ValueList = ~p~n", [ValueList]),
                            Values = make_line(ValueList),
                            io:format("Values = ~p~n", [Values]),
                            Values;
                        _ ->
                            []
                    end
            end;
		_->
			BASE:getScalarValues(Prop,Params)
	end.
    
%% new ooooooooo

%% instanllation
make_cur_line(Isdn) ->
    [{"ISDN"++integer_to_list(Isdn)++"PacketsSent","PacketsSent"},
     {"ISDN"++integer_to_list(Isdn)++"PacketsReceived","PacketsReceived"},
     {"ISDN"++integer_to_list(Isdn)++"BytesSent","BytesSent"},
     {"ISDN"++integer_to_list(Isdn)++"BytesReceived","BytesReceived"},
     {"ISDN"++integer_to_list(Isdn)++"PacketsLost","PacketsLost"}].

%% create AG device line
make_line(ValueList) ->
    [{"All Stat","FXS-1/0/8000"}] ++
    make_line_t(ValueList, []).

make_line_t([], Result) ->
    Result;
make_line_t([ValueList=#'cwmp:ParameterValueStruct'{}|T], Result) ->
    LineId = ValueList#'cwmp:ParameterValueStruct'.'Value',
    LineIdList = getLineKeyValues(LineId),
    io:format("LineIdList = ~p~n", [LineIdList]),
    VResult = proc_line(LineIdList, Result),
    make_line_t(T, VResult);
make_line_t([H|T], Result) ->
    make_line_t(T, Result).

proc_line([], Result) ->
    Result;
proc_line([{D,H}|T], Result) ->
    VResult=lists:keystore(D, 1, Result, {D,H}),
    proc_line(T, VResult).

%% Value obtained from a line number line number
get_linenumber(H) ->
    Line = string:tokens(H, "/"),
    case string:len(Line) of
        3 ->
            lists:nth(2, Line);
        _ ->
            []
    end.
    
%% build drop-down content
getLineKeyValues(LineId) ->
    LineIdList = string:tokens(LineId, ","),
    exe_getLineKeyValues(LineIdList).
    
%% construct the line through the loop the contents of the drop-down box
exe_getLineKeyValues([]) ->
    [];
exe_getLineKeyValues([H|T]) ->
    Line = string:tokens(H, "/"),
    case string:len(Line) of
        3 ->
            [{lists:nth(1, Line),H}] ++
            exe_getLineKeyValues(T);
        _ ->
            [] ++ exe_getLineKeyValues(T)
    end.
    
    
%%oooooooooooooo
 
make_isdn(Isdn) ->
    make_isdn_t(Isdn,[]).
make_isdn_t(0,R) -> R;
make_isdn_t(I,Res) ->
    make_isdn_t(I-1,Res ++ [{"ISDN"++integer_to_list(I)++"PacketsSent","ISDN"++integer_to_list(I)++"PacketsSent"},{"ISDN"++integer_to_list(I)++"PacketsReceived","ISDN"++integer_to_list(I)++"PacketsReceived"},{"ISDN"++integer_to_list(I)++"BytesSent","ISDN"++integer_to_list(I)++"BytesSent"},{"ISDN"++integer_to_list(I)++"BytesReceived","ISDN"++integer_to_list(I)++"BytesReceived"},{"ISDN"++integer_to_list(I)++"PacketsLost","ISDN"++integer_to_list(I)++"PacketsLost"}]). 


make_id(String) ->
    List = string:tokens(String,"_"),  
    [Manufacturer] = lists:sublist(List,1,1),
    [Oui] = lists:sublist(List,2,1),
    [Serialnumber] = lists:sublist(List,4,1), 
    Manufacturer++"_"++Oui++"_"++Serialnumber.    

%Pvlaue is [{'cwmp:ParameterValueStruct', anyAttribs, 'Name', 'Value'},{'cwmp:ParameterValueStruct', anyAttribs, 'Name', 'Value'}]
parse_set_parameter_value(Pvalue) ->
    case is_list(Pvalue) of
	    false ->
		    THIS:set_attribute(?CATEGORY,nodata),
			THIS:set_attribute(?NO_DATA,true);
		true ->	
            parse_set_parameter_value_t(Pvalue,length(Pvalue))
	end.		
parse_set_parameter_value_t(Pv,0) ->true; 
parse_set_parameter_value_t(P,Num) ->
    [A|B] = P,
	THIS:set_attribute(A#'cwmp:ParameterValueStruct'.'Name',A#'cwmp:ParameterValueStruct'.'Value'),
	parse_set_parameter_value_t(B,Num-1).    
    
    
%% @spec get_classifier(Param) -> List
%% Param = atom()
%% List = [Tuple]
%% Tuple = {Status, Logic, Value}
%% Status = 'error'|'warning'| 'good' 
%% Logic = '!=' | '==' | '>' | '<' | 'contain'
%% Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error)->  
	Cls = case THIS:get_property(error_classifier) of
				{ok,{error_classifier,Classifier}}->
					Classifier;
				_->
					[]
			end,
            Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,5)); 
get_classifier(warning)->
	Cls = case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[]
	end,
    Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,5)); 	
get_classifier(good)->
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[]
	end,
    Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,5)).    