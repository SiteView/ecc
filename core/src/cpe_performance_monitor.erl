-module(cpe_performance_monitor,[BASE]).
-extends(browsable_base).
-compile(export_all).
-include("tt.hrl").
-include("monitor.hrl").
-include("monitor_template.hrl").

new() ->
    Obj= browsable_base:new(),
	{?MODULE,Obj}.



update() ->
    {ok,{_,DeviceS}} = THIS:get_property(cpe),
    {ok,{_,Browse}} = THIS:get_property(browse),
    Id = make_id(DeviceS),
    DeviceN = dbcs_tr069:get_deviceById(Id),    
    DeviceIp = DeviceN#tr069_device.ip,
    AcsName = DeviceN#tr069_device.acsname,
    Manufacturer = DeviceN#tr069_device.manufacturer,
    OUI = DeviceN#tr069_device.oui,
    ProductClass = DeviceN#tr069_device.productclass, 
    SerialNumber = DeviceN#tr069_device.serialnumber,        
	Parameters = get_parameters(Browse,[]),
    case api_tr069:getParameterValue(Id,Parameters) of
    [] -> 
        State = set_parameter(Parameters,[]),     
        THIS:set_attribute(?CATEGORY,nodata),
        THIS:set_attribute(?STATE_STRING,State), 
	    THIS:set_attribute(?NO_DATA,true);
    ValueList ->
        %-record('cwmp:ParameterValueStruct', {anyAttribs, 'Name', 'Value'}).    
        State = set_parameter(Parameters,ValueList),
        THIS:set_attribute(?STATE_STRING,State)         
    end.
    
set_parameter(Parameters,ValueList) ->
    set_parameter_t(Parameters,length(Parameters),ValueList,"").
set_parameter_t(_P,0,_V,R) -> R;
set_parameter_t([A|B],Len,V,Res) ->
    case lists:keysearch(A,3,V) of
    {value,{_,_,_,Value}} ->
        THIS:set_attribute(A,Value),
        set_parameter_t(B,Len-1,V,A++"="++Value++"<br>"++Res);
    _ ->
        THIS:set_attribute(A,"n/a"),
        set_parameter_t(B,Len-1,V,A++"=n/a"++"<br>"++Res)      
    end.



get_parameters([{Para,_}|B],R) ->
    get_parameters(B,[Para|R]);
get_parameters([],Res) -> Res.    

get_template_property() ->
    BASE:get_template_property() ++ 
	[
        #property{name=cpe, title="CPE", type=cpe, order=1,description=""}  
	].

getBrowseData(Params) -> 
 	CPE = proplists:get_value(cpe,Params),
    case CPE of
    [] ->
        [];
    _ ->         
        Id = make_id(CPE),
        CpeDevice= dbcs_tr069:get_deviceById(Id),         
        %LocalCode = platform:getLocalCode(),
        Path =   "templates.applications/"++textutils:space2empty(textutils:replacechar(CpeDevice#tr069_device.profile)),
        %%io:format("Path   !!!!!!!!!!!!!!!!!~n~p~n", [Path]),
        %Path = "templates.applications/"++ "Device1.0[](baseline1),VoiceService1.0[1](voiceservice1,tgprofile1)",         
        case file:consult(Path) of
        {ok,Bin} ->
            Bin;    
        _ ->
            []
        end
    end.    
 

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
            Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,10)); 
get_classifier(warning)->
	Cls = case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[]
	end,
    Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,10)); 	
get_classifier(good)->
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[]
	end,
    Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,10)).    

	
	