-module(total_bytes_sent_monitor,[BASE]).
-extends(atomic_monitor).
-compile(export_all).
-include("tt.hrl").
-include("monitor.hrl").
-include("monitor_template.hrl").

new() ->
    Base = atomic_monitor:new(),
    Base:set_attribute(tr069monitor_time,0),
    Base:set_attribute(tr069monitor_value,0),
	{?MODULE,Base}.
    
update() ->
    {ok,{_,DeviceS}} = THIS:get_property(cpe),
    Id = make_id(DeviceS),
    Parameters = [
                    "InternetGatewayDevice.LANDevice.1.WLANConfiguration.1.TotalBytesSent"
                    ],
    case api_tr069:getParameterValue(Id,Parameters) of
    [] ->     
        THIS:set_attribute(?CATEGORY,nodata),
        THIS:set_attribute(?STATE_STRING,"No data"), 
	    THIS:set_attribute(?NO_DATA,true);
    ValueList -> 
        {ok,{_,LastTime}} = THIS:get_attribute(tr069monitor_time),
        {ok,{_,LastValue}} = THIS:get_attribute(tr069monitor_value),
        Now = sv_datetime:now(),
        Vluaes = list_to_integer(element(4,ValueList)),
        NowV = (Vluaes - LastValue)/(Now - LastTime),
        io:format("LastTime = ~p~n", [LastTime]),
        io:format("LastValue = ~p~n", [LastValue]),
        io:format("Now = ~p~n", [Now]),
        io:format("Vluaes = ~p~n", [Vluaes]),
        io:format("NowV = ~p~n", [NowV]),
        THIS:set_attribute(tr069monitor_time,Now),
        THIS:set_attribute(tr069monitor_value,Vluaes),
        THIS:set_attribute(averanger_value, NowV),
        THIS:set_attribute(?STATE_STRING,"AverageTotalBytesSent = "++ erlang:integer_to_list(NowV))         
    end.

   
get_template_property() ->
    BASE:get_template_property() ++ 
	[
        #property{name=cpe, title="CPE", type=cpe, order=1,description=""}  
	].

getStateProperties(This,Params) ->
   [
        #property{name=averanger_value,title="Average Total Bytes Sent",type=numeric,state=true,configurable=false}
    ].


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
			end;
get_classifier(warning)->
	Cls = case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[]
	end;
get_classifier(good)->
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[]
	end.