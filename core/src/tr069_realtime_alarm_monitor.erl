-module(tr069_realtime_alarm_monitor,[BASE]).

-extends(atomic_monitor).
-compile(export_all).
-include("tt.hrl").
-include("monitor.hrl").
-include("monitor_template.hrl").

new() ->
    Obj = atomic_monitor:new(),
    Obj:set_attribute(alarm_type,""),
	{?MODULE,Obj}.
    
update() ->
    {ok,{_,Alarm}} = THIS:get_attribute(alarm_type),
    if Alarm == "" ->
        THIS:set_attribute(?STATE_STRING,"no alarm"), 
        THIS:set_attribute(value,1),
        THIS:set_attribute(?CATEGORY,?GOOD_CATEGORY); 
    true ->
        THIS:set_attribute(?CATEGORY,?ERROR_CATEGORY),
        THIS:set_attribute(value,-1),
        THIS:set_attribute(?STATE_STRING,Alarm),
        THIS:set_attribute(alarm_type,"")
    end. 
   
get_match() ->
    {ok,{_,Match}} = THIS:get_property(match_content),
    Match.
    
set(String,Num) ->
    THIS:set_attribute(value,Num),
    THIS:set_attribute(alarm_type,String). 
   
     
get_template_property() ->
    BASE:get_template_property() ++ 
	[
        %#property{name=user, title="User", type=user, order=1,description=""}, 
        #property{name=match_content,title="Match Content",type=text,order=1,description="optional Perl regular expression to match against the output of the device alarm."},	
        #property{name=value, title="Value", type=numeric, order=3,editable=false,configurable=false,state=true}         
	].     
 

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
					[{value,'<',0}]
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


defaultTitle(Params)->
	User= proplists:get_value(user,Params),
	if
		length(User)>0->
			BASE:defaultTitle(Params) ++":" ++ User;
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
	if length(Errs) >0 ->
	    {error,Errs};
    true ->
	    {ok,""}
	end.