%%
%% application_base
%%
%%
-module(application_base,[BASE]).
-extends(atomic_monitor).
-compile(export_all).

-include("monitor_template.hrl").
-include("monitor.hrl").

-define(MAX_COUNTERS,10).
-define(APP_TEMPLATE_PATH,"templates.applications").

new()->
	Obj = atomic_monitor:new(),
	Obj:set_attribute(maxCounters,?MAX_COUNTERS),
	{?MODULE,Obj}.
	
getAvailableCounters(_)->[].

getHostname()->"".

getReturnURL()->"".

getCountersContent()->[].

getCountersProperty()->[].

setCountersContent(_)->ok.

buildThresholdsArray()->[].

getLabels()->[].

clearCounterCache()->
	ok.

getValues()->
	[].

getTemplateFile()->"".


getTemplatePath()->
	?APP_TEMPLATE_PATH.


increaseCounters(I)->
	THIS:set_attribute(maxCounters,I).

getDefaultCounters(This)->
	This:getTemplateContent(This:getTemplatePath(),This:getTemplateFile(),false).

getCountersParameter()->"".

getTemplateContent(Path,File,Flag)->
	case file:consult(filename:join(Path,File)) of
		{ok,Terms}->
		%%io:format("file:~p~n",[Terms]),
			case Flag of
				true->
					lists:filter(fun(X)->
						case is_record(X,counters) of
							true->
								true;
							_->
								case  startsWith(X,"//") of 
									true->
										true;
									_->
										false
								end 
						end
					end,Terms);
				_->
					lists:filter(fun(X)->case is_record(X,counters) of true-> X#counters.default;_->false end end,Terms)
			end;
		_->
			[]
	end.
	
startsWith(S,S1) ->
	case string:str(S,S1) of
		1->
			true;
		_->
			false
	end.

getMaxCounters()->
	case THIS:get_attribute(maxCounters) of
		{ok,{_,V}} when V < ?MAX_COUNTERS ->
			V;
		{ok,_}->
			?MAX_COUNTERS;
		_->
			0
	end.