%% @author Cheng kaiyang <kaiyang.cheng@dragonflow.com>
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc URLcontent Base.
%%
%% Description: Base module for Apache monitor,Iplanet monitor,Oracle9i monitor,
%% To provide base function
-module(urlcontent_base,[BASE]). 
-extends(application_base).
-compile(export_all).
-include("monitor_template.hrl").
-include("monitor.hrl").

-export([new/0,buildRegExp/0,defaultTitle/1,getActiveCounters/1,getCounters/2,getAvailableCounters/1,getURLContentCounters/3,get_template_property/0]).

%% @spec new() -> ok
%% @doc Start the monitor instance.
new()->
	Obj = application_base:new(),
	{?MODULE,Obj}.

%% @spec buildRegExp() -> string()
%% @doc build regular expression for each counters.
buildRegExp() ->[].

%% @spec defaultTitle(Params) -> Title
%% where
%% Params = [{key,Vale}]
%% Key = string()
%% Value = string()
%% Title = string()
%% @doc Give monitor a default title.
defaultTitle(Params)->
	URL = proplists:get_value(url,Params),
	if
		length(URL)>0->
			BASE:defaultTitle(Params) ++":" ++ URL;
		true ->
			BASE:defaultTitle(Params)
	end.

%% @spec getActiveCounters(Monitor) -> integer()
%% @doc account the number of active counters.
getActiveCounters(This)->
	Cnt = This:getCountersContent(),
	Max = This:getMaxCounters(),
	if
		length(Cnt) > Max ->
			Max;
		true ->
			length(Cnt)
	end.

%% @spec getCounters(Monitor,DefaultCounters) -> Counters
%% where
%% DefaultCounters = Counters
%% Counters = {Counternumber,Countervalue}
%% Counternumber = integer()
%% Countervalue = string()
%% @doc get default counters.
getCounters(This,S)->
	THIS:getURLContentCounters(This,S,false).

%% @spec getAvailableCounters(Monitor) -> Counters
%% Counters = {Counternumber,Countervalue}
%% Counternumber = integer()
%% Countervalue = string()
%% @doc get all counters from the template files.
getAvailableCounters(This)->
	THIS:getURLContentCounters(This,"",true).
	
%% @spec getURLContentCounters(Monitor,DefaultCounters,Index) -> Counters
%% where
%% DefaultCounters = Counters
%% Index = bool()
%% Counters = {Counternumber,Countervalue}
%% Counternumber = integer()
%% Countervalue = string()
%% @doc get counters depend on the paramters.
%%
%% Description: Get counters from appointed path files
%% The files contained the counters content and comments
%% counters content format is {counters,Counternumber,Countername,Flag}.
%% Countername = atom()
%% Flag = bool()
%% comments format is //++Desc
%% Desc = string()
getURLContentCounters(_,S,false) when length(S) > 0->
	S;
getURLContentCounters(This,_,Flag)->
	R = This:getTemplateContent(This:getTemplatePath(), This:getTemplateFile(),Flag),
	%%io:format("r:~p~n",[R]),
	for(R).
	
for([])->[];
for([F|R]) ->
	case is_record(F,counters) of
		true ->
			[{atom_to_list(F#counters.id),atom_to_list(F#counters.name)}]++for(R);
		_->
			case  startsWith(F,"//") of
				true ->
					[F]++for(R);
				_->
					for(R)
			end
	end.

startsWith(S,S1) ->
	case string:str(S,S1) of
		1->
			true;
		_->
			false
	end.

getURLContentData()->ok.

oid2string([])->"";
oid2string([I|T])->
	case T of
		[]->
			integer_to_list(I);
		_->
			integer_to_list(I) ++ "." ++ oid2string(T)
	end.

%% @spec get_template_property() -> PropertyList
%% where
%% PropertyList = [PropertyRecord]
%% PropertyRecord = record()
%% @doc Add properties to template property list.
get_template_property()->
	BASE:get_template_property() ++ 
	[
	 #property{name=countersInError,title="counters in error",type=numeric,state=true,configurable=false}
	].