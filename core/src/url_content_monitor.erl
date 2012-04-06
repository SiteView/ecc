%% @author Cheng kaiyang <kaiyang.cheng@dragonflow.com>
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc URL Content Monitor.
%% 
%% Description: URL Content Monitor is a specialized variation of the URL Monitor that can match up to ten different values from the content of a specified URL. 
%% The matched values are displayed with the status of the monitor in the monitor group table
%% Using re module for regular expressions
-module(url_content_monitor,[BASE]).  
-extends(url_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

%% @spec new() -> ok
%% @doc Start the monitor instance.
new()->
	Base = url_monitor:new(),
	{?MODULE,Base}.

%% @spec verify(Params) -> ErrorList
%% where
%% Params = [{atom(),integer()|string()}]
%% ErrorList = [string()]
%% @doc check content match is missing.
verify(Params)->
	Errs =
	case proplists:get_value(contentMatch,Params) of
		""->
			[{contentMatch,"Match Content is missing."}];
		_->
            []
	end ++
	case BASE:verify(Params) of
		{error,E}->
			E;
		_->
			[]
	end,
	if
		length(Errs)>0 ->
			{error,Errs};
		true ->
			{ok,""}
	end.
    
checkURL(S,S1,S2,S3,S4,S5,Array,S6,S7,S8,Stringbuffer,L,S9,I,J,Stringbuffer1)->
    {ok,{_,Profile}} = THIS:get_attribute(profile),
    URLContext = urlcontext:new(null),
    URLContext:setEncodePostData("contentTypeUrlencoded"),
    HTTPRequestSettings = httprequestsettings:new(S,S6,S7,"",true,S3,S4,S5,[],3,0,0),
    HTTPRequestSettings:init(),
    M = url_monitor:new(),
    M:set_attribute(profile,Profile),
    %%checkURL(HTTPRequestSettings,URLContext,ContentMatch,ErrorContent,PostData,"",50000,OtherHeader,Timeout,URL,null)
    Result = try M:checkURL(HTTPRequestSettings,URLContext,S1,S2,Array,Stringbuffer,L,S9,J,Stringbuffer1,null) of
        URLResults ->
            URLResults
    catch
    error:X->X,
    io:format("error is:~p~n",[X]),
    #urlresults{status=-1000}
    after
    M:delete()
    end,
    Result.
    

%% @spec get_classifier(Flag) -> Threshold
%% where
%% Flag = (error|warning|good)
%% Threshold = [{Attribute,Opera,Value}]
%% Attribute = atom()
%% Opera = atom()
%% Value = (integer()|atom()|string())
%% @doc Set default threshold value.
get_classifier(error)->
	Cls = case THIS:get_property(error_classifier) of
				{ok,{error_classifier,Classifier}}->
					Classifier;
				_->
					[{status,'!=',200}]
			end,
	if 
		length(Cls) < 10 ->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,10-length(Cls)));
		true ->
			Cls
	end;
get_classifier(warning)->
	Cls =case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'!=',200}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,10-length(Cls)));
		true ->
			Cls
	end;
get_classifier(good)->
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'==',200}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,10-length(Cls)));
		true ->
			Cls
	end.

%% @spec get_template_property() -> PropertyList
%% where
%% PropertyList = [PropertyRecord]
%% PropertyRecord = record()
%% @doc Add properties to template property list.
get_template_property()->
	BASE:get_template_property() ++
	  [
		#property{name=valueLabels,title="Match Value Labels",type=text,description="Labels for the values matched on the content output, separated by a \",\"",order=2},
		#property{name=contentMatch,title="Match Content",type=text,description="match against content of URL, using a string or a regular expression or XML names.",order=3}

	  ].