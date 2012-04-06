%% @author Cheng kaiyang <kaiyang.cheng@dragonflow.com>
%% @copyright 2010 dragonflow, Inc.
%% @version 1.0
%% @doc Api url sequence monitor.
-module(api_url_sequence_monitor).
-compile(export_all).
-include("monitor_template.hrl").
-include("monitor.hrl").

-export([get_step_response/1,process_html/1,get_links/1,get_forms/1,get_frames/1,get_refreshes/1]).

-define(MAXSTEP,20).
-define(STEPPROPERTIES,["referenceType","reference","contentMatch","errorContent","postData","userName","password","domain","whenToAuthenticate","stepDelay","stepName","encoding","encodePostData"]).
-define(REFERENCETYPE,["url","link","form","frame","refresh"]).

%% @spec get_step_response(MonitorData) -> [{ok,Result}|{error,Reason}]
%% where
%%	MonitorData = [{Key,Value}]
%%	Key = string()
%%	Value = string()
%%	Result = [{atom(),term()}]
%%	Reason = string()
%% @doc Request for last step, and parse response.
%% Page in the values ​​of all attributes,

get_step_response(Params) ->
    case url_sequence_client:request(Params) of
        {ok,Result} ->
            try
            Body = proplists:get_value(body,Result),
            References = process_html(Body),
            {ok,Result++References}
            catch
                _:_ -> {error,"parse response body error"}
            end;
        Error ->
            Error
    end.
            
%% @spec process_html(HTML) -> [{Key,Value}]
%% where
%%	HTML = string()
%%	Key = [link|frame|refresh|formList|formInput]
%%	Value = [{Content,Title}]
%%	Content = string()
%%	Title = string()
%% @doc Parse HTML.
process_html(HTML) ->
    url_sequence_client:process_html(HTML).
    
add_monitor(Id,Params,Classifier) ->
    url_sequence_client:add_monitor(Id,Params,Classifier).
    
get_classifier() ->
    url_sequence_client:get_classifier().

get_links(Tree) ->
    url_sequence_client:getLinks(Tree).
    
get_forms(Tree) ->
    url_sequence_client:getForms(Tree).
    
get_frames(Tree) ->
    url_sequence_client:getFrames(Tree).
    
get_refreshes(Tree) ->
    url_sequence_client:getRefresh(Tree).