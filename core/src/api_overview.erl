%% @author Cheng kaiyang <kaiyang.cheng@dragonflow.com>
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc api of Multi-View.
-module(api_overview).
-compile(export_all).
-extends(api_siteview).
-include("monitor.hrl").

-export([get_info/1,groupinfo/0]).

%% @spec get_info(Params) -> [{ok,Content}|{error,Reason}]
%% where
%% Params = [{Key,Value}]
%% Key = [url|proxy|proxyusername|proxypassword|username|password|timeout]
%% Value = string()
%% Content = string()
%% Reason = string()
%% @doc For multi-view page,URL format is http://host:port/web/groupinfo, groupinfo page contain the information of groups and monitors.
get_info(Params) ->
    UrlString = proplists:get_value(url,Params),
    Proxy = proplists:get_value(proxy,Params,""),
    ProxyUserName = proplists:get_value(proxyusername,Params,""),
    ProxyPassword = proplists:get_value(proxypassword,Params,""),
    UserName = proplists:get_value(username,Params,""),
    Password = proplists:get_value(password,Params,""),
    TimeoutString = proplists:get_value(timeout,Params),
    Timeout = if
        TimeoutString==undefined ->
            60000;
        true ->
            list_to_integer(TimeoutString)*1000
    end,
    if
        UrlString==undefined ->
            {error,"missing url"};
        true ->
            Profile = httputils:createProfile(multiview),
            M=url_monitor:new(),
            M:set_attribute(profile,Profile),
            SocketSession = socketsession:new(M),
            SocketSession:initialize(M),
            URLResults = M:checkURL(browsable_urlcontent,SocketSession,UrlString,"","",Proxy,ProxyUserName,ProxyPassword,[],UserName,Password,"","",500000,"",0,Timeout,null),
            SocketSession:delete(),
            M:delete(),
            case URLResults#urlresults.status of
                200 ->
                    {ok,URLResults#urlresults.body};
                _ ->
                    {error,"http connect error"}
            end
    end.
    
%% @spec groupinfo() -> Info
%% where
%% Info = string()
%% @doc For groupinfo page, groupinfo page should only contain the information of the result created by this function,
%% groupinfo page must be create before mulit-view page.
groupinfo() ->
    local_group_info:info().
    
%%info Content Description
%%In order for the group, the group monitors and so on the following
%%group£ºsiteview:id,  id:groupid,  title:groupname,  category:groupstatus,  statestring:group description,  level:groupLevel£¬basicgroup:0£¬And so on
%%monitor£ºgid:Belongs to group id,  id:monitorid,  title:monitorname,  category:monitor status,  statestring:monitor description
%%<!--CURRENTSTATE ++ Content ++ ENDCURRENTSTATE--> Identified as an information block