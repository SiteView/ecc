%% ---
%% post
%%
%%---
-module(post,[BASE,Monitor,Rule]).
-extends(action).
-compile(export_all).

-include("monitor.hrl").
-include("monitor_template.hrl").
-include("alert.hrl").

-define(PRODUCT_NAME,"elecc").

-define(LOG_TYPE,"Post alert sent").
-define(TIMEOUT,5).

new(Monitor,Rule)->
	Obj = action:new(Monitor,Rule),
	% Obj:set_monitor(Monitor),
	% Obj:set_rule(Rule),
	% Obj:set_attribute(runType,2),
	{?MODULE,Obj,Monitor,Rule}.


execute()->
	Msg = ?PRODUCT_NAME ++ " Alert, " ++ 
	case Monitor:get_attribute(?CATEGORY) of 
		{ok,{_,Category}} -> 
			atom_to_list(Category);
		_->
			""
	end ++ "," ++
	case Monitor:get_property(?NAME) of
		{ok,{_,Name}}->
			Name;
		_->
			""
	end ++ "," ++
	case Monitor:get_attribute(?STATE_STRING) of
		{ok,{_,State}}->
			State;
		_->
			""
	end,
	{ok,{_,Params}} = Rule:get_property(action_param), 
	{ok,{_,Enabled}} = Rule:get_property(enabled),
	case Rule:get_property(disabled) of
		{ok,{_,true}}->
			{error,"disabled"};
		_->
			case THIS:check_enabled(Enabled) of
				false ->
					{error,"time_disabled"};
				_->
					sendPostAlert(Params,Msg)
			end
	end.

check_schedule(Shedule)->
	Day = calendar:day_of_the_week(date()),
	case lists:keysearch(Day,1,Shedule) of
		{value,{_,Flag,St,Et}}-> % "12:12","24:24"
			[HHS,MMS|_] = case string:tokens(St,":") of
								[]->
									["0","0"];
								[T1]->
									[T1,"0"];
								[T2,T3|_]->
									[T2,T3]
							end,
			[HHE,MME|_] = case string:tokens(Et,":") of
								[]->
									["24","60"];
								[T4]->
									[T4,"60"];
								[T5,T6|_]->
									[T5,T6]
							end,
			Sth = case string:to_integer(HHS) of
					{error,_}->
						0;
					{V1,_}->
						V1
				end,
			Stm = case string:to_integer(MMS) of
					{error,_}->
						0;
					{V2,_}->
						V2
				end,
			Seh = case string:to_integer(HHE) of
					{error,_}->
						24;
					{V3,_}->
						V3
				end,
			Sem = case string:to_integer(MME) of
					{error,_}->
						60;
					{V4,_}->
						V4
				end,
			Sts = sv_datetime:time({Sth,Stm,0}),
			Ste = sv_datetime:time({Seh,Sem,0}),
			Now = sv_datetime:time(time()),
			%io:format("from:~p to ~p~n",[Sth,Seh]),
			case Flag of
				"enable" ->
					if
						Now >= Sts andalso Now =< Ste ->
							true;
						true ->
							false
					end;
				_->
					if
						Now < Sts orelse Now > Ste ->
							true;
						true ->
							false
					end
			end;
		_->
			true
	end.

verify(Params)->
	{ok,""}.

sendPostAlert(Params,Content)->
    ContentMax = 50000,
    put(submitted,false),
    put(errorMsg,"unknown"),
    put(errStatus,""),
    put(postStatus,0),
    put(postDuration,0),
    put(postSize,0),
    put(postDataArray,[]),
    URL = Params#post_alert.url,
    Proxy = Params#post_alert.proxy,
    ChallengeResponse = Params#post_alert.challengeResponse,
    Username = Params#post_alert.username,
    Password = Params#post_alert.password,
    ProxyUserName = Params#post_alert.proxyUserName,
    ProxyPassword = Params#post_alert.proxyPassword,
    Temp = Params#post_alert.template,
	if
        length(URL)=:=0 ->
            put(errorMsg,"missing CGI URL");
        true ->
            Path = "templates.post" ++ "/" ++ Temp,
            case file:read_file(Path) of
                {error,_} ->
                    put(errorMsg,"missing template: "++Temp);
                {ok,Bin} ->
                    TemplateMessage = binary_to_list(Bin),
                    PostData = string:tokens(THIS:process_msg(TemplateMessage,Monitor),"\r\n"),
                    put(postDataArray,PostData),
                    put(errorMsg,"")
            end,
            ErrorMsg = get(errorMsg),
            if
                length(ErrorMsg)=:=0 ->
                    Match = "",
                    ErrorContent = "",
                    Timeout = 1000000,
                    OtherHeader = "",
                    Profile = httputils:createProfile(postalert),
                    M=url_monitor:new(),
                    M:set_attribute(profile,Profile),
                    SocketSession = socketsession:new(M),
                    SocketSession:initialize(M),
                    URLResults = M:checkURL(browsable_urlcontent,SocketSession,URL,Match,ErrorContent,Proxy,ProxyUserName,ProxyPassword,get(postDataArray),Username,Password,"","",ContentMax,OtherHeader,0,Timeout,null),
                    SocketSession:delete(),
                    %%io:format("url params is:~p,~p,~p~n",[URLResults#urlresults.status,URLResults#urlresults.totalDuration,URLResults#urlresults.totalBytes]),
                    put(postStatus,httputils:lookupStatus(URLResults#urlresults.status)),
                    put(postDuration,URLResults#urlresults.totalDuration),
                    put(postSize,URLResults#urlresults.totalBytes),
                    put(contentBuffer,URLResults#urlresults.contentBuffer),
                    Isgood = URLResults#urlresults.status == 200,
                    put(submitted,Isgood),
                    M:delete();
                true ->
                    ok
            end
    end,
    %%Reserved parameters as the parameters of error mail
    Detail = " alert-url: "++"Post"++"\n"++" alert-postData: "++post2string(get(postDataArray))++"\n"++" alert-replyStatus: "++get(postStatus)++"\n"++" alert-replySize: "++integer_to_list(length(get(postDataArray)))++"\n"++" alert-replyDuration: "++integer_to_list(get(postDuration))++"\n"++" alert-replyContent: "++get(contentBuffer)++"\n",
    Submitted = get(submitted),
    %%{post, Failed, Message, Monitor, Group, MonitorId, URL, PostData, Status, Size, Duration, Content}
    %%DISPALY alert parametes
    {ok,{_,MonitorName}} = Monitor:get_property(name),
    {ok,{_,MonitorID}} = Monitor:get_property(id),
    Msg = if
        (not Submitted) ->
            %%send error message to reference  default mail
            Mail = "There was a problem sending a SiteView post alert."++"\n\n"++get(errorMsg)++"\n\n"++Detail++"\n",
            % THIS:logAlert(?LOG_TYPE,URL,Content,get(postStatus),"fail"),
			THIS:logAlert(?LOG_TYPE,URL,get(postStatus),Content,"fail"),
            {post, true,get(postStatus)++", "++URL,MonitorName,MonitorID,URL,post2string(get(postDataArray)),get(postStatus),integer_to_list(length(get(postDataArray))),integer_to_list(get(postDuration)),get(contentBuffer)};
        true ->
            % THIS:logAlert(?LOG_TYPE,URL,Content,post2string(get(postDataArray)),"ok"),
			 THIS:logAlert(?LOG_TYPE,URL,post2string(get(postDataArray)),Content,"ok"),
            {post, false,get(postStatus)++", "++URL,MonitorName,MonitorID,URL,post2string(get(postDataArray)),get(postStatus),integer_to_list(length(get(postDataArray))),integer_to_list(get(postDuration)),get(contentBuffer)}
    end,
    {ok,Msg}.
    
post2string([])->"";
post2string([F|R])->
    F++"\r\n"++post2string(R).

getScalarValues(Prop,Params)->
	case Prop of
		template->
			[{filename:basename(X),filename:basename(X)}||X<-filelib:wildcard("templates.post/*")];
		_->
			BASE:getScalarValues(Prop,Params)
	end.

%%url,template,proxy,challengeResponse,username,password,proxyUserName,proxyPassword
get_template_property()->
	%BASE:get_template_property() ++ 
	[
	#property{name=url,title="URL",type=text,description="the URL of the CGI to receive the form submission (example: http://www.siteview.com/cgi/error_alert.pl)"},
	#property{name=template,title="Template",type=scalar,description="the template used to create the form submission."},
    #property{name=proxy,title="HTTP Proxy",type=text,description="proxy server to use including port (example: proxy.siteview.com:8080)"},
    #property{name=challengeResponse,title="NT Challenge Response",type=text,description="when selected, use NT Challenge Response authorization"},
    #property{name=username,title="Authorization User Name",type=text,description="optional user name if the Action URL requires authorization"},
    #property{name=password,title="Authorization Password",type=password,description="optional password if the Action URL requires authorization"},
    #property{name=proxyUserName,title="Proxy Server User Name",type=text,description="optional user name if the proxy server requires authorization"},
    #property{name=proxyPassword,title="Proxy Server Password",type=password,description="optional password if the proxy server requires authorization"}
	].