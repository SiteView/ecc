-module(sound,[BASE,Monitor,Rule]).
-extends(action).
-compile(export_all).

-include("monitor.hrl").
-include("monitor_template.hrl").
-include("alert.hrl").

-define(PRODUCT_NAME,"elecc").
-define(TIMEOUT,5).
-define(LOG_TYPE,"Sound alert sent").

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
	{ok,{Ok,Id}}=Rule:get_property(id),
	case Rule:get_property(disabled) of
		{ok,{_,true}}->
			{error,"disabled"};
		_->
			case THIS:check_enabled(Enabled) of
				false ->
					{error,"time_disabled"};
				_->
					api_alert:update([{id,Id},{enabled,true}]),
					playSoundAlert(Params,Msg)
			end
	end.

verify(Params)->
	{ok,""}.
    
playSoundAlert(Params,Content)->
	io:format("playSoundAlert,~p,~p~n",[Params,Content]),
    S1 = Params#sound_alert.file,
    Path = "templates.sound" ++ "/" ++ S1,
    FullFileName = 
    case filelib:is_file(Path) of
        true ->
            Path;
        _ ->
            Path ++ ".wav"
    end,
    catch sdlagent:playaudio(FullFileName, 3000),
    % THIS:logAlert(?LOG_TYPE, "localhost",Content,"","ok"),
	THIS:logAlert(?LOG_TYPE, "localhost",Content,Content,"ok"),
    {ok,[{ok,Content, sound}]}.
    %%sdlagent:audio_server(true, FullFileName).

sendMail([],_,_,_,_,_,_,_,Msg)->{ok,Msg};
sendMail(["other"|T],Params,Title,Content,MailServer,MailUser,MailPasswd,FromAddress,Msg)->
	io:format("sendMail1,~p,~p~n",[Params,Content]),
	To = string:tokens(Params#mail_alert.other,","),
	F = fun(X)->
		case mail:smtp_mail_send(MailServer,25,MailUser,MailPasswd,FromAddress,X,Title,Content,"",?TIMEOUT) of
			ok->
				THIS:logAlert(?LOG_TYPE,Title,Content,"ok"),
				{ok,X,Content};
			{error,Result} when is_binary(Result)->
				THIS:logAlert(?LOG_TYPE,binary_to_list(Result),"","fail"),
				{error,X,binary_to_list(Result)};
			{error,Result}->
				THIS:logAlert(?LOG_TYPE,Result,"","fail"),
				{error,X,Result}
		end
	end,
	Ret = lists:map(F,To),
	sendMail(T,Params,Title,Content,MailServer,MailUser,MailPasswd,FromAddress,Msg ++ Ret);
sendMail([M|T],Params,Title,Content,MailServer,MailUser,MailPasswd,FromAddress,Msg)->
	io:format("sendMail2,~p,~p~n",[Params,Content]),
	Ret = 
	case preferences:get(additional_email_settings,list_to_atom(M)) of
		{ok,[{_,Am}|_]}->
			CheckShedule = THIS:check_schedule(Am#additional_email_settings.schedule),
			if 
				Am#additional_email_settings.disable =/= "true" andalso CheckShedule ->
					case mail:smtp_mail_send(MailServer,25,MailUser,MailPasswd,FromAddress,Am#additional_email_settings.email,Title,Content,"",?TIMEOUT) of
						ok->
							THIS:logAlert(?LOG_TYPE,Title,Content,"ok"),
							{ok,M,Content};
						{error,Result} when is_binary(Result)->
							THIS:logAlert(?LOG_TYPE,binary_to_list(Result),"","fail"),
							{error,M,binary_to_list(Result)};
						{error,Result} ->
							THIS:logAlert(?LOG_TYPE,Result,"","fail"),
							{error,M,Result}
					end;
				true ->
					{error,M,"disabled or schedule"}
			end;
		_->
			{error,M,"get additional_email_settings error"}
	end,
	sendMail(T,Params,Title,Content,MailServer,MailUser,MailPasswd,FromAddress,[Ret] ++ Msg).
    
getScalarValues(Prop,Params)->
	case Prop of
		file->
			[{filename:rootname(filename:basename(X)),filename:rootname(filename:basename(X))}||X<-filelib:wildcard("templates.sound/*")];
		_->
			BASE:getScalarValues(Prop,Params)
	end.

get_template_property()->
	%BASE:get_template_property() ++ 
	[
%%	#property{name=to,title="To",type=scalar,multiple = true,allowother = true,listSize=5,description="either choose one or more e-mail setting(s), or enter the e-mail address of the person to send the mail to, separate multiple addresses with commas (example: " ++ ?SUPPORT_MAIL ++ "," ++ ?SALES_MAIL ++ ")"},
%%	#property{name=template,title="Template",type=scalar,description="choose which template to use for formatting the contents of the message.  If you are sending mail to a pager, choose one of the \"short\" templates."}
	].