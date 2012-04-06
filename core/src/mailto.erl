%% ---
%% mailto
%%
%%---
-module(mailto,[BASE,Monitor,Rule]).
-extends(action).
-compile(export_all).

-include("monitor.hrl").
-include("monitor_template.hrl").
-include("alert.hrl").
-include("ecc_oem.hrl").


-define(SUPPORT_MAIL,?OEM("support@siteview.com")).
-define(SALES_MAIL,?OEM("sales@siteview.com")).
-define(PRODUCT_NAME,?OEM("SiteView")).

-define(LOG_TYPE,"Email alert sent").
-define(TIMEOUT,5).

new(Monitor,Rule)->
	Obj = action:new(Monitor,Rule),
	{?MODULE,Obj,Monitor,Rule}.

get_monitor()->{ok,{monitor,Monitor}}.

get_rule()->{ok,{rule,Rule}}.

execute()->
	io:format("Start~~~~~~~~~~~~~~~~"),
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
	end,
	%~ mail title without state string
	%~ ++ "," ++
	%~ case Monitor:get_attribute(?STATE_STRING) of
		%~ {ok,{_,State}}->
			%~ State;
		%~ _->
			%~ ""
	%~ end,
	{ok,{_,Params}} = Rule:get_property(action_param), 
	{ok,{_,Enabled}} = Rule:get_property(enabled),
	{ok,{Ok,Id}}=Rule:get_property(id),
	
	
	io:format("alert rule mail to test"),
	
	case Rule:get_property(disabled) of
		{ok,{_,true}}->
			{error,"disabled"};
		_->
			case THIS:check_enabled(Enabled) of
				false ->
					{error,"time_disabled"};
				_->
					api_alert:update([{id,Id},{enabled,true}]),
					sendMailAlert(Params,Msg)
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
	

sendMailAlert(Params,Content)->
	% {ok,[{_,MailServer}]} = preferences:get(email_settings,mailServer),
	% {ok,[{_,FromAddress}]} = preferences:get(email_settings,fromAddress),
	% {ok,[{_,MailUser}]} = preferences:get(email_settings,mailUser),
	% {ok,[{_,MailPasswd}]} = preferences:get(email_settings,mailPassword),
    %{ok,[{_,MailEncode}]} = preferences:get(email_settings,mailEncode),
	% {ok,[{_,MailServerBackup}]} = preferences:get(email_settings,mailServerBackup),
	MailServer = case preferences:get(email_settings,mailServer) of
				{ok,[{_,V1}]}->
					V1;
				_->
					""
				end,
	FromAddress =  case preferences:get(email_settings,mailUser) of
				{ok,[{_,V2}]}->
					V2;
				_->
					""
				end,
				
	MailUser = case preferences:get(email_settings,mailUser)  of
				{ok,[{_,V3}]}->
					V3;
				_->
					""
				end,
				
	MailPasswd = case preferences:get(email_settings,mailPassword) of
				{ok,[{_,V4}]}->
					V4;
				_->
					""
				end,
				
	MailServerBackup = case preferences:get(email_settings,mailServerBackup) of
				{ok,[{_,V6}]}->
					V6;
				_->
					""
				end,
	
	Msg = THIS:createMessage("templates.mail",Params#mail_alert.template),
    %使用用户配置的收邮件客户端编码方式
    % Msg = iconv:convert(httputils:pageEncode(), MailEncode,RawMsg),
    % Content = iconv:convert(httputils:pageEncode(), MailEncode,RawContent),
	{Title,Cnt} = THIS:split_msg(Msg,Content),
	%%Title = "siteview",
	%%Cnt = Msg,
	% TitleP = iconv:convert(httputils:pageEncode(), MailEncode,Title),
	% CntP = iconv:convert(httputils:pageEncode(), MailEncode,Cnt),
	case THIS:check_server([MailServer,MailServerBackup],"") of
		{ok,Server1}->
            DutyEmail = api_dutytable:get_duty_info(Params#mail_alert.duty,email),
            io:format("~n~n DutyEmail:~p~n~n",[DutyEmail]),
            sendforDuty(DutyEmail,Title,Cnt,Server1,MailUser,MailPasswd,FromAddress),
			sendMail(Params#mail_alert.sendto,Params,Title,Cnt,Server1,MailUser,MailPasswd,FromAddress,"");
		{error,_}->
			% io:format("----can't connect to smtp server----~n"),
			THIS:logAlert(?LOG_TYPE,"can't connect to smtp server","","fail")
	end.
	
check_server([],Err)->{error,Err};
check_server([""|T],Err)->
	THIS:check_server(T,"smtp server is empty");
check_server([S|T],Err)->
    Ret = gen_tcp:connect(S, 25, [binary, {packet, 0}]),
    case Ret of
        {ok, Socket}->
            gen_tcp:close(Socket),
			{ok,S};
		_->
			THIS:check_server(T,"can't connect to smtp server " ++ S)
	end.
	
split_msg(Msg,DefaultTitle)->
	case string:str(Msg,"[Subject:") of
		0->
			{DefaultTitle,Msg};
		St->
			case string:str(Msg,"]") of
				0->
					{DefaultTitle,Msg};
				En->
					if
						En < St->
							{DefaultTitle,Msg};
						true ->
							{string:substr(Msg,St + length("[Subject:"), En - St - length("[Subject:")),
							 string:substr(Msg,1,St-1) ++ string:substr(Msg,En+1,length(Msg) - En)}
					end
			end
	end.
    
sendforDuty([],_,_,_,_,_,_)->{ok};
sendforDuty([M|T],Title,Content,MailServer,MailUser,MailPasswd,FromAddress)->
	io:format("sendforDuty:~p~n",[Content]),
	MailEncode = case preferences:get(email_settings,mailEncode) of
				{ok,[{_,V5}]}->
					V5;
				_->
					"utf-8"
				end,
	
	TitleP = iconv:convert(httputils:pageEncode(), MailEncode,Title),
	CntP = iconv:convert(httputils:pageEncode(), MailEncode,Content),
	io:format("CntP1:~p~n",[CntP]),
    case mail:smtp_mail_send(MailServer,25,MailUser,MailPasswd,FromAddress,M,TitleP,CntP,"",?TIMEOUT) of
        ok->
            % THIS:logAlert(?LOG_TYPE,M,Title,Content,"ok"),
			THIS:logAlert(?LOG_TYPE,M,Title,CntP,"ok"),
            io:format("~nok,~p,~p,~p",[M,Title,Content]),
            {ok,M,Content};
        {error,Result} when is_binary(Result)->
            % THIS:logAlert(?LOG_TYPE,M,binary_to_list(Result),"","fail"),
			THIS:logAlert(?LOG_TYPE,M,binary_to_list(Result),CntP,"fail"),
            io:format("~nok,~p,~p",[M,binary_to_list(Result)]),
            {error,M,binary_to_list(Result)};
        {error,Result}->
            % THIS:logAlert(?LOG_TYPE,M,Result,"","fail"),
			THIS:logAlert(?LOG_TYPE,M,Result,CntP,"fail"),
            io:format("~nok,~p,~p",[M,Result]),
            {error,M,Result}
    end,
    sendforDuty(T,Title,Content,MailServer,MailUser,MailPasswd,FromAddress).   
    
sendMail([],_,_,_,_,_,_,_,Msg)->{ok,Msg};
sendMail(["other"|T],Params,Title,Content,MailServer,MailUser,MailPasswd,FromAddress,Msg)->
    io:format("sendMail2:~p~n",[Params]),
	MailEncode = case preferences:get(email_settings,mailEncode) of
				{ok,[{_,V5}]}->
					V5;
				_->
					"utf-8"
				end,
	TitleP = iconv:convert(httputils:pageEncode(), MailEncode,Title),
	CntP = iconv:convert(httputils:pageEncode(), MailEncode,Content),
	To = string:tokens(Params#mail_alert.other,","),
	F = fun(X)->
		io:format("sendMail,~p,~p,~p,~p~n",[Content,MailUser,Title,X]),
		io:format("CntP2:~p~n",[CntP]),
		case mail:smtp_mail_send(MailServer,25,MailUser,MailPasswd,FromAddress,X,TitleP,CntP,"",?TIMEOUT) of
			ok->
				% THIS:logAlert(?LOG_TYPE,X,Title,Content,"ok"),
				THIS:logAlert(?LOG_TYPE,X,Title,CntP,"ok"),
				io:format("Datasucess,~p,~p,~p,~p~n",[Content,MailUser,Title,X]),
				{ok,X,Content};
			{error,Result} when is_binary(Result)->
			    io:format("Datafailure,~p,~p,~p,~p~n",[Content,MailUser,Title,X]),
				% THIS:logAlert(?LOG_TYPE,X,binary_to_list(Result),"","fail"),
				THIS:logAlert(?LOG_TYPE,X,binary_to_list(Result),CntP,"fail"),
				{error,X,binary_to_list(Result)};
			{error,Result}->
				io:format("Datafailure,~p,~p,~p,~p~n",[Content,MailUser,Title,X]),
				% THIS:logAlert(?LOG_TYPE,X,Result,"","fail"),
				THIS:logAlert(?LOG_TYPE,X,Result,CntP,"fail"),
				{error,X,Result}
		end
	end,
	Ret = lists:map(F,To),
	sendMail(T,Params,Title,Content,MailServer,MailUser,MailPasswd,FromAddress,Msg ++ Ret);
sendMail([M|T],Params,Title,Content,MailServer,MailUser,MailPasswd,FromAddress,Msg)->
	MailEncode = case preferences:get(email_settings,mailEncode) of
				{ok,[{_,V5}]}->
					V5;
				_->
					"utf-8"
				end,
	Ret = 
	case preferences:get(additional_email_settings,list_to_atom(M)) of
		{ok,[{_,Am}|_]}->
			CheckShedule = THIS:check_schedule(Am#additional_email_settings.schedule),
			if 
				Am#additional_email_settings.disable =/= "true" andalso CheckShedule ->
					{NewTitle,NewContent} = case Am#additional_email_settings.template of
						"use_alert"->
							{Title,Content};
						Temp->
							TempMsg = THIS:createMessage("templates.mail",Temp),
							THIS:split_msg(TempMsg,Title)
					end,
					NewTitleP = iconv:convert(httputils:pageEncode(), MailEncode,NewTitle),
					NewCntP = iconv:convert(httputils:pageEncode(), MailEncode,NewContent),
					io:format("CntP3:~p~n",[NewCntP]),
					case mail:smtp_mail_send(MailServer,25,MailUser,MailPasswd,FromAddress,Am#additional_email_settings.email,NewTitleP,NewCntP,"",?TIMEOUT) of
						ok->
							% THIS:logAlert(?LOG_TYPE,Am#additional_email_settings.email,NewTitle,NewContent,"ok"),
							THIS:logAlert(?LOG_TYPE,Am#additional_email_settings.email,NewTitle,NewCntP,"ok"),
							{ok,M,Content};
						{error,Result} when is_binary(Result)->
							% THIS:logAlert(?LOG_TYPE,Am#additional_email_settings.email,binary_to_list(Result),"","fail"),
							THIS:logAlert(?LOG_TYPE,Am#additional_email_settings.email,binary_to_list(Result),NewCntP,"fail"),
							{error,M,binary_to_list(Result)};
						{error,Result} ->
							% THIS:logAlert(?LOG_TYPE,Am#additional_email_settings.email,Result,"","fail"),
							THIS:logAlert(?LOG_TYPE,Am#additional_email_settings.email,Result,NewCntP,"fail"),
							{error,M,Result}
					end;
				true ->
					{error,M,"disabled or schedule"}
			end;
		_->       
			
            case re:run(M,"\\w+([-+.]\\w+)*@\w+([-.]\\w+)*\\.\\w+([-.]\\w+)*") of
            {match, Captured}->
				TitleP = iconv:convert(httputils:pageEncode(), MailEncode,Title),
				CntP = iconv:convert(httputils:pageEncode(), MailEncode,Content),
				io:format("CntP4:~p~n",[CntP]),
                case mail:smtp_mail_send(MailServer,25,MailUser,MailPasswd,FromAddress,M,TitleP,CntP,"",?TIMEOUT) of
                    ok->
                        % THIS:logAlert(?LOG_TYPE,M,Title,Content,"ok"),
						THIS:logAlert(?LOG_TYPE,M,Title,CntP,"ok"),
                        io:format("~nok,~p,~p,~p",[M,Title,Content]),
                        {ok,M,Content};
                    {error,Result} when is_binary(Result)->
                        % THIS:logAlert(?LOG_TYPE,M,binary_to_list(Result),"","fail"),
						THIS:logAlert(?LOG_TYPE,M,binary_to_list(Result),CntP,"fail"),
                        io:format("~nok,~p,~p",[M,binary_to_list(Result)]),
                        {error,M,binary_to_list(Result)};
                    {error,Result}->
                        % THIS:logAlert(?LOG_TYPE,M,Result,"","fail"),
						THIS:logAlert(?LOG_TYPE,M,Result,CntP,"fail"),
                        io:format("~nok,~p,~p",[M,Result]),
                        {error,M,Result}
                end;
            _->
			{error,M,"get additional_email_settings error"}
            end
	end,
	sendMail(T,Params,Title,Content,MailServer,MailUser,MailPasswd,FromAddress,[Ret] ++ Msg).

get_addtional_mail([])->[];
get_addtional_mail([N|T])->
	case preferences:get(additional_email_settings,list_to_atom(N)) of
		{ok,[]}->
			get_addtional_mail(T);
		{ok,Rm}->
			[Y||{_,Y}<-Rm] ++ get_addtional_mail(T);
		_->
			get_addtional_mail(T)
	end.

getScalarValues(Prop,Params)->
	case Prop of
		to->
			case preferences:all(additional_email_settings) of
				{ok,Emails}->
					[{Y#additional_email_settings.name,atom_to_list(X)}||{X,Y}<-Emails]++[{"Other","other"}];
				_->
					[]
			end;
		template->
			[{filename:basename(X),filename:basename(X)}||X<-filelib:wildcard("templates.mail/*"),filelib:is_file(X),not filelib:is_dir(X)];
		_->
			BASE:getScalarValues(Prop,Params)
	end.
	
get_template_file_list()->
	[filename:basename(X)||X<-filelib:wildcard("templates.mail/*"),filelib:is_file(X),not filelib:is_dir(X)].
	
	
read_template_file(Name)->
	case file:read_file("templates.mail/" ++ Name) of
		{ok,Bin}->
			{ok,binary_to_list(Bin)};
		Else->
			Else
	end.

write_template_file(Name,Data)when is_binary(Data)->
	case file:write_file("templates.mail/" ++ Name,Data) of
		ok->
			{ok,Name};
		Else->
			Else
	end;
write_template_file(Name,Data)->
	case file:write_file("templates.mail/" ++ Name,list_to_binary(Data)) of
		ok->
			{ok,Name};
		Else->
			Else
	end.

remove_template_file(Name)->
	case file:delete("templates.mail/" ++ Name) of
		ok->
			{ok,Name};
		Else->
			Else
	end.

recover(This)->
	{ok,{_,Params}} = Rule:get_property(action_param), 
	% {ok,[{_,MailServer}]} = preferences:get(email_settings,mailServer),
	% {ok,[{_,FromAddress}]} = preferences:get(email_settings,fromAddress),
	% {ok,[{_,MailUser}]} = preferences:get(email_settings,mailUser),
	% {ok,[{_,MailPasswd}]} = preferences:get(email_settings,mailPassword),
    %{ok,[{_,MailEncode}]} = preferences:get(email_settings,mailEncode),
	%{ok,[{_,MailServerBackup}]} = preferences:get(email_settings,mailServerBackup),
	
	MailServer = case preferences:get(email_settings,mailServer) of
				{ok,[{_,V1}]}->
					V1;
				_->
					""
				end,
	FromAddress =  case preferences:get(email_settings,fromAddress) of
				{ok,[{_,V2}]}->
					V2;
				_->
					""
				end,
				
	MailUser = case preferences:get(email_settings,mailUser)  of
				{ok,[{_,V3}]}->
					V3;
				_->
					""
				end,
				
	MailPasswd = case preferences:get(email_settings,mailPassword) of
				{ok,[{_,V4}]}->
					V4;
				_->
					""
				end,
				
	MailServerBackup = case preferences:get(email_settings,mailServerBackup) of
				{ok,[{_,V6}]}->
					V6;
				_->
					""
				end,	
	
    Cnt = THIS:createMessage("templates.mail","Recover"),
	Title = ?PRODUCT_NAME ++ " Alert,recover " ++
	case Monitor:get_property(?NAME) of
		{ok,{_,Name}}->
			Name;
		_->
			"Unkown"
	end,
    % TitleP = iconv:convert(httputils:pageEncode(), MailEncode,Title),
	% CntP = iconv:convert(httputils:pageEncode(), MailEncode,Cnt),
	case THIS:check_server([MailServer,MailServerBackup],"") of
		{ok,Server1}->
            DutyEmail = api_dutytable:get_duty_info(Params#mail_alert.duty,email),
            sendforDuty(DutyEmail,Title,Cnt,Server1,MailUser,MailPasswd,FromAddress),
			sendMail(Params#mail_alert.sendto,Params,Title,Cnt,Server1,MailUser,MailPasswd,FromAddress,"");
		{error,_}->
			% io:format("----can't connect to smtp server----~n"),
			THIS:logAlert(?LOG_TYPE,"can't connect to smtp server","","fail")
	end.
	
	
get_template_property()->
	%BASE:get_template_property() ++ 
	[
	#property{name=to,title="To",type=scalar,multiple = true,allowother = true,listSize=5,description="either choose one or more e-mail setting(s), or enter the e-mail address of the person to send the mail to, separate multiple addresses with commas (example: " ++ ?SUPPORT_MAIL ++ "," ++ ?SALES_MAIL ++ ")"},
	#property{name=template,title="Template",type=scalar,description="choose which template to use for formatting the contents of the message.  If you are sending mail to a pager, choose one of the \"short\" templates."}
	].
    