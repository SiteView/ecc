-module(sms,[BASE,Monitor,Rule]).
-extends(action).
-compile(export_all).

-include("monitor.hrl").
-include("monitor_template.hrl").
-include("alert.hrl").


-define(SUPPORT_MAIL,"support@siteview.com").
-define(SALES_MAIL,"sales@siteview.com").
-define(PRODUCT_NAME,"elecc").

-define(LOG_TYPE,"SMS alert sent").
-define(TIMEOUT,5).

-define(SMSDLL,"jsSms").  %%-- yi.duan JiangSu Message

new(Monitor,Rule)->
	Obj = action:new(Monitor,Rule),
	{?MODULE,Obj,Monitor,Rule}.
    
get_monitor()->{ok,{monitor,Monitor}}.

get_rule()->{ok,{rule,Rule}}.

execute() ->
	
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
					{ok,{Ok,Id}}=Rule:get_property(id),
					sendSMSAlert(Params, true)
			end
	end.

recover(This)->
    {ok,{_,Params}} = Rule:get_property(action_param), 
	sendSMSAlert(Params, false).
 
% sendSMSAlert(Params, IsAlert) ->
    % CATEGORY = case Monitor:get_attribute(?CATEGORY) of 
		% {ok,{_,Category}} -> 
			% atom_to_list(Category);
		% _->
			% ""
	% end,
	% NAME = case Monitor:get_property(?NAME) of
		% {ok,{_,Name}}->
			% Name;
		% _->
			% ""
	% end,
	% STATE_STRING = case Monitor:get_attribute(?STATE_STRING) of
		% {ok,{_,State}}->
			% State;
		% _->
			% ""
	% end, 
    % LocalCode = string:to_lower(platform:getLocalCode()),
    % TemplateFile = if
        % IsAlert ->
            % Params#sms_alert.template;
        % true ->
            % "Recover"
    % end,
    % Msg = THIS:createMessage("templates.sms", TemplateFile), 
    % [Phone0] = Params#sms_alert.phone,
    % Phone01 = string:tokens(Phone0, ","),
    % Phone1 = api_dutytable:get_duty_info(Params#sms_alert.duty,phone),%%设定的手机号码 和值班表中的手机号码 
    % Phone = Phone01 ++ Phone1,
    % io:format("Phone: ~p~n", [Phone]),
    % case Params#sms_alert.type of
    % "Web" ->        
        % [SMSUrl] = Params#sms_alert.url,
        % [Username] = Params#sms_alert.username,
        % [Password] = Params#sms_alert.password,
        % {{Y,M,D},{H,F,S}} = erlang:localtime(), 
        % if   LocalCode == "utf-8" ->
            % M = Msg;
        % true ->
            % M = iconv:convert(LocalCode,"utf-8",Msg)  
        % end, 
        %% Msg = "alert from siteivew. monitor: " ++ NAME ++" Status: " ++ CATEGORY++" Time: " ++ integer_to_list(Y)++"_"++integer_to_list(M)++"_"++integer_to_list(D)++" "++integer_to_list(H)++ ":"++integer_to_list(F)++":"++integer_to_list(S),                  
        % F1 = 
            % fun(X)->
                % Url = SMSUrl ++ "?user="++ Username++"&pwd="++Password++"&phone="++X++"&msg="++ mochiweb_util:quote_plus(M),         
                % case http:request(get,{Url,[{"te","trailers, deflate;q=0.5"}]},[],[]) of
                % {ok,{{_,Code,_},_,ReturnString}} ->
                    % case Code of   
                    % 200 ->
                        % Index = string:str(ReturnString,"SMS Submit Successfully"), 
                        % if Index > 0 ->
                            % THIS:logAlert(?LOG_TYPE,X,NAME,"SMS Submit Successfully","ok"),
                            % {ok,{sms,Msg}};
                        % true ->
                            % THIS:logAlert(?LOG_TYPE,X,NAME,"fail","fail"), 
                            % {error,{sms,"sand message error"}}
                        % end;                    
                    % _->
                        % THIS:logAlert(?LOG_TYPE,X,NAME,"fail","fail"), 
                        % {error,{sms,"sand message error"}}  
                    % end;  
                % {error, Reason} ->
                    % THIS:logAlert(?LOG_TYPE,X,NAME,"fail","fail"), 
                    % {error,{sms,"fail"}};
                % _ ->
                    % THIS:logAlert(?LOG_TYPE,X,NAME,"fail","fail"), 
                    % {error,{sms,"sand message error"}}            
                % end
            % end,
        % Ret = lists:map(F1,Phone),
        % {ok, {sms, Ret}};
    % "GSM" ->
        % F1 = 
            % fun(X)->
                % io:format("XPhone: ~p~n", [X]),
                % case gsmOperate:sendMessage("COM1",X,Msg) of
                    % 0 ->
                        % THIS:logAlert(?LOG_TYPE,X,NAME,"SMS Submit Successfully","ok"),
                        % {ok,{sms,Msg}}; 
                    % _ ->
                        % THIS:logAlert(?LOG_TYPE,X,NAME,"fail","fail"), 
                        % {error,{sms,"fail"}}
                % end
            % end,
        % Ret = lists:map(F1,Phone),
        % {ok, {sms, Ret}};
    % _ ->
        % THIS:logAlert(?LOG_TYPE,Phone,NAME,"fail","fail"), 
        % {error,{sms,"error"}} 
    % end. 
	
sendSMSAlert(Params, IsAlert) ->
	NAME = case Monitor:get_property(?NAME) of
		{ok,{_,Name}}->
			io:format("Name  Name  Name  Name  ~p~n",[Name]),
			Name;
		_->
			io:format("Name  Name  Name  Name  is empty~n",[]),
			""
	end,
    TemplateFile = if
        IsAlert ->
            Params#sms_alert.template;
        true ->
            "Recover"
    end,
    Msg = THIS:createMessage("templates.sms", TemplateFile), 
	
    Phone1 = api_dutytable:get_duty_info(Params#sms_alert.duty,phone),%%设定的手机号码 和值班表中的手机号码 	
    [Phone2] = Params#sms_alert.other,
	if 
		Phone1 =:= [] -> 
			sendforDuty(Phone2,Params,NAME,Msg),
			sendSMS(Phone2,Params,NAME,Msg,IsAlert,[]);
		Phone1 =/= [] -> 
			sendforDuty(Phone1,Params,NAME,Msg),
			sendSMS(Phone1,Params,NAME,Msg,IsAlert,[])
	end.
	
sendforDuty(Phone,Params,NAME,Msg)->
	SmsEncode = case preferences:get(sms_settings,smsEncode) of
		{ok,[{_,V5}]}->
			V5;
		_->
			"utf-8"
		end,
	MsgP = iconv:convert(httputils:pageEncode(), SmsEncode,Msg),
	{ok,do_sendSMS(Phone,NAME,MsgP,Params,Msg)}.
	

sendSMS([],_,_,_,_,Result)->{ok,Result};	
sendSMS(["other"|T],Params,NAME,Msg,IsAlert,Result)->
	SmsEncode = case preferences:get(sms_settings,smsEncode) of
		{ok,[{_,V5}]}->
			V5;
		_->
			"utf-8"
		end,
	MsgP = iconv:convert(httputils:pageEncode(), SmsEncode,Msg),
	[Other] =  Params#sms_alert.other,
	To = string:tokens(Other,","),
	Ret = do_sendSMS(To,NAME,MsgP,Params,Msg),
    sendSMS(T,Params,NAME,Msg,IsAlert,Result++Ret);
sendSMS([Phone|T],Params,NAME,Msg,IsAlert,Result)->	
	SmsEncode = case preferences:get(sms_settings,smsEncode) of
		{ok,[{_,V5}]}->
			V5;
		_->
			"utf-8"
		end,
	Ret = 
	case preferences:get(additional_sms_settings,list_to_atom(Phone)) of
		{ok,[{_,Ap}|_]}->
			CheckShedule = THIS:check_schedule(Ap#additional_sms_settings.schedule),
			if 
				Ap#additional_sms_settings.disable =/= "true" andalso CheckShedule ->
					NewContent = case Ap#additional_sms_settings.template of
						"use_alert"->
							Msg;
						Temp->
							if
								IsAlert->
									THIS:createMessage("templates.mail",Temp);
								true->
									THIS:createMessage("templates.mail","Recover")
							end
					end,
					MsgP = iconv:convert(httputils:pageEncode(), SmsEncode,NewContent),
					To = [Ap#additional_sms_settings.email],
					
					do_sendSMS(To,NAME,MsgP,Params,NewContent);
					
				true ->
					[{error,Ap#additional_sms_settings.email,"disabled or schedule"}]
			end;
		_->
			[{error,sms,"sms setting not found"}]
	end,
	sendSMS(T,Params,NAME,Msg,IsAlert,Result++Ret).

get_web_sms_config(Key) ->
	Conf = "conf/websms.conf",
	case file:consult(Conf) of
		{ok,Data} ->
			Fun = fun([],_) ->
						case Key of
							user	-> "user";
							pwd		-> "pwd";
							phone	-> "phone";
							msg		-> "msg";
							_		-> "none"
						end;

					([Item|Items],Self) ->
						case Item of
							{Key,Node} -> Node;
							_ -> Self(Items,Self)
						end
				end,
			Fun(Data,Fun);
		_ -> 
			case Key of
				user	-> "user";
				pwd		-> "pwd";
				phone	-> "phone";
				msg		-> "msg";
				_		-> "none"
			end
	end.
	
%%(master@sitevmecc2)1> send sms to:"13612345678"
%%(master@sitevmecc2)1> send SMSUrl :["www.baidu.com"]
%%(master@sitevmecc2)1> send Username :["user"]
%%(master@sitevmecc2)1> send Password :["password"]
%%(master@sitevmecc2)1> send MsgP :"SiteView\r\nMonitor: CPU Utilization\r\nGroup:
  %%-server1\r\nSample #: 1\r\nState :28% used\r\nTime:  2011-11-02 10:13:28\r\n"
%%(master@sitevmecc2)1> send Msg :"SiteView\r\nMonitor: CPU Utilization\r\nGroup:
  %%-server1\r\nSample #: 1\r\nState :28% used\r\nTime:  2011-11-02 10:13:28\r\n"

do_sendSMS(To,NAME,MsgP,Params,Msg)->
	io:format("send sms to:~p~n",[To]),

	case Params#sms_alert.type of
		"Web" ->        
			[SMSUrl] = Params#sms_alert.url,
			[Username] = Params#sms_alert.username,
			[Password] = Params#sms_alert.password,
			F1 = fun(X)->
					%~ Url = SMSUrl ++ "?user="++ Username++"&pwd="++Password++"&phone="++X++"&msg="++ mochiweb_util:quote_plus(MsgP),
					Url = lists:flatten([SMSUrl,"?",get_web_sms_config(systemid),"=",get_web_sms_config(systemidvalue),"&",get_web_sms_config(user),"=",Username,"&",get_web_sms_config(pwd),"=",Password,"&",get_web_sms_config(phone),"=",X,"&",get_web_sms_config(msg),"=",mochiweb_util:quote_plus(MsgP)]),
					case httpc:request(get,{Url,[{"te","trailers, deflate;q=0.5"}]},[],[]) of
					{ok,{{_,Code,_},_,ReturnString}} ->
						case Code of   
						200 ->
							THIS:logAlert(?LOG_TYPE,X,NAME,"SMS Submit Successfully","ok"),
							{ok,{X,Msg}};              
						_->
							THIS:logAlert(?LOG_TYPE,X,NAME,"fail","fail"), 
							{error,{X,"sand message error"}}  
						end;  
					{error, Reason} ->
						THIS:logAlert(?LOG_TYPE,X,NAME,"fail","fail"), 
						{error,{X,"fail"}};
					_ ->
						THIS:logAlert(?LOG_TYPE,X,NAME,"fail","fail"), 
						{error,{X,"sand message error"}}            
					end
				end,
			lists:map(F1,To);
		"GSM" ->
			F1 = 
				fun(X)->
					io:format("XPhone: ~p~n", [X]),
					case gsmOperate:sendMessage("COM1",X,MsgP) of
						0 ->
							THIS:logAlert(?LOG_TYPE,X,NAME,"SMS Submit Successfully","ok"),
							{ok,{X,Msg}}; 
						_ ->
							THIS:logAlert(?LOG_TYPE,X,NAME,"fail","fail"), 
							{error,{X,"fail"}}
					end
				end,
			lists:map(F1,[To]);
		"DLL" ->
			[SMSUrl] = Params#sms_alert.url,
			[Username] = Params#sms_alert.username,
			[Password] = Params#sms_alert.password,
		
			io:format("sendMessageByDll ~n"),
			io:format("send SMSUrl :~p~n",[SMSUrl]),
			io:format("send Username :~p~n",[Username]),
			io:format("send Password :~p~n",[Password]),
			io:format("send MsgP :~p~n",[MsgP]),
			io:format("send Msg :~p~n",[Msg]),
			
		    F1 = 
				fun(PhoneNumber)->
				    %% spawn(?MODULE, sendMessageByDllThread, [PhoneNumber,Msg,SMSUrl,Username,Password])
					case sendMessageByDll({PhoneNumber,Msg,SMSUrl,Username,Password}) of
						"ok" ->
							io:format("sendMessageByDll ok ~n"),
							{ok,{PhoneNumber,Msg}};
						_->
							io:format("sendMessageByDll error ~n"),
							{error,{PhoneNumber,"fail"}}
					end
				end,
			lists:map(F1,[To]);
		_ ->
			THIS:logAlert(?LOG_TYPE,string:join(To,","),NAME,"fail","fail"), 
			[]
	end.
	
loop(RequestId,Data) ->
    receive
        {http, {RequestId, Result}} ->
                {ok,binary_to_list(Result)};   
        _->
            {error,-1}       
    end.

get_template_file_list()->
	[filename:basename(X)||X<-filelib:wildcard("templates.sms/*"),filelib:is_file(X),not filelib:is_dir(X)].
	
	
read_template_file(Name)->
	case file:read_file("templates.sms/" ++ Name) of
		{ok,Bin}->
			{ok,binary_to_list(Bin)};
		Else->
			Else
	end.

write_template_file(Name,Data)when is_binary(Data)->
	case file:write_file("templates.sms/" ++ Name,Data) of
		ok->
			{ok,Name};
		Else->
			Else
	end;
write_template_file(Name,Data)->
	case file:write_file("templates.sms/" ++ Name,list_to_binary(Data)) of
		ok->
			{ok,Name};
		Else->
			Else
	end.

remove_template_file(Name)->
	case file:delete("templates.sms/" ++ Name) of
		ok->
			{ok,Name};
		Else->
			Else
	end.	

getScalarValues(Prop,Params)->
	case Prop of
		template->
			[{filename:basename(X),filename:basename(X)}||X<-filelib:wildcard("templates.sms/*")];
		phone->
			case preferences:all(additional_sms_settings) of
				{ok,Emails}->
					[{Y#additional_sms_settings.name,atom_to_list(X)}||{X,Y}<-Emails];
				_->
					[]
			end;
		_->
			BASE:getScalarValues(Prop,Params)
	end.

get_template_property()->
	%BASE:get_template_property() ++ 
	[
	#property{name=template,title="Template",type=scalar,description="choose which template to use for formatting the contents of the message.  If you are sending mail to a pager, choose one of the \"short\" templates."}
	].    
 
%%------------------------------------------------------------------
%% yi.duan add dll alert 
init() ->
    {_, Path} = file:get_cwd(),  
    case erl_ddll:load_driver(Path++"\\tools", ?SMSDLL) of
	ok -> ok;
	{error, already_loaded}  -> ok;
	{error,ErrorDesc} ->
		io:format("===>~p~n", [erl_ddll:format_error(ErrorDesc)]),
		exit({error, could_not_load_driver});
	_ ->  exit({error, could_not_load_driver})
    end.  

sendMessageByDll(Msg) ->
	case init() of
	  ok ->
		 Port = open_port({spawn, ?SMSDLL}, [binary]),
		 Bin = term_to_binary(Msg),
		 port_command(Port,Bin),
		 Result = receive_port_data(Port),
		 case Result of
			{error,timeout} -> {error,timeout};
			_ ->	    
			port_close(Port),
			Result,
			binary_to_term(Result)
		 end;
	  Error -> Error
	end.

receive_port_data(Port) ->
    receive
        {Port, {data, Data}} ->  Data
	after 8000 ->  {error,timeout}
    end.

sendMessageByDllThread(PhoneNumber,Msg,SMSUrl,Username,Password) ->
	case sendMessageByDll({PhoneNumber,Msg,SMSUrl,Username,Password}) of
		"ok" ->
			io:format("sendMessageByDll ok ~n"),
			{ok,{PhoneNumber,Msg}};
		_->
			io:format("sendMessageByDll error ~n"),
			{error,{PhoneNumber,"fail"}}
	end.

	
	