%% ---
%% Mail Monitor
%%
%%---

%% @copyright 2008-2009 Dragonflow
%% @author Jian Huang <jian.huang@dragonflow.com>
%% @version 1.0

%% @doc telnet monitor
%%
%%This module is to test telnet service:
%%1. support POP3, IMAP4, SMTP protocol(not support SSL/TLS)
%%2. test send mail in SMTP mode, which can attach one attachment
%%3. test receive mail in pop3 or imap4 mode, which can match content(not sppport MIME content match)

-module(mail_monitor, [BASE]).
-compile(export_all).
-extends(atomic_monitor).
-include("monitor.hrl").
-include("monitor_template.hrl").

-define(SMTPPORT, 25).
-define(POP3PORT, 110).
-define(IMAP4PORT, 143).

-define(SUBJECT, "SiteView Mail Monitor test message").
-define(CONTENT, "Just a test").

%% @spec new() -> Obj
%% @type Obj = term()
%% @doc create a new instance for mail monitor
new()->
	Base = atomic_monitor:new(),
	Base:set_attribute(roundTripTime, 0),
	Base:set_attribute(smtpTime, 0),
	Base:set_attribute(receiveTime, 0),
	{?MODULE,Base}.

%% @spec defaultTitle(Params) -> List
%% @type Params = [term()]
%% @type List = string()
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	BASE:defaultTitle(Params) ++ ": " ++ case proplists:get_value(smtpServer,Params) of undefined->"";V->V end.

%% @spec update() -> Result
%% @type Result = term()
%% @doc update is the run function called by schedule to test the mail service	
update()->
	%%get property of monitor
	{ok, {_, Action}}             = THIS:get_property(action),
	{ok, {_, SmtpServer}}         = THIS:get_property(smtpServer),
	{ok, {_, SendFromAddr}}       = THIS:get_property(sendFromAddr),
	{ok, {_, SendToAddr}}         = THIS:get_property(sendToAddr),
	{ok, {_, ReceivingProtocol}}  = THIS:get_property(receivingProtocol),
	{ok, {_, Pop3Server}}         = THIS:get_property(pop3Server),
	{ok, {_, Pop3Account}}        = THIS:get_property(pop3Account),
	{ok, {_, Pop3Password}}       = THIS:get_property(pop3Password),
	{ok, {_, ContentMatch}}       = THIS:get_property(contentMatch),
	{ok, {_, Timeout}}            = THIS:get_property(timeout),
	{ok, {_, _CheckEvery}}        = THIS:get_property(checkEvery),
	{ok, {_, Attachment}}         = THIS:get_property(attachment),
	{ok, {_, SmtpUser}}           = THIS:get_property(smtpUser),
	{ok, {_, SmtpPass}}           = THIS:get_property(smtpPassword),
	
	%%three actions of test are support
	%%1. only send a mail
	%%3. send and receive mail parallely
	THIS:set_attribute(status, 0),
	THIS:set_attribute(roundTripTime, 0),
	THIS:set_attribute(smtpTime, 0),
	THIS:set_attribute(receiveTime, 0),	
	THIS:set_attribute(matchvalue, "True"),

	case Action of
		%%only send a mail
		"send" ->
			{SmtpSendTime, Ret} = timer:tc(mail, smtp_mail_send, [SmtpServer, ?SMTPPORT,SmtpUser, SmtpPass, SendFromAddr, SendToAddr, ?SUBJECT, ?CONTENT, Attachment, Timeout]),
			case Ret of
				ok ->
                    SSmtpSendTime = sv_datetime:microSecondsToStrSeconds(SmtpSendTime),
					THIS:set_attribute(roundTripTime, SmtpSendTime/1000),
					THIS:set_attribute(smtpTime, SmtpSendTime),
					THIS:set_attribute(status, 200),
					THIS:set_attribute(?CATEGORY, good),
					THIS:set_attribute(?STATE_STRING, "successfully sent in " ++ SSmtpSendTime ++ "sec");
				_ ->
					{error, Reason} = Ret,
                    Line = iconv:convert("gb2312","utf-8", Reason),
					THIS:set_attribute(?CATEGORY,  error),
					THIS:set_attribute(?STATE_STRING, "send failed: " ++ Line)
			end;
		%%only receive a mail
		"receive" ->
			case ReceivingProtocol of
				%%use pop3 protocol to receive a mail
				"pop3" ->
					{Pop3ReceiveTime,  Ret} = timer:tc(mail, pop3_message_read, [Pop3Server, ?POP3PORT, Pop3Account, Pop3Password, ContentMatch, Timeout]),
                    case Ret of
						ok ->
                            SPop3ReceiveTime = sv_datetime:microSecondsToStrSeconds(Pop3ReceiveTime),
							THIS:set_attribute(roundTripTime, Pop3ReceiveTime/1000),
							THIS:set_attribute(receiveTime, Pop3ReceiveTime),
							THIS:set_attribute(status, 200),
							THIS:set_attribute(?CATEGORY, good),
							THIS:set_attribute(?STATE_STRING, "successfully retrieved in " ++ SPop3ReceiveTime ++ "sec");
						_ ->
							{error, Reason} = Ret,
                            Line = iconv:convert("gb2312","utf-8", Reason),
							THIS:set_attribute(?CATEGORY, error),
							THIS:set_attribute(?STATE_STRING, "retrieve failed: " ++ Line)
					end;
				%%use imap4 to receive a mail	
				"imap4" ->
					{Imap4ReceiveTime, Ret} = timer:tc(mail, imap4_message_read, [Pop3Server, ?IMAP4PORT, Pop3Account, Pop3Password, ContentMatch, Timeout]),
                    case Ret of
						ok ->
                            SImap4ReceiveTime = sv_datetime:microSecondsToStrSeconds(Imap4ReceiveTime),
							THIS:set_attribute(roundTripTime, Imap4ReceiveTime/1000),
							THIS:set_attribute(receiveTime, Imap4ReceiveTime),
							THIS:set_attribute(status, 200),
							THIS:set_attribute(?CATEGORY, good),
							THIS:set_attribute(?STATE_STRING, "successfully retrieved in " ++ SImap4ReceiveTime ++ "sec");
						_ ->
							{error, Reason} = Ret,
                            Line = iconv:convert("gb2312","utf-8", Reason),
							THIS:set_attribute(?CATEGORY, error),
							THIS:set_attribute(?STATE_STRING, "retrieve failed: " ++ Line)
					end;
				_ ->
					ReceivingProtocol
			end;
		%%send and receive a mail	
		"sendrecv" ->
			%%first send a mail
			{SmtpSendTime, SendRet} = timer:tc(mail, smtp_mail_send, [SmtpServer, ?SMTPPORT,SmtpUser, SmtpPass, SendFromAddr, SendToAddr, ?SUBJECT, ?CONTENT, Attachment, Timeout]),
			%%then receive a mail 
			case ReceivingProtocol of
				"pop3" ->
					{Pop3ReceiveTime, RecvRet} = timer:tc(mail, pop3_message_read, [Pop3Server, ?POP3PORT, Pop3Account, Pop3Password, ContentMatch, Timeout]),
					%%calculate total time
					RoundTripTime = SmtpSendTime + Pop3ReceiveTime,
					SRoundTripTime = sv_datetime:microSecondsToStrSeconds(RoundTripTime),
					if
						%%all success
						SendRet =:= ok andalso RecvRet =:= ok ->    
							SSmtpSendTime = sv_datetime:microSecondsToStrSeconds(SmtpSendTime),
                            SPop3ReceiveTime = sv_datetime:microSecondsToStrSeconds(Pop3ReceiveTime),
							THIS:set_attribute(status, 200),
							THIS:set_attribute(?CATEGORY, good),
                            THIS:set_attribute(roundTripTime, RoundTripTime/1000),
							THIS:set_attribute(?STATE_STRING, "successfully looped in " ++ SRoundTripTime ++ "sec" ++ "<br>"
																"send time " ++ SSmtpSendTime ++ "sec" ++ "<br>"
																"receive time " ++ SPop3ReceiveTime ++ "sec");
						%%receive fail
						SendRet =:= ok ->                      
							{error, Reason} = RecvRet,
                            Line = iconv:convert("gb2312","utf-8", Reason),
							THIS:set_attribute(?CATEGORY, error),
							THIS:set_attribute(?STATE_STRING, "retrieve failed: " ++ Line);
						%%send error
						RecvRet =:= ok ->                     
							{error, Reason} = SendRet,
                            Line = iconv:convert("gb2312","utf-8", Reason),
							THIS:set_attribute(?CATEGORY, error),
							THIS:set_attribute(?STATE_STRING, "send failed: " ++ Line);
						%%all error
						true ->
							{error, SReason} = SendRet,
							{error, RReason} = RecvRet,
                            SLine = iconv:convert("gb2312","utf-8", SReason),
                            RLine = iconv:convert("gb2312","utf-8", RReason),
							THIS:set_attribute(?CATEGORY, error),
							THIS:set_attribute(?STATE_STRING, "send failed: " ++ SLine ++ "<br>" ++ "retrieve failed: " ++ RLine)
					end;
				"imap4" ->
					{Imap4ReceiveTime, RecvRet} = timer:tc(mail, imap4_message_read, [Pop3Server, ?IMAP4PORT, Pop3Account, Pop3Password, ContentMatch, Timeout]),
					RoundTripTime = SmtpSendTime + Imap4ReceiveTime,
					SRoundTripTime = sv_datetime:microSecondsToStrSeconds(RoundTripTime),
					if
						SendRet =:= ok andalso RecvRet =:= ok ->      
							SSmtpSendTime = sv_datetime:microSecondsToStrSeconds(SmtpSendTime),
                            SImap4ReceiveTime = sv_datetime:microSecondsToStrSeconds(Imap4ReceiveTime),
							THIS:set_attribute(?CATEGORY, good),
                            THIS:set_attribute(roundTripTime, RoundTripTime/1000),
							THIS:set_attribute(status, 200),
							THIS:set_attribute(?STATE_STRING, "successfully looped in " ++  SRoundTripTime ++ "sec"++ "<br>"
																"send time " ++ SSmtpSendTime ++ "sec" ++ "<br>"
																"receive time " ++ SImap4ReceiveTime ++ "sec");
						SendRet =:= ok ->
							{error, Reason} = RecvRet,
                            Line = iconv:convert("gb2312","utf-8", Reason),
							THIS:set_attribute(?CATEGORY, error),
							THIS:set_attribute(?STATE_STRING, "retrieve failed: " ++ Line);
						RecvRet =:= ok ->
							{error, Reason} = SendRet,
                            Line = iconv:convert("gb2312","utf-8", Reason),
							THIS:set_attribute(?CATEGORY, error),
							THIS:set_attribute(?STATE_STRING, "send failed: " ++ Line);
						true ->
							{error, SReason} = SendRet,
							{error, RReason} = RecvRet,
                            SLine = iconv:convert("gb2312","utf-8", SReason),
                            RLine = iconv:convert("gb2312","utf-8", RReason),
							THIS:set_attribute(?CATEGORY, error),
							THIS:set_attribute(?STATE_STRING, "send failed: " ++ SLine ++ "<br>" ++ "retrieve failed: " ++ RLine)
					end
			end;
		_ ->
			{error, ""}
	end.

%% @spec getScalarValues(Prop, Params) -> List
%% @type Prop = atom
%% @type Params = [term()]
%% @type List = [{Show, Name}]
%% @type Show = string()
%% @type Name = string()
%% @doc getScalarValues is the function called by schedule to set values of dropdown box
%%   Prop define the property name of dropdown box 
%%	 Show define the show item of dropdown box
%%	 Name define the name that can get through get_property function
getScalarValues(Prop, Params) ->
	case Prop of
		action ->
			[{"Send & Receive", "sendrecv"}, {"Receive Only", "receive"}, {"Send Only", "send"}];
		receivingProtocol ->
			[{"POP3", "pop3"}, {"IMAP4", "imap4"}];
		_ ->
			BASE:getScalarValues(Prop, Params)
	end.

%% @spec verify(Params) -> {ok, []} | {error, Reason}
%% @type Params = [term()]
%% @type Reason = string()
%% @doc verify is the function called by schedule to verify the parameter of monitor inputed by user
%%	mail monitor verify depend on the action user select
%%  1. Send action. Verify the send from email count, send to email count ans smtp server
%%  2. Receive action. Verify the pop3/iamp4 server and receive email acount
%%  3. Send and receive action. Verify both two 
verify(Params) ->
    %% get the action
    Action = proplists:get_value(action, Params), 
    case Action of
		%%condition 3
        "sendrecv" ->
            Ret = verifySend(smtpServer, Params),
            case Ret of
                {ok, ""} ->
                    case verifyRecv(pop3Server, Params) of
						{ok, ""} ->
							case BASE:verify(Params) of
								{error,E}->
									{error, E};		
								_->
									{ok, []}
							end;
						{error, Err}  ->
							case BASE:verify(Params) of
								{error,E}->
									{error, E++Err};		
								_->
									{error,Err}
							end
					end;
				{error, Error} ->
					case BASE:verify(Params) of
						{error,E}->
							{error, E ++ Error};		
						_->
							{error, Error}
					end
			end;						
		%%condition 2	
        "receive" ->
            case verifyRecv(pop3Server, Params) of
				{ok, ""} ->
					case BASE:verify(Params) of
						{error,E}->
							{error, E};		
						_->
							{ok, []}
					end;
				{error, Err}  ->
					case BASE:verify(Params) of
						{error,E}->
							{error, E++Err};		
						_->
							{error,Err}
					end
			end;
		%%condition 1	
        "send" ->
            case verifySend(smtpServer, Params) of
				{ok, ""} ->
					case BASE:verify(Params) of
						{error,E}->
							{error, E};		
						_->
							{ok, []}
					end;
				{error, Err}  ->
					case BASE:verify(Params) of
						{error,E}->
							{error, E++Err};		
						_->
							{error,Err}
					end
			end		
    end.

verifySend(State, Params)->
    case State of
		%%smtp server is not allowed null and spaces
        smtpServer ->
            case proplists:get_value(smtpServer, Params) of
                "" ->
                    {error, [{smtpServer, "Sending mail server is not allowed null"}]};
                Ret ->
					case string:rstr(Ret," ") of
						0 ->
							verifySend(sendFromAddr, Params);
						_ ->
							{error, [{smtpServer, "No spaces are allowed"}]}
					end
						
            end;
		%%send from address is not allowed null and spaces, and must contain "@"
        sendFromAddr ->
            case proplists:get_value(sendFromAddr, Params) of
                "" ->
                    {error, [{sendFromAddr, "Send from address is not alllowed null"}]};
                Ret ->
					case string:rstr(Ret," ") of
						0 ->
							case string:rstr(Ret,"@") of
								0 ->
									{error, [{sendFromAddr, "Email address is not valid"}]};
								_ ->
									verifySend(sendToAddr, Params)
							end;
						_ ->
							{error, [{sendFromAddr, "No spaces are allowed"}]}
					end
            end;
		%%send from address is not allowed null and spaces, and must contain "@"
        sendToAddr ->
            case proplists:get_value(sendToAddr, Params) of
                "" ->
                    {error, [{sendToAddr, "Send to address is not allowed null"}]};
                Ret ->
					case string:rstr(Ret," ") of
						0 ->
							case string:rstr(Ret,"@") of
								0 ->
									{error, [{sendToAddr, "Email address is not valid"}]};
								_ ->
									{ok, []}
							end;
						_ ->
							{error, [{sendToAddr, "No spaces are allowed"}]}
					end
            end
    end.

verifyRecv(State, Params)->
    case State of
		%%pop3 server is not allowed null and spaces		
        pop3Server ->
            case proplists:get_value(pop3Server, Params) of
                "" ->
                     {error, [{pop3Server, "Receiving Mail Server Not Null"}]};
               Ret ->
					case string:rstr(Ret," ") of
						0 ->
							verifyRecv(pop3Account, Params);
						_ ->
							{error, [{pop3Server, "No spaces are allowed"}]}
					end
            end;
		%%pop3 receive address user name is not allowed null
        pop3Account ->
            case proplists:get_value(pop3Account, Params) of
                "" ->
                    {error, [{pop3Account, "Receiving mail account user name is not allowed null"}]};
                _ ->
                    verifyRecv(pop3Password, Params)
            end;
		%%pop3 receive address password is not allowed null
        pop3Password ->
            case proplists:get_value(pop3Password, Params) of
                "" ->
                    {error, [{pop3Password, "Receiving mail account password is not allowed null"}]};
                _ ->
                    {ok, []}
            end
    end.

	
%% @spec get_template_property() -> List
%% @type List = [term()]
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
%% 1.elements show on gui interface
%% 2.elements to evaluate the return value of ftp server 
get_template_property() ->
	BASE:get_template_property() ++ 
	[
		#property{name=action, type=scalar, order=1, title="Action", description="the action the monitor will take with the e-mail message. For example, send and receive or receive only.", default=""},
		#property{name=smtpServer, type=text, order=2, title="Sending Mail Server (SMTP)", description="hostname of mail server where test message is sent - for example, mail.siteview.com"},
		#property{name=sendFromAddr, type=text, order=3, title="Send From Address", description="address where test message is from -for example, support@dragonflow.com" },
		#property{name=sendToAddr, type=text, order=4, title="Send To Address", description="address where test message is sent to - for example, support@dragonflow.com"},
		#property{name=receivingProtocol, type=scalar, order=5, title="Receiving Protocol", description="the protocol used to receive the message", default="POP3/IMAP4"},
		
		#property{name=pop3Server, type=text, order=6, title="Receiving Mail Server", description="the mail server where the test message is received - for example, mail.siteview.com"},
		#property{name=pop3Account, type=text, order=7, title="Receiving Mail Server User Name", description="the account name for the receiving mailbox - for example, support"},
		#property{name=pop3Password, type=password, order=8, title="Receiving Mail Server Password", description="the password for the receiving mailbox"},
		
		#property{name=contentMatch, type=text, order=1, advance=true, title="Receive Content Match", description="for Receive Only, the text to match in the contents of the received message. (example: Subject: MySubject)"},
		#property{name=timeout, type=numeric,  order=2, advance=true, title="Timeout", description="amount of time (in seconds) to keep checking for the message to be received",default=300,baselinable=true},
		#property{name=checkEvery, type=numeric, order=3, advance=true, title="POP Check Delay", description="delay (in seconds) between each login to the receiving server, while waiting for the message to arrive", default=10},
		#property{name=attachment, type=text, order=4, advance=true, title="Attachment", description="Enter the full path name of a file to attach to the email"},
		#property{name=smtpUser, type=text, order=5, advance=true, title="SMTP User", description="User name for smtp-auth "},
		#property{name=smtpPassword, type=password, order=6, advance=true, title="SMTP Password", description="the password for smtp-auth server"},
		
		#property{name=roundTripTime, type=numric, order=7, state=true, title="round trip time(milliseconds)", configurable=false},
		#property{name=status, type=numric, order=8, state=true, title="status", configurable=false},
		#property{name=smtpTime, type=numric, order=9, state=true, title="Send time ", configurable=false},
		#property{name=receiveTime, type=numric, order=10, state=true, title="Receive time(milliseconds)", configurable=false},
		#property{name=matchValue, type=text, order=11, state=true, title="content match", configurable=false}
	].

%% @spec get_classifier(error) -> List
%% @type List = [Tuple]
%% @type Tule = {status, Logic, Value}
%% @type Logic = '!=' | '==' | '>' | '<' | 'contain
%% @type Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error)->
	_Cls = case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_ ->
			[{roundTripTime, '==', "0"}]
	end;
get_classifier(warning) ->
	_Cls =case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_ ->
			[{roundTripTime, '==', "0"}]
	end;
get_classifier(good) ->
	_Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_ ->
			[{roundTripTime, '!=', "0"}]
	end.
	