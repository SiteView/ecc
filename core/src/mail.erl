%% ---
%% Mail Libarary
%%
%%---

%% @copyright 2008-2009 Dragonflow
%% @author Jian Huang <jian.huang@dragonflow.com>
%% @version 1.0

%% @doc Mail Libarary
%%
%%This module is to provide tools for  mail monitor
%%1. support POP3, IMAP4, SMTP protocol(not support SSL/TLS)
%%2. send mail
%%3. receive mail

-module(mail).
-compile(export_all).

-define(CRLF, "\r\n").
-define(DATAEND, ?CRLF ++ "." ++ ?CRLF).
-define(POP3OK, "+OK").
-define(POP3ERR, "-ERR").
-define(POP3DATAEND, "\r\n.\r\n").
-define(IMAP4OK, "OK").
-define(IMAP4NO, "NO").
-define(IMAP4No, "No").
-define(IMAP4TAG, "*").
-define(IMAP4DATAEND, ".\r\n").

-define(SMTPPORT, 25).
-define(POP3PORT, 110).
-define(IMAP4PORT, 143).

-define(Weeks, [{7, "Sun"}, {1, "Mon"}, {2, "Tue"}, {3, "Wed"}, {4, "Thu"}, {5, "Fri"}, {6, "Sat"}]).
-define(Months, [{1, "Jan"}, {2, "Feb"}, {3, "Mar"}, {4, "Apr"}, {5, "May"}, {6, "Jun"}, {7, "Jul"}, {8, "Aug"}, {9, "Sep"}, {10, "Oct"}, {11, "Nov"}, {12, "Dec"}]).

%%--------------------------------------SMTP Section----------------------------------------------
%%smtp test function 
smtp_test()->
    smtp_mail_send("mail.dragonflow.com", 25, "jian.huang", "iamcoming", "jian.huang@dragonflow.com", "jian.huang@dragonflow.com", "hello", "hello", "f:\\pro.txt", 60).

%% @spec smtp_mail_send(SmtpServer, SmtpPort, UserName, Password, From, To, Subject, Body, Attachment, Time)-> {error,Resean} | ok
%% @type SmtpServer = string()
%% @type SmtpPort = integer()
%% @type UserName = string()
%% @type Password =  string()
%% @type From = string()
%% @type To = string()
%% @type Subject = string()
%% @type Body = string()
%% @type Attachment = string()
%% @type Timeout = integer()
%% @doc Send mail use smtp protocol
%%	SmtpServer is the hostname or ip of the SMTP server
%%	SmtpPort is the listen port of SMTP service
%%  Username and Password is option, they can be null
%%  From defines the send email account of the test mail
%%  To defines the receive email account of thr test mail
%%  Subject and Body is the subject and content of the test mail
%%  Attachement id the full path of the attachment file
%%  Timeout define the longest time the smtp session last
smtp_mail_send(SmtpServer, SmtpPort, UserName, Password, From, To, Subject, Body, Attachment, Timeout)->
	%%connect tcp to smtp server
    Ret = gen_tcp:connect(SmtpServer, SmtpPort, [binary, {packet, 0}]),
    case Ret of
        {ok, Socket}->
			%%real smtp function entry
            Result = smtp_receive(Socket, [], UserName, Password, From, To, Subject, Body, Attachment, Timeout*1000),
            gen_tcp:close(Socket),
            Result;
        _->
            {error, "can't connect to smtp server " ++ SmtpServer ++ " at " ++ integer_to_list(SmtpPort) ++ " port"}
    end.

%%real smtp send function entry
smtp_receive(Socket, Data, UserName, Password, From, To, Subject, Body, Attachment, Timeout)->
    receive
        {tcp, Socket, Bin}->
			%%concat the message content receive before with the new received content
            Sofar = [Bin|Data],
            Data2 = list_to_binary(lists:reverse(Sofar)),
			%%anlyse the content
            OK = smtp_response_analyze(Data2),
            case OK of
				%%we receive the welcome message from the smtp server
                {next, 220}->
                    HostName = net_adm:localhost(),
                    UserNameLen = string:len(UserName),
                    PasswordLen = string:len(Password),
                    BAuth = UserNameLen > 0 andalso PasswordLen > 0,
                    if
						%%if we have to auth, send hello to smtp server and wait for require authoration message 
                        BAuth ->
                           smtp_request_hello(ehlo, Socket, HostName),
                           stmp_receive(auth, Socket, [], UserName, Password, From, To, Subject, Body, Attachment, Timeout);
						%%no user and password, just wait for other message
                        true ->
                           smtp_request_hello(helo, Socket, HostName),
                           stmp_receive(unauth, Socket, [], UserName, Password, From, To, Subject, Body, Attachment, Timeout)
                    end;
				%%welcome message is not complete, receive further	
                continue ->
                    smtp_receive(Socket, Sofar, UserName, Password, From, To, Subject, Body, Attachment, Timeout);
				%%error, return the smtp error message	
                _ ->
                    {error, Data2}
            end;
		%%connection closed	
        {tcp_closed, Socket}->
            {error, "tcp connection closed"}
		after Timeout ->
			{error, "timeout"}
    end.

%%second smtp function
stmp_receive(Param, Socket, Data, UserName, Password, From, To, Subject, Body, Attachment, Timeout)->
    receive
        {tcp, Socket, Bin} ->
			%%concat the received data with the content received before
            Sofar = [Bin|Data],
            Data2 = list_to_binary(lists:reverse(Sofar)),
			
			%%analyse the message
            OK = smtp_response_analyze(Data2),
            Line = binary_to_list(Data2),
            case OK of
				%%request sent has been accepted
                {next, 250} -> 
					%%smtp status handle
                    case Param of
						%%status that authoration to be sent
                        auth ->
							%%send the "AUTH LOGIN" and wait for username require message
                            smtp_request_auth(Socket), 
                            stmp_receive(username, Socket, [], UserName, Password, From, To, Subject, Body, Attachment, Timeout);
						%%no authration are required, send the from email address	
                        unauth ->
                            stmp_request_from(Socket, From),
                            stmp_receive(from, Socket, [], UserName, Password, From, To, Subject, Body, Attachment, Timeout);
						%%from email has been accpeted, send the to eamil address	
                        from ->
                            stmp_request_to(Socket, To),
                            stmp_receive(to, Socket, [], UserName, Password, From, To, Subject, Body, Attachment, Timeout);
						%%to email address has benn accepted, send the email content	
                        to ->
                            smtp_request_data(Socket),
                            stmp_receive(data, Socket, [], UserName, Password, From, To, Subject, Body, Attachment, Timeout);
						%%email content sent ok, quit the smtp session	
                        data ->
                            smtp_request_quit(Socket),
                            stmp_receive(quit, Socket, [], UserName, Password, From, To, Subject, Body, Attachment, Timeout);
						%%quit suucess
						quit ->
							ok;
						%%contiue to get content	
                        _ ->
                            stmp_receive(Param, Socket, [], UserName, Password, From, To, Subject, Body, Attachment, Timeout)
                    end;
				%%smtp require the username and password	
                {next, 334} -> 
                    case Param of
						%%send username, and wait for password require message
                        username ->
                            smtp_request_auth_username(Socket, UserName),
                            stmp_receive(password, Socket, [], UserName, Password, From, To, Subject, Body, Attachment, Timeout);
						%%send the password an wait for authoration successful message
                        password ->
                            stmp_request_auth_password(Socket, Password),
                            stmp_receive(password, Socket, [], UserName, Password, From, To, Subject, Body, Attachment, Timeout);
                        _ ->
                            stmp_receive(Param, Socket, [], UserName, Password, From, To, Subject, Body, Attachment, Timeout)
                    end;
				%%authoration success, and send the from email address	
                {next, 235} -> 
                    stmp_request_from(Socket, From),
                    stmp_receive(from, Socket, [], UserName, Password, From, To, Subject, Body, Attachment, Timeout);
				%%email sent begin	
                {next, 354} -> 
                    smtp_request_mail(Socket, From, To, Subject, Body, Attachment),
                    stmp_receive(data, Socket, [], UserName, Password, From, To, Subject, Body, Attachment, Timeout);
				%%quit success return now	
                {next, 221} -> 
                    ok;
				%%username and passowrd authoration error	
                {error, 535} -> 
                    {error, Line};
				%%usrname not exist error	
                {error, 550} -> 
                    {error, Line};
				%%content not complete, receive continue	
                continue ->
                    stmp_receive(Param, Socket, Sofar, UserName, Password, From, To, Subject, Body, Attachment, Timeout);
				%%other error	
                _ ->
                    {error, Line}
            end;
        {tcp_closed, Socket}->
            {error, "tcp connection closed"}
    after Timeout ->
        {error, "connection timeout"}
    end.
	
%%analyse the smtp response
smtp_response_analyze(Bin)->
    Line = binary_to_list(Bin),
    IdxEnd = string:str(Line, ?CRLF),
    if
        IdxEnd > 0 ->
            Code= list_to_integer(string:substr(Line, 1, 3)),
            if
				%%welcome message
                Code =:= 220 ->    
                    {next, Code}; 
				%%operation okk	
                Code =:= 250 ->    
                    {next, Code};
				%%authoration required	
                Code =:= 334 ->     
                    {next, Code};
				%%authoration success	
                Code  =:= 235 ->    
                    {next, Code};
				%%wait for send email	
                Code =:= 354 ->  
                    {next, Code};
				%%quit success	
                Code =:= 221 -> 
                    {next, Code};
				%%authoration fail	
                Code =:= 535 ->     
                    {error, Code};
				%%username is invalid	
                Code =:= 550 ->  
                    {error, Code};
				%%email address not exist	
                Code =:= 501 ->  
                    {error, Code};
				%%other error	
                true ->
                    {error, Code}
            end;
		%%response incomplete	
        true->
            continue
    end.

%% Send hello To SmtpServer
smtp_request_hello(Say, Socket, HostName)->
    Hello = case Say of
        helo ->
            "HELO " ++ HostName ++ ?CRLF;
        ehlo ->
            "EHLO " ++ HostName ++ ?CRLF;
        _ ->
            "HELO " ++ HostName ++ ?CRLF
    end,
    socket_send(Socket, Hello).

%%send auth login
smtp_request_auth(Socket)->
    Auth = "AUTH LOGIN" ++ ?CRLF,
    socket_send(Socket, Auth).
    
%%send username, which will be encode in base64
smtp_request_auth_username(Socket, UserName)->
    CmdUserName = base64:encode_to_string(UserName) ++ ?CRLF, 
    socket_send(Socket, CmdUserName).
    
%%send password, which will be encode in base64	
stmp_request_auth_password(Socket, Password)->
    CmdPassword = base64:encode_to_string(Password) ++ ?CRLF,
    socket_send(Socket, CmdPassword).
    
%%send the from email	
stmp_request_from(Socket, From)->
    CmdFrom = "MAIL FROM: <" ++ From ++ ">" ++ ?CRLF,
    socket_send(Socket, CmdFrom).
   
%%senf the to message   
stmp_request_to(Socket, To)->
    CmdTo = "RCPT TO: <" ++ To ++ ">" ++ ?CRLF,
    socket_send(Socket, CmdTo).
 
%%send the email content sent request 
smtp_request_data(Socket)->
    Data = "DATA" ++ ?CRLF,
    socket_send(Socket, Data).

%%format the email content and send it
smtp_request_mail(Socket, From, To, Subject, Body, Attachment)->
    Timestamp = integer_to_list(timestamp()),
    Boundary = "=====SiteView" ++ Timestamp ++ "_=====",
	%%From address
    MsgFrom = "From: " ++ From ++ ?CRLF,
	%%To address
    MsgTo   =  "To: " ++ To ++ ?CRLF,
	%%subject
    MsgSubject = "Subject: " ++ Subject ++ ?CRLF,
	%%mail box
    MsgXMailer = "X-Mailer: SiteView" ++ ?CRLF,
	%%mime version
    MsgMimeVersion = "MIME-Version: 1.0" ++ ?CRLF,
	%%email date
    MsgDate = "Date: " ++ get_date() ++ ?CRLF,
	%%attachement file is available
    BAttach = filelib:is_file(Attachment),
	
	%%format the content
    MsgContent = if
		%%if have attachment, email must be encoded in mime format 
        BAttach ->  
            FileName = filename:basename(Attachment),
			%%mime require boundary
            ContentType = "Content-Type: multipart/mixed;boundary=\"" ++ Boundary ++ "\"" ++?CRLF ++ ?CRLF,
            Boundary2 = "--" ++ Boundary,
            Depict = "This is a multi-part message in MIME format." ++ ?CRLF ++ ?CRLF ++ Boundary2 ++ ?CRLF,
			%%content type of email body is text
            ContentType1 = "Content-Type: text/plain; charset=gb2312" ++ ?CRLF,
			%%no encoding of body
            ContentTransEncoding = "Content-Transfer-Encoding: " ++ "7bit" ++ ?CRLF ++ ?CRLF,
            BBody = Body ++ ?CRLF ++ ?CRLF ++ Boundary2 ++ ?CRLF,
			%%content type of attachment is text
%%            ContentType2 = "Content-Type: text/plain;charset=us-ascii" ++ ?CRLF, 
            ContentType2 = "Content-Type: application/octet-stream;" ++ ?CRLF, 
			%%for some attachement is not ascii text, format it in base64 encoding
            ContentTransEncoding1 = "Content-Transfer-Encoding: " ++ "base64" ++ ?CRLF,
			%%define the attachement file name
            ContentDisposition = "Content-Disposition: attachment;\r\n    filename=\"" ++ FileName ++ "\"" ++ ?CRLF ++ ?CRLF,
			%%read file and enconding it
            {ok, Bin} = file:read_file(Attachment),
            EncodeBin = base64:encode_to_string(Bin),
			%%return body
            BinAttach = EncodeBin ++ ?CRLF,
            DataEnd = Boundary2 ++ "--" ++ ?CRLF ++ ?DATAEND,
            ContentType ++ Depict ++ ContentType1 ++ ContentTransEncoding ++ BBody 
            ++ ContentType2 ++ ContentTransEncoding1 ++ ContentDisposition ++ BinAttach ++ DataEnd;
		%%no attchment	
        true ->
			%%plain text email will formated
            ContentType = "Content-Type: text/plain;\r\n\tcharset=gb2312" ++ ?CRLF,
            ContentTransferEncoding = "Content-Transfer-Encoding: " ++ "7bit" ++ ?CRLF ++ ?CRLF,
            ContentType ++ ContentTransferEncoding ++ Body ++ ?DATAEND
        end,
	%%send the email content	
    DATA = MsgDate ++ MsgFrom ++ MsgTo ++ MsgSubject ++ MsgXMailer ++ MsgMimeVersion ++ MsgContent,
    socket_send(Socket, DATA).

%%send quit command
smtp_request_quit(Socket)->
    Quit = "QUIT" ++ ?CRLF,
    socket_send(Socket, Quit).

socket_send(Socket, Data)->
    Ret = gen_tcp:send(Socket, list_to_binary(Data)),
    if
        Ret =:= ok ->
            ok;
        true ->
            Ret
    end.

%%format the date string, which defined by smtp
get_date()->
    Now = erlang:now(),
    Local = calendar:now_to_local_time(Now),
    Utc = calendar:now_to_datetime(Now),
    DiffMinute = (calendar:datetime_to_gregorian_seconds(Utc) - calendar:datetime_to_gregorian_seconds(Local)) div 60,
    Timezone = abs(DiffMinute div 60),
    STimezone = integer_to_list(Timezone),
    {{Year, Month, Day},{Hour, Minute, Second}} = Local,
    WeekNum = calendar:day_of_the_week(Year, Month, Day),
    {value, {_,Sweek}} = lists:keysearch(WeekNum, 1, ?Weeks),
    {value, {_, Smonth}} = lists:keysearch(Month, 1, ?Months),
    Sday = integer_to_list(Day),
    Syear = integer_to_list(Year),
    SHour = integer_to_list(Hour),
    SMinute = integer_to_list(Minute),
    SSecond = integer_to_list(Second),
    T = Sweek ++ ", " ++ Sday ++ " " ++ Smonth ++ " " ++  Syear ++ " " ++ SHour ++ ":" ++ SMinute ++ ":" ++ SSecond,
    if
        DiffMinute < 0 ->
            T ++ " +0" ++ STimezone ++ "00";
        true ->
            T ++ " -0" ++ STimezone ++ "00"
    end.

%%-----------------------------------POP3 Section------------------------------------------------------------
pop3_test() ->
    pop3_message_read("mail.dragonflow.com", 110, "jian.huang", "iamcoming", "LL", 60).
    
%% @spec pop3_message_read(Pop3Server, Pop3Port, UserName, Password, ContentMatch, Timeout)-> {error,Reason} | ok
%% @type Pop3Server = string()
%% @type Pop3Port = integer()
%% @type UserName = string()
%% @type Password =  string()
%% @type ContentMatch = string()
%% @type Timeout = integer()
%% @doc receive mail use pop3 protocol
%%	Pop3Server is the hostname or ip of the POP3 server
%%	Pop3Port is the listen port of POP3 service
%%  Username and Password is option, they can be null
%%  ContentMacth is the content macth to email body or title
pop3_message_read(Pop3Server, Pop3Port, UserName, Pass, ContentMatch, Timeout)->
	%%connect to pop3 server
    Ret = gen_tcp:connect(Pop3Server, Pop3Port, [binary, {packet, 0}]),
    case Ret of
        {ok, Socket} ->
			%%wait for pop3 welcome message
            RetConnect = pop3_receive(Socket, [], connect, 0, 0, Timeout*1000),
            case RetConnect of
                {ok, {connect, _}} ->
					%%welcome message come, do next handle
                    Result = pop3_message_read(login, null, [null], Socket, UserName, Pass, ContentMatch, Timeout*1000),
                    gen_tcp:close(Socket),
                    Result;
                {error,{connect, Data}} ->
					%%pop3 not accept the connection
                    {error, Data};
                _ ->
					%%unknow error
                    RetConnect
            end;
        _ ->
		%%tcp connection error 
        {error, "can't connect to pop3 server " ++ Pop3Server ++ " at " ++ integer_to_list(Pop3Port) ++ " port"}
    end.

%%further handle with the receive file
pop3_message_read(State, Param, [L|W], Socket, UserName, Pass, ContentMatch, Timeout)->
	%%we handle the stata machine of session
    case State of
		%%slogin status
        login ->
			%%send the username and password
            Ret = pop3_send_login(Socket, UserName, Pass, Timeout),
            case Ret of
                {ok, _} ->
					%%begin to send the list command
                    pop3_message_read(list, null,  [null], Socket, null, null, ContentMatch, Timeout);
                {error, Data} ->
                    {error, Data};
                _ ->
                    Ret
            end;
		%%list status
        list ->
			%%send list command
            Ret = pop3_send_list(Socket, Timeout),
            case Ret of
                {ok, MailList} ->
                    if
						%%no mail here
                        MailList =:= [] ->
                            ok;
                        true ->
							%%begin to send the receive email command	
                            pop3_message_read(retr, null, MailList, Socket, null, null, ContentMatch, Timeout)
                    end;
                {error, Data} ->
                    {error, Data};
                _ ->
                    Ret
            end;
		%%send the receive email command, if success, begin to read email content
        retr ->
            {MailIdx, _} = L,
			%%send the receive email command
            Ret = pop3_send_retr(Socket, MailIdx, Timeout),
            case Ret of
                {ok, Data} ->
					%%mail content received, match the content
                    case match_content(ContentMatch, Data) of
                        true ->
							%%match it, begin to delete the mail
                            pop3_message_read(dele, MailIdx, [null], Socket, null, null, ContentMatch, Timeout);
                        false ->
							%%fail to match 
                            if
								%%if there are left mail, recieve continue
                                W /= [] ->
                                    pop3_message_read(retr, null, W, Socket, null, null, ContentMatch, Timeout);
								%%no mail, left the pop3 session
                                true ->
									{error, "Content match error"}
                                   
                            end
                    end;
                {error, Data} ->
                    {error, Data};
                _ ->
                    Ret
            end;
		%%delete status	
        dele ->
			%%send the delete message
            Ret = pop3_send_dele(Socket, Param, Timeout),
            case Ret of
                {ok, _} ->
                     pop3_message_read(quit, null, [null], Socket, null, null, null, Timeout);
                {error, Data} ->
                    {error, Data};
                _ ->
                    Ret
            end;
		%%quit status	
        quit ->
			%%send quit command
            pop3_send_quit(Socket, Timeout),
            ok;
        _ ->
            State
    end.

%%receive and parse the response from pop3 server
pop3_receive(Socket, Data, Operate, LParam, WParam, Timeout) ->
    receive
        {tcp, Socket, Bin} ->
			%%concat the new and old message
            Sofar = [Bin|Data],
            Bin2 = list_to_binary(lists:reverse(Sofar)),
			%%analyse the response
            OK = pop3_response_analyze(Bin2, LParam),
            case OK of
				%%operation ok
                {ok, Line} ->
                    {ok, {Operate, Line}};
				%%operation error	
                {err, Line} ->
                    {error, {Operate, Line}};
				%%message is not complete, receive continue	
                continue ->
                    pop3_receive(Socket, Sofar, Operate, LParam, WParam, Timeout)
            end;
		%%tcp connection closed	
        {tcp_closed, Socket} ->
            {error, "tcp_closed"}
	%%timeout		
    after Timeout ->
        {error, "timeout"}
    end.

%%analyse the response
pop3_response_analyze(Bin, Param)->
    Line = binary_to_list(Bin),
    case Param of
		%%in mode of receive the email, we are care of three status 
        dot ->
			%%there are only 2 response status: OK or Error
			%%In addtion, we have also to determinate the end of email
			LineSize = length(Line),
			if
				LineSize > 5 ->
					Line1 = string:substr(Line, 1,  5);
				true ->
					Line1 = Line
			end,
            IdxOK = string:str(Line1, ?POP3OK),
            IdxEnd = string:str(Line, ?POP3DATAEND),
            IdxERR = string:str(Line, ?POP3ERR),
            if
                IdxOK > 0, IdxEnd > 0->
                    {ok, Line};
                IdxERR > 0 ->
                    {err, Line};
                IdxEnd =:= 0 ->
                    continue;
                true ->
                    {err, Line}
            end;
		%%in mode of command, we only are care of command response  	
        _ ->
            IdxOK = string:str(Line, ?POP3OK),
            IdxERR = string:str(Line, ?POP3ERR),
            if
                IdxOK >0 ->
                    {ok, Line};
                IdxERR > 0 ->
                    {err, Line};
                true ->
                    {err, Line}
            end
    end.

%%send the username and password
pop3_send_login(Socket, UserName, Password, Timeout) ->
	%%send username
    UserNameReq = "USER " ++ UserName ++ ?CRLF,
    socket_send(Socket, UserNameReq),
	%%receive response
    Ret = pop3_receive(Socket, [], user, 0, 0, Timeout),
    case Ret of
        {ok, {user, _}} ->
			%%username ok, send the password
            UserPassReq = "PASS " ++ Password ++ ?CRLF,
            socket_send(Socket, UserPassReq),
			%%receive response
            Ret2 = pop3_receive(Socket, [], pass, 0, 0, Timeout),
			%%return status
            case Ret2 of
                {ok, {pass, Data2}} ->
                    {ok, Data2};
                {error, {pass, Data2}} ->
                    {error, Data2};
                _ ->
                    Ret2
            end;
        {error, {user, Data}} ->
            {error, Data};
        {error, Data} ->
            {error, Data}
    end.

%%send stat command
pop3_send_stat(Socket, Timeout) ->
    StatReq = "STAT" ++ ?CRLF,
    socket_send(Socket, StatReq),

	Ret = pop3_receive(Socket, [], stat, 0, 0, Timeout),
    case Ret of
        {ok, {stat, Data}}->
			%%get mail number and total size
            TagList = string:tokens(Data, " "),
            MailNum = list_to_integer(lists:nth(2, TagList)),
            MailBytes = num_string(lists:nth(3, TagList)),
            {ok, MailNum, MailBytes};
        {error, {stat, Data}} ->
            {error, Data};
        {error, Data} ->
            {error, Data}
    end.

%%send quit command
pop3_send_quit(Socket, _Timeout) ->
    QuitReq = "QUIT" ++ ?CRLF,
    socket_send(Socket, QuitReq).

%%send list command, only list defininte mail
pop3_send_list(Socket, I, Timeout) ->
    ListReq = "LIST " ++ integer_to_list(I) ++ ?CRLF,
    socket_send(Socket, ListReq),
	
    Ret = pop3_receive(Socket, [], list, dot, 0, Timeout),
    case Ret of
        {ok, {list, Data}}->
			%%get the mail list
            TagList1 = string:tokens(Data, "\r\n"),
			[_|TagList2] = TagList1,
            TagList3 = TagList2 -- ["."],
            TupleList = list_item_parse(TagList3),
            {ok, TupleList};
        {error, {list, Data}}->
            {error, Data};
        {error, Data} ->
            {error, Data}
    end.

%%send list command, list all mail
pop3_send_list(Socket, Timeout) ->
    ListReq = "LIST " ++ ?CRLF,
    socket_send(Socket, ListReq),
	
    Ret = pop3_receive(Socket, [], list, dot, 0, Timeout),
    case Ret of
        {ok, {list, Data}} ->
			%%get mail list
            TagList1 = string:tokens(Data, "\r\n"),
			[_|TagList2] = TagList1,
            TagList3 = TagList2 -- ["."],
            TupleList = list_item_parse(TagList3),
            {ok, TupleList};
        {error, {list, Data}}->
            {error, Data};
        {error, Data} ->
            {error, Data}
    end.

%%send uidl command
pop3_send_uidl(Socket, I, Timeout) ->
    UidlReq = "UIDL " ++ integer_to_list(I) ++ ?CRLF,
    socket_send(Socket, UidlReq),
    Ret = pop3_receive(Socket, [], uidl, 0, 0, Timeout),
    case Ret of
        {ok, {uidl, Data}} ->
            {ok, Data};
        {error, {uidl, Data}} ->
            {error, Data};
        {error, Data} ->
            {error, Data}
    end.

%%send uidl command
pop3_send_uidl(Socket, Timeout) ->
    UidlReq = "UIDL " ++ ?CRLF,
    socket_send(Socket,UidlReq),
	
    Ret = pop3_receive(Socket, [], uidl, 0, 0, Timeout),
    case Ret of
        {ok, {uidl, Data}} ->
            {ok, Data};
        {error, {uidl, Data}} ->
            {error, Data};
        {error, Data} ->
            {error, Data}
    end.

%%send receive command, I is the mailid
pop3_send_retr(Socket, I, Timeout) ->
    RetrReq = "RETR " ++ integer_to_list(I) ++ ?CRLF,
    socket_send(Socket, RetrReq),
	
    Ret = pop3_receive(Socket, [], retr, dot, 0, Timeout),
    case Ret of
        {ok, {retr, Data}} ->
            {ok, Data};
        {error, {retr, Data}} ->
            {error, Data};
        {error, Data} ->
            {error, Data}
    end.

%%send top command
pop3_send_top(Socket, I, J, Timeout) ->
    TopReq = "TOP " ++ integer_to_list(I) ++ " " ++  integer_to_list(J) ++ ?CRLF,
    socket_send(Socket, TopReq),
	
    Ret = pop3_receive(Socket, [], top, 0, 0, Timeout),
    case Ret of
        {ok, {top, Data}} ->
            {ok, Data};
        {error, {top, Data}} ->
            {error, Data};
        {error, Data} ->
            {error, Data}
    end.

%%send top command
pop3_send_top(Socket, I, Timeout) ->
    TopReq = "TOP " ++ integer_to_list(I) ++ ?CRLF,
    socket_send(Socket, TopReq),
    Ret = pop3_receive(Socket, [], top, 0, 0, Timeout),
    case Ret of
        {ok, {top, Data}} ->
            {ok, Data};
        {error, {top, Data}} ->
            {error, Data};
        {error, Data} ->
            {error, Data}
    end.

%%send delete command, I is the mailid
pop3_send_dele(Socket, I, Timeout) ->
    DeleReq = "DELE " ++ integer_to_list(I) ++ ?CRLF,
    socket_send(Socket, DeleReq),
	
    Ret = pop3_receive(Socket, [], dele, 0, 0, Timeout),
    case Ret of
        {ok, {dele, Data}} ->
            {ok, Data};
        {error, {dele, Data}} ->
            {error, Data};
        {error, Data} ->
            {error, Data}
    end.

%%send reset command
pop3_send_rset(Socket, Timeout) ->
    RsetReq = "RSET" ++ ?CRLF,
    socket_send(Socket, RsetReq),
	
    Ret = pop3_receive(Socket, [], rset, dot, 0, Timeout),
    case Ret of
        {ok, {rset, Data}} ->
            {ok, Data};
        {error, {rset, Data}} ->
            {error, Data};
        {error, Data} ->
            {error, Data}
    end.

%%send noop command
pop3_send_noop(Socket, Timeout) ->
    NoopReq = "NOOP" ++ ?CRLF,
    socket_send(Socket, NoopReq),
	
    Ret = pop3_receive(Socket, [], noop, 0, 0, Timeout),
    case Ret of
        {ok, {noop, Data}} ->
            {ok, Data};
        {error, {noop, Data}} ->
            {error, Data};
        {error, Data} ->
            {error, Data}
    end.
	

%%---------------------------------IMAP4-------------------------------------------------------------------
imap4_test() ->
    imap4_message_read("mail.dragonflow.com", ?IMAP4PORT, "jian.huang", "iamcoming", "123", 60).
    
%% @spec Imap4_message_read(Imap4Server, Imap4Port, UserName, Password, ContentMatch, Timeout)-> {error,Reason} | ok
%% @type Imap4Server = string()
%% @type Imap4Port = integer()
%% @type UserName = string()
%% @type Password =  string()
%% @type ContentMatch = string()
%% @type Timeout = integer()
%% @doc receive mail use imap4 protocol
%%	Imap4Server is the hostname or ip of the IMAP4 server
%%	Imap4Port is the listen port of IMAP4 service
%%  Username and Password is option, they can be null
%%  ContentMacth is the content macth to email body or title
imap4_message_read(Imap4Server, Imap4Port, UserName, Password, ContentMatch, Timeout) ->
	%%connect imap server
    Ret = gen_tcp:connect(Imap4Server, Imap4Port, [binary, {packet, 0}]),
    case Ret of
        {ok, Socket} ->
			%%receive the welcome message, attention: "*" is the prefix of the welcome response message
            RetConnect = imap4_receive(Socket, [], connect, "*", nildot, Timeout*1000),
            case RetConnect of
				%%welcome mesage come
                {ok, {connect, _}} ->
					%%real handle function of imap4
                    Result = imap4_message_read(capability, null, [null], Socket, UserName, Password, ContentMatch, 0, Timeout*1000),
                    gen_tcp:close(Socket),
                    Result;
				%%error happen	
                {error,{connect, Data}} ->
                    {error, Data};
                _ ->
                    RetConnect
            end;
        _ ->
         {error, "can't connect to imap4 server " ++ Imap4Server ++ " at " ++ integer_to_list(Imap4Port) ++ " port"}
    end.

%%defininte status machine handle function of IMAP4
imap4_message_read(State, Param, [_T|_W], Socket, UserName, Password, ContentMatch, TagNum, Timeout) ->
    case State of
		%%capability status
        capability ->
			%%send capability command
            RetCap = imap4_request_capability(Socket, TagNum, Timeout),
            case RetCap of
                {ok, _} ->
					%%success receive capability response, to send login in message
                    imap4_message_read(login, null, [null], Socket, UserName, Password, ContentMatch, TagNum + 1, Timeout);
                {error, Data} ->
                    {error, Data};
                _ ->
                    RetCap
            end;
		%%login status	
        login ->
			%%send username and password
            RetLogin = imap4_request_login(Socket, UserName, Password, TagNum, Timeout),
            case RetLogin of
                {ok, _} ->
					%%success authoration, to send select command
                    imap4_message_read(select, null, [null], Socket, null, null, ContentMatch, TagNum + 1, Timeout);
                {error, Data} ->
                    {error, Data};
                _ ->
                    RetLogin
            end;
		%%slelct status	
        select ->
			%%send the select command
            RetSel = imap4_request_select(Socket, TagNum, Timeout),
            case RetSel of
                {ok, Line} ->
					%%success get the list
					%%look for the mail count, first look for EXISTS keyword
                    IdxExists = string:str(Line, "EXIS"),
                    if
                        IdxExists > 0 -> 
                            MailCount = get_count(Line, IdxExists-1, 0, []),
                            if
								%%no mail here
                                MailCount =:= "0" ->
                                    ok;
                                 true ->
									%%try to get email
                                    imap4_message_read(fetchflags, list_to_integer(MailCount), [null], Socket, null, null, ContentMatch, TagNum + 1, Timeout)
                            end;
                        true ->
                            ok
                    end;
                {error, Data} ->
                    {error, Data};
                _ ->
                    RetSel
            end;
		%%get flag of newest email	
        fetchflags ->
            StrCount = integer_to_list(Param),
			%%send the fecth flag command
            RetFetchFlags = imap4_request_fetch_flags(Socket, StrCount, TagNum, Timeout),
            case RetFetchFlags of
                {ok, _} ->
					%%success get flag, try to get body of newest email
                    imap4_message_read(fetchbody, Param, [null], Socket, null, null, ContentMatch, TagNum + 1, Timeout);
                {error, Data} ->
                    if
						%%something error and there still have email, get the second newest email
                        Param > 1 ->
                            imap4_message_read(fetchflags, Param - 1, [null], Socket, null, null, ContentMatch, TagNum + 1, Timeout);
                        true ->
                            {error, Data}
                    end;
                _ ->
                    RetFetchFlags
            end;
		%%get the body of the mail	
        fetchbody ->
			%%send fetch body message
            RetFetchBody = imap4_request_fetch_body(Socket, integer_to_list(Param), TagNum, Timeout),
            case RetFetchBody of
                {ok, Data} ->
					%%get the body, match the content
                    case match_content(ContentMatch, Data) of
                        true ->
							%%match success, delete the mail
                            imap4_message_read(storedel, Param, [null], Socket, null, null, null, TagNum + 1, Timeout);
                        false ->
							%%fail, to send the storeseen message 
                            imap4_message_read(storeseen, Param, [null], Socket, null, null, ContentMatch, TagNum + 1, Timeout)
                    end;
                {error, Data} ->
                    {error, "Fetch mail error"};
                _ ->
                    RetFetchBody
            end;
		%%delete status	
        storedel ->
			%%send the delete command
            RetStore = imap4_request_store_dele(Socket, integer_to_list(Param), TagNum, Timeout),
            case RetStore of 
                {ok, _} ->
					%%success, send the expunge message
                    imap4_message_read(expunge, Param, [null], Socket, null, null, null, TagNum + 1, Timeout);
                {error, Data}->
                    {error, Data};
                _ ->
                    RetStore
            end;
		%%storseen status	
        storeseen ->
			%%send the storeseen command
             RetStore = imap4_request_store_seen(Socket, integer_to_list(Param), TagNum, Timeout),
             case RetStore of
                {ok, _} ->
                    if
						%%if there still some mail, get the body of the newest email
                        Param > 1 ->
                            imap4_message_read(fetchflags, Param - 1, [null], Socket, null, null, ContentMatch, TagNum + 1, Timeout);
						%%no email, to quit
						true ->
                            imap4_message_read(quit, Param, [null], Socket, null, null, null, TagNum + 1, Timeout)
                    end;
                {error, Data} ->
                    {error, Data};
                _ ->
                    RetStore
             end;
		%%expunge status	 
        expunge ->
			%%send the expunge command
            RetExp = imap4_request_expunge(Socket , TagNum, Timeout),
            case RetExp of
                {ok, _} ->
					%%success, to quit
                    imap4_message_read(quit, Param, [null], Socket, null, null, null, TagNum + 1, Timeout);
                {error, Data} ->
                    {error, Data};
                _ ->
                    RetExp
            end;
		%%send the quit command	
        quit ->
            RetQuit = imap4_request_quit(Socket, TagNum, Timeout),
            case RetQuit of
                {ok, _} ->
                    ok;
                 {error, Data}->
                    {error, Data};
                _ ->
                    RetQuit
            end;
        _ ->
            State
    end.

%%receive imap response message
imap4_receive(Socket, Data, Operate, LParam, WParam, Timeout) ->
    receive
        {tcp, Socket, Bin} ->
			%%concat the content new and old 
            Sofar = [Bin|Data],
            Bin2 = list_to_binary(lists:reverse(Sofar)),
			%%analyse the imap4 message
            OK = imap4_response_analyze(Bin2, LParam, WParam),
            case OK of
				%%operation correct response 
                {ok, Line} ->
                    {ok, {Operate, Line}};
				%%operation fail	
                {err, Line} ->
                    {error, {Operate, Line}};
				%%message not complete	
                continue ->
                    imap4_receive(Socket, Sofar, Operate, LParam, WParam, Timeout)
            end;
		%%connection close	
        {tcp_closed, Socket} ->
            {error, "tcp_closed"}
	%%timeout		
    after Timeout ->
        {error, "timeout"}
    end.

%%analyse the imap4 response message
imap4_response_analyze(Bin, Tag, Param)->
    Line = binary_to_list(Bin),
    case Param of
		%%for command we only care about OK, NO response
		%%diferent from pop3, some prefix must add before OK or NO response, that is Tag
        nildot->
            IdxTag = string:str(Line, Tag),
            IdxOK  = string:str(Line, ?IMAP4OK),
            IdxNO  = string:str(Line, ?IMAP4NO),
            IdxNo = string:str(Line, ?IMAP4No),
            BNO = (IdxNO > 0) or (IdxNo > 0),
            if
				%%Tag exist and error keyword exist, something error
                IdxTag > 0 andalso BNO ->
                    {err, Line};
				%%No Tag found, continue to receive	
                IdxTag =:= 0 ->
                    continue;
				%%No ok found continue	
                IdxOK =:= 0 ->
                    continue;
				%%Tag and ok all exsit, correct response message	
                IdxTag > 0 andalso IdxOK > 0 ->
                    {ok, Line};
				%%something error	
                true ->
                    {err, Line}
            end;
		%%for recieve mail process, we also care the end of email body	
        dot ->
            IdxTag = string:str(Line, Tag),
            IdxEnd = string:str(Line, ?IMAP4DATAEND),
            IdxOK  = string:str(Line, ?IMAP4OK),
			LineSize = length(Line),
			if
				LineSize > 10 ->
					Line1 = string:substr(Line, 1,  10);
				true ->
					Line1 = Line
			end,
            IdxNO  = string:str(Line1, ?IMAP4NO),
            IdxNo = string:str(Line1, ?IMAP4No),
            BNO = (IdxNO > 0) or (IdxNo > 0),
            if
				%%Tag exist and error keyword exist, something error
                IdxTag > 0 andalso BNO ->
                    {err, Line};
				%%No Tag found, continue to receive	
                IdxEnd =:= 0 ->
                    continue;
				%%No ok found continue	
                IdxTag =:= 0 ->
                    continue;
				%%No ok found continue	
                IdxOK =:= 0 ->
                    continue;
				%%Tag and ok all exsit, correct response message	
                IdxTag > 0 andalso IdxOK > 0 andalso IdxEnd > 0 ->
                    {ok, Line};
				%%something error	
                true ->
                    {err, Line}
            end
    end.

%%send the capability command
imap4_request_capability(Socket, TagNum, Timeout)->
	%%the prefix of imap4 command must increment 1 each time
	%%format the prefix
    Tag = integer_to_list(TagNum),
    TAG = "A" ++ Tag ++ " ",

	%%format the capability command
	CAP = "CAPABILITY" ++ ?CRLF,
    CapReq = TAG ++ CAP,
	
	%%send the capability command
    socket_send(Socket, CapReq),
	
	%%receive response
    Ret = imap4_receive(Socket, [], capability, TAG, dot, Timeout),
    case Ret of
        {ok, {capability, Data}} ->
            {ok, Data};
        {error, {capability, Data}} ->
            {error, Data};
        {error, Data} ->
            {error, Data}
    end.

%%send login command
imap4_request_login(Socket, UserName, Password, TagNum, Timeout) ->
	%%the prefix of imap4 command must increment 1 each time
	%%format the prefix
    Tag = integer_to_list(TagNum),
    TAG = "A" ++ Tag ++ " ",
	
	%%format the login command
    LOGIN = "LOGIN " ++ UserName ++ " " ++ Password ++ ?CRLF,
    LoginReq = TAG ++ LOGIN,
	
	%%send the login command
    socket_send(Socket, LoginReq),
	
	%%receive response
    Ret = imap4_receive(Socket, [], login, TAG, dot, Timeout),
    case Ret of
        {ok, {login, Data}} ->
            {ok, Data};
        {error, {login, Data}} ->
            {error, Data};
        {error, Data} ->
            {error, Data}
    end.

imap4_request_select(Socket, TagNum, Timeout) ->
	%%the prefix of imap4 command must increment 1 each time
	%%format the prefix
    Tag = integer_to_list(TagNum),
    TAG = "A" ++ Tag ++ " ",
   
	%%format the select command
	SELECT = "SELECT INBOX" ++ ?CRLF,
    ReqSel = TAG ++ SELECT,

	%%send the select command
	socket_send(Socket, ReqSel),
	
	%%receive response
    Ret = imap4_receive(Socket, [], select, TAG, dot, Timeout),
    case Ret of
        {ok, {select, Data}} ->
            {ok, Data};
        {error, {select, Data}} ->
            {error, Data};
        {error, Data} ->
            {error, Data}
    end.

imap4_request_fetch_flags(Socket, I, TagNum, Timeout) ->
	%%the prefix of imap4 command must increment 1 each time
	%%format the prefix
    Tag = integer_to_list(TagNum),
    TAG = "A" ++ Tag ++ " ",

	%%format the fetch flag command
	FETCHFLAGS = "FETCH " ++ I ++ " FLAGS" ++ ?CRLF,
    FetchFlagsReq = TAG ++ FETCHFLAGS,

	%%send the fetch flag command
	socket_send(Socket, FetchFlagsReq),
	
	%%receive response
    Ret = imap4_receive(Socket, [], fetchflags, TAG, dot, Timeout),
    case Ret of
        {ok, {fetchflags, Data}} ->
            {ok, Data};
        {error, {fetchflags, Data}} ->
            {error, Data};
        {error, Data} ->
            {error, Data}
    end.

imap4_request_fetch_body(Socket, I, TagNum, Timeout) ->
	%%the prefix of imap4 command must increment 1 each time
	%%format the prefix
    Tag = integer_to_list(TagNum),
    TAG = "A" ++ Tag ++ " ",

	%%format the fetch body command
	FETCHBODY = "FETCH " ++ I ++ " BODY[TEXT]" ++ ?CRLF,
    FetchBodyReq = TAG ++ FETCHBODY,

	%%send the fetch body command
	socket_send(Socket, FetchBodyReq),
	
	%%receive response
    Ret = imap4_receive(Socket, [], fetchbody, TAG, dot, Timeout),
    case Ret of
        {ok, {fetchbody, Data}} ->
            {ok, Data};
        {error, {fetchbody, Data}} ->
            {error, Data};
        {error, Data} ->
            {error, Data}
    end.

imap4_request_store_dele(Socket, I, TagNum, Timeout) ->
	%%the prefix of imap4 command must increment 1 each time
	%%format the prefix
    Tag = integer_to_list(TagNum),
    TAG = "A" ++ Tag ++ " ",

	%%format the delete command
	STORE = "STORE " ++ I ++ " +FLAGS " ++ "(\\deleted)" ++ ?CRLF,
    StoreReq = TAG ++ STORE,

	%%send the delete command
	socket_send(Socket, StoreReq),
	
	%%receive response
    Ret = imap4_receive(Socket, [], storedel, TAG, dot, Timeout),
        case Ret of
        {ok, {storedel, Data}} ->
            {ok, Data};
        {error, {storedel, Data}} ->
            {error, Data};
        {error, Data} ->
            {error, Data}
    end.

imap4_request_store_seen(Socket, I, TagNum, Timeout) ->
	%%the prefix of imap4 command must increment 1 each time
	%%format the prefix
    Tag = integer_to_list(TagNum),
    TAG = "A" ++ Tag ++ " ",

	%%format the store seen command
	STORE = "STORE " ++ I ++ " -FLAGS " ++ "(\\Seen)" ++ ?CRLF,
    StoreReq = TAG ++ STORE,

	%%send the store seen command
	socket_send(Socket, StoreReq),
	
	%%receive response
    Ret = imap4_receive(Socket, [], storeseen, TAG, dot, Timeout),
        case Ret of
        {ok, {storeseen, Data}} ->
            {ok, Data};
        {error, {storeseen, Data}} ->
            {error, Data};
        {error, Data} ->
            {error, Data}
    end.

imap4_request_expunge(Socket, TagNum, Timeout) ->
	%%the prefix of imap4 command must increment 1 each time
	%%format the prefix
    Tag = integer_to_list(TagNum),
    TAG = "A" ++ Tag ++ " ",

	%%format the expunge command
	EXPUNGE = "EXPUNGE" ++ ?CRLF,
    ExpReq = TAG ++ EXPUNGE,

	%%send the expunge command
	socket_send(Socket, ExpReq),
	
	%%receive response
    Ret = imap4_receive(Socket, [], expunge, TAG, dot, Timeout),
        case Ret of
        {ok, {expunge, Data}} ->
            {ok, Data};
        {error, {expunge, Data}} ->
            {error, Data};
        {error, Data} ->
            {error, Data}
    end.

imap4_request_quit(Socket, TagNum, Timeout)->
	%%the prefix of imap4 command must increment 1 each time
	%%format the prefix
    Tag = integer_to_list(TagNum),
    TAG = "A" ++ Tag ++ " ",

	%%format the quit command
	LOGOUT = "LOGOUT" ++ ?CRLF,
    QuitReq = TAG ++ LOGOUT,

	%%send the quit command
	socket_send(Socket, QuitReq),
	
	%%receive response
    Ret = imap4_receive(Socket, [], quit, TAG, dot, Timeout),
        case Ret of
        {ok, {quit, Data}} ->
            {ok, Data};
        {error, {quit, Data}} ->
            {error, Data};
        {error, _} ->
            {ok, "BYE"}
    end.   

%%-----------------------------utils--------------------------------------------
%%if the content match
match_content(Match, Bin) ->
    if
		%%match pattern null, return ok
        Match =:= "" ->
            true;
        true ->
            Idx = string:str(Bin, Match),
            if
				%%contain match pattern
                Idx > 0 ->
                    true;
				%%no match	
                true ->
                     false
            end
    end.
	
%%change string to number, trim space in tne string	
num_string(S)->
    num_string(S, []).
num_string([], L)->L;
num_string([L|W], SNum)->
    if
        L >= 48 ->
            if
                L =< 59 ->
                    num_string(W, SNum ++ [L]);
                true ->
                    num_string(W, SNum)
            end;
        true ->
            num_string(W, SNum)
    end.
	
%%change the mail list string to a formated erlang list	
list_item_parse(L) ->
    list_item_parse(L, []).
list_item_parse([], Sum)->Sum;
list_item_parse([L|W], Sum) ->
    Tag = string:tokens(L, " "),
    Num = lists:nth(1, Tag),
    Bytes = lists:nth(2, Tag),
    Tuple =[{list_to_integer(Num), list_to_integer(Bytes)}],
    list_item_parse(W, Sum ++ Tuple).
	
timestamp() ->
	{A,B,C}=erlang:now(),
	round((A*1000000+B)*1000+C/1000).

%%get email count in select response message
get_count(Line, IdxExists, Flag, List) ->
	if
		IdxExists > 0 ->
			Char = lists:nth(IdxExists, Line),
			if 
				Char >= 48 andalso Char  =< 59 ->
					List1 = [Char] ++ List,
					get_count(string:substr(Line, 1, IdxExists-1), IdxExists-1, Flag, List1);
				Flag =:= 0 ->
					Flag1 = 1,
					get_count(string:substr(Line, 1, IdxExists-1), IdxExists-1, Flag1, List);
				Flag =:= 1 ->
					List;
				true ->
					"0"
			end;
		true -> 
			"0"
	end.
	