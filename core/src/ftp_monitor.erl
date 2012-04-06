
%% @copyright 2008-2009 Dragonflow
%% @author Naindi zhou, ZhouJian Huang <jian.huang@dragonflow.com>
%% @version 1.0

%% @doc ftp monitor
%%
%%This module is to test ftp service:
%%1. test the connection of telnet service
%%2. get the destination file from the ftp server
%%3. match the file content with user input string
%%4. reset, remember or match the checksum of the file

-module(ftp_monitor, [BASE]).
-compile(export_all).
-extends(atomic_monitor).
-include("monitor.hrl").
-include("monitor_template.hrl").

%% @spec new() -> Obj
%% @type Obj = term()
%% @doc create a new instance for ftp monitor
new()->
	Base = atomic_monitor:new(),
    Base:set_attribute(lastChecksum, -1),
	Base:set_attribute(lastCheckContentTime, 0),
	Base:set_attribute(lastCheckContentSize, 0),
    Base:set_attribute(checkContentResetTime, 0),
	Base:set_attribute(status, 0),
	{?MODULE,Base}.
    
%% @spec defaultTitle(Params) -> List
%% @type Params = [term()]
%% @type List = string()
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	BASE:defaultTitle(Params) ++ ": " ++ case proplists:get_value(ftpserver,Params) of undefined->"";V->V end.

%% @spec update() -> Result
%% @type Result = term()
%% @doc update is the run function called by schedule to test the  ftp service
update()->
	%%get property of the monitor
    {ok, {_, FtpServer}}          = THIS:get_property(ftpserver),
    {ok, {_, UserName}}           = THIS:get_property(username),
    {ok, {_, Password}}           = THIS:get_property(password),
    {ok, {_, Timeout}}            = THIS:get_property(timeout),
    {ok, {_, Proxy}}              = THIS:get_property(ftpproxy),
    {ok, {_, Mode}}               = THIS:get_property(mode),
    {ok, {_, ProxyUserName}}      = THIS:get_property(proxyusername),
    {ok, {_, ProxyPassword}}      = THIS:get_property(proxypassword),
    {ok, {_, File}} 	  	  = THIS:get_property(file), 

	THIS:set_attribute(status, 0),
	THIS:set_attribute(roundTripTime, 0),
	THIS:set_attribute(size, 0),
	THIS:set_attribute(matchvalue, "False"),
	
		%%get hostname and port from server string
		case string:tokens(FtpServer, ":") of
			[A, B] ->
				Server = A,
				Port1 = B;
			[A] ->
				Server = A,
				Port1 = 21;
			_ ->
				Server = FtpServer,
				Port1 = 21
		end,
		case inet:getaddr(Server, inet) of 
			{ok, Server1} ->
				ok;
			{error, _} ->
				Server1 = {0,0,0,0}
		end,
		
		case Proxy of
		%%connect to ftp server directly	
        ""->
			%%make a new connection to ftp server
			StartTime = now(),
            T = ftp:new([{timeout, Timeout*1000}, {mode, get_mode(Mode)}]),
            Session = T:open_session(Server1, Port1),
            case Session of
                {connected,_}->
					%%send login code to ftp server
                    LoginRet = T:ftp_login(Session, UserName, Password),
                    case LoginRet of
                        ok->
							%%download the file and check content of the file
                            ftp_check_file(T, Session, StartTime);
                        {error, Reason}->
							%%login error
                            THIS:set_attributes(0, error, Reason),
                            T:close_session(Session)
                    end;
				%%connection with ftp server error	
                {error, _Reason}->
                    THIS:set_attributes(0, error, "unable to connect to server")
            end;
		%%use proxy to connect ftp server
        _->
			%%star inet module
			inets:start(),
			{ok, Pid} = inets:start(httpc, [{profile, list_to_atom(pid_to_list(self()))}]),
			
			%%format the url string
			Url = lists:concat(["ftp://", UserName, ":", Password, "@", FtpServer, "/", iconv:convert("utf-8", "gbk", File)]),
			StartTime = now(), 
			%%get proxy ip and port
			{ok, Addr, Port} = analyze_proxy(Proxy),
			%%use http  to get file
			http:set_options([{proxy, {{Addr, Port}, ["localhost"]}}], list_to_atom(pid_to_list(self()))),
			case http:request(get, {Url, []}, [{timeout, Timeout*1000}, {proxy_auth, {ProxyUserName, ProxyPassword}}], [{body_format, binary}], list_to_atom(pid_to_list(self()))) of
				%%get http response
				{ok, {Status, Body}} ->
					inets:stop(httpc, Pid),
					verify_stat(Status, Body, StartTime);
				{ok, {{_, Status, _}, _Headers, Body}} ->
					inets:stop(httpc, Pid),
					verify_stat(Status, Body,StartTime);
				{error, Reason} ->
					inets:stop(httpc, Pid),
                    THIS:set_attributes(0, error, atom_to_list(Reason))
			end
    end.

verify_stat(Status, Body, StartTime) ->
    {ok, {_, ContentMatch}}       = THIS:get_property(contentmatch),
    {ok, {_, Checkcontent}}       = THIS:get_property(pcheckcontent),
	case Status of
		%%200 means get file
		200 ->
			EndTime = now(),
			RoundTime = timer:now_diff(EndTime, StartTime),			
            case match_content(ContentMatch, Body) of
                true->
					%%compare checksum of the file with stored checksum
                    CheckRet = ftp_check_content(Checkcontent, Body),
                    case CheckRet of
                        error->
							%%checksum error
                            THIS:set_attributes(0, error, "contents changed");
                        ok->
							%%checksum ok
                            Status1 = get_status_string(Body, RoundTime),
                            THIS:set_attribute(0, good, Status1),
							THIS:set_attribute(status, 200),
							THIS:set_attribute(size, size(Body)),
							THIS:set_attribute(matchvalue, "True"),
							THIS:set_attribute(roundTripTime, RoundTime/1000)
                    end;
                _->
					%%match content error
                    THIS:set_attributes(0, error, "content match error")
            end;		
		Reason ->
            THIS:set_attributes(0, error,  reason_phrase(Reason))
	end.

%%http status transform
reason_phrase(100) ->   "Continue";
reason_phrase(101) ->   "Switching Protocols" ;
reason_phrase(200) ->   "OK" ;
reason_phrase(201) ->   "Created" ;
reason_phrase(202) ->   "Accepted" ;
reason_phrase(203) ->   "Non-Authoritative Information" ;
reason_phrase(204) ->   "No Content" ;
reason_phrase(205) ->   "Reset Content" ;
reason_phrase(206) ->   "Partial Content" ;
reason_phrase(300) ->   "Multiple Choices" ;
reason_phrase(301) ->   "Moved Permanently" ;
reason_phrase(302) ->   "Moved Temporarily" ;
reason_phrase(303) ->   "See Other" ;
reason_phrase(304) ->   "Not Modified" ;
reason_phrase(305) ->   "Use Proxy" ;
reason_phrase(306) ->   "(unused)" ;
reason_phrase(307) ->   "Temporary Redirect" ;
reason_phrase(400) ->   "Bad Request";
reason_phrase(401) ->   "Unauthorized";
reason_phrase(402) ->   "Payment Required";
reason_phrase(403) ->   "Forbidden" ;
reason_phrase(404) ->   "Object Not Found" ;
reason_phrase(405) ->   "Method Not Allowed" ;
reason_phrase(406) ->   "Not Acceptable" ;
reason_phrase(407) ->   "Proxy Authentication Required" ;
reason_phrase(408) ->   "Request Time-out" ;
reason_phrase(409) ->   "Conflict" ;
reason_phrase(410) ->   "Gone" ;
reason_phrase(411) ->   "Length Required" ;
reason_phrase(412) ->   "Precondition Failed" ;
reason_phrase(413) ->   "Request Entity Too Large" ;
reason_phrase(414) ->   "Request-URI Too Large" ;
reason_phrase(415) ->   "Unsupported Media Type" ;
reason_phrase(416) ->   "Requested Range Not Satisfiable" ;
reason_phrase(417) ->   "Expectation Failed" ;
reason_phrase(500) ->   "Internal Server Error" ;
reason_phrase(501) ->   "Not Implemented" ;
reason_phrase(502) ->   "Bad Gateway" ;
reason_phrase(503) ->   "Service Unavailable" ;
reason_phrase(504) ->   "Gateway Time-out" ;
reason_phrase(505) ->   "HTTP Version not supported";
reason_phrase(_)   ->   "Unkown Error".


%% @spec getScalarValues(Prop, Params) -> List
%% @type Prop = atom()
%% @type Params = [term()]
%% @type List = [Opt]
%% @type Opt = {ShowContent, Value}
%% @type ShowContent = string()
%% @type Value = string()
%% @doc getScalarValues is the function called by schedule to show a dropdown list box and its value
%%	Prop is the element defined in get_template_property()
%%  ShowContent is the string show in draopdown box
%%	Value id the value, which will be returned when user choose one item in dropdown box
%%  In ftp monitor wen define one dropdown box for user to set the content check method. it contains four items:
%%		"nil" - no content check will be check
%%		"on" -  content check will be going on, in which checksum will be reset every time 
%%		"baseline" - content check will be going on, in which stord checksum will be compared
%%		"reset" - recalculate the checksum and stord it 
getScalarValues(Prop, Params) ->
	case Prop of
		pcheckcontent ->
			[{"no content checking", "nil"}, {"compare to last contents", "on"}, {"compare to saved contents", "baseline"},{"reset saved contents", "reset"}];
		_ ->
			BASE:getScalarValues(Prop, Params)
	end.

	
%% @spec verify(Params) -> {ok, []} | {error, Reason}
%% @type Params = [term()]
%% @type Reason = string()
%% @doc verify is the function called by schedule to verify the parameter of monitor inputed by user
%%	ftp monitor verify timeout filename and hostname
verify(Params) ->
    case verify(ftpserver, Params) of
		{ok, _} ->
			case BASE:verify(Params) of
				{error,E}->
					{error, E};
				_->
					{ok, []}
			end;
		{error, Err} ->
			case BASE:verify(Params) of
				{error,E}->
					{error, Err ++ E};
				_->
					{error, Err}
			end	
	end.
    
verify(Status, Params)->
    case Status of
        ftpserver->
            FtpSrv = proplists:get_value(ftpserver, Params),
            case FtpSrv of
                ""->
                    {error, [{ftpserver, "Ftp Server address not null"}]};
                _->
					case string:rstr(FtpSrv," ") of
						0->
							case string:tokens(FtpSrv, ":") of
								[_A, B] ->
									if
										not is_number(B) ->
											{error, [{ftpserver, "port is not a number"}]};
										true ->
											verify(file, Params)
									end;
								_ ->
									verify(file, Params)
							end;							
						_->
							{error, [{ftpserver, "no spaces are allowed"}]}
					end					
            end;
        file->
            File = proplists:get_value(file, Params),
            case File of
                ""->
                    {error, [{file, "file path not null"}]};
                _->
                    verify(timeout, Params)
            end;
        timeout->
            Timeout = proplists:get_value(timeout, Params),
            if
                is_integer(Timeout) /= true->
                    {error, [{timeout, "Time out must be a number"}]};
                Timeout =< 0 ->
                    {error, [{timeout, "Time out must > 0"}]};
                true ->
                    {ok, []}
            end;
        _->
            {ok, []}
    end.

getHostname()->
	case THIS:get_property(server) of
		{ok,{_,V}}->
			V;
		_->
			BASE:getHostname()
	end.
	
%% @spec get_template_property() -> List
%% @type List = [term()]
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
%% 1.elements show on gui interface
%% 2.elements to evaluate the return value of ftp server 
get_template_property() ->
	BASE:get_template_property() ++ 
	[
		#property{name=ftpserver, type=text, order=1, title="FTP Server", description="the IP address or host name of the FTP server (examples: 206.168.191.10, or ftp.siteview.com or to specify a different port, ftp.siteview.com:89)"},
        #property{name=file, type=text, order=2, title="File", description="the file to retrieve from the FTP server (example: /pub/file.txt)"},
        #property{name=username, type=text, order=3, title="User Name", description="user name for the FTP server (i.e. Anonymous)" },
        #property{name=password, type=password, order=4, title="Password", description="password for the FTP server (i.e. user@server.com)"},
        
        #property{name=timeout, type=numeric, order=1, advance=true, title="Timeout", description="the time out, seconds, to wait for the file to be retrieved", default=60,baselinable=true},
		#property{name=ftpproxy, type=text, order=2, advance=true, title="FTP Proxy", description="optional proxy server to use including port (example: proxy.siteview.com:8080)"},
        #property{name=mode, type=bool, order=3, advance=true, title="Passive Mode", description="use FTP's passive mode (if not using a proxy) - passive mode usually allows FTP to work through firewalls."},
        #property{name=contentmatch, type=text, order=4, advance=true, title="Match Content", description="for Receive Only, the text to match in the contents of the received message. (example: Subject: MySubject)"},
        #property{name=pcheckcontent, type=scalar, order=5, advance=true, title="Check for Content Changes", description="generate error if the content of the file changes - resetting the saved contents updates the contents checked against during the next monitor run"},
        #property{name=proxyusername, type=text, order=6, advance=true, title="Proxy Server User Name", description="optional user name if the proxy server requires authorization"},
        #property{name=proxypassword, type=password, order=7, advance=true, title="Proxy Server Password", description="optional password if the proxy server requires authorization"},
        
        #property{name=status, type=numric, order=8, state=true, title="status", configurable=false},
        #property{name=roundTripTime, type=numric, order=9, state=true, title="round trip time(in milliseconds)", configurable=false},
		#property{name=size, type=numric, order=10, state=true, title="size(bytes)", configurable=false},
		#property{name=matchvalue, type=text, order=11, state=true, title="match value", configurable=false}
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
			[{roundTripTime, '>', "100"}]
	end;

get_classifier(good) ->
	_Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_ ->
			[{roundTripTime, '>', "0"}]
	end.

%%get the data connection mode
get_mode(false)->active;
get_mode(true)->passive.

%%format the tranfer rate string
get_status_string(Bin, RoundTime)->
    SizeNum = size(Bin),
    RBbs = SizeNum * 1000.0/ (RoundTime +1),
    Sec = sv_datetime:microSecondsToStrSeconds(RoundTime+1) ++ " sec<br>",
    KBbs = "average " ++ integer_to_list(round(RBbs)) ++ " Kbytes/sec<br>",
    SSize = integer_to_list(SizeNum) ++ "bytes",
    Sec ++ KBbs ++ SSize.

%%set attribute of three property 
set_attributes(RoundTripTime, Category, StateStr)->
    THIS:set_attribute(roundTripTime, RoundTripTime/1000),
    THIS:set_attribute(?CATEGORY, Category),
    THIS:set_attribute(?STATE_STRING, StateStr).

%%split hostname string to get ip and prot
analyze_proxy(Line) when is_list(Line)->
    ListTokens = string:tokens(Line, ":"),
    Len = string:len(ListTokens),
    if
        Len =:= 2 ->
            Addr = lists:nth(1, ListTokens),
            Port = list_to_integer(lists:nth(2, ListTokens)),
            {ok, Addr, Port};
        Len =:= 1->
            Addr = lists:nth(1, ListTokens),
            {ok, Addr, 80};
        true->
            {error, "proxy address invalid"}
    end.

%%downdload file , match content and check file checksum
ftp_check_file(Obj, Session, StartTime)->
    {ok, {_, File}}               = THIS:get_property(file),
    {ok, {_, ContentMatch}}       = THIS:get_property(contentmatch),
    {ok, {_, Checkcontent}}       = THIS:get_property(pcheckcontent),

	EndTime = now(),
	RoundTime1 = timer:now_diff(EndTime, StartTime),			

	%%download file
    {RoundTime, Ret} = timer:tc(Obj, ftp_recv_bin, [Session, iconv:convert("utf-8", "gbk", File)]),
    case Ret of
		%%get file successful
        {ok,Bin}->
			%%match file content with match content user input
            case match_content(ContentMatch, Bin) of
                true->
					%%compare checksum of the file with stored checksum
                    CheckRet = ftp_check_content(Checkcontent, Bin),
                    case CheckRet of
                        error->
							%%checksum error
                            Obj:close_session(Session),
                            THIS:set_attributes(0, error, "contents changed");
                        ok->
							%%checksum ok
                            Status = get_status_string(Bin, RoundTime+RoundTime1),
                            THIS:set_attributes(RoundTime, good, Status),
							THIS:set_attribute(status, 200),
							THIS:set_attribute(size, size(Bin)),
							THIS:set_attribute(matchvalue, "True"),
							THIS:set_attribute(roundTripTime, (RoundTime+RoundTime1)/1000),
                            Obj:close_session(Session)
                    end;
                _->
					%%match content error
                    THIS:set_attributes(0, error, "content match error"),
                    Obj:close_session(Session)
            end;
		%%no such file
        {error, "file not found"}->
            THIS:set_attributes(0, error, "file not found"),
            Obj:close_session(Session);
		%%other error
        {error, Reason}->
            THIS:set_attributes(0, error, Reason),
            Obj:close_session(Session)
    end.

%%compare checksum of file
ftp_check_content(Checkcontent, Bin)->
	%%get the checksum last time
    {ok, {_, LastCheckSum}} = THIS:get_attribute(lastChecksum),
    case Checkcontent of
		%%do not compare checksum
        "nil"->
            ok;
		%%compare to last checksum, and stored checksum will be reset
        "on"->
			%%calculate the checksum of file
            CheckSum = get_checksum(Bin),
			%%reset the checksum
            THIS:set_attribute(lastChecksum, CheckSum),
			%%compare the the checksum with the value calculated last time
            if
                (LastCheckSum > 0) andalso (CheckSum /= LastCheckSum) ->
                    error;
                true->
                    ok
            end;
		%%just comapre the checksum, the sotred checksum will not be reset	
        "baseline"->
            CheckSum = get_checksum(Bin),
            if  
                LastCheckSum =:= -1 ->
                    THIS:set_attribute(lastChecksum, CheckSum),
                    ok;
                CheckSum /= LastCheckSum->
                    error;
                true->
                    ok
            end;
		%%only reset the checksum, and next time compare mode will be baseline	
        "reset"->
            CheckSum = get_checksum(Bin),
            THIS:set_attribute(lastChecksum, CheckSum),
            THIS:set_property(pcheckcontent, "baseline"),
            ok
    end.

%%calculate the crc checksum of the file content
get_checksum(Bin)->
    ZStream = zlib:open(),
    CheckSum = zlib:crc32(ZStream, Bin),
    zlib:close(ZStream),
    CheckSum.

%%look for the match content in file content
match_content(Match, Bin) ->
    if
		%%no match value, ignore match
        Match =:= "" ->
            true;
        true ->
			%%look for the content
            Idx = string:str(binary_to_list(Bin), Match),
            if
                Idx > 0 ->
                    true;
                true ->
                     false
            end
    end.
