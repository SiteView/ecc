%% @copyright 2008-2009 Dragonflow
%% @author Naindi zhou, ZhouJian Huang <jian.huang@dragonflow.com>
%% @version 1.0

%% @doc ftp library
%%
%%This module is the module realize the ftp protocol

-module(ftp, [Options]).
-compile(export_all).
-define(CRLF, "\r\n").
-define(PART, "-").
-define(SPACE, " ").
-define(ACTIVE, active).
-define(PASSIVE, passive).
-define(BUF_SIZE, 4096).
-define(DEFAULT_TIMEOUT, 60000).

%% @spec new(Option) -> Obj
%% @type Option = [Opt]
%% @type Obj = term()
%% @doc create a new instance for ftp library
%%	Available options are:
%% <dl>
%%  <dt>{mode,Mode}
%%  </dt>
%%  <dd>Mode have two options: active and passive, which corrspond to ftp data connection mode
%%  </dd>
%%  <dt>{timeout, Timeout}
%%  </dt>
%%  <dd>Specifies the timeout of ftp connection
%%  </dd>
%%  <dt>{proxy, {httpc, Host, Port, User, Password}}
%%  </dt>
%%  <dd>Host is the ip or hostname of ftp server
%%		Port is the port of ftp server
%%		User is the user name to access the ftp server
%%		Password is the password to access the ftp server
%%		Now only basic authorization tp proxy is supported	
%%  </dd>
%% </dl>
new(Options) when is_list(Options) ->
    {?MODULE,Options}.

%% @spec get_mode() -> Result
%% @type Result = active | passive
%% @doc get the data connection mode 
get_mode() ->
    case lists:keysearch(mode, 1, Options) of
        {value, {mode, Mode}} ->
            Proxy = get_proxy(),
            case Proxy of
                null ->
                    Mode;
                _->
                    ?PASSIVE
            end;
        _->
            ?PASSIVE
    end.

%% @spec get_timeout() -> Result
%% @type Result = number()
%% @doc get the timeout of connection  
get_timeout() ->
    case lists:keysearch(timeout, 1, Options) of
        {value, {timeout, Timeout}} ->
            Timeout;
        _ ->
            ?DEFAULT_TIMEOUT
    end.

%% @spec get_proxy() -> Result | null
%% @type Result = Tuple() 
%% @see new/1 
%% @doc get proxy setting
get_proxy() ->
   case lists:keysearch(proxy, 1, Options) of
    {value, {proxy, Proxy}} ->
        Proxy;
    _ ->
        null
    end.

%% @spec open_session(FTPServer, FTPPort) -> {connected, Sock} | {error, Reason}
%% @type FtpServer = string() | atom()
%% @type FTPPort = number()
%% @type Sock = term()
%% @type Reason = string()
%% @doc make the connection to ftp server and try to login in
%% 	Sock is the gen_tcp handle, which can be used by ftp_receive/1 
open_session(FtpServer, FtpPort) ->
    Proxy = get_proxy(),
    case Proxy of
		%%no proxy set
        null->
			%%connenct ftp server through tcp 
            Ret = gen_tcp:connect(FtpServer, FtpPort, [binary, {packet, 0}], get_timeout()),
            case Ret of
				%%tcp connected
                {ok, Socket} ->
					%%try to get the welcome message from server
                    WelRet = ftp_receive(Socket, []),
                    case WelRet of
						%%welcome 
                        {ok, 220, _}->
                            {connected, Socket};
						%%ftp server reject the connection
                        _ ->
                            gen_tcp:close(Socket),
                            {error, "unable to reach server"}
                    end;
				%%tcp connection error	
                {error, Reason} ->
                    {error, atom_to_list(Reason)}
            end;
		%%proxy is set	
        {httpc, ProxyIP, ProxyPort, UserName, Pass}->
			%%connect to proxy
            Ret = gen_tcp:connect(ProxyIP, ProxyPort, [binary, {packet, 0}], get_timeout()),
            case Ret of
                {ok, Socket}->
					%%make http header for Authorization, now only basic authorization is supported
                    F = fun(Name, PassWord)->
                        Len = string:len(Name) + string:len(PassWord),
                        if
                            Len > 0 ->
                                Auth = base64:encode_to_string(Name ++ ":" ++ PassWord),
                                "Proxy-Authorization: Basic " ++ Auth ++ ?CRLF;
                            true ->
                                ""
                        end
                    end,
					%%now to construct the http request content
                    FSPort = integer_to_list(FtpPort),
                    L1 = "CONNECT " ++ FtpServer ++ ":" ++ FSPort ++ " HTTP/1.0" ++ ?CRLF,
                    L3 = "User-Agent: Moz illa/4.0" ++ ?CRLF ++ ?CRLF,
                    Req = L1 ++ F(UserName, Pass) ++ L3,
					
					%%send the proxy apply packet
                    tcp_socket_send(Socket, Req),
                    receive
						%%get the proxy response
                        {tcp, Socket, Bin}->
                            Line = binary_to_list(Bin),
							%%split the response header
                            TagList = string:tokens(Line, "\r\n\r\n"),
							%%analyse the response
                            case httpc_response_analyze(lists:nth(1, TagList)) of
                                {ok, 200}->
									%%the proxy connection is setup, no to handle ftp data
                                    WelRet = ftp_receive(Socket, []),
                                    case WelRet of
										%%ftp welcome
                                        {ok, 220, _}->
                                            {connected, Socket};
										%%ftp reject the connection
                                        _ ->
                                            gen_tcp:close(Socket),
                                            {error, "unable to reach server"}
                                    end;
								%%proxy reject the connection or http header analyse error	
                                {error, _Reason}->
                                    gen_tcp:close(Socket),
                                    {error, "unable to reach server"}
                            end;
						%%tcp connection to proxy is closed	
                        {tcp_closed, Socket}->
                            {error, "tcp closed"}
                    end;
				%%	
                {error, Reason} ->
                    {error, atom_to_list(Reason)}
            end
    end.
    

%% @spec close_session(Session) -> ok | is_close
%% @type Session = term()
%% @doc close the ftp connection	
close_session(Session) ->
	%%close data connection
    erase(ftp_port_port),
	%%send bye command to ftp server
    ftp_bye(Session),
	%%close tcp connection
    case Session of
        {connected, Socket} ->
            gen_tcp:close(Socket),
            ok;
        _->
            is_close
    end.

%% receive the response from ftp server
ftp_receive(Socket,Data)->
    receive
        {tcp, Socket, Bin} ->
            Sofar = [Bin|Data],
            StrData = binary_to_list(list_to_binary(lists:reverse(Sofar))),
			%%analyse the response
            Ret = ftp_response_analyze(StrData),
            case Ret of
				%%get a command
                {ok, Code, Line} ->
                    {ok, Code, Line};
				%%command have not been receive completed, receive continue	
                {continue, _} ->
                     ftp_receive(Socket, Sofar);
                _->
                    Ret
            end;
        {tcp_closed, Socket} ->
            {error, "tcp_closed"}
    after get_timeout()->
        {error, "timeout"}
    end.
	
%%analyse the proxy response 
httpc_response_analyze(Data) when is_list(Data)->
	%%get the status code of http response
    Idx1 = string:str(Data, " "),
    HttpCode = string:substr(Data, Idx1+1, 3),
    IHttpCode = list_to_integer(HttpCode),
    case IHttpCode of
		%%status code 200, proxy accept connection
        200 ->
            {ok, 200};
		%%proxy reject the connection	
        _->
            {error, string:sub_string(Data, Idx1+4)}
    end.

%%analyse ftp response
ftp_response_analyze(Data) when is_list(Data) ->
    Len = string:len(Data),
    if
		%%try to analyse
        Len > 4 ->
			%%get the status code
            SCode = string:substr(Data, 1, 3),
            ICode = list_to_integer(SCode),
            FSCode = SCode ++ " ",
            IdxF = string:str(Data, FSCode),
			%%try to find the end of a command
            IdxE = string:rstr(Data, ?CRLF),
			%%is the code a valid ftp command code
            BCode = ftp_code(ICode) /= true,
            if
				%%command code is not valid
                BCode ->
                    {error, Data};
				%%find the end of one command	
                (IdxF > 0) andalso (IdxE + 1 =:= Len) ->
                    {ok, ICode, Data};
				%%command is not completed	
                true ->
                    {continue, Data}
            end;
        true ->
            {continue, Data}
    end.


%% @spec ftp_login(Session, UserName , Pass) -> ok | {error, Reason}
%% @type Session = term()
%% @type UserName = string()
%% @type Pass = string()
%% @doc login to ftp
%%	Session - connection handle returned by  open_session/2
%%  UserName - ftp login user name
%%  Pass -  ftp login password
ftp_login(Session, UserName, Pass)->
    case Session of
        {connected, Socket}->
            UserReq = "USER " ++ UserName ++ ?CRLF,
            PassReq = "PASS " ++ Pass ++ ?CRLF,
			%%send the user
            tcp_socket_send(Socket, UserReq),
			%%receive the response
            URet = ftp_receive(Socket, []),
            case URet of
				%%user name is accepted
                {ok, 331, _} ->
					%%send the password
                    tcp_socket_send(Socket, PassReq),
                    PRet = ftp_receive(Socket, []),
                    case PRet of
						%%login is ok
                        {ok, 230, _} ->
                            ok;
						%%login is rejected	
                        {ok, _, Reason}->
                            {error, Reason};
						%%connection error
                         {error, Reason} ->
                            {error, Reason}
                    end;
				%%user name is rejected	
                {ok, _, Reason}->
                    {error, Reason};
				%%connection error	
                {error, Reason}->
                    {error, Reason}
            end;
        _->
            {error, "Session Invalid"}
   end.

%%send syst command
ftp_sys(Session)->
    case Session of
        {connected, Socket}->
            SysReq = "SYST" ++ ?CRLF,
            tcp_socket_send(Socket, SysReq),
            SRet = ftp_receive(Socket, []),
            case SRet of
                {ok, 215, _}->
                    ok;
                {ok, _, Reason}->
                    {error, Reason};
                {error, Reason}->
                    {error, Reason}
            end;
        _->
            {error, "Session Invalid"}
    end.

%%send feat command
ftp_feat(Session)->
    case Session of
        {connected, Socket}->
            FeatReq = "FEAT" ++ ?CRLF,
            tcp_socket_send(Socket, FeatReq),
            FRet = ftp_receive(Socket, []),
            case FRet of
                {ok, 211, _}->
                    ok;
                {ok, _, Reason}->
                    {error, Reason};
                {error, Reason}->
                    {error, Reason}
            end;
        _->
            {error, "Session Invalid"}
    end.
    
%%send clnt command
ftp_clnt(Session)->
    case Session of
        {connected, Socket}->
            ClntReq = "CLNT 1.0" ++ ?CRLF,
            tcp_socket_send(Socket, ClntReq),
            CRet = ftp_receive(Socket, []),
            case CRet of
                {ok, 200, _}->
                    ok;
                {ok,_,Reason}->
                    {error, Reason};
                {error, Reason}->
                    {error, Reason}
            end;
        _->
            {error, "Session Invalid"}
    end.
    
%%send utf-8 encoding command
ftp_opts_utf8_on(Session)->
    case Session of
        {connected, Socket}-> 
            Utf8OnReq = "OPTS UTF8 ON" ++ ?CRLF,
            tcp_socket_send(Socket, Utf8OnReq),
            URet = ftp_receive(Socket, []),
            case URet of
                {ok, 220,_}->
                    ok;
                {ok, _, Reason}->
                    {error, Reason};
                {error, Reason}->
                    {error, Reason}
            end;
        _->
            {error, "Session Invalid"}
    end.

%%send get file mode command, this function set text mode
ftp_type(Session, ascii)->
    case Session of
        {connected, Socket}-> 
            TypeReq = "TYPE A" ++ ?CRLF,
            tcp_socket_send(Socket, TypeReq),
            TRet = ftp_receive(Socket, []),
            case TRet of
                {ok, 200,_}->
                    ok;
                {ok, _, Reason}->
                    {error, Reason};
                {error, Reason}->
                    {error, Reason}
            end;
        _->
            {error, "Session Invalid"}
    end;
    
%%send get file mode command, this function set binary mode
ftp_type(Session, binary)->
    case Session of
        {connected, Socket}-> 
            TypeReq = "TYPE I" ++ ?CRLF,
            tcp_socket_send(Socket, TypeReq),
            TRet = ftp_receive(Socket, []),
            case TRet of
                {ok, 200, _}->
                    ok;
                {ok, _, Reason}->
                    {error, Reason};
                {error, Reason}->
                    {error, Reason}
            end;
        _->
            {error, "Session Invalid"}
    end.
    
%%send get pwd command to see directory localtion in remote machine
ftp_pwd(Session)->
    case Session of
        {connected, Socket}-> 
            PwdReq = "PWD" ++ ?CRLF,
            tcp_socket_send(Socket, PwdReq),
            PRet = ftp_receive(Socket, []),
            case PRet of
                {ok, 257,_}->
                    ok;
                {ok, _, Reason}->
                    {error, Reason};
                {error, Reason}->
                    {error, Reason}
            end;
        _->
            {error, "Session Invalid"}
    end.

%%send file size command to get the the size of a file
ftp_size(Session, RemoteFile)->
    case Session of
        {connected, Socket}-> 
            SReq = "SIZE " ++ RemoteFile ++ ?CRLF,
            tcp_socket_send(Socket, SReq),
            SRet = ftp_receive(Socket, []),
            case SRet of
                {ok, 213, Line}->
                    SSize = string:sub_string(Line, 5, string:len(Line) - 2),
                    {ok, list_to_integer(SSize)};
                {ok, _, Reason}->
                    {error, Reason};
                {error, Reason}->
                    {error, Reason}
            end;
        _->
            {error, "Session Invalid"}
    end.
	
%%send a bye command to stop ftp session     
ftp_bye(Session)->
    case Session of
        {connected, Socket}->
            BReq = "QUIT" ++ ?CRLF,
            tcp_socket_send(Socket, BReq),
            QRet = ftp_receive(Socket, []),
                case QRet of
                    {ok, 221, _}->
                        ok;
                    {ok, _, Reason}->
                        {error, Reason};
                    {error, Reason}->
                        {error, Reason}
                end;
        _->
            {error, "Session Invalid"}
    end.

%% @spec ftp_recv_bin(Session, RemoteFile) -> {Roundtime, {ok, Bin}} | {Roundtime, {error, Reason}}
%% @type Session = term()
%% @type RemoteFile = string()
%% @type Roundtime = number()
%% @type Bin = binary()
%% @type Reason =  string()
%% @doc download a file from ftp server
%%	Session - connection handle returned by  open_session/2
%%  RemoteFile - full path of file in ftp server
%%  Bin -  file content
ftp_recv_bin(Session, RemoteFile)->
    case Session of
        {connected, _}->
            case get_mode() of
				%%get file in passive mode
                passive ->
                    ftp_recv_bin_passive(Session, RemoteFile);
				%%get file in active mode
		        active ->
                    ftp_recv_bin_port(Session, RemoteFile);
                _ ->
                    {error, "Data trasnfer mode unknow"}
            end;
        _->
            {error, "Session Invalid"}
    end.


%get file in passive mode
ftp_recv_bin_passive(Session, RemoteFile)->
	%%get the file size
    case ftp_size(Session, RemoteFile) of
        {ok, Size} ->
			%%send passive command 
            {connected, Socket} = Session,
            PReq = "PASV" ++ ?CRLF,
            tcp_socket_send(Socket, PReq),
            PRet = ftp_receive(Socket, []),
            case PRet of
				%%ftp send the host and port information for client to connect
                {ok, 227, Line}->
                    Ret227 = analyze_227_code(Line),
                    case Ret227 of
						%%get the ip and port for data connection
                        {ok, IP, Port} ->
							%%open a new tcp to transfer data
                            case ftp_open_transfer_connect(IP, Port) of
                                {connected, TSocket}->
									%%connect is setup, send the get command to server
                                    RReq = "RETR " ++ RemoteFile ++ ?CRLF,
                                    tcp_socket_send(Socket, RReq),
                                    RRet = ftp_receive(Socket, []),
											io:format("receive file, ~p~n", [RRet]),
                                    case RRet of
                                        {ok, 150, _}->
											%%server begin to send the file, now we try to receive data
                                            case ftp_recv_transfer_bin(TSocket,[], Size, 0) of
												%%file download complete
                                                {ok, Bin}->
                                                     {ok, Bin};
												%%receive file error					
                                                {error, Reason}->
                                                    {error, Reason}
                                            end;
										%%same with up
                                        {ok, 125,_}->
                                            case ftp_recv_transfer_bin(TSocket,[], Size, 0) of
                                                {ok, Bin}->
                                                    {ok, Bin};
                                                {error, Reason}->
                                                    {error, Reason}
                                            end;
										%%for some reason server not send the file 	
                                        {ok, _, Reason}->
                                            {error, Reason};
										%%connectiom error	
                                        {error, Reason}->
                                            {error, Reason}
                                    end;
								%%data connection open error	
                                {error, Reason}->
                                    {error, Reason}
                            end;
						%%passive mode set error	
                        _ ->
                            Ret227
                    end;
				%%passive mode set error	
                {ok, _, Reason} ->
                    {error, Reason};
				%%data connection open error	
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, _Reason}->
            {error, "file not found"}
    end.

%%open a new data connection to server
ftp_open_transfer_connect(TransServer, Port)->
    Ret = gen_tcp:connect(TransServer, Port, [binary, {packet, 0}], get_timeout()),
    case Ret of
        {ok, Socket}->
            {connected, Socket};
        {error, _Reason}->
            {error, "cannot open data connection"}
    end.

%%receive the file
ftp_recv_transfer_bin(Socket, Data, RemoteFileSize, CurrentSize)->
    receive
        {tcp, Socket, Bin}->
			%%file date received
            Sofar = [Bin|Data],
            RecvLen = size(Bin) + CurrentSize,
            if
				%%receive complete
                RecvLen =:= RemoteFileSize ->
                    BFile = list_to_binary(lists:reverse(Sofar)),
                    {ok, BFile};
				%%not complete, continue	
                true ->
                    ftp_recv_transfer_bin(Socket, Sofar, RemoteFileSize, RecvLen)
            end;
        {tcp_closed, Socket}->
            {error, "connection closed, transfer aborted"}
    after get_timeout()->
        {error, "data transfer connection timeout"}
    end.

%%analyse the reponse of passive command
analyze_227_code(Line)->
	%%get the ip and port string from response
    Idx = string:str(Line, "("),
    Line1 = string:substr(Line, Idx +1),
    Idx2 = string:rstr(Line1, ")"),
    Line2 = string:substr(Line1, 1, Idx2-1),
	%%split ip and port string, the format is 192,168,0,12,23,34
    {ok, List} = regexp:split(Line2, ","),
    Count = string:len(List),
    if
        Count =:= 6 ->
			%%first for number is ip
            P1 = lists:nth(1, List),
            P2 = lists:nth(2, List),
            P3 = lists:nth(3, List),
            P4 = lists:nth(4, List),
            IP = lists:concat([P1, ".", P2, ".", P3, ".", P4]),
			%%last two number is port
            Port1 = list_to_integer(lists:nth(5, List)),
            Port2 = list_to_integer(lists:nth(6, List)),
            Port = (Port1 * 256) + Port2,
            {ok, IP, Port};
        true->
            {error, "passive port analyze error"}
    end.

%%receive file in active mode
ftp_recv_bin_port(Session, RemoteFile)->
	%%get the file size
    case ftp_size(Session, RemoteFile) of
        {ok, Size} ->
			%%get a vailable port in localmachine
            PortNum = get(ftp_port_port),
            case PortNum of
				%%there is no available port
                undefined->
					%%use the port of now connection socket
                    {connected, Socket} = Session,
                    IP = get_localhost_ip_address(),
                    {ok, Port} = inet:port(Socket),
					%%change to mode that can trap exit mode of the child process
                    process_flag(trap_exit, true),
					%%start child process to get file
                    {ok, Pid} = proc_lib:start_link(ftp_util, ftp_util_listen_transfer_connect, [Port + 1, self(), get_timeout(), Size], ?DEFAULT_TIMEOUT, []),
                    ftp_receive_bin_port(Session, RemoteFile, Pid, IP);
				%%there is available port	
                _->
					%%get localhost ip
                    IP = get_localhost_ip_address(),
					%%change to mode that can trap exit mode of the child process
                    process_flag(trap_exit, true),
					%%start child process to get file
                    {ok, Pid} = proc_lib:start_link(ftp_util, ftp_util_listen_transfer_connect, [PortNum + 1, self(), get_timeout(), Size], ?DEFAULT_TIMEOUT, []),
                    ftp_receive_bin_port(Session, RemoteFile, Pid, IP)
            end;
        {error, _Reason}->
            {error, "file not found"}
    end.

%%send the port command and wait for receive process to stop
ftp_receive_bin_port(Session, RemoteFile, Pid, IP)->
    {connected, Socket} = Session,
    receive
		%%child process send the successful listen port
        {listen, LPort}->
			%%register the port
            put(ftp_port_port, LPort),
			%%send the port command
            P1 = integer_to_list(LPort div 256),
            P2 = integer_to_list(LPort rem 256),
            PReq = "PORT " ++ IP ++ "," ++ P1 ++ "," ++ P2 ++ ?CRLF,
            tcp_socket_send(Socket, PReq),
            PRet = ftp_receive(Socket, []),
            case PRet of
				%%server accept port command
                {ok, 200, _}->
					%%send the retr command to inform server to send the file
                    RReq = "RETR " ++ RemoteFile ++ ?CRLF,
                    tcp_socket_send(Socket, RReq),
                    RRet = ftp_receive(Socket, []),
                    case RRet of
						%%server start to send file
                        {ok, 150, _}->
                            ftp_receive_bin_port(Session, RemoteFile, Pid, IP);
						%%server not send file for some reason, stop the child process
                        {ok,_, Reason}->
                            Pid ! {stop},
                            {error, Reason};
						%%connection error 	
                        {error, Reason}->
                            Pid ! {stop},
                            {error, Reason}
                    end;
				%%server does not accept port command	
                {ok, _, Reason}->
                    Pid ! {stop},
                    {error, Reason};
				%connection error	
                {error, Reason}->
                    Pid ! {stop},
                    {error, Reason}
            end;
		%%file recive ok
		{ok, Bin}->
			{ok,Bin};
		%%connection error	
        {error, Reason}->
            {ok, Reason}
    after get_timeout()->
        {error, "timeout"}
    end.

%%send tcp data
tcp_socket_send(Socket, Data) when is_list(Data)->
    %io:format("Send = ~p~n",[Data]),
    Ret = gen_tcp:send(Socket, list_to_binary(Data)),
    if
        Ret =:= ok ->
            ok;
        true ->
            Ret
    end.

%%verify the code is valid
ftp_code(110) -> true ;
ftp_code(120) -> true ;
ftp_code(125) -> true ;
ftp_code(150) -> true ;
ftp_code(200) -> true ;
ftp_code(202) -> true ;
ftp_code(211) -> true ;
ftp_code(212) -> true ;
ftp_code(213) -> true ;
ftp_code(214) -> true ;
ftp_code(215) -> true ;
ftp_code(220) -> true ;
ftp_code(221) -> true ;
ftp_code(225) -> true ;
ftp_code(226) -> true ;
ftp_code(227) -> true ;
ftp_code(230) -> true ;
ftp_code(250) -> true ;
ftp_code(257) -> true ;
ftp_code(331) -> true ;
ftp_code(332) -> true ;
ftp_code(350) -> true ;
ftp_code(421) -> true ;
ftp_code(425) -> true ;
ftp_code(426) -> true ;
ftp_code(450) -> true ;
ftp_code(451) -> true ;
ftp_code(452) -> true ;
ftp_code(501) -> true ;
ftp_code(502) -> true ;
ftp_code(503) -> true ;
ftp_code(504) -> true ;
ftp_code(530) -> true ;
ftp_code(532) -> true ;
ftp_code(550) -> true ;
ftp_code(552) -> true ;
ftp_code(553) -> true ;
ftp_code(10054) -> true ;
ftp_code(10060) -> true ;
ftp_code(10061) -> true ;
ftp_code(10066) -> true ;
ftp_code(10068) -> true ;
ftp_code(_) -> false.

%%get localhost ip address
get_localhost_ip_address()->
    {ok, HostName} = inet:gethostname(),
    case inet:getaddr(HostName, inet) of
        {ok, {I1, I2, I3, I4}}->
            P1 = integer_to_list(I1),
            P2 = integer_to_list(I2),
            P3 = integer_to_list(I3),
            P4 = integer_to_list(I4),
            lists:concat([P1, ",", P2, ",", P3, ",", P4]);
        {error,_}->
            "127,0,0,1"
    end.
	