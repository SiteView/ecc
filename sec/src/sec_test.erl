-module(sec_test).
-author('oldhand<oldhand@sina.com>').
-include("sec_log.hrl").
-include("xmerl.hrl").
-compile(export_all).


%% Specify levels.
-define(LOG_EMERGENCY, 0). % system is unusable
-define(LOG_ALERT,     1). % action must be taken immediately
-define(LOG_CRITICAL,  2). % critical conditions
-define(LOG_ERROR,     3). % error conditions
-define(LOG_WARNING,   4). % warning conditions
-define(LOG_NOTICE,    5). % normal but significant condition
-define(LOG_INFO,      6). % informational
-define(LOG_DEBUG,     7). % debug-level messages

% facility codes
-define(FAC_KERN,        (0 bsl 3)). % kernel messages
-define(FAC_USER,        (1 bsl 3)). % random user-level messages
-define(FAC_MAIL,        (2 bsl 3)). % mail system
-define(FAC_DAEMON,      (3 bsl 3)). % system daemons
-define(FAC_AUTH,        (4 bsl 3)). % security/authorization messages
-define(FAC_SYSLOG,      (5 bsl 3)). % messages generated internally by syslogd
-define(FAC_LPR,         (6 bsl 3)). % line printer subsystem
-define(FAC_NEWS,        (7 bsl 3)). % network news subsystem
-define(FAC_UUCP,        (8 bsl 3)). % UUCP subsystem
-define(FAC_CRON,        (9 bsl 3)). % clock daemon
-define(FAC_AUTHPRIV,   (10 bsl 3)). % security/authorization messages (private)
-define(FAC_FTP,        (11 bsl 3)). % ftp daemon

% these codes (from 12 through 15) are reserved for system use
%-define(FAC_NTP,	(12 bsl 3)).
%-define(FAC_LOG_ALERT,	(13 bsl 3)).
%-define(FAC_LOG_AUDIT,	(14 bsl 3)).
%-define(FAC_CLOCK,	(15 bsl 3)).

-define(FAC_LOCAL0,     (16 bsl 3)). % reserved for local use
-define(FAC_LOCAL1,     (17 bsl 3)). % reserved for local use
-define(FAC_LOCAL2,     (18 bsl 3)). % reserved for local use
-define(FAC_LOCAL3,     (19 bsl 3)). % reserved for local use
-define(FAC_LOCAL4,     (20 bsl 3)). % reserved for local use
-define(FAC_LOCAL5,     (21 bsl 3)). % reserved for local use
-define(FAC_LOCAL6,     (22 bsl 3)). % reserved for local use
-define(FAC_LOCAL7,     (23 bsl 3)). % reserved for local use

conf() ->
    case filelib:is_file("sec/test.conf") of
         true -> "sec/test.conf";
         _ ->
         case filelib:is_file("../test.conf") of
             true -> "../test.conf";
             _ -> "test.conf" 
         end
    end.
    
test() -> test("127.0.0.1"). 

test(Server) ->
   {ok, Binary} =  file:read_file(conf()),
   MessageLists = string:tokens(binary_to_list(Binary),"\n"),
   F = fun([]) -> done;
	  (X) -> sleep(50),sendsyslog({Server,514},?FAC_USER,?LOG_INFO,X)
	end,
   lists:foreach(F,MessageLists).

sleep(Time)->
    receive 
	after Time ->  ok
    end.
   
send(Message) ->
   sendsyslog({"127.0.0.1",514},?FAC_USER,?LOG_INFO,Message).

sendsyslog({Host, Port},Facility, Level, Message) ->
        case gen_udp:open(0,[binary]) of
		{ok, Fd} ->
			syslog({Fd, Host, Port}, Facility,Level,Message),
			gen_udp:close(Fd);
		{error, Reason} ->
			{stop, Reason}
	end.
	
syslog({Fd, Host, Port},Facility, Level,Message) ->
	M = list_to_binary(Message),
	P = list_to_binary(integer_to_list(Facility bor Level)),
	gen_udp:send(Fd, Host, Port, <<"<", P/binary, ">", M/binary, "">>);
syslog(_,_,_,_) -> done.