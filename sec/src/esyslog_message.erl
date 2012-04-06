-module(esyslog_message).
-include("sec_log.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([parse/1,parse/2, decode_priority/1, format/1, couchdoc/1, sortable_datetime_str/1,decode_severity/1,decode_facility/1,decode_level/1]).


%% @type datetime() = {{Year, Month, Day}, {Hour, Minute, Second}}
%%      Year   = integer(),
%%      Month  = integer(),
%%      Day    = integer(),
%%      Hour   = integer(),
%%      Minute = integer(),
%%      Second = integer().

%% @type priority() = integer().
%% An integer encoding the facility and priority of a syslog message.
%% Defined in <a href="http://tools.ietf.org/html/rfc3164#section-4.1.1">RFC3164</a>

%% @type facility() = integer().
%% An integer between 0 and 23 inclusive that indicates the syslog facility. 
%% Defined in <a href="http://tools.ietf.org/html/rfc3164#section-4.1.1">RFC3164</a>

%% @type severity() = integer().
%% An integer between 0 and 7 inclusive that indicates the syslog severity. 
%% Defined in <a href="http://tools.ietf.org/html/rfc3164#section-4.1.1">RFC3164</a>

%% @type msg() = {Priority, DateTime, Host, Tag, Body}
%%      Priority = priority(),
%%      DateTime = datetime(),
%%      Host     = string(),
%%      Tag      = string(),
%%      Body     = string().


%% @spec parse(Message::string()) -> msg() | bad_message
%% @doc Parses a string into a syslog message tuple
parse(Message) -> 
    try 
        {ok, Tokens, _} = esyslog_message_lexer:string(Message),
        {ok, ParsedMessage} = esyslog_message_parser:parse(Tokens),
        io:format("ParsedMessage: ~p~n", [ParsedMessage]),
        ParsedMessage
    catch error:{badmatch, Error} ->
        io:format("Couldn't parse: ~p~n~p~n", [Message, Error]),
        bad_message
    end.

inet_parse_address({A,B,C,D}) ->
    lists:flatten(io_lib:format("~p.~p.~p.~p", [A,B,C,D])).



parse(Ip,Message) ->
    Timestamp = calendar:now_to_local_time(now()),
    %?Log({"match ~p~n", [{Ip,Message}]}),
    Parseaddress = inet_parse_address(Ip),
    case otherparses(Ip,Message) of
          {ok,{Priority,Time,"SiteviewEventlog",Tag,Body}} ->
		%?Log({"match ~p~n", [{Priority,Time,Parseaddress,Tag,Body}]}),
		{Priority,Time,Parseaddress,Tag, iconv:convert("gbk", "utf-8",Body)};
          {ok,{Priority,Time,Host,Tag,Body}} ->
		%?Log({"match ~p~n", [{Priority,Time,Host,Tag,Body}]}),
		{Priority,Time,Host,Tag,iconv:convert("gbk", "utf-8",Body)};
          _ -> 	    		    
	    case regex:regex_str("<(\\d+)>(\\S+):",Message) of
		 {ok,[Body,Priority,Tag]} -> 
		   Newbody = string:strip(Body,left,$ ),
		   %?Log({"match ~p~n", [{list_to_integer(Priority),Timestamp,Parseaddress,Tag,Newbody}]}),
		   {list_to_integer(Priority),parsertime(now),Parseaddress,Tag,Newbody};
		 _ -> 
		 case regex:regex_str("<(\\d+)>",Message) of
		     {ok,[Body,Priority]} -> 		       
		       Newbody = string:strip(Body,left,$ ),   
		       %?Log({"match ~p~n", [{list_to_integer(Priority),parsertime(now),Parseaddress,"",Newbody}]}),		       
		       {list_to_integer(Priority),parsertime(now),Parseaddress,"",Newbody};
		     _ -> 
		     ?Log({"bad_message: ~p~n", [{Ip,Message}]}),
		     bad_message         
		 end      
	    end	     
	     
     end. 	      
	     
    
otherparses(Ip,Message) ->    
    case regex:regex_str("<(\\d+)>(\\S+\\s+\\d+\\s+\\d+:\\d+:\\d+)\\s+(\\S+)\\s+(\\S+):",Message) of
             {ok,[Body,Priority,Time,Host,Tag]} ->
	               %?Log({"bad_message: ~p~n", [{Body,Priority,Time,Host,Tag}]}), 
		       case regex:regex_str("\\s(\\S+\\s+\\d+\\s+\\d+:\\d+:\\d+\.\\d+):",Body) of
		            {ok,[MatchBody,_UTC_Time]} -> {ok,{list_to_integer(Priority),parsertime(Time),Host,"",string:strip(MatchBody,left)}};			    
		            _ -> 
			       case regex:regex_str("\\s(\\S+\\s+\\d+\\s+\\d+:\\d+:\\d+\.\\d+\\s+\\S+):",Body) of
				    {ok,[MatchBody,_UTC_Time]} -> {ok,{list_to_integer(Priority),parsertime(Time),Host,"",string:strip(MatchBody,left)}};			    
				    _ -> {ok,{list_to_integer(Priority),parsertime(Time),Host,Tag,string:strip(Body,left)}}
			       end
		       end;  
             _ -> {error,nomatch}	   
    end.

        


parsertime(now) -> calendar:now_to_local_time(now());    
parsertime(Time) ->
    {{Y,M,D},{HH,MM,SS}} = calendar:now_to_local_time(now()),
    case regex:regex_str("(\\S+) (\\d+) (\\d+):(\\d+):(\\d+)",Time) of
         {ok,[_,Month,Day,H,M,S]} ->
              {{Y,ofmonth(Month),list_to_integer(Day)},{list_to_integer(H),list_to_integer(M),list_to_integer(S)}};
         _ -> {{Y,M,D},{HH,MM,SS}}
    end.


%lists:flatten(io_lib:format("~4..0w~2..0w~2..0w~2..0w~2..0w~2..0w",[Y,M,D,HH,MM,SS]))


ofmonth(Month) when is_list(Month) -> ofmonth(list_to_atom(string:to_lower(Month)));    
ofmonth(jan) -> 1;
ofmonth(feb) -> 2; 
ofmonth(mar) -> 3; 
ofmonth(apr) -> 4; 
ofmonth(may) -> 5; 
ofmonth(jul) -> 6; 
ofmonth(jun) -> 7; 
ofmonth(aug) -> 8; 
ofmonth(sep) -> 9; 
ofmonth(oct) -> 10; 
ofmonth(nov) -> 11; 
ofmonth(dec) -> 12;
ofmonth(_) -> 1.

%% @spec decode_priority(Priority::priority()) -> {facility(), severity()}
%% @doc Decodes a priority value into facility and severity
decode_priority(Priority) ->
    {decode_facility(Priority div 8), 
     decode_severity(Priority rem 8)}.

decode_facility(Facility) ->
    case Facility of
        0 -> kern;
        1 -> user;
        2 -> mail;
        3 -> system;
        4 -> auth;
        5 -> internal;
        6 -> lpr;
        7 -> nns;
        8 -> uucp;
        9 -> clock;
        10 -> authpriv;
        11 -> ftp;
        12 -> ntp;
        13 -> audit;
        14 -> alert;
        15 -> clock2; % ?
        16 -> local0;
        17 -> local1;
        18 -> local2;
        19 -> local3;
        20 -> local4;
        21 -> local5;
        22 -> local6;
        23 -> local7;
        _ -> undefined
    end.

decode_severity(Severity) ->
    case Severity of
        0 -> emerg;
        1 -> alert;
        2 -> crit;
        3 -> err;
        4 -> warn;
        5 -> notice;
        6 -> info;
        7 -> debug;
        _ -> undefined
    end.
    
decode_level(Severity) ->
    case Severity of
        emerg -> 0;
        alert -> 1;
        crit -> 2;
        err -> 3;
        warn -> 4;
        notice -> 5;
        info -> 6;
        debug -> 7;
        undefined -> -1
    end.    
    
%% @spec format(Msg::msg()) -> string()
%% @doc Pretty-print a syslog message tuple
format({Priority, Timestamp, Host, Tag, Body}) ->
    string:join([httpd_util:rfc1123_date(Timestamp), integer_to_list(Priority), Host, Tag, string:strip(Body,right,$\n)], " ").

couchdoc({Priority, Timestamp, Host, Tag, Body}) ->
    [
        {<<"priority">>, list_to_binary(integer_to_list(Priority))},
        {<<"timestamp">>, list_to_binary(sortable_datetime_str(Timestamp))},
        {<<"host">>, list_to_binary(Host)},
        {<<"tag">>, list_to_binary(Tag)},
        {<<"body">>, list_to_binary(Body)}
    ].

sortable_datetime_str(Datetime) ->
    S = lists:map(
            fun(Input) -> 
                lists:concat(
                    lists:map(
                      fun(E) -> 
                        Str = integer_to_list(E),
                        case length(Str) of
                          1 -> "0" ++ Str;
                          _ -> Str
                        end 
                      end,
                      tuple_to_list(Input)
                    )
                )
            end,
        tuple_to_list(Datetime)),
    
    lists:concat(S).

datetime_test() ->
    "20091213194050" = sortable_datetime_str({{2009, 12, 13}, {19, 40, 50}}),
    "20090113010405" = sortable_datetime_str({{2009, 1, 13}, {1, 4, 5}}),
    true.

parse_test() ->
    {{Year, _, _}, _} = calendar:now_to_local_time(now()),

    {30,
     {{Year, 12, 13}, {19, 40, 50}},
     "localhost", % host has been left off, so assume localhost
     "thttpd[1340]",
     "192.168.1.138 - admin \"GET /cgi-bin/Qdownload/html/1260751250.rcsv HTTP/1.1\" 200 138 \"http://illmatic:8080/cgi-bin/Qdownload/html/rlist.html\" \"Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; en-US; rv:1.9.1.5) Gecko/2009110\""
    } = parse("<30>Dec 13 19:40:50 thttpd[1340]: 192.168.1.138 - admin \"GET /cgi-bin/Qdownload/html/1260751250.rcsv HTTP/1.1\" 200 138 \"http://illmatic:8080/cgi-bin/Qdownload/html/rlist.html\" \"Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; en-US; rv:1.9.1.5) Gecko/2009110\""),

    {30,
     {{Year, 12, 13}, {19, 41, 03}},
     "localhost", % host has been left off, so assume localhost
     "thttpd[1340]",
     "spawned CGI process 24156 for file 'cgi-bin/Qdownload/refresh.cgi'"
    } = parse("<30>Dec 13 19:41:03 thttpd[1340]: spawned CGI process 24156 for file 'cgi-bin/Qdownload/refresh.cgi'"),
    
    {147, 
     {{Year, 11, 18}, {19, 17, 55}},
     "myhost",
     "mytag[909]",
     "yo what's really real"} = parse("<147>Nov 18 19:17:55 myhost mytag[909]: yo what's really real"),

    {147, 
     {{Year, 11, 18}, {19, 17, 55}},
     "myhost",
     "mytag[909]",
     "yo"} = parse("<147>Nov 18 19:17:55 myhost mytag[909]: yo"),
    
    %{4,
    % {{Year, 12, 20}, {16, 27, 32}},
    % "ccabanilla-mac",
    % "com.apple.launchd.peruser.501[522] (org.apache.couchdb[59972])",
    % "Exited with exit code: 1"} = parse("<4>Dec 20 16:27:32 ccabanilla-mac com.apple.launchd.peruser.501[522] (org.apache.couchdb[59972]): Exited with exit code: 1"),
     
    %{5,
    % {{Year, 12, 20}, {16, 27, 32}},
    % "ccabanilla-mac",
    % "[0x0-0x99099].com.fluidapp.FluidInstance.Gmail[32480]",
    % "Sun Dec 20 16:27:32 ccabanilla-mac FluidInstance[32480] <Error>: kCGErrorIllegalArgument: CGSGetWindowBounds: NULL window"} = parse("<5>Dec 20 16:27:32 ccabanilla-mac [0x0-0x99099].com.fluidapp.FluidInstance.Gmail[32480]: Sun Dec 20 16:27:32 ccabanilla-mac FluidInstance[32480] <Error>: kCGErrorIllegalArgument: CGSGetWindowBounds: NULL window"),

    bad_message = parse("asdf"),



    true.
