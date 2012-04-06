%% @author Cheng kaiyang <kaiyang.cheng@dragonflow.com>
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc Web Server Monitor.
%%
%% Description: If ecc is installed on a Windows platform
%% this monitor can monitor a target Windows server that has a Web Server installed on it
%% but not support installed on Unix platform so far
%% %% Versions supported: N/A
%% Platform: Windows, Unix, Linux
%% Requirement:
%% a: When the web server running on Unix or Linux,the log file path should be appointed
%%
-module(web_server_monitor,[BASE]).
-extends(server_monitor).
-compile(export_all).
-include("monitor_template.hrl").
-include("monitor.hrl").

%-export([remoteCommandLineAllowed/0,new/0,update/0,verify/1,defaultTitle/1,isError/1,getStats/0,getStatsFromPerfex/0,getStatsFromLogFile/1,getScalarValues/2,get_classifier/1,get_template_property/0]).

-define(ERROR_RESULT,[-1,-1,-1,-1,-1]).

%% @spec remoteCommandLineAllowed() -> bool()
%% @doc Appoint whether support remote command line.
remoteCommandLineAllowed()->false.

%% @spec new() -> ok
%% @doc Start the monitor instance.
new()->
	Obj = server_monitor:new(),
	Obj:set_attribute(lastRawHits,0),
	Obj:set_attribute(lastRawBytes,0),
	Obj:set_attribute(lastMeasurement,0),
	{?MODULE,Obj}.
	
%% @spec verify(Params) -> ErrorList
%% where
%% Params = [{atom(),integer()|string()}]
%% ErrorList = [string()]
%% @doc page check.
%% 
%% Description:check file is exist or not
verify(Params)->
	Errs =
	case proplists:get_value(file,Params) of
		""->
			[];
		Filename->
			case file:open(Filename,read) of
				{error,enoent}->
					[{file,"file does not exist"}];
				{error,_}->
					[{file,"this is not a file"}];
				_->
					[]
			end
	end++
	case BASE:verify(Params) of
		{error,E}->
			E;
		_->
			[]
	end,
	if
		length(Errs)>0 ->
			{error,Errs};
		true ->
			{ok,""}
	end.
	
%% @spec defaultTitle(Params) -> Title
%% where
%% Params = [PagePropertis]
%% PagePropertis = {PropertyName,PropertyValue}
%% PropertyName = atom()
%% PropertyValue = (integer()|string())
%% Title = string()
%% @doc create a default title.
defaultTitle(Params)->
	Server = proplists:get_value(server,Params),
	if
		length(Server)>0->
			BASE:defaultTitle(Params) ++":" ++ Server;
		true ->
			BASE:defaultTitle(Params)
	end.
	
%% @spec update() -> ok
%% @doc Run the monitor.
%%
%% Description: Get data from platform module
%% calculate Num = 1min/default frequency
%% Receive request/Num
%% bytes trans/Num
update()->
    {ok,{_,LastRawHits}} = THIS:get_attribute(lastRawHits),
	{ok,{_,LastRawBytes}} = THIS:get_attribute(lastRawBytes),
	{ok,{_,LastMeasurement}} = THIS:get_attribute(lastMeasurement),
    % last 
    {LRawHits, LRawBytes, LMeasurement, Success1} = if
        LastMeasurement == 0 ->
            Stats = getStats(false),
            case not isError(Stats) of
                true ->
                    platform:sleep(1000),
                    {lists:nth(1, Stats), lists:nth(2, Stats), lists:nth(3, Stats), true};
                _ ->
                    {LastRawHits, LastRawBytes, LastMeasurement, false}
            end;
        true ->
            {LastRawHits, LastRawBytes, LastMeasurement, true}
    end,
    %now
    Results = getStats(true),
    RawHits = lists:nth(1, Results),
    RawBytes = lists:nth(2, Results),
    Measurement = lists:nth(3, Results),
    Frequency = lists:nth(4, Results),
    % hit, byte
    {Hits, Bytes} = case lists:nth(5, Results) of
        1 ->
            {RawHits - LRawHits, RawBytes - LRawBytes};
        _ ->
            {RawHits, RawBytes}
    end,
    
    Interval = Measurement - LMeasurement,
    
    {LastHitsPerMinute, LastBytesPerMinute, Success2} = 
        case ((not isError(Results)) and (Interval>0) and (Frequency>0)) of
            true ->
                NormalizedInterval = Interval/Frequency,
                LHitsPerMinute = case 60 * Hits / NormalizedInterval of
                    LHP when LHP<0 ->
                        0;
                    LHP ->
                        LHP
                end,
                LBytesPerMinute = if
                    RawBytes/=-1 ->
                        case 60 * Bytes / NormalizedInterval of
                            LBP when LBP<0 ->
                                0;
                            LBP ->
                                LBP
                        end;
                    true ->
                        -1
                end,
                {LHitsPerMinute, LBytesPerMinute, Success1};
            _ ->
                {-1, -1, false}
        end,
	if
		Success2 ->
			THIS:set_attribute(lastHits, Hits),
			THIS:set_attribute(lastBytes, Bytes),
			THIS:set_attribute(lastHitsPerMinute, LastHitsPerMinute),
			THIS:set_attribute(lastRawHits, RawHits),
			THIS:set_attribute(lastRawBytes, RawBytes),
			THIS:set_attribute(lastMeasurement, Measurement),
			if
				(LastBytesPerMinute/=-1) ->
                    State = httputils:floatToString(LastHitsPerMinute/1, 2) ++ " hits/min" ++ ", " ++ httputils:floatToString(LastBytesPerMinute/1, 2) ++ " bytes/min",
					THIS:set_attribute(lastBytesPerMinute, LastBytesPerMinute);
				true ->
                    State = httputils:floatToString(LastHitsPerMinute/1,2) ++ " hits/min",
					THIS:set_attribute(lastBytesPerMinute,0)
			end,
            THIS:set_attribute(status, 200),
			THIS:set_attribute(?STATE_STRING,State);
		true ->
			Er = isError(Results),
			if
				Er ->
					THIS:set_attribute(?STATE_STRING,"web server not found");
				true ->
					THIS:set_attribute(?STATE_STRING,"no data")
			end,
            THIS:set_attribute(status, 400),
			THIS:set_attribute(?NO_DATA,true),
			THIS:set_attribute(lastHits,0),
			THIS:set_attribute(lastBytes,0),
			THIS:set_attribute(lastHitsPerMinute,0),
			THIS:set_attribute(lastBytesPerMinute,0),
			THIS:set_attribute(lastMeasurement,0)
	end.

%% @spec isError(Data) -> bool
%% where
%% Data = tuple()
%% @doc check result from command line.
isError(Result)->
    ((lists:nth(1, Result)==-1) or (lists:nth(3, Result)==-1) or (lists:nth(4, Result)==-1)).
	
%% @spec getStats(IsLastCommand) -> tuple
%% @doc get web status form perfex.exe or logfile.
getStats(IsLastCommand)->
    getStatsFromPerfex(IsLastCommand).
	
%% @spec getStatsFromPerfex(_IsLastCommand) -> tuple
%% @doc get web status form perfex.exe.
getStatsFromPerfex(_IsLastCommand)->
    try
	{ok,{_,Machine}} = THIS:get_property(machine),
	{ok,{_,Server}} = THIS:get_property(server),
	ToolPath = platform:perfexCommand(Machine),
	{ServerName, ServerType} = case string:str(Server,"|") of
		0->
			{"unknown", "unknown"};
		Index ->
			{string:sub_string(Server,Index+1), string:sub_string(Server,1,Index-1)}
	end,
	%鉴别不同的web server
	{PerfexName, BeforeHits, AfterHits, BeforeBytes, AfterBytes, WebSiteProHack} = 
        case ServerType of
                "WebSite" ->
                    {"WebServer", "Requests/sec:", null, "WebServer KBytes/sec:", null, "Data Bytes Sent/Sec:"};
                "Microsoft" ->
                    {"HTTP Service", "Get Requests:", "PERF_COUNTER_RAWCOUNT", "Bytes Total/sec:", "PERF_COUNTER_BULK_COUNT", null};
                "Microsoft4" ->
                    {"Web Service", "Total Get Requests:", "PERF_COUNTER_RAWCOUNT", "Bytes Total/sec:", "PERF_COUNTER_BULK_COUNT", null};
                "Netscape" ->
                    {"Netscape Server", "Server Total Requests:", "PERF_COUNTER_RAWCOUNT", "Server Total Bytes:", "PERF_COUNTER_RAWCOUNT", null};
                "Netscape3"++_ ->
                    PN = if
                        (ServerType == "Netscape35") ->
                            "Netscape Enterprise 3.5";
                        true ->
                            "Netscape Enterprise 3.0"
                    end,
                    {PN, "Server Total Requests:", "PERF_COUNTER_RAWCOUNT", "Server Total Bytes:", "PERF_COUNTER_RAWCOUNT", null};
                O ->
                    io:format("Unsupported web server type: "++O),
                    {null, null, null, null, null, null}
        end,
    
	Cmd = case Machine of
		"http://"++_ ->
			Machine ++ "perfex.exe?" ++ "-cgi " ++PerfexName;
		_->
			ToolPath ++ " \"" ++ PerfexName ++ "\""
	end,
	CmdLine = command_line:new(),
    % result
	Rets = CmdLine:exec(Cmd),
    FoundServer = if
        length(ServerName)==0 ->
            true;
        true ->
            false
    end,
    Lines = [string:strip(X) || X<- string:tokens(Rets,"\r\n")],
    {Frequency, Time, Bytes, Hits, WebSiteProHacked} = parse_line(Lines, -1, -1, -1, -1, FoundServer, false, BeforeHits, AfterHits, BeforeBytes, AfterBytes, WebSiteProHack, ServerName),
    NBytes = if
        ((ServerType=="WebSite") and (not WebSiteProHacked)) ->
            Bytes * 1024;
        true ->
            Bytes
    end,
    [ Hits, NBytes, Time, Frequency, 1 ]
    catch
    _:ErrorReason ->io:format("! error in get data:~p~n",[ErrorReason]),
    ?ERROR_RESULT
    end.
    
	
parse_line([], Frequency, Time, Bytes, Hits, _, WebSiteProHacked, _, _, _, _, _, _) ->
    {Frequency, Time, Bytes, Hits, WebSiteProHacked};
parse_line([F|R], Frequency, Time, Bytes, Hits, FoundServer, WebSiteProHacked, BeforeHits, AfterHits, BeforeBytes, AfterBytes, WebSiteProHack, ServerName) ->
    NFrequency = if
        Frequency<0 ->
            findLong(F, "PerfFreq:", null);
        true ->
            Frequency
    end,
    NTime = if
        Time<0 ->
            findLong(F, "PerfTime:", null);
        true ->
            Time
    end,
    {NBytes, NHits, NewWebSiteProHacked} = if
        FoundServer ->
            B = if
                Bytes<0 ->
                    findLong(F, BeforeBytes, AfterBytes);
                true ->
                    Bytes
            end,
            H = if
                Hits<0 ->
                    findLong(F, BeforeHits, AfterHits);
                true ->
                    Hits
            end,
            {NB, NWebSiteProHacked} = if
                ((B<0) and (WebSiteProHack/=null)) ->
                    {findLong(F, WebSiteProHack, null), true};
                true ->
                    {B, WebSiteProHacked}
            end,
            {NB, H, NWebSiteProHacked};
        true ->
            {Bytes, Hits, WebSiteProHacked}
    end,
    NFoundServer = case "name: " ++ ServerName of 
        F ->
            true;
        _ ->
            FoundServer
    end,
    if
        ((NBytes>=0) and (NHits>=0)) ->
            {NFrequency, NTime, NBytes, NHits, NewWebSiteProHacked};
        true ->
            parse_line(R, NFrequency, NTime, NBytes, NHits, NFoundServer, NewWebSiteProHacked, BeforeHits, AfterHits, BeforeBytes, AfterBytes, WebSiteProHack, ServerName)
    end.
    
    
findLong(S,Before,After) ->
	Index = string:str(S, Before),
    if
        Index==0 ->
            -1;
        true ->
            Index2 = Index + length(Before),
            FDIndex = find_first_digit(S, Index2),
            if
                FDIndex==length(S) ->
                    -1;
                true ->
                    {Result, NIndex} = get_digit_value(S, FDIndex, -1),
                    if
                        ((After/=null) and (NIndex<length(S))) ->
                            EndIndex = httputils:indexOf(S, After, NIndex),
                            if
                                EndIndex==0 ->
                                    -1;
                                true ->
                                    Result
                            end;
                        true ->
                            Result
                    end
            end
    end.
    
find_first_digit(S, Index) ->
    if
        Index >= length(S) ->
            Index;
        true ->
            Char = lists:nth(Index, S),
            if
                ((Char=<57) and (Char>=48)) -> %digit
                    Index;
                true ->
                    find_first_digit(S, Index+1)
            end
    end.

get_digit_value(S, Index, Result) ->
    if
        Index=<length(S) ->
            C = lists:nth(Index, S),
            if
                (not ((C=<57) and (C>=48))) ->
                    {Result, Index};
                true ->
                    NR = if
                        Result>0 ->
                            Result * 10;
                        true ->
                            0
                    end,
                    get_digit_value(S, Index+1, list_to_integer([C])+NR)
            end;
        true ->
            {Result, Index}
    end.
	
%% @spec getScalarValues(Property,Params) -> ValueList
%% where
%% Property = atom()
%% Params = [{PropertyName,PropertyValue}]
%% PropertyName = atom()
%% PropertyValue = string()
%% ValueList = [{Scalarname,Scalarvalue}]
%% Scalarname = string()
%% Scalarvalue = string()
%% @doc Set scalar properties value.
getScalarValues(Prop,Params)->
	Index = platform:isWindows(),
	case Prop of
		server when Index ->
			{value,{_,Machine}} = lists:keysearch(machine,1,Params),
			Array = platform:getWebServers(Machine),
			R = case Array of
				[]->
					[];
				_->
					[X|[Y|_]] = Array,
					[{Y,X}]
				end,
			R;
		_->
			BASE:getScalarValues(Prop,Params)
	end.
	
%% @spec get_classifier(Flag) -> Threshold
%% where
%% Flag = (error|warning|good)
%% Threshold = [{Attribute,Opera,Value}]
%% Attribute = atom()
%% Opera = atom()
%% Value = (integer()|atom()|string())
%% @doc Set default threshold value.
get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'==',400}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'==',400}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier; 
		_->
			[{status,'==',200}]
	end.
	
%% @spec get_template_property() -> PropertyList
%% where
%% PropertyList = [PropertyRecord]
%% PropertyRecord = record()
%% @doc Add properties to template property list.
get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=server,title="Web Server",description="the web server application to monitor",type=scalar,order=2},
	#property{name=file,title="Log File Pathname",description="the web server log file",type=text,advance=true,optional=true,order=3},
	#property{name=lastHitsPerMinute,title="hits/min",type=numeric,state=true,configurable=false},
	#property{name=lastBytesPerMinute,title="bytes/min",type=numeric,state=true,configurable=false},
    #property{name=status,title="Status",type=numeric,state=true,configurable=false}
	].