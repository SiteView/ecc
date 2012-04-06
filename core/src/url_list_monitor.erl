%% @author Cheng kaiyang <kaiyang.cheng@dragonflow.com>
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc URL List Monitor.
%% 
%% Description: URL List Monitor is used to check a large list of URLs,
%% URL List is specified by giving a filename containing the list of URLs to check
-module(url_list_monitor,[BASE]).
-export([new/0,verify/1,update/0,urlList/13,start/0,wait_for_responses/1,stopListMonitor/0,updateProperties/0,total/3,openGood/0,closeGood/0,logGood/1,logError/1,getURLsFromFile/3,getNumberOfUrls/0,getCostInLicensePoints/0,get_classifier/1,get_template_property/0]).
-extends(atomic_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

%% @spec new() -> ok
%% @doc Start the monitor instance.
new()->
	Base = atomic_monitor:new(),
    Base:set_attribute(left,0),
    Base:set_attribute(good,0),
    Base:set_attribute(bad,0),
    Base:set_attribute(lastModified,0),
    Base:set_attribute(urls,null),
    Base:set_attribute(urlData,null),
    Base:set_attribute(duration,0),
	{?MODULE,Base}.

%% @spec verify(Params) -> ErrorList
%% where
%% Params = [{atom(),integer()|string()}]
%% ErrorList = [string()]
%% @doc check file is exist.
verify(Params)->
	Errs =
	case proplists:get_value(pause,Params) of
		""->
			[{pause,"pause missing"}];
		Pause->
			if
				not is_number(Pause)->
				%	io:format("is_number(Pause) is_number(Pause)~p~n",[Params]),
					[{pause,"pause must be a number"}];
					
				Pause < 0 ->
					[{pause,"pause was less than "++ integer_to_list(0) ++" seconds"}];
				true ->
					[]
			end
	end ++ 
	case proplists:get_value(filename,Params) of
		""->
			[{filename,"URL List Name is missing."}];
		Filename1->
            Filename = iconv:convert(httputils:pageEncode(),httputils:getMachineEncode(""),Filename1),
			case file:open(Filename,read) of
				{error,enoent}->
					[{filename,"URL List Name does not exist"}];
				{error,_}->
					[{filename,"URL List Name is not a file"}];
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

%% @spec update() -> ok
%% @doc Run the monitor.
%%
%% Description: Get urls from file,and then process them
update()->
    THIS:set_attribute(s,""),
    {ok,{_,RawFile}} = THIS:get_property(filename),
    File = iconv:convert(httputils:pageEncode(),httputils:getMachineEncode(""),RawFile),
    {ok,{_,L2}} = THIS:get_attribute(lastModified),
    {ok,FileInfo} = file:read_file_info(File),
    L1 = case platform:platformName() of
        'nt' ->
            calendar:datetime_to_gregorian_seconds(element(6,FileInfo));
        _->
            calendar:datetime_to_gregorian_seconds(element(7,FileInfo))
    end,
    THIS:set_attribute(lastModified,L1),
    {ok,{_,URLData}} = THIS:get_attribute(urlData),
    if
    %%Original url list file to change the read or read only, or with the original contents have been read
        URLData =:= null orelse L2 =/= L1 ->
            T2 = httputils:currentTimeMillis(), 
            {ok,{_,Server}} = THIS:get_property(server),
            try getURLsFromFile(File,Server,false) of
                URLData1 ->
                    J = length(URLData1),
                    THIS:set_attribute(urlData,URLData1),
                    T3 = httputils:currentTimeMillis(),
                    L7 = T3-T2,
                    io:format("Read URL List, "++integer_to_list(J)++" urls in "++integer_to_list(L7)++" ms~n")
            catch
                error:X->X,
                THIS:set_attribute(lastModified,-1),
                THIS:set_attribute(s,"error reading "++File)
            end;
        true ->
            ok
    end,
    {ok,{_,S}} = THIS:get_attribute(s),
    if
        S=/="" ->
            THIS:set_attribute(left,"n/a"),
            THIS:set_attribute(good,"n/a"),
            THIS:set_attribute(bad,"n/a"),
            THIS:set_attribute(duration,"n/a"),
            THIS:set_attribute(?STATE_STRING,S),
            THIS:set_attribute(?NO_DATA, true);
        true ->
            {ok,{_,Time}} = THIS:get_property(timeout),
            I = if
                Time*1000=:=0 ->
                    60000;
                true ->
                    Time*1000
            end,
            %%{ok,{_,Threads}} = THIS:get_property(threads),
            {ok,{_,URLData2}} = THIS:get_attribute(urlData),
            {ok,{_,Pause}} = THIS:get_property(pause),
            {ok,{_,LogName}} = THIS:get_property(logName),
            {ok,{_,ErrorLog}} = THIS:get_property(errorLog),
            {ok,{_,Proxy}} = THIS:get_property(proxy),
            {ok,{_,UserName}} = THIS:get_property(userName),
            {ok,{_,Password}} = THIS:get_property(password),
            %%{ok,{_,Retries}} = THIS:get_property(retries),
            {ok,{_,ProxyUsername}} = THIS:get_property(proxyUsername),
            {ok,{_,ProxyPassword}} = THIS:get_property(proxyPassword),
            urlList(URLData2,Pause,LogName,ErrorLog,Proxy,UserName,Password,[],I,ProxyUsername,ProxyPassword,"Pragma: No-Cache",""),
            stopListMonitor()
   end.
            
%% @spec urlList(URLData,Pause,LogName,ErrorLog,Proxy,UserName,Password,PostData,Timeout,ProxyUsername,ProxyPassword,URLOtherHeader,IgnoreErrors) -> ok
%% where
%% URLData = list()
%% Pause = integer()
%% LogName = string()
%% ErrorLog = string()
%% Proxy = string()
%% UserName = string()
%% Password = string()
%% PostData = list()
%% Timeout = integer()
%% ProxyUsername = string()
%% ProxyPassword = string()
%% URLOtherHeader = string()
%% IgnoreErrors = string()
%% @doc process url request and update the state string and write log.
urlList(URLData,Pause,LogName,ErrorLog,Proxy,UserName,Password,PostData,Timeout,ProxyUsername,ProxyPassword,URLOtherHeader,IgnoreErrors) ->
    THIS:set_attribute(errorURLs,[]),
    THIS:set_attribute(index,1),
    THIS:set_attribute(nBadURLs,0),
    THIS:set_attribute(nGoodURLs,0),
    THIS:set_attribute(nConnectionErrorURLs,0),
    THIS:set_attribute(done,0),
    THIS:set_attribute(duration1,0),
    %%THIS:set_attribute(threads,Threads),
    THIS:set_attribute(pause,Pause),
    THIS:set_attribute(stopped,false),
    THIS:set_attribute(goodLog,null),
    THIS:set_attribute(urls1,URLData),
    THIS:set_attribute(goodName,LogName),
    THIS:set_attribute(errorName,ErrorLog),
    THIS:set_attribute(proxy,Proxy),
    THIS:set_attribute(username,UserName),
    THIS:set_attribute(password,Password),
    %%THIS:set_attribute(retries,Retries),
    %%THIS:set_attribute(retriesLeft,Retries),
    THIS:set_attribute(postData,PostData),
    THIS:set_attribute(timeout,Timeout),
    THIS:set_attribute(proxyUsername,ProxyUsername),
    THIS:set_attribute(proxyPassword,ProxyPassword),
    THIS:set_attribute(otherHeaders,URLOtherHeader),
    THIS:set_attribute(ignoreErrors,IgnoreErrors),
    openGood(),
    updateProperties(),
    start().

%% @spec start() -> ok
%% @doc spawn process for asynchronous request.
%%
%% Description: For each url request,creating a process to deal with it
start() ->
    {ok,{_,Class}} = THIS:get_property(?CLASS),
	{ok,{_,Id}} = THIS:get_property(?ID),
    Profile = list_to_atom(atom_to_list(Class) ++ "-" ++ atom_to_list(Id)),
    inets:start(httpc, [{profile, Profile}]),
    {ok,{_,URLs}} = THIS:get_attribute(urlData),
    {ok,{_,Username}} = THIS:get_attribute(username),
    {ok,{_,Password}} = THIS:get_attribute(password),
    {ok,{_,Proxy}} = THIS:get_attribute(proxy),
    {ok,{_,ProxyUserName}} = THIS:get_attribute(proxyUsername),
    {ok,{_,ProxyPassword}} = THIS:get_attribute(proxyPassword),
    {ok,{_,Timeout}} = THIS:get_attribute(timeout),
    {ok,{_,Pause}} = THIS:get_attribute(pause),
    HTTPRequestSettings = httprequestsettings:new("",Username,Password,"",false,Proxy,ProxyUserName,ProxyPassword,[],3,0,0),
    SetOption = if
        ((Proxy=/=null) and (Proxy=/="")) ->
            ProxyHost = HTTPRequestSettings:getProxyHost(),
            ProxyPort = list_to_integer(HTTPRequestSettings:getProxyPort()),
            [{proxy,{{ProxyHost,ProxyPort},[]}}];
        true ->
            []
    end,
    httpc:set_options(SetOption,Profile),
    ProxyAuth = if
        ((ProxyUserName=/=null) and (ProxyUserName=/=""))->
            [proxy_auth,{HTTPRequestSettings:getProxyUserName(),HTTPRequestSettings:getProxyPassword()}];
        true ->
            []
    end,
    Head = [],
    HTTPOptions = [{timeout,Timeout},{relaxed, true}]++ProxyAuth,
    try run(URLs,Username,Password,Head,HTTPOptions,Profile) of
        R ->
            R
    catch
    error:X->X
    after
    HTTPRequestSettings:delete()
    end.
    

run([],_,_,_,_,_) ->ok;
run([F|R],Username,Password,Head,HTTPOptions,Profile) ->
    {Status,URL,Time} = 
    try request(F,Username,Password,Head,HTTPOptions,Profile) of
        Rr ->
            Rr
    catch
    error:X->X,
    {-1,F,0}
    end,
    total(Status,Time,URL),
    run(R,Username,Password,Head,HTTPOptions,Profile).
    
%%use head method to request the url
request(Url,Username,Password,Head,HTTPOptions,Profile) ->
    T = httputils:timeMillis(),
    NewState = case httpc:request(get,{Url,Head},HTTPOptions,[],Profile) of
        {ok,{{_,401,_},ResponseHead,_}}->
            do_auth(Username,Password,ResponseHead,head,Head,Url,HTTPOptions,Profile);
        {ok,{{_,Status,_},_,_}} ->
            Status;
        {error,_}->
            %%io:format("URL:~p Reason:~p~n",[Url,Reason]),
            400
    end,
    T1 = httputils:timeMillis(),
    {NewState,Url,T1-T}.
    
do_auth(Username,Password,ResponseHead,Method,RequestHead,URL,HTTPOptions,Profile) ->
    AuthResponse = string:strip(proplists:get_value("www-authenticate",ResponseHead,"")),
    AuthRequest = case httputils:startsWithIgnoreCase(AuthResponse,"basic") of
        0 ->
            url_auth:digest_auth(Username,Password,ResponseHead,URL,Method);
        _->
            url_auth:basic_auth(Username,Password)
    end,
    case httpc:request(head,{URL,RequestHead++AuthRequest},HTTPOptions,[],Profile) of
        {ok,{{_,Status,_},_,_}} ->
            Status;
        {error,_}->
            400
    end.
        

%% @spec wait_for_responses(Count) -> ok
%% @doc wait for process response.
%%
%% Description: When get a message from process,the loop will sleep 4 second
wait_for_responses(0) ->
    stopListMonitor();
wait_for_responses(Count) ->
    receive
        {response,{Status,URL,Time}} ->
            %%io:format("url is:~p,status is:~p~n",[URL,Status]),
            total(Status,Time,URL),
            %%Waiting for other reasons may be due to speed the process did not complete the requested data back
            platform:sleep(2000),
            wait_for_responses(Count-1);
        _ ->
            ok
    end.

%% @spec stopListMonitor() -> ok
%% @doc stop request url.
%%
%% Description: Change the stopped flag,and close log file
stopListMonitor() ->
    THIS:set_attribute(stopped,true),
    closeGood().


%% @spec updateProperties() -> ok
%% @doc update state string and threshold properties.
updateProperties() ->
    {ok,{_,NGoodURLs}} = THIS:get_attribute(nGoodURLs),
    {ok,{_,NBadURLs}} = THIS:get_attribute(nBadURLs),
    {ok,{_,Duration}} = THIS:get_attribute(duration1),
    {ok,{_,NConnectionErrorURLs}} = THIS:get_attribute(nConnectionErrorURLs),
    L = if
        NGoodURLs =/= 0 ->
            Duration/NGoodURLs;
        true ->
            0
    end,
    {ok,{_,URLs}} = THIS:get_attribute(urls1),
    Len = length(URLs),
    {ok,{_,Done}} = THIS:get_attribute(done),
    L1 = Len-Done,
    THIS:set_attribute(duration,L),
    THIS:set_attribute(good,NGoodURLs),
    THIS:set_attribute(bad,NBadURLs),
    THIS:set_attribute(left,L1),
    S = integer_to_list(NGoodURLs) ++ " good, " ++ integer_to_list(NBadURLs) ++ " errors, " ++ integer_to_list(L1) ++ " left, avg " ++ integer_to_list(round(L)) ++ " ms",
    THIS:set_attribute(?STATE_STRING,S).

%% @spec total(State,Time,URL) -> ok
%% where
%% State = integer()
%% Time = integer()
%% URL = string()
%% @doc create a log message and append it to log file.
total(State,Time,URL) ->
    S1 = "[URL List] " ++ httputils:dateToString(erlang:localtime()) ++ "," ++ integer_to_list(State) ++ "," ++ integer_to_list(Time) ++ "," ++ URL,
    SSS = S1++"\n",
    THIS:inc_attribute(done),
    Flag = State=:=?URLok,
    {ok,{_,ErrorURLs}} = THIS:get_attribute(errorURLs),
    {ok,{_,Duration}} = THIS:get_attribute(duration1),
    if
        Flag ->
            THIS:inc_attribute(nGoodURLs),
            THIS:set_attribute(duration1,Duration+Time);
        true ->
            THIS:set_attribute(errorURLs,ErrorURLs++[URL]),
            THIS:inc_attribute(nBadURLs),
            logError(SSS),
            if
                State < 0 ->
                    THIS:inc_attribute(nConnectionErrorURLs);
                true ->
                    ok
           end
   end,
   logGood(SSS),
   updateProperties().

%% @spec openGood() -> ok
%% @doc open the total log file.
%%
%% Description: Get the pid of process which accessing the log file and if log is not exist,create one
openGood() ->
    {ok,{_,GoodName}} = THIS:get_attribute(goodName),
    if
        GoodName =:= "" ->
            ok;
        true ->
            try file:open(GoodName,[append]) of
                {ok,Pid} ->
                    THIS:set_attribute(goodLog,Pid);
                {error,_} ->
                    io:format("Could not open log file "++GoodName++"~n")
            catch
            error:X->X,
            io:format("Could not open log file "++GoodName++"~n")
            end
    end.

%% @spec closeGood() -> ok
%% @doc close the total log file.
%%
%% Description: Close the process which accessing the log file
closeGood() ->
    {ok,{_,GoodLog}} = THIS:get_attribute(goodLog),
    if
        GoodLog =:= null ->
            ok;
        true ->
            try file:close(GoodLog)
            catch
            error:X->X,
            {ok,{_,GoodName}} = THIS:get_attribute(goodName),
            io:format("Could not close log file "++GoodName++"~n")
            after
            THIS:set_attribute(goodLog,null)
            end
    end.

%% @spec logGood(S) -> ok
%% @doc append the message to log file.
%% where
%% S = string()
logGood(S) ->
    {ok,{_,GoodLog}} = THIS:get_attribute(goodLog),
    {ok,{_,GoodName}} = THIS:get_attribute(goodName),
    if
        GoodLog =:= null ->
            ok;
        true ->
            try file:write(GoodLog,S) of
                ok ->
                    ok;
                _->
                    io:format("Could not write log file "++GoodName++"~n")
            catch
            error:X->X,
            io:format("Could not write log file "++GoodName++"~n")
            end
    end.

%% @spec logError(S) -> ok
%% @doc append the message to errorlog file.
%% where
%% S = string()
logError(S) ->
    {ok,{_,ErrorName}} = THIS:get_attribute(errorName),
    if
        ErrorName =:= ""->
            ok;
        true ->
            try
            {ok,Pid} = file:open(ErrorName,[append]),
            file:write(Pid,S),
            file:close(Pid)
            catch
            error:X->X,
            io:format("Could not write to log file "++ErrorName++"~n")
            end
    end.
    
    
%% @spec getURLsFromFile(S,S1,Flag) -> Result
%% where
%% S = string()
%% S1 = string()
%% Flag = bool()
%% Result = list()
%% @doc Get urls from file,if server is not blank,use server to filter the urls.
getURLsFromFile(S,S1,Flag) ->
    SS1 = if
        S1=/="" ->
            ";"++S1++";";
        true ->
            S1
    end,
    {ok,FileContent} = file:read_file(S),
    S2 = binary_to_list(FileContent),
    for1(S,SS1,Flag,S2,1,1,1,1,[]).
    

for1(S,S1,Flag,S2,I,J,K,L,Result) ->
    Index = string:len(S2),
    if
        I<Index ->
            LL = case httputils:indexOf(S2,"\r\n",I) of
                0 ->
                    string:len(S2);
                N->
                   N-1
            end,
            S3 = string:strip(string:sub_string(S2,I,LL)),
            Index1 = string:len(S1),
            Index2 = string:str(S3,S1),
            if
                ((Index1=/=0) and (Index2=:=0)) ->
                    for1(S,S1,Flag,S2,LL+3,J,K+1,LL,Result);
                true ->
                    I1 = string:str(S3,";"),
                    S4 = if
                        (I1=/=0 and (not Flag)) ->
                            J1 = httputils:indexOf(S3,";",I1+1),
                            if
                                J1=/=0 ->
                                    K1 = httputils:indexOf(S3,";",J1+1),
                                    if
                                        K1=/=0 ->
                                            L1 = httputils:indexOF(S3,";",K1+1),
                                            if
                                                L1=/=0 ->
                                                    S5 = string:sub_string(S3,J1+1,K1-1),
                                                    S6 = string:sub_string(S3,K1+1,L1-1),
                                                    S7 = string:sub_string(S3,L1+1,string:len(S3)),
                                                    Index3 = httputils:startsWith(S7,"secure"),
                                                    SS7 = if
                                                        Index3 ->
                                                            "https://";
                                                        true ->
                                                            "http://"
                                                    end,
                                                    I2 = httputils:indexOf(S3,";",L1+1),
                                                    S8 = if
                                                        I2=/=0 ->
                                                            string:sub_string(S3,I2+1,string:len(S3));
                                                        true ->
                                                            ""
                                                    end,
                                                    SS4 = if
                                                        S6=/="" ->
                                                            SS7++S5++":"++S6;
                                                        true ->
                                                            SS7++S5
                                                    end,
                                                    if
                                                        S8=/="" ->
                                                            Index4 = httputils:startsWith(S8,"/"),
                                                            SSS4 = if
                                                                (not Index4) ->
                                                                    SS4++"/";
                                                                true ->
                                                                    SS4
                                                            end,
                                                            SSS4++S8;
                                                        true->
                                                            SS4
                                                    end;
                                                true ->
                                                    ""
                                            end;
                                        true ->
                                            ""
                                    end;
                                true ->
                                    ""
                            end;
                        true ->
                            S3
                    end,
                    if
                        S4=:="" ->
                            io:format("URL List format error, skipping URL "++integer_to_list(K)++": "++S3++"~n"),
                            for1(S,S1,Flag,S2,LL+3,J,K+1,LL,Result);
                        true ->
                            for1(S,S1,Flag,S2,LL+3,J+1,K+1,LL,Result++[S4])
                    end
            end;
        true ->
            Result
    end.
   
%% @spec getNumberOfUrls() -> Number
%% where
%% Number = integer()
%% @doc Get number of urls.
getNumberOfUrls() ->
    {ok,{_,File}} = THIS:get_property(filename),
    I = try getURLsFromFile(File,"",false) of
        Array ->
            length(Array)
    catch
    error:X->X,
    io:format("error reading file"++File++"~n"),
    0
    end,
    I.

%% @spec getCostInLicensePoints() -> Number
%% where
%% Number = integer()
%% @doc Get number of url list monitor costed license points.
getCostInLicensePoints() ->
    getNumberOfUrls() *1.
    
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
			[{bad,'>',0}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{bad,'>',0}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{bad,'==',0}]
	end.
	
getHostname()->
	"url list monitor".
	
%% @spec get_template_property() -> PropertyList
%% where
%% PropertyList = [PropertyRecord]
%% PropertyRecord = record()
%% @doc Add properties to template property list.
get_template_property()->
	BASE:get_template_property() ++
	  [
		#property{name=filename,title="URL List Name",type=text,description="path name of file containing URL list (e.g. e:/mydir/urls.txt)",order=1},
        #property{name=server,title="Server",type=text,description="(optional) only monitor URLs whose server field matches this field, default is all URLs",advance=true,order=2},
		#property{name=logName,title="Log",type=text,description="(optional) path name of log file for all URL results, default is no logging",advance=true,order=3},
        #property{name=errorLog,title="Error Log",type=text,description="(optional) path name of log file for URL errors, default is no logging",advance=true,order=4},
		%%#property{name=threads,title="Threads",type=numeric,description="(optional) number of separate threads to use, default is 4",advance=true,optional=true,order=5,default=4},
		#property{name=pause,title="Pause",type=numeric,description="(optional) pause, in milliseconds, between each URL check, default is 1000",advance=true,optional=true,order=6,default=1000},
        %%#property{name=retries,title="Retries",type=numeric,description="(optional) number of times to retry URLs that return errors, default is 0 (never retry error URLs)",advance=true,optional=true,order=7,default=0},
        #property{name=proxy,title="HTTP Proxy",type=text,description="optional list of proxy servers to use including port (example: proxy.siteview.com:8080)",advance=true,optional=true,order=8},
        #property{name=proxyUsername,title="HTTP Proxy User Name",type=text,description="The proxy server user name",advance=true,optional=true,order=9},
        #property{name=proxyPassword,title="HTTP Proxy Password",type=password,description="The proxy server password",advance=true,optional=true,order=10},
		#property{name=userName,title="Authorization User Name",type=text,description="optional user name if the URL requires authorization.",advance=true,optional=true,order=11},
		#property{name=password,title="Authorization Password",type=password,description="optional password if the URLs require authorization",advance=true,optional=true,order=12},
		#property{name=timeout,title="Timeout",type=numeric,description="timeout per URL",advance=true,optional=true,order=13,default=60,baselinable=true},
		#property{name=bad,title="errors",type=numeric,state=true,configurable=false},
		#property{name=left,title="left",type=numeric,state=true,configurable=false},
		#property{name=duration,title="duration",type=numeric,state=true,configurable=false}
	  ].