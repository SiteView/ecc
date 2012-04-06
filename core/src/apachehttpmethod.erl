-module(apachehttpmethod,[R]).
-compile(export_all).

-define(Params,[method,connection,location,contentLength,lastModified,date,refreshRedirect,responseHeaders,headersProcessed]).

new(R) ->
	put(method, null),
	put(connection, ""),
	put(location, ""),
	put(contentLength, -1),
	put(lastModified, 0),
    put(date,0),
	put(refreshRedirect, false),
	put(responseHeaders, null),
    put(headersProcessed, false),
    put(method, R),
	{?MODULE,R}.

cleanup() ->
    [erase(K) || K <- ?Params],
    ok.

getMethod() ->
    get(method).
    
getConnection() ->
    HeadersProcessed = get(headersProcessed),
    if
        (not HeadersProcessed) ->
            put(headersProcessed, processHeaders());
        true ->
            ok
    end,
    get(connection).

getLocation() ->
    HeadersProcessed = get(headersProcessed),
    if
        (not HeadersProcessed) ->
            put(headersProcessed, processHeaders());
        true ->
            ok
    end,
    get(location).
    
getContentLength() ->
    HeadersProcessed = get(headersProcessed),
    if
        (not HeadersProcessed) ->
            put(headersProcessed, processHeaders());
        true ->
            ok
    end,
    get(contentLength).
    
getLastModified() ->
    HeadersProcessed = get(headersProcessed),
    if
        (not HeadersProcessed) ->
            put(headersProcessed, processHeaders());
        true ->
            ok
    end,
    get(lastModified).
    
getDate() ->
    HeadersProcessed = get(headersProcessed),
    if
        (not HeadersProcessed) ->
            put(headersProcessed, processHeaders());
        true ->
            ok
    end,
    get(date).
    
getRefreshRedirect() ->
    HeadersProcessed = get(headersProcessed),
    if
        (not HeadersProcessed) ->
            put(headersProcessed, processHeaders());
        true ->
            ok
    end,
    get(refreshRedirect).
    
getResponseBody() ->
    case R of
        {ok,{{_,_,_},_,Body}} ->
            Body;
        _ ->
            ""
    end.

getResponseBodyAsString() ->
    getResponseBody().

getResponseHeaders()->
    ResponseHeaders = get(responseHeaders),
    if
        ResponseHeaders =/= null ->
            ResponseHeaders;
        true ->
            case R of
                {ok,{{_,_,_},Header,_}} ->
                    Header;
                _->
                    []
            end
    end.

    
getResponseHeadersAsString() ->
    ResponseHeaders = get(responseHeaders),
    if
        ResponseHeaders =/= null ->
            ResponseHeaders;
        true ->
            case R of
                {ok,{{Version,Status,StatusLine},Header,_}} ->
                    S1 = Version++" "++integer_to_list(Status)++" "++StatusLine++"\r\n",
                    S2 = forhead(Header),
                    S1++S2;
                _->
                    ""
            end
    end.
            
forhead([])->"";
forhead([{K,V}|T]) ->
    K++": "++V++"\r\n"++forhead(T).
    
processHeaders() ->
    case R of
        {ok,{{_,_,_},Header,_}} ->
            HeaderDict = dict:from_list(Header),
            Connection = case dict:find("connection", HeaderDict) of
                {ok,Con} ->
                    Con;
                _->
                    ""
            end,
            put(connection, Connection),
            Location1 = case dict:find("location", HeaderDict) of
                {ok,Loc} ->
                    Loc;
                _->
                    null
            end,
            Location2 = if
                Location1=:=null ->
                    case dict:find("content-location", HeaderDict) of
                        {ok,Loc1} ->
                            Loc1;
                        _->
                            null
                    end;
                true ->
                    Location1
            end,
            %%io:format("location is:~p~n",[Location2]),
            if
                Location2=/=null ->
                    put(location, Location2),
                    I = string:str(Location2,", "),
                    if
                        I=/=0 ->
                            J = string:str(Location2,"://"),
                            S = if
                                J=/=0 ->
                                    string:sub_string(Location2,1,J+3);
                                true ->
                                    ""
                            end,
                            J1 = string:str(Location2,", "),
                            S1 = if
                                J1=/=0 ->
                                    S++string:sub_string(Location2,J+2,string:len(Location2));
                                true ->
                                    S
                            end,
                            put(location, S1);
                        true ->
                            ok
                    end;
                true ->
                    ok
            end,
            NewLocation = get(location),
            if
                NewLocation=:="" ->
                    RefreshURL = getMetaRefreshForRedirectIfFound(),
                    %%io:format("refresh is:~p~n",[RefreshURL]),
                    put(location, RefreshURL);
                true ->
                    ok
            end,
            ContentLength = case dict:find("content-length", HeaderDict) of
                {ok,CL} ->
                    list_to_integer(CL);
                _->
                    -1
            end,
            put(contentLength, ContentLength),
            LastModified = case dict:find("last-modified", HeaderDict) of
                {ok,LM} ->
                    T = case httputils:convert_netscapecookie_date(LM) of
                        {error,_} ->
                            {date(), time()};
                        TT->
                            TT
                    end,
                    calendar:datetime_to_gregorian_seconds(T);
                _->
                    calendar:datetime_to_gregorian_seconds({date(), time()})
            end,
            put(lastModified, LastModified),
            Date = case dict:find("date", HeaderDict) of
                {ok,Da} ->
                    T1 = case httputils:convert_netscapecookie_date(Da) of
                        {error,_}->
                            {date(), time()};
                        TT1 ->
                            TT1
                    end,
                    calendar:datetime_to_gregorian_seconds(T1);
                _->
                    calendar:datetime_to_gregorian_seconds({date(), time()})
            end,
            put(date, Date);
        _ ->
            ok
    end.
    
getMetaRefreshForRedirectIfFound() ->
    case getResponseBody() of
        null ->
            "";
        S2 ->
            F1 = string:str(S2,"http-equiv")=/=0,
            F2 = string:str(S2,"HTTP-EQUIV")=/=0,
            if
                F1 orelse F2 ->
                    Tag = try yaws_html:parse(S2) of
                        Tags ->
                            Tags
                    catch
                    _:X->X,
                    %%io:format("process error is:~p~n",[X]),
                    []
                    end,    
                    findrefresh(Tag,"");
                true ->
                    ""
            end
    end.
                    
%%Only when the refresh time of 0 to jump into the location in this               
findrefresh([],URL)->URL;
findrefresh([{begin_tag,_,Content,_}|T],URL)->
    V = proplists:get_value('http-equiv',Content),
    if
        V=:=undefined ->
            findrefresh(T,URL);
        true ->
            Flag = string:to_lower(V)=:="refresh",
            if
                Flag ->
                    C = proplists:get_value(content,Content,""),
                    J = string:str(C,";"),
                    if
                        J=/=0 ->
                            I = case string:to_integer(string:sub_string(C,1,J-1)) of
                                {error,_}->
                                    0;
                                {N,_}->
                                    N
                            end,
                            if
                                I=:=0 ->
                                    S = string:strip(string:sub_string(C,J+1,string:len(C))),
                                    Flag1 = httputils:startsWithIgnoreCase(S,"url="),
                                    S1 = if
                                        Flag1 ->
                                            string:strip(string:sub_string(S,string:len("url=")+1,string:len(S)));
                                        true ->
                                            S
                                    end,
                                    Flag2 = httputils:startsWith(S1,"'") orelse httputils:startsWith(S1,"\""),
                                    Flag3 = httputils:endsWith(S1,"'") orelse httputils:endsWith(S1,"\""),
                                    S2 = if
                                        Flag2 ->
                                            string:strip(string:sub_string(S1,2,string:len(S1)));
                                        true ->
                                            S1
                                    end,
                                    S3 = if
                                        Flag3 ->
                                            string:strip(string:sub_string(S2,1,string:len(S2)-1));
                                        true ->
                                            S2
                                    end,
                                    put(refreshRedirect, true);
                                true ->
                                    S3=""
                            end,
                            findrefresh(T,S3);
                        true ->
                            findrefresh(T,URL)
                    end;
                true ->
                    findrefresh(T,URL)
            end
    end;
findrefresh([_|T],URL)->
    findrefresh(T,URL).
    
getStatusCode()->
    case R of
        {ok,{{_,Status,_},_,_}} ->
            Status;
        _->
            -1
    end.
            
            