%%
%%socketsession ets
%%
%%
-module(socketsession,[Monitor]).
-compile(export_all).

-define(Params,[cache,maxCachedSockets,context,inRemoteRequest,cookies,originalUserName,originalPassword,refererURL,encodePostData,domain,
                                authenticationWhenRequested,allowClose,certFilename,certPassword,encodingForStream,sslOut,sslIn,sslProcess,sslKey,byteBuffer,charBuffer]).

new(Monitor) ->
	put(cache, ""),
	put(maxCachedSockets, 0),
	put(context, null),
	put(inRemoteRequest, false),
	put(cookies, []),
	put(originalUserName, ""),
	put(originalPassword, ""),
	put(refererURL, ""),
	put(encodePostData, null),
	put(domain, ""),
	put(authenticationWhenRequested, ""),
	put(allowClose, true),
	put(certFilename, ""),
	put(certPassword, ""),
	put(encodingForStream, ""),
	put(sslOut, null),
	put(sslIn, null),
	put(sslProcess, null),
	put(sslKey, ""),
	put(byteBuffer, null),
	put(charBuffer, null),
	{?MODULE,Monitor}.
	
initialize(Monitor) ->
	put(context, Monitor),
	put(maxCachedSockets, 4).
    
cleanup() ->
    [erase(K) || K <- ?Params],
    ok.
    
delete() ->ok.
    
getMonitor() ->
    Monitor.
    
getContext() ->
    get(context).

getCookie() ->
    get(cookies).
    
setRefererURL(S) ->
    put(refererURL, S).
    
getRefererURL() ->
    get(refererURL).
    
setStreamEncoding(S) ->
    put(encodingForStream, S).
    
getStreamEncoding() ->
    get(encodingForStream).
    
getEncodePostData() ->
	EncodePostData = get(encodePostData),
    if
        EncodePostData=:=null ->
            "contentTypeUrlencoded";
        true ->
            EncodePostData
    end.
	
setEncodePostData(S) ->
	put(encodePostData, S).
    
getDomain() ->
	get(domain).
	
setDomain(S) ->
	put(domain, S).
    
getAuthenticationWhenRequested() ->
    AuthenticationWhenRequested = get(authenticationWhenRequested),
    if
        AuthenticationWhenRequested=:=null ->
            "Use Global Preference";
        true ->
            AuthenticationWhenRequested
    end.
	
setAuthenticationWhenRequested(S) ->
	put(authenticationWhenRequested, S).
    
getByteBuffer() ->
    ByteBuffer = get(byteBuffer),
    if
        ByteBuffer =:= null ->
            "";
        true ->
            ByteBuffer
    end.

getCharBuffer() ->
    CharBuffer = get(charBuffer),
    if
        CharBuffer =:= null ->
            "";
        true ->
            CharBuffer
    end.

closeSSL() ->
    %%application:stop(ssl).
    ok.

getVersion() ->
    MaxCachedSockets = get(maxCachedSockets),
    if
        MaxCachedSockets>0 ->
            "1.1";
        true ->
            "1.0"
    end.

expiredCookie(Map) ->
	NowSec = calendar:datetime_to_gregorian_seconds({date(), time()}),
    case proplists:get_value("expires",Map) of
        undefined ->
            false;
        Value ->
            case httputils:convert_netscapecookie_date(Value) of
                {error,_}->
                    false;
                Time ->
                    ExpireTime = calendar:datetime_to_gregorian_seconds(Time),
                    ExpireTime - NowSec =< 0
            end
    end.
    
addCookie(Cookie,Map) ->
	Flag1 = expiredCookie(Map),
	S = proplists:get_value("key",Map),
	{Array,Flag} = foraddcookie(Cookie,Map,S,false,Flag1,Cookie),
	Cookie1 = if
		((not Flag) and (not Flag1)) ->
			Array++[Map];
		true->
			Array
	end,
	Cookie1.
	
	
foraddcookie([],_,_,Flag,_,Cookie)->{Cookie,Flag};
foraddcookie([F|R],Map,S,Flag,Flag1,Cookie)->
    S1 = string:to_lower(S),
	Mdomain = string:to_lower(proplists:get_value("domain",Map)),
	Mpath = string:to_lower(proplists:get_value("path",Map)),
	Cdomain = string:to_lower(proplists:get_value("domain",F)),
	Cpath = string:to_lower(proplists:get_value("path",F)),
	Ckey = string:to_lower(proplists:get_value("key",F)),
	if
		((Ckey=:=S1) and (Cdomain=:=Mdomain) and (Cpath=:=Mpath))->
			if
				Flag1 ->
					foraddcookie(R,Map,S,Flag,Flag1,lists:delete(F, Cookie));
				true ->
					foraddcookie(R,Map,S,true,Flag1,lists:delete(F, Cookie)++[Map])
			end;
		true->
			foraddcookie(R,Map,S,Flag,Flag1,Cookie)
	end. 

makeCookie(URL) ->
	{_, _, Host, _, _, _} = http_uri:parse(URL),
	File = getFile(URL),
	I = string:str(File,"?"),
	S = if
		I=/=0 ->
			string:sub_string(File,1,I-1);
		true ->
			File
	end,
	Flag = (not httputils:endsWith(S,"/")),
	S1 = if
		Flag ->
			J = string:rstr(S,"/"),
			if
				J=/=0 ->
					string:sub_string(S,1,J-1);
				true ->
					S
			end;
		true ->
			S
	end,
	[{"domain",Host},{"path",S1}].
    
    
update1Cookie(Cookies,S,S1) ->
	Flag1 = httputils:startsWithIgnoreCase(S,"Set-Cookie:"),
	if
		Flag1 ->
			Hashmap = makeCookie(S1),
			AS = string:tokens(string:strip(string:sub_string(S,string:len("Set-Cookie:")+1,string:len(S))),";"),
			Map = setcookie(AS,Hashmap,true,S1),
			addCookie(Cookies,Map);
		true ->
			Cookies
	end.
    
setcookie([],Map,_,_)->Map;
setcookie([F1|R],Map,Flag,URL) ->
    F = string:strip(F1),
    Flag1 = httputils:startsWithIgnoreCase(F,"path="),
    Flag2 = httputils:startsWithIgnoreCase(F,"domain="),
    Flag3 = ((httputils:startsWithIgnoreCase(F,"secure")) and (string:str(F,"=")=:=0)),
    Flag4 = httputils:startsWithIgnoreCase(F,"expires="),
    Flag5 = ((string:str(F,"=")=/=0) and Flag),
    if
        Flag1 ->
            setcookie(R,lists:keyreplace("path", 1, Map, {"path",string:sub_string(F,string:len("path=")+1,string:len(F))}),Flag,URL);
        Flag2 ->
            S2 = string:sub_string(F,string:len("domain=")+1,string:len(F)),
            K = string:str(S2,":"),
            SS2 = if
                K=/=0 ->
                    string:sub_string(S2,1,K-1);
                true ->
                    S2
            end,
            setcookie(R,lists:keyreplace("domain", 1, Map, {"domain",SS2}),Flag,URL);
        Flag3 ->
            setcookie(R,Map++[{"secure",""}],Flag,URL);
        Flag4 ->
            setcookie(R,Map++[{"expires",string:sub_string(F,string:len("expires=")+1,string:len(F))}],Flag,URL);
        Flag5 ->
            J = string:str(F,"="),
            setcookie(R,Map++[{"key",string:sub_string(F,1,J-1)},{"value",string:sub_string(F,J+1,string:len(F))}],false,URL);
        true ->
            setcookie(R,Map,Flag,URL)
     end.
    
%%区别于原函数，直接把响应头传过来
updateCookies([],_)->ok;
updateCookies([{Key, Value}|R],S1) ->
    case string:to_lower(Key) of
        "set-cookie" ->
            S3 = "Set-Cookie:"++Value,
            Cookies = get(cookies),
            C = update1Cookie(Cookies,S3,S1),
            put(cookies, C);
        _ ->
            ok
    end,
    updateCookies(R,S1).

addCookieParameters([],_)->ok;
addCookieParameters([F|R],S) ->
	Cookies = get(cookies),
	C = update1Cookie(Cookies,F,S),
    put(cookies, C),
    addCookieParameters(R,S).
    
getCookieHeader(URL) ->
    Map = getCookies(URL),
    Cookies = get(cookies),
    Cookie = loop(Cookies,Map,""),
    [{"Cookie",Cookie}].
    
loop([],_,Vector)->Vector;
loop([F|R],Map,Vector) ->
    S1 = proplists:get_value("key",F),
    Map2 = proplists:get_value(S1,Map),
    NewCookie = if
        Map2 =:= F ->
            S3 = proplists:get_value("key",F),
            S4 = proplists:get_value("value",F),
            Cookie = S3++"="++S4,
            Dot = if
                Vector=/="" ->
                    "; ";
                true ->
                    ""
            end,
            Dot++Cookie;
        true ->
            ""
    end,
    loop(R,Map,Vector++NewCookie).

getCookies(URL)->
    Cookies = get(cookies),
    loop1(Cookies,URL,[]).

loop1([],_,Map)->Map;
loop1([F|R],URL,Map) ->
    {Scheme, _, Host, _, _, _} = http_uri:parse(URL),
    Flag1 = httputils:startsWithIgnoreCase(Host,proplists:get_value("domain",F)),
    Flag2 = httputils:startsWithIgnoreCase(getFile(URL),proplists:get_value("path",F)),
    Flag3 = ((string:to_lower(atom_to_list(Scheme)) =/= string:to_lower("http")) orelse (proplists:get_value("secure",F)=:=undefined)),
    if
        (Flag1 and Flag2 and Flag3) ->
            io:format("getCookies get in this case~n"),
            S2 = proplists:get_value("key",F),
            Map2 = proplists:get_value(S2,Map),
            NewMap = if
                Map2=/=undefined ->
                    Path1 = string:len(proplists:get_value("path",F)),
                    Path2 = string:len(proplists:get_value("path",Map2)),
                    if
                        Path1>Path2 ->
                            lists:keyreplace(S2, 1, Map, {S2,F});
                        true ->
                            Map
                    end;
                true ->
                    Map++[{S2,F}]
            end;
        true ->
            NewMap = Map
    end,
    loop1(R,URL,NewMap).

%%classic cookie head can't use in inet request head
getCookieHeader(S,Flag) ->
    HashMap = getCookies(S),
    Cookies = get(cookies),
    NewS = loop2(Cookies,HashMap,[],Flag),
    if
        NewS=/="" ->
            NewS++"\r\n";
        true ->
            NewS
    end.
    
loop2([],_,Head,_)->Head;
loop2([F|R],Map,Head,Flag) ->
    Key = proplists:get_value("key",F),
    Map2 = proplists:get_value(Key,Map),
    NewS = if
        Map2 =:= F ->
            Prefix = if
                Head=/="" ->
                    if
                        Flag ->
                            Head++"\r\n"++"Cookie: ";
                        true ->
                            Head++"; "
                    end;
                true ->
                    "Cookie: "
            end,
            Prefix++proplists:get_value("key",F)++"="++proplists:get_value("value",F);
        true ->
            ""
    end,
    loop2(R,Map,NewS,Flag).

getFile(URL) ->
	I = string:str(URL,":"),
	J = string:len(":"),
	if
		I=/=0 ->
			Per = string:substr(URL,I+1,2),
			J1 = if
				Per=:="//" ->
					3;
				true ->
					J
			end,
			S = string:sub_string(URL,I+J1,string:len(URL)),
			if
				J1=:=3 ->
					K = string:str(S,"/"),
					if
						K=/=0 ->
							string:sub_string(S,K,string:len(S));
						true ->
							"/"
					end;
				true ->
					S
			end;
		true ->
			""
	end.

	
	
	