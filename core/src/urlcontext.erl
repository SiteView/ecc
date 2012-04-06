-module(urlcontext,[Mod]).
-compile(export_all).

-define(Params,[domain,context,cookies,refererURL,encodingForStream,redirectBase,encodePostData]).

new(Mod) ->
    put(domain, []),
	put(context, null),
	put(cookies, []),
	put(refererURL, ""),
	put(encodingForStream, "GBK"),
	put(redirectBase, ""),
	put(encodePostData, ""),
	put(context, Mod),
	{?MODULE,Mod}.

cleanup() ->
    [erase(K) || K <- ?Params],
    ok.
    
delete() ->ok.

getMonitor() ->
    get(context).

getRefererURL() ->
    get(refererURL).
    
setRefererURL(S) ->
    put(refererURL, S).
    
setDomain(S)->
    put(domain, S).

getDomain()->
    get(domain).
    
setStreamEncoding(S) ->
    put(encodingForStream, S).
    
getStreamEncoding() ->
    get(encodingForStream).
    
getRedirectBase() ->
     get(redirectBase).
    
setRedirectBase(S) ->
    put(redirectBase, S).
    
getEncodePostData() ->
    EncodePostData = get(encodePostData),
	if
        EncodePostData =:= "" ->
            "contentTypeUrlencoded";
        true ->
            EncodePostData
    end.

setEncodePostData(S) ->
    put(encodePostData, S).
    
getCookies() ->
    get(cookies).

setCookies(Array) ->
    put(cookies, Array).

addPeriodToDomainIfNeeded(S) ->
    case S of
        "."++_ ->
            S;
        _->
            "."++S
    end.

expiredCookie(Map) ->
	NowSec = calendar:datetime_to_gregorian_seconds({date(), time()}),
    case proplists:get_value("expires",Map) of
        undefined ->
            false;
        Value ->
            Result = case httputils:convert_netscapecookie_date(Value) of
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

printCookie(Array) ->
    io:format("cookies are:~p~n",[Array]).
    
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
    

updateOrAddOneCookie(Cookies,S,S1) ->
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
            {_, _, Host, _, _, _} = http_uri:parse(URL),
            S2 = string:sub_string(F,string:len("domain=")+1,string:len(F)),
            K = string:str(S2,":"),
            SS2 = if
                K=/=0 ->
                    string:sub_string(S2,1,K-1);
                true ->
                    S2
            end,
            SSS2 = if
                Host=/=SS2 ->
                    addPeriodToDomainIfNeeded(SS2);
                true ->
                    SS2
            end,
            setcookie(R,lists:keyreplace("domain", 1, Map, {"domain",SSS2}),Flag,URL);
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

%%Different from the original function, the first pass over directly to the response
updateCookies([],_)->ok;
updateCookies([{Key, Value}|R],S1) ->
    case string:to_lower(Key) of
        "set-cookie" ->
            S3 = "Set-Cookie:"++Value,
            Cookies = get(cookies),
            C = updateOrAddOneCookie(Cookies,S3,S1),
            put(cookies, C);
        _ ->
            ok
    end,
    updateCookies(R,S1).


addCookieParameters([],_)->ok;
addCookieParameters([F|R],S) ->
	Cookies = get(cookies),
	C = updateOrAddOneCookie(Cookies,F,S),
    put(cookies, C),
    addCookieParameters(R,S).
    
    
getCookieHeader(URL) ->
    Map = getCookies(URL),
    Cookies = get(cookies),
    loop(Cookies,Map,"").
    
loop([],_,Vector)->Vector;
loop([F|R],Map,Vector) ->
    S1 = proplists:get_value("key",F),
    Map2 = proplists:get_value(S1,Map),
    NewCookie = if
        Map2 =:= F ->
            S2 = proplists:get_value("domain",F),
            S3 = proplists:get_value("key",F),
            S4 = proplists:get_value("value",F),
            S5 = proplists:get_value("path",F),
            S6 = proplists:get_value("expires",F),
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
    {Protocol, _, _, _, _, _} = http_uri:parse(URL),
    Cookies = get(cookies),
    loop1(Cookies,Protocol,[]).
    
loop1([],_,Map)->Map;
loop1([F|R],Protocol,Map) ->
    Flag = proplists:get_value("secure",F) =:= undefined orelse Protocol=:=https,
    if
        Flag ->
            S1 = proplists:get_value("key",F),
            case proplists:get_value(S1,Map) of
                undefined ->
                    loop1(R,Protocol,Map++[{S1,F}]);
                Map2->
                    Path1 = proplists:get_value("path",F,""),
                    Path2 = proplists:get_value("path",Map2,""),
                    Flag1 = string:len(Path1)>string:len(Path2),
                    if
                       Flag1 ->
                            loop1(R,Protocol,Map++[{S1,F}]);
                        true ->
                            loop1(R,Protocol,Map)
                    end
            end;
        true ->
            loop1(R,Protocol,Map)
    end.
            
