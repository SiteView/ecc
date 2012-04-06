-module(httprequestsettings,[URL,UserName,Password,Domain,Flag,Proxy,ProxyUserName,ProxyPassword,Vector,I,J,K]).
-compile(export_all).

-define(Params,[url,host,port,scheme,authUserName,authPassword,authNTLMDomain,authenticationOnFirstRequest,proxy,proxyHost,proxyPort,proxyUserName,
                                proxyPassword,proxyAuthDomain,headers,requestTimeoutMS,connectionTimeoutMS,retriesOnRecoverableException,certFilename,certPassword,
                                http11,httprequestsettings_cookies,acceptAllUntrustedCerts,acceptInvalidCerts,httprequestsettings_encodePostData]).

-define(CONTENT_TYPE,"Content-Type").
-define(CONTENT_TYPE_DEFAULT,"application/x-www-form-urlencoded").
-define(DEFAULT_PROXY_PORT,80).
-define(DEFAULT_RECOVERABLE_RETRIES,3).
-define(CHARSET,"; charset=").
-define(CHARSET_DEFAULT,"UTF-8").


new(URL,UserName,Password,Domain,Flag,Proxy,ProxyUserName,ProxyPassword,Vector,I,J,K) ->
	put(url, null),
	put(host, ""),
    put(port, 0),
	put(scheme, null),
	put(authUserName, null),
	put(authPassword, null),
    put(authNTLMDomain, ""),
    put(authenticationOnFirstRequest, false),
    put(proxy, null),
    put(proxyHost, null),
    put(proxyPort, 80),
    put(proxyUserName, null),
    put(proxyPassword, null),
    put(proxyAuthDomain, ""),
    put(headers, []),
    put(requestTimeoutMS, -1),
    put(connectionTimeoutMS, -1),
    put(retriesOnRecoverableException, 3),
	put(certFilename, null),
    put(certPassword, null),
    put(http11, true),
    put(httprequestsettings_httprequestsettings_cookies, null),
    put(acceptAllUntrustedCerts, null),
    put(acceptInvalidCerts, null),
    put(httprequestsettings_encodePostData, true),
    put(authUserName, UserName),
    put(authPassword, Password),
    put(authNTLMDomain, Domain),
    put(authenticationOnFirstRequest, Flag),
    put(proxy, Proxy),
    put(proxyUserName, ProxyUserName),
    if
        ProxyUserName=/="" ->
            L = string:str(ProxyUserName,"\\"),
            if
                L=/=0 ->
                    put(proxyAuthDomain, string:sub_string(ProxyUserName,1,L-1)),
                    put(proxyUserName, string:sub_string(ProxyUserName,L+string:len("\\"),string:len(ProxyUserName)));
                true ->
                    ok
            end;
        true ->
            ok
    end,
    put(proxyPassword,ProxyPassword),
    if
        Vector=/=null ->
            put(headers, Vector);
        true ->
            ok
    end,
    if
        Proxy=/="" ->
            I1 = string:str(Proxy,":"),
            if
                I1=/=0 ->
                    put(proxyHost, string:sub_string(Proxy,1,I1-1)),
                    put(proxyPort, string:sub_string(Proxy,I1+1,string:len(Proxy)));
                true ->
                    put(proxyHost, Proxy)
            end;
        true ->
            ok
    end,
    put(retriesOnRecoverableException, I),
    put(connectionTimeoutMS, J),
    put(requestTimeoutMS, K),
	{?MODULE,URL,UserName,Password,Domain,Flag,Proxy,ProxyUserName,ProxyPassword,Vector,I,J,K}.

init() ->
    setUrl(URL).
    
cleanup() ->
    [erase(X) || X <- ?Params],
    ok.
    
delete() ->ok.

setUrl(S) ->
	S1 = httputils:replaceAll(string:strip(S)," ","%20"),
    put(url, S1),
	case http_uri:parse(S) of
		{error,R} ->
			io:format("parse url error:~p~n",[R]);
		{Scheme,_,Host,Port,_,_} ->
            put(host, Host),
            put(port, Port),
            put(scheme, Scheme)
	end.

getUrl() ->
    get(url).

getPort() ->
    get(port).
    
getScheme() ->
    get(scheme).
    
getAuthUserName() ->
    get(authUserName).

getAuthPassword() ->
    get(authPassword).
    
getProxy() ->
    get(proxy).

getProxyHost() ->
    get(proxyHost).
    
getProxyPort() ->
    get(proxyPort).

getProxyUserName() ->
    get(proxyUserName).

getProxyPassword() ->
    get(proxyPassword).
    
getHeaders() ->
    get(headers).
    
getAuthNTLMDomain() ->
    get(authNTLMDomain).
    
setAuthNTLMDomain(null) ->ok;
setAuthNTLMDomain(S) ->
    put(authNTLMDomain, S).
    
getAuthenticationOnFirstRequest() ->
    get(authenticationOnFirstRequest).
    
setAuthenticationOnFirstRequest(Bool) ->
    put(authenticationOnFirstRequest, Bool).
    
getProxyAuthDomain() ->
    get(proxyAuthDomain).
    
setProxyAuthDomain(S) ->
    put(proxyAuthDomain, S).

setAuthUserName(S) ->
    put(authUserName, S).
    
setAuthPassword(S) ->
    put(authPassword, S).
    
setProxy(S) ->
    put(proxy, S).
    
setProxyHost(S) ->
    put(proxyHost, S).
    
setProxyPort(Num) ->
    put(proxyPort, Num).

setProxyUserName(S) ->
    put(proxyUserName, S).
    
setProxyPassword(S) ->
    put(proxyPassword, S).
    
setHeaders(Array) ->
    put(headers, Array).
    
addHeaders(Array) ->
    H = get(headers),
    put(headers, H++[Array]).
    
getHost() ->
    get(host).

setHost(S) ->
    put(host, S).
    
getRetries() ->
    get(retriesOnRecoverableException).
    
setRetries(Re) ->
    put(retriesOnRecoverableException, Re).
    
getConnectionTimeoutMS() ->
    ConnectionTimeoutMS = get(connectionTimeoutMS),
    if
        ConnectionTimeoutMS>0 ->
            ConnectionTimeoutMS;
        true ->
            120000
    end.

setConnectionTimeoutMS(Num) ->
    put(connectionTimeoutMS, Num).
    
getRequestTimeoutMS() ->
    get(requestTimeoutMS).

setRequestTimeoutMS(Num) ->
    put(requestTimeoutMS, Num).
    
getCertFilename() ->
    CertFilename = get(certFilename),
    if
        CertFilename=/=null ->
            CertFilename;
        true ->
            ""
    end.
    
setCertFilename(S) ->
    put(certFilename, S).
    
getCertPassword() ->
    CertPassword = get(certPassword),
    if
        CertPassword=/=null ->
            CertPassword;
        true ->
            ""
    end.

setCertPassword(S) ->
    put(certPassword, S).

getHttp11() ->
    get(http11).
    
setHttp11(Bool) ->
    put(certFilename, Bool).
    
getCookies() ->
    get(httprequestsettings_cookies).
    
setCookies(Vec) ->
    put(httprequestsettings_cookies, Vec).
    
getAcceptAllUntrustedCerts() ->
    get(acceptAllUntrustedCerts).
    
setAcceptAllUntrustedCerts(Bool) ->
    put(acceptAllUntrustedCerts, Bool).
    
getAcceptInvalidCerts() ->
    get(acceptInvalidCerts).
    
setAcceptInvalidCerts(Bool)->
    put(acceptInvalidCerts, Bool).
    
getEncodePostData() ->
    get(httprequestsettings_encodePostData).
    
setEncodePostData(Bool) ->
    put(httprequestsettings_encodePostData, Bool).

    

    


