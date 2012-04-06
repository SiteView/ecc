-module(proxy_web_auth).
-compile(export_all).

out401(_Arg, _Auth, _Realm) ->
	io:format("auth:~p,realm:~p~n",[_Auth,_Realm]),
	ok.