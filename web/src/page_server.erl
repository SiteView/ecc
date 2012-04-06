%%
%% page_server
%%
%%
-module(page_server).
-compile(export_all).

respond(Req,Path,Raw_path)->
	Params = string:tokens(Raw_path,"&"),
	case length(Params) of
		0->
			ok;
		1->
			[P1|_] = Params,
			respond(Req,Path,Raw_path,P1,[]);
		2->
			[P1,P2|_] = Params,ok;
			%respond(Req,Path,Raw_path,P1,P2,[]);
		_->
			[P1,P2|T] = Params,ok
			%respond(Req,Path,Raw_path,P1,P2,getParams(T))
	end.

getParams([])->[];
getParams([P|T])->
	case regexp:match(P,"=") of
		nomatch->
			[] ++ getParams(T);
		{match,I,_}->
			[{list_to_atom(string:substr(P,1,I-1)),string:substr(P,I+1,length(P)-I)}] ++ getParams(T)
	end.



respond(Req,Path,Raw_path,"returnUrl="++ReturnUrl,Params)->
	io:format("page_server:respond...~n"),
	Servs = platform:getServers(),
	"<html><head><title>siteview</title></head>" ++
	"<body>" ++
	"<h2>Choose Server</h2><form action=" ++ mochiweb_util:unquote(ReturnUrl) ++ " method=POST>" ++
	"<table width=400px><tr><td width=100px></td><td width=300px></td></tr>" ++
	"<tr><td>Server:</td>"++
	"<td><select name=machine>" ++ print_servers(Servs) ++ "<select><td></tr>" ++
	"<tr><td>Other Server:</td><td><input type=textbox name=other></td></tr>" ++
	"<tr><td><input type=submit value=Choose>Server</input></td></tr>" ++
	"</table></form>" ++
	"<p><a href=\"" ++ mochiweb_util:unquote(ReturnUrl) ++ "\">Return Back</a></p>" ++
	"</body></html>".


print_servers([])->"";
print_servers([S|T])->
		"<option value=" ++ element(2,S) ++ ">" ++ element(1,S) ++ "</option>" ++ print_servers(T).