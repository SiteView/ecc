-module(web_common).
-author('oldhand<oldhand@sina.com>').
-include("log.hrl").

-export([respond/1]).
-export([urlencode/1,urldecode/1,path2parms/1,get_params_from_body/1]).
		 
respond(params_error)->{200,[{'Content-Type',"text/plain;charset=utf-8"},{'Cache-Control',"no-cache"}],"<result><error>paramter error</error></result>"};
respond(ok) -> {200, [{'Content-Type',"text/plain;charset=utf-8"},{'Cache-Control',"no-cache"}], "404: Resource not found\n"};
respond(other) -> {200, [{'Content-Type',"text/plain;charset=utf-8"},{'Cache-Control',"no-cache"}], "404: Resource not found\n"};
respond(error) -> {200, [{'Content-Type',"text/plain;charset=utf-8"},{'Cache-Control',"no-cache"}], "404: Resource not found\n"};
respond(func_not_found) -> {200, [{'Content-Type',"text/plain;charset=utf-8"},{'Cache-Control',"no-cache"}], "404: module or function not found\n"};
respond(func_not_error) -> {200, [{'Content-Type',"text/plain;charset=utf-8"},{'Cache-Control',"no-cache"}], "<result><error>call function error</error></result>\n"};
respond(unknown) -> 
     Unknown = "<?xml version='1.0' encoding='UTF-8'?><errors><error code=\"unknown\">invalid request</error></errors>",
	 {200, [{'Content-Type',"text/plain;charset=utf-8"},{'Cache-Control',"no-cache"}], Unknown};
respond(Body) when is_list(Body)-> {200, [{'Content-Type',"text/plain;charset=gbk"},{'Cache-Control',"no-cache"}], Body};
respond(Body) when is_binary(Body)-> {200, [{'Content-Type',"text/plain;charset=utf-8"},{'Cache-Control',"no-cache"}], Body};
respond(Code) when is_integer(Code) -> {Code, [{'Content-Type',"text/plain;charset=utf-8"},{'Cache-Control',"no-cache"}],  "404: Resource not found\n"};
respond(_) -> {200, [{'Content-Type',"text/plain;charset=utf-8"},{'Cache-Control',"no-cache"}], "404: Resource not found\n"}.

urldecode(String) -> mochiweb_util:unquote(String).


urlencode(String) -> mochiweb_util:urlencode(String).

%%get_parameter(Path,Key)->
%%	NewPath = "&" ++ Path ++ "&",
%%	case regexp:match(NewPath,"&" ++ atom_to_list(Key)++"=(.)*&") of
%%		nomatch->
%%			{error,not_found};
%%		{match,B,E}->
%%			{Key,string:sub_string(NewPath,B+string:len(atom_to_list(Key))+1,E)}
%%	end.


path2parms(Path)->
	io:format("~p~n",[Path]),
	L = string:tokens(Path,"/"),
	[binary_to_list(base64:decode(X)) || X<-L].
	%%process_path(Path).


get_params_from_body(Req)->
	Body = Req:recv_body(),
	case Body of
		undefined->
			[];
		_->
			xml2list:to_params(binary_to_list(Body))
	end.