-module(common).
-author('oldhand<oldhand@sina.com>').
-include("log.hrl").

-compile(export_all).
		 
		 


respond(ok) -> {200, [{'Content-Type',"text/plain;charset=utf-8"},{'Cache-Control',"no-cache"}], "404: Resource not found\n"};
respond(other) -> {200, [{'Content-Type',"text/plain;charset=utf-8"},{'Cache-Control',"no-cache"}], "404: Resource not found\n"};
respond(error) -> {200, [{'Content-Type',"text/plain;charset=utf-8"},{'Cache-Control',"no-cache"}], "404: Resource not found\n"};
respond(unknown) -> 
     Unknown = "<?xml version='1.0' encoding='UTF-8'?><errors><error code=\"unknown\">invalid request</error></errors>",
	 {200, [{'Content-Type',"text/plain;charset=utf-8"},{'Cache-Control',"no-cache"}], Unknown};
respond(Body) when is_list(Body)-> {200, [{'Content-Type',"text/plain;charset=utf-8"},{'Cache-Control',"no-cache"}], Body};
respond(Body) when is_binary(Body)-> {200, [{'Content-Type',"text/plain;charset=utf-8"},{'Cache-Control',"no-cache"}], Body};
respond(Code) when is_integer(Code) -> {Code, [{'Content-Type',"text/plain;charset=utf-8"},{'Cache-Control',"no-cache"}],  "404: Resource not found\n"};
respond(_) -> {200, [{'Content-Type',"text/plain;charset=utf-8"},{'Cache-Control',"no-cache"}], "404: Resource not found\n"}.
respond(Code,Body) when is_integer(Code) -> {Code, [{'Content-Type',"text/plain;charset=utf-8"},{'Cache-Control',"no-cache"}],Body}.

urldecode(String) -> mochiweb_util:unquote(String).


urlencode(String) -> mochiweb_util:urlencode(String).

strmacth(String,Prefix,Suffix) -> strmacth(lists:prefix(Prefix,String),lists:suffix(Suffix,String),String,Prefix,Suffix).
strmacth(true,true,String,Prefix,Suffix) -> {match,lists:sublist(String,length(Prefix)+1,length(String)-length(Prefix)-length(Suffix))}; 
strmacth(_,_,_,_,_) ->  nomatch.


prefixmacth(String,Key) -> prefixmacth(lists:prefix(Key,String),String,Key).
prefixmacth(false,_,_) ->  nomatch;  
prefixmacth(true,String,Key) ->  lists:sublist(String,length(Key)+1,length(String)-length(Key)). 

suffixmacth(String,Key) -> suffixmacth(lists:suffix(Key,String),String,Key).
suffixmacth(false,_,_) ->  nomatch;  
suffixmacth(true,String,Key) -> lists:sublist(String,1,length(String)-length(Key)). 
