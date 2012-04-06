-module(mod_soap).
-vsn("0.5").
-include_lib("inets/include/httpd.hrl").
-compile(export_all).
-define(PRINT_MESSAGE(Text,Content),io:format("<~p:~p>(~p): "++Text,[?MODULE,?LINE,self()|Content])).
-define(soap_ver_1_1,"1.1").
-define(soap_ver_1_2,"1.2").
-define(envelope_xsd(X),case X of 
                            "1.1" -> "soap-envelope-1.1.xsd";
                            "1.2" -> "soap-envelope-1.2.xsd"
                        end).

create_callback_table() ->
    case ets:info(soap_callback,name) of
        undefined ->
            ets:new(soap_callback, [public,named_table]);
		_ ->
            io:format("soap_callback table already created\n")
    end.    

delete_callback_table() ->
  ets:delete(soap_callback).

add_callback(URL,WSD,Fun) ->
   io:format("url is ~p\n",[URL]),
   io:format("WSD is ~p\n",[WSD]),
   io:format("Fun is ~p\n",[Fun]),
  case soap_registry_server:add_wsdl(URL,WSD) of
    ok -> add_callback(URL,Fun);
    NotOk -> NotOk
  end.

addWsdls([]) ->
  ok;
addWsdls([{Url, WSD, Fun} | Tail]) ->
    io:format("url111 is ~p\n",[Url]),
  add_callback(Url,WSD,Fun),
  addWsdls(Tail).

add_callback(URL,Fun) ->
  case ets:insert(soap_callback,{URL,Fun}) of
    true -> ok;
    NotTrue -> NotTrue
  end.


do_post(Ip,Info,Uri) ->
	case ets:lookup(soap_callback,Uri) of
		[{_Url, Fun}] ->
			case soap_registry_server:get_xsd(Uri) of
				{ok, Model} ->
					parse_soap_body(Ip,Info, Model, Fun);
                                  _->
				   ok
			end;
                _->
		ok
	end.


parse_soap_body(Ip,Line,Model,Fun) ->
   %io:format("Line is  ~p~n",[Line]),
 case Line of
	undefined->
		 do_soap_body(Ip,undefined,undefined,Model,Fun);
    _->
	  Len=size(Line),
	  if(Len>0)->
		case (catch erlsom:parse(Line,Model)) of
			{error, Error} ->
			   io:format("could not parse err= ~p~n",[Error]),
			    make_error([], "Server", "Server error", []);
			{ok,Parsed} ->
			     %%io:format("xml is  ~p~n",[Parsed]),
			    {Header,Body,SoapVer} = header_body(Parsed),
			    do_soap_body(Ip,Header,Body,Model,Fun);
			 Other ->
			      io:format("Other~p~n~n",[Other]),
				  make_error([], "Server", "Server error", [])
		end;
	     true->
		do_soap_body(Ip,undefined,undefined,Model,Fun)
	     end
      end.

header_body({'P:Envelope',_AnyArgs, {'P:Header', _AnyArgs3,Header }, {'P:Body', _AnyArgs2, Body},_}) ->
    %% SOAP 1.1
	{Header, Body,?soap_ver_1_1};
header_body({'P:Envelope',_AnyArgs,Header , {'P:Body', _AnyArgs2, Body},_}) ->
    %% SOAP 1.1
	{Header, Body,?soap_ver_1_1};

header_body(Other) ->
	{undefined,undefined,Other}.

do_soap_body(Info,undefined,undefined,Other,Model,Fun,Line) ->
    ?PRINT_MESSAGE("Parsing failed, Result:~p~nModel:~p~nLine:~p~n",[Other,Model,Line]),
    {proceed,Info#mod.data}.   	

do_soap_body(Ip,Header, Body,Model,Fun) ->
      %io:format("do_soap_body Header is::::::::: ~p\n",[Header]),
    %io:format("do_soap_body Body is::::::::::: ~p\n",[Body]),
    case (catch Fun(Ip,Header, Body)) of
        {ok,undefined}->
		{ok,[],[]};
        {ok,HttpHdr,undefined}->
		{ok,HttpHdr,[]};
    	{ok,Res} ->
	        %io:format("res is ~p\n",[Res]),
      		make_reply(undefined, Res, [], Model, ?soap_ver_1_1);
    	{ok,Hdr,Res} ->
      		make_reply(Hdr, Res, [], Model, ?soap_ver_1_1);
        {ok,Hdr,Res, HttpHdr} ->
      		make_reply(Hdr, Res, HttpHdr, Model, ?soap_ver_1_1);
    	{error,FaultCode, FaultString, FaultDetail,HttpHdr} ->
		    %io:format("error1 is:::::::: ~p\n",[FaultDetail]),
      		make_error(FaultDetail,FaultCode, FaultString, HttpHdr);
	    {error,FaultCode, FaultString,FaultDetail} ->
	        %io:format("error2 is:::::::: ~p\n",[FaultDetail]),
      		make_error(FaultDetail,FaultCode, FaultString, []);
    	{error, FaultCode, FaultString} ->
		    %io:format("error3 is::::::::: ~p\n",[FaultString]),
      		make_error([],FaultCode, FaultString, []);
    	{error,FaultString} ->
                make_error([],"Client", FaultString, []);
		_Other ->
            io:format("do_soap_body _Other ::::::~p~n~n",[_Other]),
			{ok,[],[]}            			
  	end.

do_soap_body(Re)->
  %io:format("mod_soap Re::::::::::~p~n",[Re]),
  case soap_registry_server:get_xsd("/monitor") of
   {ok,Model}->
    case Re of
        {ok,undefined}->
		{ok,[],[]};
        {ok,HttpHdr,undefined}->
		{ok,HttpHdr,[]};
    	{ok,Res} ->
	        %io:format("res is ~p\n",[Res]),
      		make_reply(undefined, Res, [], Model, ?soap_ver_1_1);
    	{ok,Hdr,Res} ->
      		make_reply(Hdr, Res, [], Model, ?soap_ver_1_1);
        {ok,Hdr,Res, HttpHdr} ->
      		make_reply(Hdr, Res, HttpHdr, Model, ?soap_ver_1_1);
    	{error,FaultCode, FaultString, FaultDetail,HttpHdr} ->
      		make_error(FaultDetail,FaultCode, FaultString, HttpHdr);
	    {error,FaultCode, FaultString,FaultDetail} ->
      		make_error(FaultDetail,FaultCode, FaultString, []);
    	{error, FaultCode, FaultString} ->
      		make_error([],FaultCode, FaultString, []);
    	{error,FaultString} ->
            make_error([],"Client", FaultString, []);
		_Other ->
            io:format("do_soap_body _Other222 ::::::~p~n~n",[_Other]),
			{ok,[],[]}				
	end;
    _Other->
	       %io:format("do_soap_body _Other:::::::::~p~n",[_Other]), 
           make_error([],"Server", "Server error", [])
      end.

make_reply(SoapHeader, SoapBody, HttpHeaders, Model, SoapVer) ->
    %io:format("make_reply11111111111111111111111111"),
    Envelope = case SoapVer of
                   ?soap_ver_1_1 ->
        				{'soap:Envelope',undefined,SoapHeader,
                        {'soap:Body',undefined,SoapBody},undefined};
                   ?soap_ver_1_2 ->
            			{'soap:Envelope',undefined,SoapHeader,
                        {'soap:Body',undefined,SoapBody}}
                 end,   
   	case (catch erlsom:write(Envelope,Model)) of
    	{ok, XML} -> 
	    %io:format("reply is ok ~p~n",[XML]),
	  Content = "<?xml version=\"1.0\" encoding=\"utf-8\"?>"++XML,
	  {ok,HttpHeaders, Content};
    	_R -> 
		    %io:format("make_reply:rrrrrrrrrrrr::::::::~p~n",[_R]),
            make_error([], "Server", "Server error", HttpHeaders)
	end.

make_reply(Info, String, Headers) ->
  %io:format("make_reply2222222222222222222222"),
  Content = "<?xml version=\"1.0\" encoding=\"utf-8\"?>"++String,
  {break,[{response,{response,
                       [{code,200},
                        {content_length,integer_to_list(length(Content))},
                        {content_type,"text/xml; charset=UTF8"}|
                        Headers],
                       Content
                      }
            }|
            Info#mod.data]}.



make_error(ErrorDetail, FaultCode, FaultString, HttpHeaders) ->
	?PRINT_MESSAGE("Error is ~s~n",[FaultString]),
	Content = "<?xml version=\"1.0\" encoding=\"utf-8\"?>" ++
          yaws_soap_lib:makeFault(FaultCode,FaultString,ErrorDetail),
	  {error,HttpHeaders,Content}.

parse_content_type(ContentType) ->
	{BasicType,Rest} = get_until(ContentType,";"),
	Params = get_header_params(Rest),
	{BasicType,Params}.

parse_parts(Boundary,ContentTypeParams,Body) ->
	case regexp:split(Body,"[\n\r]*.*"++Boundary++".*[\r\n]*") of
	{ok,PostElementsPlus} ->
		PostElements=remove_all([],PostElementsPlus),
		ParsedElements =
		lists:map(
		  fun(PE) ->
			  {PEHeader,PEBody} = header_lines(PE),
			  {isolate_args(PEHeader),
			   PEBody}
		  end,
		  PostElements),
		case lists:keysearch("start",1,ContentTypeParams) of
		{value,{"start",StartElementId}} ->
			[StartElement] =
			get_by_content_id(StartElementId,
					  ParsedElements),
			{StartElement,lists:delete(StartElement,ParsedElements)};
		_ ->
			?PRINT_MESSAGE("No start part in ~p, using the first~n",[ContentTypeParams]),
			{hd(ParsedElements),tl(ParsedElements)}
		end;
	_ ->
		?PRINT_MESSAGE("No boundary ~s in ~n~s~n",[Boundary,Body]),
		{[],[]}
	end.

get_by_content_id(ContentID,Elements) ->
	lists:filter(
	  fun({PEHeader,_PEBody}) ->
		  lists:member({"content-id",ContentID},PEHeader);
	 (_) ->
		  false
	  end,
	  Elements).

isolate_args(Fields) ->
	lists:map(fun isolate_arg/1, Fields).

isolate_arg(Str) -> isolate_arg(Str, []).

isolate_arg([$:,$ |T], L) -> {string:to_lower(lists:reverse(L)), T};
isolate_arg([H|T], L)	  -> isolate_arg(T, [H|L]).

get_header_params(String) ->
	get_header_params(String,[]).

get_header_params([],Res) ->
	lists:reverse(Res);
get_header_params([$ |More],Res) ->
	get_header_params(More,Res);
get_header_params([$\t|More],Res) ->
	get_header_params(More,Res);
get_header_params([$\r|More],Res) ->
	get_header_params(More,Res);
get_header_params([$\n|More],Res) ->
	get_header_params(More,Res);
get_header_params([$\;|More],Res) ->
	get_header_params(More,Res);
get_header_params(String,Res) ->
	case get_until(String,"=") of
	{Name,[]} ->
		lists:reverse([{Name,[]}|Res]);
	{Name,Rest} ->
		{Param,MoreRest} =
		get_string(Rest),
		get_header_params(MoreRest,[{Name,Param}|Res])
	end.

get_string([$"|More]) ->
	get_quoted_string(More);
get_string(String) ->
	get_unquoted_string(String).

get_unquoted_string(String) ->
	get_unquoted_string(String,[]).


get_unquoted_string([$\\,$ |More],Res) ->
	get_unquoted_string(More,[$ |Res]);
get_unquoted_string([$ |More],Res) ->
	{lists:reverse(Res),More};
get_unquoted_string([],Res) ->
	{lists:reverse(Res),[]};
get_unquoted_string([H|T],Res) ->
	get_unquoted_string(T,[H|Res]).

get_quoted_string(String) ->
	get_quoted_string(String,[]).

get_quoted_string([$\\,$"|More],Res) ->
	get_unquoted_string(More,[$ |Res]);
get_quoted_string([$"|More],Res) ->
	{lists:reverse(Res),More};
get_quoted_string([],Res) ->
	{lists:reverse(Res),[]};
get_quoted_string([H|T],Res) ->
	get_quoted_string(T,[H|Res]).


get_until(List,Element) ->
	get_until(List,Element,[]).

get_until([],_,Res) ->
	{lists:reverse(Res),[]};
get_until(L,H,Res) ->
	case lists:prefix(H,L) of
	true ->
		{lists:reverse(Res),lists:nthtail(length(H),L)};
	false ->
		get_until(tl(L),H,[hd(L)|Res])
	end.

header_lines(String) ->
	{yes,Header,Rest} =
	scan_header(String,[]),
	{ok,HeaderLines} =
	regexp:split(Header,"\\r?\\n"),
	{HeaderLines,Rest}.

scan_header([$\r,$\n,$\t|T],L) -> scan_header(T,[$ |L]);
scan_header([$\n,$\t|T],L) -> scan_header(T,[$ |L]);
scan_header([$\n,$ |T],L) -> scan_header(T,[$ |L]);
scan_header([$\r,$\n|T],L) -> scan_header([$\n|T],L);
scan_header([$\n,$\r|T],L) -> scan_header([$\n|T],L);
scan_header([$\n|T], [$\n|L]) -> {yes, lists:reverse(L), T};
scan_header([$\n|T], [$\r,$\n,$\r|L]) -> {yes, lists:reverse(L), T};
scan_header([$\n|T], [$\r,$\n|L]) -> {yes, lists:reverse(L), T};
scan_header([H|T],	L)				  -> scan_header(T, [H|L]);
scan_header([], L)					  -> {no, L}.

remove_all(Element,List) ->
	case lists:delete(Element,List) of
	List ->
		List;
	Less ->
		remove_all(Element,Less)
	end.

monitor_result(Info,{break,Result}) ->
	ReqHeaders =
	[pformat(Tag)++": "++pformat(Value)++"\r\n"||{Tag,Value}<-Info#mod.parsed_header],
	{ParsedRespHeaders,RespContent} =
	case lists:keysearch(response,1,Result) of
		{value,{response,
			{response,
			 RHeaders,
			 RContent}
		   }} ->
		{RHeaders,RContent};
		_ ->
		{"",""}
	end,
	RespHeaders =
	[pformat(Tag)++": "++pformat(Value)++"\r\n"||{Tag,Value}<-ParsedRespHeaders],
	Req=lists:flatten(ReqHeaders++"\r\n"++Info#mod.entity_body),
	Resp=lists:flatten(RespHeaders++"\r\n"++RespContent),
	monitor_server:send_data(make_long(length(Req))++
				 make_long(length(Resp))++
				 Req++Resp);
monitor_result(_Info,_OtherResult) ->
	ok.

make_long(Long) ->
	[Long band 255,
	 (Long bsr 8) band 255,
	 (Long bsr 16) band 255,
	 (Long bsr 24) band 255].

pformat(I) when integer(I) ->
	integer_to_list(I);
pformat(S) when list(S) ->
	S;
pformat(A) when atom(A) ->
	atom_to_list(A);
pformat(Other) ->
	lists:flatten(io_lib:format("~w",[Other])).

priv_dir() -> 
    filename:join([filename:dirname(code:which(mod_soap)),"..", "resources"]).
    
remove(_) -> 
    io:format("Callback table removed!~n"),
  	delete_callback_table().

load(Config) -> 
  Conf_dir= filename:join([priv_dir(),Config]),
  [Line]=node_conf:get_nodes("WSDL",Conf_dir),
  case string:tokens(Line, " ") of
    ["WSDL", Url, Wsdl, ModuleName] ->
         Wsdl_dir=filename:join([priv_dir(),Wsdl]),
      Module = list_to_atom(ModuleName), 
      add_callback(Url,Wsdl_dir, 
        fun(Ip,H, B) -> Module:handler(Ip,H,B) end),
        {ok, Line};
    _ -> 
      {ok, Line}
  end.

%% Does this make sense? I assume that "mod_soap" is always there,
%% otherwise the mod_soap:load() function would not be called...
startModule(["mod_soap" | _T],Config) ->
  	io:format("start module~n"),
  	% monitor_server:start_link(),
  	% io:format("monitort_server started~n"),
  	case soap_registry_server:status() of
		started ->
			do_nothing;
		stopped ->
		    soap_registry_server:start_link()
	end,
  	io:format("registry_server started~n"),
  	create_callback_table(),
	load(Config),
  	io:format("callback table created~n");

startModule([_X | T],Condir) ->
  	startModule(T,Condir);
startModule([],_C) ->
 	ok.


