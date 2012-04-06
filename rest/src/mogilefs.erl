-module(mogilefs).

-define(DELETE,"DELETE").
-define(GET_DOMAINS,"GET_DOMAINS").
-define(GET_PATHS,"GET_PATHS").
-define(RENAME,"RENAME").
-define(LIST_KEYS,"LIST_KEYS").
-define(CREATE_OPEN,"CREATE_OPEN").
-define(CREATE_CLOSE,"CREATE_CLOSE"). 
-define(SUCCESS,"OK").    % Tracker success code
-define(ERROR,"ERR").   % Tracker error code
-define(DEFAULT_PORT,6001).    % Tracker port
-define(CREATE_DOMAIN,"CREATE_DOMAIN").
-define(DELETE_DOMAIN,"DELETE_DOMAIN").
-define(CREATE_CLASS,"CREATE_CLASS").
-define(UPDATE_CLASS,"UPDATE_CLASS").
-define(DELETE_CLASS,"DELETE_CLASS").
-define(LIST_CLASS,"LIST_CLASS").
-define(STATS,"STATS").
-define(SET_STATE,"SET_STATE").
-define(GET_HOSTS,"GET_HOSTS").
-define(GET_DEVICES,"GET_DEVICES").
-define(LIST_FIDS,"LIST_FIDS").

-define(TIMEOUT, 10000).
-compile(export_all).
-record(mogilefs,{sock,domain,class}).


test()->
	{ok,M} = new("192.168.4.119",6001,"3ren","3ren"),
    io:format("stats:~p~n",[stats(M)]),
	io:format("getDomains:~p~n",[getDomains(M)]),
	Result = setData(M,[{"key","91512e0d2a8b09e"}],<<"012312746921876498764921649142">>),
	io:format("setData:~p~n",[Result]),
	io:format("exists:~p~n",[exists(M,[{"key","91512e0d2a8b09e"}])]),
	io:format("getData:~p~n",[getData(M,[{"key","91512e0d2a8b09e"}])]),
	io:format("delete:~p~n",[delete(M,[{"key","91512e0d2a8b09e"}])]),
	io:format("exists:~p~n",[exists(M,[{"key","91512e0d2a8b09e"}])]),
	io:format("getData:~p~n",[getData(M,[{"key","91512e0d2a8b09e"}])]).



new(IP,Port,Domain,Class) ->	
    inets:start(),
	case gen_tcp:connect(IP, Port, [binary, {active, false}, {packet, raw}]) of	
	     {ok, Socket}  -> {ok,#mogilefs{sock=Socket,domain=Domain,class=Class}};
		 {error, Reason} -> {error,Reason}
	end.


getData(M,Key)->
     case getPaths(M,Key) of
	      {ok,[{_,Path}|_]} ->		     
			 case http:request(Path) of
			      {ok,{_,_,Body}} -> {ok,list_to_binary(Body)};
				  _ -> error
			 end;
		  _ -> error
	 end.


getPaths(M,Key) ->
	case doRequest(M,?GET_PATHS,Key) of
	     [?SUCCESS|Rest] -> {ok,hd(Rest)};
		 _ -> error
	end.



doRequest(#mogilefs{sock=Sock},?GET_DOMAINS,Args) -> 
         do_Request(Sock,?GET_DOMAINS++"&"++mochiweb_util:urlencode(Args)++"\n");
doRequest(#mogilefs{sock=Sock},?CREATE_DOMAIN,Args) -> 
         do_Request(Sock,?CREATE_DOMAIN++"&"++mochiweb_util:urlencode(Args)++"\n");
doRequest(#mogilefs{sock=Sock},?DELETE_DOMAIN,Args) -> 
         do_Request(Sock,?DELETE_DOMAIN++"&"++mochiweb_util:urlencode(Args)++"\n");
doRequest(#mogilefs{sock=Sock},?STATS,Args) -> 
         do_Request(Sock,?STATS++"&"++mochiweb_util:urlencode(Args)++"\n");
doRequest(#mogilefs{sock=Sock},?CREATE_CLASS,Args) -> 
         do_Request(Sock,?CREATE_CLASS++"&"++mochiweb_util:urlencode(Args)++"\n");
doRequest(#mogilefs{sock=Sock},?UPDATE_CLASS,Args) -> 
         do_Request(Sock,?UPDATE_CLASS++"&"++mochiweb_util:urlencode(Args)++"\n");
doRequest(#mogilefs{sock=Sock},?DELETE_CLASS,Args) -> 
         do_Request(Sock,?DELETE_CLASS++"&"++mochiweb_util:urlencode(Args)++"\n");	
doRequest(#mogilefs{sock=Sock,domain=Domain,class=Class},Cmd,Args) -> 
         do_Request(Sock,Cmd++"&"++mochiweb_util:urlencode([{domain,Domain},{class,Class}])++"&"++mochiweb_util:urlencode(Args)++"\n");			 
doRequest(_,_,_) -> error.

do_Request(Sock,Extra)	->
    Extrabin = list_to_binary(Extra),
	case gen_tcp:send(Sock,<<Extrabin/binary>>) of
	    ok -> recv_header(Sock,"",[],<<>>,<<>>);
		{error, Reason} -> {error, Reason}
	end.
	

recv_header(_, _, _, <<"\r\n", _Rest/binary>>, Header) ->
	case string:tokens(binary_to_list(Header), " ") of
		[?SUCCESS,Result] -> [?SUCCESS,mochiweb_util:parse_qs(Result)];
		Tokens -> Tokens
	end;
recv_header(Sock, Callback, Args, <<>>, Header) ->
    {ok, Bin} = gen_tcp:recv(Sock, 0, ?TIMEOUT),
	recv_header(Sock, Callback, Args, Bin, Header);

recv_header(Sock, Callback, Args, <<B:8, Rest/binary>>, Header) ->
    recv_header(Sock, Callback, Args, Rest, <<Header/binary, B:8>>).


getDomains(M) ->
    case doRequest(M,?GET_DOMAINS,[]) of
	     [?SUCCESS|Rest] -> {ok,hd(Rest)};
		 _ -> error
	end.

	
	
createDomain(M,Domain)->
     case doRequest(M,?CREATE_DOMAIN,[{"domain",Domain}]) of
	     [?SUCCESS|Rest] -> {ok,hd(Rest)};
		 _ -> error
	 end.	
	
	
deleteDomain(M,Domain)->
     case doRequest(M,?DELETE_DOMAIN,[{"domain",Domain}]) of
	     [?SUCCESS|Rest] -> {ok,hd(Rest)};
		 _ -> error
	 end.		


createClass(M,Domain,Class,Mindevcount)->
     case doRequest(M,?CREATE_CLASS,[{"domain",Domain},{"class",Class},{"mindevcount",Mindevcount}]) of
	     [?SUCCESS|Rest] -> {ok,hd(Rest)};
		 _ -> error
	 end.		

deleteClass(M,Domain,Class,Mindevcount)->
	 case doRequest(M,?DELETE_CLASS,[{"domain",Domain},{"class",Class},{"mindevcount",Mindevcount}]) of
	     [?SUCCESS|Rest] -> {ok,hd(Rest)};
		 _ -> error
	 end.	
	
updateClass(M,Domain,Class,Mindevcount)->
     case doRequest(M,?UPDATE_CLASS,[{"domain",Domain},{"class",Class},{"mindevcount",Mindevcount}]) of
	     [?SUCCESS|Rest] -> {ok,hd(Rest)};
		 _ -> error
	 end.
	


stats(M)->
     case doRequest(M,?STATS,[]) of
	     [?SUCCESS] -> {ok,[]};
		 _ -> error
	 end.
	

exists(M,Key) ->
     case doRequest(M,?GET_PATHS,Key) of
	     [?SUCCESS|_] -> true;
		 _ -> false
	 end.

setData(M,Key,Data)->
    case  doRequest(M,?CREATE_OPEN,Key) of
	   [?SUCCESS|Rest] ->
	        case hd(Rest) of
			     [{"devid",Devid},{"fid",Fid},{"path",Path}] -> 
						Len = integer_to_list(size(Data)),
						Header = [{"content-length",Len},{"content-type","application/octet-stream"}],
						case http:request(put,{Path,Header,"application/octet-stream",<<Data/binary>>},[],[]) of
						      {ok,_} -> 
							       case doRequest(M,?CREATE_CLOSE,Key++[{"devid",Devid},{"fid",Fid},{"path",Path}]) of
								        [?SUCCESS] -> {ok,[]};
										_ -> error
								   end;							   	   
							  _ -> error
						end;
			      _ -> error
			end;
	   _ -> error	
	end.
%	[Result|Rest] = doRequest(M,?CREATE_OPEN,Key),
%	if Result =:= ?SUCCESS ->
%		[[{"devid",Devid},{"fid",Fid},{"path",Path}]] = Rest,
%		inets:start(),
%		Len = integer_to_list(size(Data)),
%		Header = [{"content-length",Len},{"content-type","application/octet-stream"}],
%		R = http:request(put,{Path,Header,"application/octet-stream",<<Data/binary>>},[],[]),
%		inets:stop(),
%		%io:format("http resut:~p~n",R),
%			case R of
%			{ok,_}->
%				[Result1|Rest1] = doRequest(M,?CREATE_CLOSE,Key++[{"devid",Devid},{"fid",Fid},{"path",Path}]),
%					[Result1|Rest1];
%				
%			 _->
%				false
%		end;
%	   true->
%		false
%	end.



delete(M,Key)->
     case doRequest(M,?DELETE,Key) of
	     [?SUCCESS|_] -> true;
		 _ -> false
	 end.
	
		
close(M)->
	#mogilefs{sock=Sock}=M,
	gen_tcp:close(Sock).

	


