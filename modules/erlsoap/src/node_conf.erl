-module(node_conf).
-compile(export_all).

read_config_file(FileName) ->
    case file:open(FileName, [read]) of
	{ok, Stream} ->
	    read_config_file(Stream, []);
	{error, _Reason} ->
	    {error, "fail"}
    end.
read_config_file(Stream, SoFar) ->
    case io:get_line(Stream, []) of
	eof ->
	    file:close(Stream),
	    {ok, lists:reverse(SoFar)};
	{error, Reason} ->
	    file:close(Stream),
	    {error, Reason};
	[$#|_Rest] ->
	    read_config_file(Stream, SoFar);
	Line ->
	    {ok, NewLine, _}=regexp:sub(clean(Line),"[\t\r\f ]"," "),
	    case NewLine of
		[] ->
		    read_config_file(Stream, SoFar);
		_Other ->
		    read_config_file(Stream, [NewLine|SoFar])
	    end
    end.

clean(String) ->
    {ok,CleanedString,_} = 
	regexp:gsub(String, "^[ \t\n\r\f]*|[ \t\n\r\f]*\$",""),
    CleanedString.

ping_tracker([],Msg)->
   case Msg of
      ok->
         ok;
      error->
         fail
   end;
ping_tracker([H|T],M)->
      case net_adm:ping(list_to_atom(string:strip(H))) of
          pong->
	    ping_tracker([],ok);
          pang->
	    ping_tracker(T,error)
       end.

ping_base([],Msg)->
   case Msg of
      ok->
         ok;
      error->
         fail
   end;
ping_base([H|T],M)->
      case  mnesia:change_config(extra_db_nodes,[list_to_atom(string:strip(H))]) of
          {ok,[N]}->
	    ping_base([],ok);
          _->
	    ping_base(T,error)
       end.


load(Nodes,Name)->
  lists:filter(fun(X)->
          string:str(string:strip(X),Name)==1
        end,Nodes).



get_nodes(Nodename,FileName)->
   Re=read_config_file(FileName),
   case Re of
      {ok,Nodes}->
        R=load(Nodes,Nodename);
      
      _->
            error
      end.


     
          


