-module(tuplesxml).

-include("log.hrl").
-include("config.hrl").
-include("xmerl.hrl").

-compile(export_all).
%-export([convert/1]).

%%%file:write_file("test.txt",io_lib:format("~p",[tuplesxml:tuple_to_xml([])])).
%%7> file:write_file("test1.xml",tuplesxml:tuple_to_xml([{aaa,"bbbb"}])).

func_get_convert() ->
    {ok,Data} =  file:consult("test.conf"),
    Data. 
    
func_get_convert(_) ->
    {ok,Data} =  file:consult("test.conf"),
    Data.   

func_get_convert(_Host,_Req,_Path,_Raw_path) ->
    case file:consult("test.conf") of
        {ok,Data} ->	
           %?Log({"func_get_convert: ~p~n",[Data]}),     
           Xml = tuple_to_xml(Data),
           %?Log({"func_get_convert: ~p~n",[Xml]}),
           common:respond(Xml);
        _ -> 
           common:respond(other)
	end.

testxml() -> 
  {ok,Data} =  file:consult("test.txt"),
  io:format("~p~n",[Data]),
  Result = tuple_to_xml(hd(Data)),
  restlog:writelog(Result).
  
tuple_to_xml([]) ->
    Xml = [<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?><feed xmlns=\"http://www.w3.org/2005/Atom\" xmlns:xn=\"http://www.siteview.com/atom/1.0\"><value type=\"string\"></value></feed>">>],
    list_to_binary(Xml);

tuple_to_xml(Value) when is_list(Value) ->
  case is_string(Value) of
        true ->
           Xml = [<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?><feed xmlns=\"http://www.w3.org/2005/Atom\" xmlns:xn=\"http://www.siteview.com/atom/1.0\"><value type=\"string\">">>,Value,<<"</value></feed>">>],
           list_to_binary(Xml);
        _ ->  
           %F = fun(X) -> tuples_xml_convert(X) end,
           %Xmldata = lists:map(F,Value),
           Xmldata = tuples_xml_convert(Value),
           Xml = [<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?><feed xmlns=\"http://www.w3.org/2005/Atom\" xmlns:xn=\"http://www.siteview.com/atom/1.0\">">>,Xmldata,<<"</feed>">>],
           list_to_binary(Xml)
   end;
   
tuple_to_xml(Value) when is_tuple(Value) -> 
       Xmldata = tuples_xml_convert(Value), 
       %Xmldata = tuples_xml_convert({tuple,tuple_to_list(Value)}), 
       Xml = [<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?><feed xmlns=\"http://www.w3.org/2005/Atom\" xmlns:xn=\"http://www.siteview.com/atom/1.0\">">>,Xmldata,<<"</feed>">>],
       list_to_binary(Xml);
  
  
tuple_to_xml(Value) when is_binary(Value) ->
    Xml = [<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?><feed xmlns=\"http://www.w3.org/2005/Atom\" xmlns:xn=\"http://www.siteview.com/atom/1.0\"><value type=\"binary\">">>,Value,<<"</value></feed>">>],
    list_to_binary(Xml);

tuple_to_xml(Value) when is_atom(Value) ->
    Xml = [<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?><feed xmlns=\"http://www.w3.org/2005/Atom\" xmlns:xn=\"http://www.siteview.com/atom/1.0\"><value type=\"string\">">>,atom_to_list(Value),<<"</value></feed>">>],
    list_to_binary(Xml);

tuple_to_xml(Value) when is_integer(Value) ->
    Xml = [<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?><feed xmlns=\"http://www.w3.org/2005/Atom\" xmlns:xn=\"http://www.siteview.com/atom/1.0\"><value type=\"integer\">">>,integer_to_list(Value),<<"</value></feed>">>],
    list_to_binary(Xml);
    
tuple_to_xml(_) ->
    Xml = codeerror("errorcode:100","Convert is faulted!"),
    list_to_binary(Xml).
    
    
codeerror(Code,Msg) ->
     lists:flatten(["<?xml version=\"1.0\" encoding=\"UTF-8\" ?>",
                      "<feed xmlns=\"http://www.w3.org/2005/Atom\" xmlns:xn=\"http://www.siteview.com/atom/1.0\">",
                      "<tuple>"
                      "<value type=\"string\">",Code,"</value>" 
                      "<value type=\"string\">",Msg,"</value>"
                      "</tuple>",
                      "</feed>"]). 
                      
sure_is_atom(Value) when is_atom(Value)->
   case hd(atom_to_list(Value)) of
        Element when Element > $9-> true;
        Element when Element < $0-> true;
        _ -> false
   end;
   
sure_is_atom(_) -> false.
      

tuples_xml_convert(Item) ->      
    case is_string(Item) of
          true -> [<<"<value type=\"string\">">>,html_util:escape(Item),<<"</value>">>];
          _  when is_integer(Item) -> [<<"<value type=\"integer\">">>,integer_to_list(Item),<<"</value>">>];
          _  when is_float(Item) -> [<<"<value type=\"float\">">>,io_lib:format("~p",[Item]),<<"</value>">>];
          _  when is_atom(Item) -> [<<"<value type=\"atom\">">>,html_util:escape(atom_to_list(Item)),<<"</value>">>];
          _  when is_tuple(Item) ->                   
                  case Item of
                        {}  -> [<<"<tuple></tuple>">>];  
                        {Value}  ->                            
                            [<<"<tuple>">>,tuples_xml_convert(Value),<<"</tuple>">>]; 
                        {Name,Value} when is_atom(Name) andalso Value =:= [] ->  
                                   NewName = atom_to_list(Name),
                                   [<<"<">>,NewName,<<"></">>,NewName,<<">">>];       
                        {tuple,Value}  ->   
                            %?Log({"func_get_convert: ~p~n",[Item]}),
                             F = fun(X) -> tuples_xml_convert(X) end,
                             NewValue = lists:map(F,Value),
                             [<<"<tuple>">>,NewValue,<<"</tuple>">>];
                       {tuple1,Value}  ->   
                            %?Log({"func_get_convert: ~p~n",[Item]}),
                             F = fun(X) -> tuples_xml_convert(X) end,
                             lists:map(F,Value);    
                        {Name,Value} when is_atom(Name) -> 
                                   NewName = atom_to_list(Name),     
                                   case is_string(Value) of
                                         true -> [<<"<key_">>,NewName,<<" type=\"string\">">>,html_util:escape(Value),<<"</key_">>,NewName,<<">">>];
                                         _ when is_atom(Value) -> [<<"<key_">>,NewName,<<" type=\"atom\">">>,html_util:escape(atom_to_list(Value)),<<"</key_">>,NewName,<<">">>];
                                         _ when is_integer(Value) -> [<<"<key_">>,NewName,<<" type=\"integer\">">>,integer_to_list(Value),<<"</key_">>,NewName,<<">">>];
                                         _ when is_float(Value) -> [<<"<key_">>,NewName,<<" type=\"float\">">>,io_lib:format("~p",[Value]),<<"</key_">>,NewName,<<">">>];
                                         _ when is_binary(Value) -> [<<"<key_">>,NewName,<<" type=\"binary\">">>,binary_to_xml(Value),<<"</key_">>,NewName,<<">">>];
                                         _ -> 
                                             NewValue = tuples_xml_convert(Value),
                                             [<<"<key_">>,NewName,<<">">>,NewValue,<<"</key_">>,NewName,<<">">>]
                                   end;
                        {Name,Value} when is_atom(Name) andalso is_tuple(Value) ->  
                                    NewValue = tuples_xml_convert({tuple,tuple_to_list(Value)}),
                                    NewName = atom_to_list(Name), 
                                    [<<"<key_">>,NewName,<<">">>,NewValue,<<"</key_">>,NewName,<<">">>];
                        _ ->  
                                 %?Log({"func_get_convert: ~p~n",[Item]}),
                                 case element(1,Item) of
                                       Element1 when is_atom(Element1) ->
                                           NewValue = tuples_xml_convert({tuple1,lists:nthtail(1,tuple_to_list(Item))}),
                                           NewName = atom_to_list(Element1), 
                                           [<<"<key_">>,NewName,<<">">>,NewValue,<<"</key_">>,NewName,<<">">>];
                                       _ ->
                                           NewValue = tuples_xml_convert({tuple,tuple_to_list(Item)}),
                                           NewValue
                                           %~ [<<"<listitem>">>,NewValue,<<"</listitem>">>]
                                 end
                  end;                        
          _  when is_binary(Item) -> [<<"<value type=\"binary\">">>,binary_to_xml(Item),<<"</value>">>];
          _  when is_list(Item) -> 
                        F = fun(X) -> tuples_xml_convert(X) end,
                        NewValue = lists:map(F,Item),
                        [<<"<listitem>">>,NewValue,<<"</listitem>">>];
          _ -> <<"">>
    end.      

binary_to_xml(Value) ->
   case isprint_binary(Value) of
       true -> html_util:escape(binary_to_list(Value));
       _ ->  [<<"<binary>">>,convert_to_binary(Value,[]),<<"</binary>">>]
   end.

isprint_binary(<<Element, Next/binary>>) when Element < 127 andalso Element > 31 ->  isprint_binary(Next);
isprint_binary(<<_, _/binary>>)  ->  false;
isprint_binary(<<>>)  -> true.

convert_to_binary(<<Element, Next/binary>>,[]) ->  convert_to_binary(Next,[integer_to_list(Element)]);
convert_to_binary(<<Element, Next/binary>>,Acc) ->  convert_to_binary(Next,[[",",integer_to_list(Element)]|Acc]);
convert_to_binary(<<>>,Acc)  -> lists:flatten(lists:reverse(Acc)).
    

is_string(Value) when not is_list(Value) -> false;
%is_string([L|_]) when length(L) > 1 -> false;
is_string([L|_]) when is_integer(L) andalso L > 255 -> false;
is_string([L|_]) when not is_integer(L)-> false;
is_string([[]]) -> false;
is_string([_|R]) -> is_string(R);
is_string([]) -> true.




%~ is_string(Value) when not is_list(Value) -> false;
%~ %is_string([L|_]) when length(L) > 1 -> false;
%~ is_string([L|_]) when is_integer(L) andalso L > 255 -> false;
%~ is_string([L|_]) when not is_list(L) andalso not is_integer(L)-> false;
%~ is_string([L|R]) when is_list(L) -> 
    %~ case is_string(L) of
        %~ false  -> false;
	%~ _ -> is_string(R)
    %~ end;
%~ is_string([_|R]) -> is_string(R);
%~ is_string([]) -> true.




is_list_list([]) ->  false;
is_list_list([Head|_]) when is_list(Head)->  true; 
is_list_list(null) ->  false;
is_list_list(_) ->  false.  

test() ->
    {ok, Binary} =  file:read_file("test.xml"),
    Xmldata = xml_common:simplexml_read_string(binary_to_list(Binary)), 
    Xmllist = xmerl_xs:select("*", Xmldata),
    xml_tuple_convet(Xmllist).
    
xml_to_tuple(Xml) ->  
    Xmldata = xml_common:simplexml_read_string(Xml), 
    Xmllist = xmerl_xs:select("*", Xmldata),
    xml_tuple_convet(Xmllist).
    
xml_tuple_convet(Xml) ->    
  F = fun(X) -> 
           %io:format("~p~n",[X#xmlElement.name]),
           case xmerl_xs:select("*", X) of
                [] -> 
                    Value = html_util:unescape(string:strip(lists:flatten(xmerl_xs:value_of(X)))),
                    case X#xmlElement.name of
                         value ->
                             case xml_common:get_xmlAttribute_value("@type",X) of
                                  "string" when Value=:="\n" orelse Value=:=[] -> [];
                                  "string" -> Value;
                                  "atom" -> list_to_atom(Value);
                                  "binary" -> list_to_binary(Value);
                                  "integer" -> list_to_integer(Value);
                                  "float" -> list_to_float(Value);
                                  _ -> Value
                            end;
                         binary ->  xml_to_binary(Value);
                         tuple when Value =:= "\n" -> {};
                         _ ->
                            case xml_common:get_xmlAttribute_value("@type",X) of
                                  "string" when Value=:="\n" -> [];
                                  "string" -> {stripkey(X#xmlElement.name),Value};
                                  "atom" -> {stripkey(X#xmlElement.name),list_to_atom(Value)};
                                  "binary" -> {stripkey(X#xmlElement.name),list_to_binary(Value)};
                                  "integer" -> {stripkey(X#xmlElement.name),list_to_integer(Value)};
                                  "float" -> {stripkey(X#xmlElement.name),listtofloat(Value)};
                                  _ -> {stripkey(X#xmlElement.name),Value}
                            end
                    end;
                ChildXml ->  
                       %io:format("___________~p~n",[ChildXml]),
                          Newvalue = xml_tuple_convet(ChildXml),
                          case X#xmlElement.name of
                                tuple -> list_to_tuple(Newvalue);
                                listitem -> Newvalue;                                
                                _ when length(Newvalue) =:= 1 -> {stripkey(X#xmlElement.name),hd(Newvalue)};
                                _ -> list_to_tuple([stripkey(X#xmlElement.name)|Newvalue])
                          end
           end
         end,
  lists:map(F,Xml).
  
listtofloat(Value) ->
  case string:str(Value,".") of
        Pos when Pos =:= length(Value) -> 0;
        Pos when Pos > 1 -> list_to_float(Value);        
        0 -> list_to_integer(Value);
        _ ->  0
  end.  
  
xml_to_binary(Value) ->
    ListData = string:tokens(Value,","),
    F = fun(X) -> list_to_integer(X) end,
    Lists = lists:map(F,ListData),
    list_to_binary(Lists).


stripkey(Name) when is_atom(Name) -> 
    Namestr = atom_to_list(Name),
    case lists:prefix("key_",Namestr) of  
          true -> 
               list_to_atom(lists:sublist(Namestr,5,length(Namestr)-4));
          _ -> Name
    end;
stripkey(Name)  -> Name.


