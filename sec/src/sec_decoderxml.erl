-module (sec_decoderxml).
-author('oldhand<oldhand@sina.com>').
-include("sec_log.hrl").
-include("xmerl.hrl").
-compile(export_all).


test() ->    
    {ok, Binary} =  file:read_file("decoder.xml"),
    Xml = lists:flatten(["<?xml version=\"1.0\" encoding=\"UTF-8\"?><feed>",binary_to_list(Binary),"</feed>"]),
    Xmldata = sec_common:simplexml_read_string(Xml), 
    %?Log({"~p~n",[Xmldata]}),
    Xmllist = xmerl_xs:select("*", Xmldata),
    Result = xml_tuple_convet(Xmllist),
    F = fun(X) ->  X =/= errorformat end,
    NewResult = lists:filter(F,Result),
    file:write_file("test.txt",io_lib:format("~p",[NewResult])),
    NewResult.

decoderxml(XmlPath) ->	
    {ok, Binary} =  file:read_file(XmlPath),
     Xml = lists:flatten(["<?xml version=\"1.0\" encoding=\"UTF-8\"?><feed>",binary_to_list(Binary),"</feed>"]),
    Xmldata = sec_common:simplexml_read_string(Xml), 
    Xmllist = xmerl_xs:select("*", Xmldata),
    F = fun(X) ->  X =/= errorformat end,
    FilterData = lists:filter(F,xml_tuple_convet(Xmllist)),
    Fun = fun(X, Y)-> element(2,X) < element(2,Y) end,
    lists:sort(Fun, FilterData).
   
xml_tuple_convet(Xml) ->    
  F = fun(X) -> 
           case xmerl_xs:select("*", X) of
                [] -> 
                    Value = sec_html_util:unescape(lists:flatten(xmerl_xs:value_of(X))),   
                    case  X#xmlElement.name of
                        regex -> case sec_common:get_xmlAttribute_value("@offset",X) of
                                       "after_prematch" -> {after_prematch,Value};
                                       _ -> {X#xmlElement.name,Value}    
                                  end; 
                        if_sid -> {if_sid,if_sid(Value)};
                        if_matched_sid -> {if_matched_sid,list_to_integer(Value)};
                        var -> case sec_common:get_xmlAttribute_value("@name",X) of
                                       null -> errorformat;
                                       Name -> {list_to_atom(Name),Value}    
                                  end; 
                        _ -> {X#xmlElement.name,Value}
                    end;
                ChildXml ->  
                          Newvalue = xml_tuple_convet(ChildXml),
                          case X#xmlElement.name of                               
                                decoder -> 
                                  case sec_common:get_xmlAttribute_value("@name",X) of
                                       null -> errorformat;
                                       Name -> {decoder,Name,Newvalue}    
                                  end; 
                                  group -> 
                                  case sec_common:get_xmlAttribute_value("@name",X) of
                                       null -> errorformat;
                                       Name -> {group,groupname(Name),Newvalue}    
                                  end;            
                                  rule -> 
                                  case {sec_common:get_xmlAttribute_value("@id",X),
                                         sec_common:get_xmlAttribute_value("@level",X),
                                         sec_common:get_xmlAttribute_value("@ignore",X),
                                         sec_common:get_xmlAttribute_value("@frequency",X),
                                         sec_common:get_xmlAttribute_value("@timeframe",X)} of                                         
                                       {[],_,_,_,_} -> errorformat;       
                                       {_,[],_,_,_} -> errorformat;
                                       {Id,Level,Ignore,Frequency,Timeframe} -> {rule,{{id,list_to_integer(Id)},
                                                                                        {level,esyslog_message:decode_severity(list_to_integer(Level))},
                                                                                        {ignore,Ignore},
                                                                                        {frequency,Frequency},
                                                                                        {timeframe,Timeframe}},Newvalue}                                                                         
                                  end;                                    
                                _ -> errorformat
                          end
           end
         end,
  lists:map(F,Xml).

if_sid(Sid) ->  
   Sidlist = string:tokens(Sid,","),
   F  = fun(X) -> list_to_integer(string:strip(X))
        end,
   lists:map(F,Sidlist).   
   
groupname(Name) ->  
   Namelist = string:tokens(Name,","),
   F  = fun(X) -> string:strip(X)
        end,
   lists:map(F,Namelist).    

testrules() ->  file:write_file("test1.txt",io_lib:format("~p",[rules()])).

rewrite(File) ->
    {ok, Binary} = file:read_file(File),
    file:write_file(File,list_to_binary(["<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<feed>\n",Binary,"</feed>"])).
    
rules() ->
   case  file:list_dir("../rules") of
        {ok, Files} -> 
              Rulesdata= rulesfile(Files,"../rules",[]),
              Fun = fun(X, Y)-> element(1,X) < element(1,Y) end,
              lists:sort(Fun, Rulesdata);
	    ErrorReason -> ErrorReason
   end.
   
rules(RulesPath) ->
   case  file:list_dir(RulesPath) of
        {ok, Files} -> 
            rulesfile(Files,RulesPath,[]);
	    ErrorReason -> ErrorReason
   end.   
   
rulesfile([File|L],RulesPath,Acc)->
      Filename = RulesPath ++ "/" ++ File, 
      case filelib:is_dir(Filename) of
           false ->                
                %?Log({"~p~n",[Filename]}),
                {ok, Binary} =  file:read_file(Filename),
                 Xml = lists:flatten(["<?xml version=\"1.0\" encoding=\"UTF-8\"?><feed>",binary_to_list(Binary),"</feed>"]),
                Xmldata = sec_common:simplexml_read_string(Xml), 
                Xmllist = xmerl_xs:select("*", Xmldata),
                F = fun(XX) ->  XX =/= errorformat end,       
                %?Log({"~p~n",[lists:filter(F,xml_tuple_convet(Xmllist))]}),                
                rulesfile(L,RulesPath,Acc++[{File,lists:filter(F,xml_tuple_convet(Xmllist))}]);                
           _ -> rulesfile(L,RulesPath,Acc)
      end; 
rulesfile([],_,Acc)-> Acc.
    

