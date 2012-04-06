-module(sec_common).



-include("xmerl.hrl").

-export([simplexml_read_string/1,get_value_of/2,get_list_value_of/2,get_xmlAttribute_value/2,is_list_list/1]).


simplexml_read_string(Str) ->
   Options = [{space,normalize},{encoding,"utf-8"}],
   {XML,_} = xmerl_scan:string(Str,Options),
   XML.
   
get_list_value_of(Str,XmlElement) ->
    Xmldata = xmerl_xs:select(Str,XmlElement),  
    case xmerl_xs:select(Str,XmlElement) of 
        [] -> null;
        Xmldata ->
        F = fun(_x) -> lists:flatten(xmerl_xs:value_of(_x)) end,
        lists:map(F, Xmldata)
    end.

get_value_of(Str,XmlElement) ->
    case xmerl_xs:select(Str,XmlElement) of
	     [] -> null;
		 Xmldata -> lists:flatten(xmerl_xs:value_of(Xmldata))      
	end.
  
    
get_xmlAttribute_value(Str,XmlElement) ->
    case xmerl_xs:select(Str,XmlElement) of 
	    [] -> null;
		[Xmlattribute] -> lists:flatten(Xmlattribute#xmlAttribute.value);
		_ -> null
    end.


 
is_list_list([]) ->  false;
is_list_list([Head|_]) when is_list(Head)->  true; 
is_list_list(null) ->  false;
is_list_list(_) ->  false.    
    
