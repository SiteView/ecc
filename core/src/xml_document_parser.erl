%
%base from xml_parser 
%make a list of tuple from xml file.
%
-module(xml_document_parser).
-include("xmerl.hrl").
-compile(export_all).

%XmlFilePath is string
parser({filepath,XmlFilePath}) ->
    case file:read_file(XmlFilePath) of
    {ok,Binary} ->
        Options = [{space,preserve}],
        {XML,_} = xmerl_scan:string(binary_to_list(Binary),Options),
        make_parser(xmerl_xs:select("*", XML));         
    {error,_} ->
        {error,"read file error!"}  
    end;
    
%XmlFileContent is string
parser({content,XmlFileContent}) ->  
    Options = [{space,preserve}],
    {XML,_} = xmerl_scan:string(XmlFileContent,Options),
    make_parser(xmerl_xs:select("*", XML)).     


make_parser(XML) ->    
    F = fun(X) ->
        case xmerl_xs:select("*", X) of
        [] ->
            %Value = string:strip(lists:flatten(xmerl_xs:value_of(X))),
            Parents = X#xmlElement.parents, 
            ElementName = atom_to_list(X#xmlElement.name),              
            case X#xmlElement.content  of
            [] ->
                Attributes = X#xmlElement.attributes,
                xmlElement_attributes_parser(Parents,ElementName,Attributes);                
            ContentList ->

                Attributes = X#xmlElement.attributes, 

                %~ xmlElement_attributes_parser(Parents,ElementName,Attributes) ++            
                %~ xmlElement_content_parser(Parents,ElementName,ContentList)
				NodeAttributes = xmlElement_attributes_parser(Parents,ElementName,Attributes),
				Content = xmlElement_content_parser(Parents,ElementName,ContentList),
				[{Node,Value}] = Content,
				{Node++" "++string:join(NodeAttributes," "),Value}
				
            end;
        ChildXml ->      
            make_parser(ChildXml)  
        end
    end,
    lists:flatten(lists:map(F,XML)).                        



xmlElement_attributes_parser(Parents,ElementName,Attributes) ->
    xmlElement_attributes_parser_t(Parents,ElementName,Attributes,length(Attributes),[]).
xmlElement_attributes_parser_t(_Pare,_ElementN,_C,0,R) -> R;
xmlElement_attributes_parser_t(Pare,EleName,[A|B],Len,Res) ->
    case A of
    {xmlAttribute,Name,_,_,_,_,_,_,Value,_} ->
        %~ xmlElement_attributes_parser_t(Pare,EleName,B,Len-1,[{make_parents_string(Pare)++EleName++"/"++atom_to_list(Name),Value}|Res]);  
		xmlElement_attributes_parser_t(Pare,EleName,B,Len-1,["'"++atom_to_list(Name)++"="++Value++"'"|Res]);  
    _ ->
        xmlElement_attributes_parser_t(Pare,EleName,B,Len-1,Res)
    end.


xmlElement_content_parser(Parents,ElementName,ContentList) ->
    xmlElement_content_parser_t(Parents,ElementName,ContentList,length(ContentList),[]).
xmlElement_content_parser_t(_Pare,_ElementN,_C,0,R) -> R;
xmlElement_content_parser_t(Pare,EleName,[A|B],Len,Res) ->
    case A of
    {xmlText,_,_,_,Value,_Type} ->
		Full_node = make_parents_string(Pare)++EleName,
		Index = get_index(Full_node),
        xmlElement_content_parser_t(Pare,EleName,B,Len-1,[{make_parents_string(Pare)++EleName++Index,Value}|Res]);      
    {xmlComment,_,_,_,Value} ->
		Full_node = make_parents_string(Pare)++EleName,
		Index = get_index(Full_node),
        xmlElement_content_parser_t(Pare,EleName,B,Len-1,[{make_parents_string(Pare)++EleName++Index,Value}|Res]);  
    {xmlPI,Name,_,Value} ->
        xmlElement_content_parser_t(Pare,EleName,B,Len-1,[{atom_to_list(Name),Value}|Res]); 
    _ ->
        xmlElement_content_parser_t(Pare,EleName,B,Len-1,Res)
    end.

get_index(Name) ->
	case erlang:get(Name) of
		undefined ->
			erlang:put(Name,1),
			"#1";
		Value ->
			New_value = Value+1,
			erlang:put(Name,New_value),
			"#"++integer_to_list(New_value)
	end.


make_parents_string(Parents) ->
    make_parents_string_t(Parents,length(Parents),""). 
make_parents_string_t(_P,0,R) -> R;
make_parents_string_t([{Name,_Sta}|B],Len,Res) ->
    make_parents_string_t(B,Len-1,atom_to_list(Name)++"#"++integer_to_list(round(_Sta/2))++"/"++Res).      