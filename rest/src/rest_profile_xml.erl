-module(rest_profile_xml).

-author('oldhand <oldhand@sina.com>').

-include("log.hrl").
-include("config.hrl").
-include("xmerl.hrl").


-export([simplify_profile_entry/1]).
-export([profile_to_xml/4]).

%~ <profile>
  %~ <id>dragonflow</id> 
  %~ <name>siteview</name> 
  %~ <password>123456</password> 
  %~ <ldapserver type="atom">127.0.0.1</ldapserver> 
  %~ <ldapsecurity>test</ldapsecurity>
  %~ <desc>siteview</desc>
  %~ <title>dragonflow</title>
  %~ <email>siteview@dragonflow.com</email>
  %~ <disabled>false</disabled>
  %~ <groups type="number">1,2,3,4,5</groups>
  %~ <rights>china</rights>
%~ </profile>

simplify_profile_entry(Str) ->
   Xmldata = xml_common:simplexml_read_string(Str), 
   entry_to_record(Xmldata).

entry_to_record(Entry_xmlElement) -> 
   #profile{
	id = xml_common:get_value_of("id",Entry_xmlElement),
	name= xml_common:get_value_of("name",Entry_xmlElement),
	password= xml_common:get_value_of("password",Entry_xmlElement),
	ldapserver= xml_common:get_value_of("ldapserver",Entry_xmlElement), 
	ldapsecurity= xml_common:get_value_of("ldapsecurity",Entry_xmlElement),
	desc= xml_common:get_value_of("desc",Entry_xmlElement),
	title= xml_common:get_value_of("title",Entry_xmlElement),
	email= xml_common:get_value_of("email",Entry_xmlElement), 
	disabled= xml_common:get_value_of("disabled",Entry_xmlElement),
	groups= xml_common:get_value_of("groups",Entry_xmlElement),
	rights= xml_common:get_value_of("rights",Entry_xmlElement)
	}.

profile_to_xml([],_,Host,Req) -> 
           Path = Req:get(path),
           Domain = config:getconfig('domain'),
           lists:flatten(["<?xml version=\"1.0\" encoding=\"UTF-8\"?><feed xmlns=\"http://www.w3.org/2005/Atom\" xmlns:xn=\"http://www.",Domain,"/atom/1.0\"><title>profile feed for ",Domain,"</title><id>http://",Host,".",Domain,html_util:escape(Path),"</id><updated>",common:getUTCTime(),"</updated><xn:size>0</xn:size></feed>"]);
profile_to_xml(Lists,Total,Host,Req) ->   
	   Path = Req:get(path),
           Domain = config:getconfig('domain'),
           Entry_lists = lists:map(fun(X) -> entry_lists(X,Host,Domain) end,Lists),
           Xml = [<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?><feed xmlns=\"http://www.w3.org/2005/Atom\" xmlns:xn=\"http://www.">>,Domain,<<"/atom/1.0\"><title>profile feed for ">>,Domain,<<"</title><id>http://">>,Host,<<".">>,Domain,html_util:escape(Path),<<"</id><updated>">>,common:getUTCTime() ,<<"</updated><xn:size>">>,integer_to_list(Total),<<"</xn:size>">>, Entry_lists ,<<"</feed>">>],
           list_to_binary(Xml).


entry_lists(X,Host,Domain) ->	  
      Entry_my_lists = get_entry_my_list(X#content.my),	 
      Header = [<<"<entry xmlns=\"http://www.w3.org/2005/Atom\" xmlns:xn=\"http://www.">>,Domain,<<"/atom/1.0\" xmlns:my=\"http://">>,Host,<<".">>,Domain,<<"/xn/atom/1.0\">">>],
	  Id = [<<"<id>http://">>,Host,<<".">>,Domain,<<"/">>,binary_to_list(X#content.id),<<"</id>">>],
	  Xn_type = content_entry_lists(<<"<xn:type>">>,X#content.xn_type,<<"</xn:type>">>),
	  Xn_id = content_entry_lists(<<"<xn:id>">>,X#content.id,<<"</xn:id>">>),
	  Title = content_entry_lists(<<"<title type=\"text\">">>,X#content.title,<<"</title>">>),
	  Author = content_entry_lists(<<"<author><name>">>,X#content.author,<<"</name></author>">>),
     [Header,Id,Xn_type,Xn_id,Title,Author,Entry_my_lists,<<"</entry>">>].
    
content_entry_lists(_,null,_) -> <<>>;
content_entry_lists(_,undefined,_) -> <<>>;
content_entry_lists(Prefix,Data,Suffix) when is_atom(Data) -> [Prefix,atom_to_list(Data),Suffix];
content_entry_lists(Prefix,Data,Suffix) -> [Prefix,Data,Suffix].

get_entry_my_list(null) ->   <<>>;    
get_entry_my_list(undefined) ->   <<>>;  
get_entry_my_list([]) ->   <<>>;  
get_entry_my_list(Content_my) ->
   lists:map(fun(Y) -> entry_my_list(Y) end,Content_my).
    
entry_my_list({_,_,null}) -> <<>>;

entry_my_list({Name,Typedata,Data}) when is_binary(Data) -> 
       Atomname = common:auto_to_list(Name),
       [<<"<my:">>,Atomname,<<" xn:type=\"">>,common:auto_to_list(Typedata),<<"\">">>,Data,<<"</my:">>,Atomname,<<">">>];

	
entry_my_list({Name,Typedata,Data}) when is_list(Data) -> 
      Atomname = common:auto_to_list(Name),
	  Xn_value = lists:map(fun(Y) -> [<<"<xn:value>">>,Y,<<"</xn:value>">>] end,Data),
	  [<<"<my:">>,Atomname,<<" xn:type=\"">>,common:auto_to_list(Typedata),<<"\">">>,Xn_value,<<"</my:">>,Atomname,<<">">>];

	
entry_my_list(_) -> <<>>.