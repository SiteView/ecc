-module(rest_profile).

-include("log.hrl").
-include("config.hrl").



-export([func_post_profile/4]).
-export([func_get_profile/4]).   
-export([func_get_profile_browse/5]).  

	   
func_post_profile(_Host,_Req,_Path,_Raw_path) ->
	   ?Log({"______~p~n",[mochiweb_headers:to_list(_Req:get(headers))]}),	   
	   Data = binary_to_list(_Req:recv_body()),
	   Recordxml = rest_profile_xml:simplify_profile_entry(Data),
	   ?Log({"func_post_profile: ~p~n",[Recordxml]}),	   
	   Result = api_user_spl:user_create(Recordxml#profile.name,Recordxml#profile.password,Recordxml#profile.ldapserver,
	            Recordxml#profile.ldapsecurity,Recordxml#profile.desc,Recordxml#profile.title,Recordxml#profile.email,
		    Recordxml#profile.disabled,Recordxml#profile.groups,Recordxml#profile.rights),
	    ?Log({"func_post_profile: ~p~n",[Result]}),
	   case Result of
		  {ok,existed} ->
			   ?Log({"func_post_profile data existed!~n"}),
			   XmlData = "<?xml version='1.0' encoding='UTF-8'?><errors><error code=\"auth:1\">Invalid user</error></errors>",
			   common:respond(403,XmlData);
		  {ok,Content} ->
	   %      ?Log({"func_new_profile: ~p~n",[Content]}),
			 ?Log({"func_post_profile return data ok!~n"}),
			Xml = rest_profile_xml:profile_to_xml([Content],1,_Host,_Req),
			common:respond(Xml);
		   _ -> 
		   XmlData = "<?xml version='1.0' encoding='UTF-8'?><errors><error code=\"auth:1\">Invalid user</error></errors>",
		   common:respond(403,XmlData)
	   end.
       
func_get_profile(Host,Req,Path,_Raw_path) ->
       func_get_profile(Host,Req,common:strmacth(Path,"(id='","')")).
       
func_get_profile(_Host,Req,{match,ProfileId}) -> 
       ?Log({"______~p~n",[mochiweb_headers:to_list(Req:get(headers))]}), 
	   Result = api_user_spl:get_user(ProfileId),
	   ?Log({"func_post_profile: ~p~n",[Result]}),
       Xml = tuplesxml:tuple_to_xml(Result),
       common:respond(Xml);

func_get_profile(_,_,_) ->  common:respond(other).


	  
       
func_get_profile_browse(_Host,_Req,_Path,_Raw_path,_) -> 
       ?Log({"______~p~n",[mochiweb_headers:to_list(_Req:get(headers))]}), 
	   Result = api_user_spl:browse_users(),
	   ?Log({"func_post_profile: ~p~n",[Result]}),
       Xml = tuplesxml:tuple_to_xml(Result),
       common:respond(Xml).
        