-module(rest_deviceversion). 
-export([func_deviceversion_get/5,func_deviceversion_set/5,func_deviceversion_upgrade/5,func_deviceversion_gethistory/5]).
-include("xmerl.hrl").
-import(xmerl_xs,[xslapply/2,value_of/1,select/2,built_in_rules/2]).
-import(lists,[keysearch/3]).


%%check the current version
func_deviceversion_get(Host,_Req,Path,Raw_path,Child) ->
	case _Req:recv_body() of
	    "" ->
			web_common:respond(unknown);
        Body ->		
		    Options=[{space,normalize},{encoding,"utf-8"}],
	        {Xml,_} = xmerl_scan:string(Body,Options),
            Trans = template_version(Xml),
			[Type1|_] = Trans,
			[Type] = Type1,
			Result = dbcs_deviceversion:get_deviceversion_match("my.filetype="++Type),
			web_common:respond("<response>"++forversion(lists:reverse(Result))++"</response>")
	end.

forversion([]) ->
    "";
forversion([F|R]) ->
    {value,{version,Version}}=keysearch(version,1,F),
forversion(R)++"<version>"++atom_to_list(Version)++"</version>".
	
template_version(E = #xmlElement{name = 'params'}) ->
[
value_of(select("type",E))
];
template_version(E) -> built_in_rules(fun template_version/1, E).

	
	
%%version config
func_deviceversion_set(Host,_Req,Path,Raw_path,Child) ->
	case _Req:recv_body() of
	    "" ->
			web_common:respond("<error>unknown_request</error>");
        Body ->		
		    Options=[{space,normalize},{encoding,"utf-8"}],
	        {Xml,_} = xmerl_scan:string(Body,Options),
            Trans = template_set(Xml),
			[Type1|[Version1|[Path1|[Ip1|[Name1|[Password1|_]]]]]] = Trans,
		    [Type]=Type1,[Version]=Version1,[Path]=Path1,[Ip]=Ip1,[Name]=Name1,[Password]=Password1,
            Result = dbcs_deviceversion:set_deviceversion([{filetype,Type},{version,Version},{filepath,Path},{ftpip,Ip},{ftpuser,Name},{ftppasswd,Password}]),
            case Result of
			    {ok,_} ->
				    web_common:respond("<ok>setversion_ok</ok>");
				{error,_} ->
				    web_common:respond("<error>setversion_error</error>")
			end
	end.
			
template_set(E = #xmlElement{name = 'params'}) ->
[   value_of(select("type",E)),
    value_of(select("version",E)),
	value_of(select("path",E)),
	value_of(select("ip",E)),
	value_of(select("name",E)),
	value_of(select("password",E))
];
template_set(E) -> built_in_rules(fun template_set/1, E).

%%version upgrade
func_deviceversion_upgrade(Host,_Req,Path,Raw_path,Child) ->
	case _Req:recv_body() of
	    "" ->
			web_common:respond("<error>unknown_request</error>");
        Body ->		
		    Options=[{space,normalize},{encoding,"utf-8"}],
	        {Xml,_} = xmerl_scan:string(Body,Options),
            Trans = template_set(Xml),
			[Gid1|[Did1|[Type1|[Version1|_]]]]= Trans,
			case list_length(Gid1) of
			    0->
				    case list_length(Did1) of
					    0->
						    web_common:respod("<error>miss_parameter</error>");
						Others->
						    for_did(Did1,Type1,Version1)
					end;
				Others->
				    case list_length(Did1) of
					    0->
						    for_gid(Gid1,Type1,Version1);
						N->
						    for_bothgid(Gid1,Type1,Version1,Did1)
			        end
			end
	end.
			
template_upgrade(E = #xmlElement{name = 'upgrade'}) ->
[   value_of(select("upgrades/upgrade/gid",E)),
    value_of(select("upgrades/upgrade/did",E)),
    value_of(select("upgrades/upgrade/type",E)),
    value_of(select("upgrades/upgrade/version",E))
];
template_upgrade(E) -> built_in_rules(fun template_upgrade/1, E).

for_did([],[],[])->
    ok;
for_did([A|B],[C|D],[E|F])->
    dbcs_deviceversion:upgrade_deviceversion([{did,A},{type,C},{version,E}]),
	for_did(B,D,F).
	
for_gid([],[],[])->
    ok;
for_gid([A1|B1],[C1|D1],[E1|F1])->
    dbcs_deviceversion:upgrade_deviceversion([{gid,A1},{type,C1},{version,E1}]),
	for_gid(B1,D1,F1).

for_bothgid([],Rtype,Rversion,D)->
    for_bothdid(D,Rtype,Rversion);
for_bothgid([A2|B2],[C2|D2],[E2|F2],D)->
    dbcs_deviceversion:upgrade_deviceversion([{gid,A2},{type,C2},{version,E2}]),
    for_bothgid(B2,D2,F2,D).
	
for_bothdid([],[],[])->
    ok;
for_bothdid([A3|B3],[C3|D3],[E3|F3])->
    dbcs_deviceversion:upgrade_deviceversion([{did,A3},{type,C3},{version,E3}]),
    for_bothdid(B3,D3,F3).
	
	
%%upgrade history
func_deviceversion_gethistory(Host,_Req,Path,Raw_path,Child) ->
	case _Req:recv_body() of
	    "" ->
			web_common:respond(unknown);
        Body ->		
		    Options=[{space,normalize},{encoding,"utf-8"}],
	        {Xml,_} = xmerl_scan:string(Body,Options),
            Trans = template_version(Xml),
			[Timestart1|[Timeend1|[Devicename1|_]]] = Trans,
			[Timestart2] = Timestart1,Timestart=list_to_atom(Timestart2),
			[Timeend2] = Timeend1,Timeend=list_to_atom(Timeend2),
			[Devicename] = Devicename1,
			Result = dbcs_deviceversion:get_deviceversionhistory("my.time<="++Timeend++"&my.time>="++Timestart++"&my.name="++Devicename),
			case Result of
			    {error,_}->
			        web_common:respond("<error>devicehistory_error</error>");
			    Others ->
			        web_common:respond("<response>"++"<versionhistory>"++forversion(lists:reverse(Result))++"</versionlist>"++"</response>")
		    end
	end.

forversionhistory([]) ->
    "";
forversionhistory([F|R]) ->
		{value,{deviceid,Did}}=keysearch(device,1,F),
		{value,{devicename,Devicename}}=keysearch(devicename,1,F),
		{value,{time,Time}}=keysearch(time,1,F),
		{value,{version1,Version1}}=keysearch(version1,1,F),
		{value,{version2,Version2}}=keysearch(version2,1,F),
		{value,{result,Result}}=keysearch(result,1,F),
		forversionhistory(R)++"<did>"++atom_to_list(Did)++"</did>"
		++"<devicename>"++atom_to_list(Devicename)++"</devicename>"++"<time>"++atom_to_list(Time)++"</time>"
		++"<version1>"++atom_to_list(Version1)++"</version1>"++"<version2>"++atom_to_list(Version2)++"</version2>"
		++"<result>"++atom_to_list(Result)++"</result>".
	
template_versionhistory(E = #xmlElement{name = 'params'}) ->
[   value_of(select("timestart",E)),
    value_of(select("timeend",E)),
    value_of(select("devicename",E))
];
template_versionhistory(E) -> built_in_rules(fun template_versionhistory/1, E).

%%length of the list
list_length([]) ->
    0;
list_length([First | Rest]) ->
    1 + list_length(Rest).