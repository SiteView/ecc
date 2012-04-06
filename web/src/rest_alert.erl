-module(rest_alert). 
-export([func_alert_process/5,func_alert_list/5]).
-include("xmerl.hrl").
-import(xmerl_xs,[xslapply/2,value_of/1,select/2,built_in_rules/2]).
-import(lists,[keysearch/3]).

	
%%alert handle
func_alert_process(Host,_Req,Path,Raw_path,Child) ->
	case _Req:recv_body() of
	    "" ->
			web_common:respond("<error>request_unknown<error>");
        Body ->		
%%parse body£¬get propertie value
		    Options=[{space,normalize},{encoding,"utf-8"}],
	        {Xml,_} = xmerl_scan:string(Body,Options),
            Trans = template(Xml),
			[Alertid1|[Username1|[Remark1|_]]] = Trans,
			case list_length(Alertid1) of
			    1->
			        [Alertid2] = Alertid1,Alertid = list_to_atom(Alertid2),
			        [Username] = Username1,
			        [Remark] = Remark1,
					Result = dbcs_alert:create_alert([{id,Alertid},{dealuser,Username},{dealmemo,Remark}]),
					case Result of 
                        {ok,_} ->
		                    web_common:respond("<ok>process_ok</ok>");
	                    {error,_} ->
		                    web_common:respond("<error>process_error</error>")
                    end;
				0->
				    web_common:respond("<error>miss_parameters</error>")
			end
	end.

template(E = #xmlElement{name = 'params'}) ->
[    
    value_of(select("alertid",E)),
	value_of(select("username",E)),
	value_of(select("remark",E))
];
template(E) -> built_in_rules(fun template/1, E).


%%alert history
func_alert_list(Host,_Req,Path,Raw_path,Child) ->
	case _Req:recv_body() of
	    "" ->
			web_common:respond("<error>request_unknown<error>");
        Body ->		
		    Options=[{space,normalize},{encoding,"utf-8"}],
	        {Xml,_} = xmerl_scan:string(Body,Options),
            Trans = template_list(Xml),
			[Timestart1|[Timeend1|[Class1|[Devicename1|[Lastline1|_]]]]] = Trans,
			[Timestart]=Timestart1,
			[Timeend]=Timeend1,
			[Class]=Class1,
			[Devicename]=Devicename1,
			[Lastline]=Lastline1,
            Result = dbcs_alert:get_alert_match("my.time<="++Timeend++"&my.time>="++Timestart++"&class="++Class++"&my.target="++Devicename),
            %line
			Len = list_length(Result)*6,
			    if
			        Len>Lastline ->
				        web_common:respond("<response><alertinfo>"++forline(Result,Lastline div 6)++"<lastline>"++atom_to_list(Lastline div 6 * 6)++"</lastline>"++"</alertinfo></response>");
			        Len=<Lastline ->
			            web_common:respond("<response><alertinfo>"++foreach(lists:serverse(Result))++"<lastline>"++atom_to_list(Len)++"</lastline>"++"</alertinfo></response>")
			    end
	end.
	
foreach([]) ->
    "";
foreach([F|R]) ->
    {value,{id,Id}}=keysearch(id,1,F),
	{value,{address,Add}}=keysearch(address,1,F),
	{value,{time,Time}}=keysearch(time,1,F),
	{value,{target,Tar}}=keysearch(target,1,F),
	{value,{faultcode,Fc}}=keysearch(faultcode,1,F),
	{value,{faultstr,Fs}}=keysearch(faultstr,1,F),
	foreach(R)++"<alertid>"++atom_to_list(Id)++"</alertid>"
	++"<time>"++atom_to_list(Time)++"</time>"++"<devicename>"++atom_to_list(Tar)++"</devicename>"
	++"<address>"++atom_to_list(Add)++"</address>"++"<faultcode>"++atom_to_list(Fc)++"<faultcode>"
	++"<faultstr>"++atom_to_list(Fs)++"</faultstr>".
	
forline(_,0) ->
    "";
forline([F|R],N) ->
    {value,{id,Id}}=lists:keysearch(id,1,F),
	{value,{address,Add}}=lists:keysearch(address,1,F),
	{value,{time,Time}}=lists:keysearch(time,1,F),
	{value,{target,Tar}}=lists:keysearch(target,1,F),
	{value,{faultcode,Fc}}=lists:keysearch(faultcode,1,F),
	{value,{faultstr,Fs}}=lists:keysearch(faultstr,1,F),
	forline(R,N-1)++"<alertid>"++atom_to_list(Id)++"</alertid>"
	++"<time>"++atom_to_list(Time)++"</time>"++"<devicename>"++atom_to_list(Tar)++"</devicename>"
	++"<address>"++atom_to_list(Add)++"</address>"++"<faultcode>"++atom_to_list(Fc)++"<faultcode>"
	++"<faultstr>"++atom_to_list(Fs)++"</faultstr>".
	
template_list(E = #xmlElement{name = 'params'}) ->
[    
    value_of(select("timestart",E)),
	value_of(select("timeend",E)),
	value_of(select("alertid",E)),
	value_of(select("devicename",E)),
	value_of(select("lastline",E))
];
template_list(E) -> built_in_rules(fun template_list/1, E).


%%length of the list
list_length([]) ->
    0;
list_length([First | Rest]) ->
    1 + list_length(Rest).


