%% Author: Administrator
%% Created: 2011-7-14
%% Description: TODO: Add description to exhange2007_monitor
-module(exhange2007_monitor,[BASE]).
-extends(atomic_monitor).
-compile(export_all).
-include("monitor_template.hrl").
-include("monitor.hrl").
-define(MAX_COUNTER,20).
-define(TIMEOUT,60*1000).

new()->
	Base = atomic_monitor:new(),
	Base:set_attribute(countersInError,0),
	{?MODULE,Base}.
getCounterSize()->
	{ok, {_, Browse}} = THIS:get_property(browse),
	Len = length(Browse).
%% @spec defaultTitle(Params) ->string()
%%  Params = [term()]
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	Host = proplists:get_value(pServer,Params),
	if
		length(Host)>0->
			BASE:defaultTitle(Params) ++":" ++ Host;
		true ->
			BASE:defaultTitle(Params)
	end.
%% @spec get_counters() -> CounterList
%% @type CounterList = [Counter]
%% @type Counter = [string(), string()]
%% @doc get counters that user has selected.
get_counters() ->
	{ok, {_, Browse}} = THIS:get_property(browse),
	Len = length(Browse),
	if
		(Len > 0) ->
			Browse;
		true ->
			[]
	end.
initCounters()->
	THIS:set_attribute(countersInError, 0),
	lists:foreach(fun(X)->THIS:set_attribute(element(1,X),"n/a"),THIS:set_attribute(element(2,X),"n/a") end, THIS:get_counters()).	
%% @spec update() -> ok
%% @doc Run the monitor.
%%
%% Description: Get counters value with perfex.exe
update()->
	 case getCounterSize() > ?MAX_COUNTER of
		   true->
		    THIS:set_attribute(?NO_DATA,true),
		    THIS:set_attribute(?CATEGORY,?NO_DATA),
		    THIS:set_attribute(countersInError,?MAX_COUNTER),
		    THIS:set_attribute(?STATE_STRING,lists:flatten(io_lib:format("the counters > MAX_COUNTER:~p",[?MAX_COUNTER])));
		  _ ->
			 {ok, {_, Server}} = THIS:get_property(pServer),
	         {ok, {_, UserDomain}} = THIS:get_property(pUserDomain),
	         {ok, {_, MailboxAlias}} = THIS:get_property(pMailboxAlias),
	         {ok, {_, PscFilePath}} = THIS:get_property(pPscFilePath),
			 initCounters(),
			 CC=perfexCommand1(Server,UserDomain,MailboxAlias,PscFilePath),
			 Values= THIS:counterValues(CC),
			 Counterss=THIS:get_counters(),
			 updateValues(Counterss,"",0,Values)
	  end,
	ok.
updateValues([],SS,Counter_Errors,Values)->
	THIS:set_attribute(?STATE_STRING, iconv:convert( "gbk","utf-8",SS)),
	THIS:set_attribute(contersInError, Counter_Errors),
	ok;
updateValues([H|T],SS,Counter_Errors,Values)->
	{K,V}=H,
	case lists:keyfind(K, 1, Values) of
		false ->
			THIS:set_attribute(K, "N/A"),
			THIS:set_attribute(V, "N/A"),
			NewSS=SS++V++"=N/A,",
			NewCounter_Errors=Counter_Errors+1;
		{_,V1} ->
			THIS:set_attribute(K, iconv:convert("gbk","utf-8",V1)),
			THIS:set_attribute(V, iconv:convert("gbk","utf-8",V1)),
			NewCounter_Errors=Counter_Errors,
			NewSS=SS++V++"="++V1++","
	end,
	updateValues(T,NewSS,NewCounter_Errors,Values).
convert1([H|R], Acc)->
	case H > 255 of
		true->
			<<H1:8, H2:8>> = <<H:16>>,
			List = iconv:convert("utf-32", "utf-8", [0, 0, H1, H2]),
			convert1(R, lists:reverse(List) ++ Acc);
		_ ->
			convert1(R, [H|Acc])
	end;
convert1([], Acc)->
lists:reverse(Acc).
%%  io:format("convert1:~p ~n", [L]),
%%  L.
%% %%   io:format("convert1:~p ~n", [L]),
%%  	iconv:convert("utf-8", "gbk", lists:reverse(Acc)).

convert1(Msg)->
%% 	io:format("oldstatus:~p ~n", [Msg]),
	convert1(Msg, []).
counterValues(CC)->
	MAPI_Connectivity=lists:keyfind("MAPI Connectivity", 1, CC),
	R1=case MAPI_Connectivity of
		false ->
			[];
		{N,K} ->
			case K of
				[]->
					[];
				_ ->
					[{"MAPI-Result",element(2,lists:keyfind("Result", 1, K))},
					 {"MAPI-Latency",element(2,lists:keyfind("Latency", 1, K))}]
			end
	   end,
	Exchange_Search=lists:keyfind("Exchange Search", 1, CC),
	R2=case Exchange_Search of
		false ->
			[];
		{N1,K1} ->
			case K1 of
				[]->
					[];
				_ ->
					[{"ResultFound",element(2,lists:keyfind("ResultFound", 1, K1))},
					 {"SearchTime",element(2,lists:keyfind("SearchTime", 1, K1))}]
			end
	   end,
    Mail_Flow=lists:keyfind("Mail Flow", 1, CC),
	R3=case Mail_Flow of
		false ->
			[];
		{N2,K2} ->
			case K2 of
				[]->
					[];
				_ ->
					[{"TestMailflowResult",element(2,lists:keyfind("TestMailflowResult", 1, K2))},
					 {"MessageLatencyTime",element(2,lists:keyfind("MessageLatencyTime", 1, K2))}]
			end
	   end,
   OWA_Connectivity=lists:keyfind("OWA Connectivity", 1, CC),
	R4=case OWA_Connectivity of
		false ->
			[];
		{N3,K3} ->
			case K3 of
				[]->
					[];
				_ ->
					[{"OWA-Result",element(2,lists:keyfind("Result", 1, K3))},
					 {"OWA-Latency",element(2,lists:keyfind("Latency", 1, K3))}]
			end
	   end,
   Web_Services_Connectivity=lists:keyfind("Web Services Connectivity", 1, CC),
	R5=case Web_Services_Connectivity of
		false ->
			[];
		{N4,K4} ->
			case K4 of
				[]->
					[];
				_ ->
					ServicesKV=buildServicesKV([],K4,[],[]),
					RR1=case lists:keyfind("GetFolder", 1, ServicesKV) of 
							false ->
								[];
							{_,[{_,K4K1},{_,K4k2}]}->
								[{"GetFolder-Result",K4K1},{"GetFolder-Latency",K4k2}]
						end,
					RR2=case lists:keyfind("FolderSync", 1, ServicesKV) of 
							false ->
								[];
							{_,[{_,K4K3},{_,K4k4}]}->
								[{"SyncFolderItems-Result",K4K3},{"SyncFolderItems-Latency",K4k4}]
						end,
					RR3=case lists:keyfind("CreateItem", 1, ServicesKV) of 
							false ->
								[];
							{_,[{_,K4K5},{_,K4k6}]}->
								[{"CreateItem-Result",K4K5},{"CreateItem-Latency",K4k6}]
						end,		
					RR4=case lists:keyfind("DeleteItem", 1, ServicesKV) of 
							false ->
								[];
							{_,[{_,K4K7},{_,K4k8}]}->
								[{"DeleteItem-Result",K4K7},{"DeleteItem-Latency",K4k8}]
						end,
					RR1++RR2++RR3++RR4
			end
	   end,
   R1++R2++R3++R4++R5.
buildServicesKV(R,[],Perfname,SubR)->
	R++[{Perfname,SubR}];
buildServicesKV(R,[H|E],Perfname,SubR)->
	{K,V}=H,
	case K of
		"PerformanceCounterName" ->
			NewR=case Perfname of
					 [] ->
						 R;
					 _ ->
						 R++[{Perfname,SubR}]
				 end,
			buildServicesKV(NewR,E,V,[]);
		"Result" ->
			buildServicesKV(R,E,Perfname,SubR++[H]);
		"Latency"->
			buildServicesKV(R,E,Perfname,SubR++[H]);
		_ ->
			buildServicesKV(R,E,Perfname,SubR)
	end.
			
buildexchange([],RR,_)->
	RR;
buildexchange([H|E],RR,Isss)->
	{KK,VV}=H,
	V=case  Isss of
		  true ->
			  case VV of
				  "True"->
					  "$true";
				  "Flase"->
					  "$false";
				  _ ->
					  "$false"
			  end;
		  false ->
			  VV
	  end,
	buildexchange(E,RR++" "++V,Isss).
		
		
	
perfexCommand1(Server,UserDomain,MailboxAlias,PscFilePath)->
%%  <isHubTransportServer> <isClientAccessServer> <isEdgeServer> <isMailboxServer> <isUnifiedMessagingServer>
	case file:consult("templates.applications/scripts.exchange2007/exchange.ps1") of
		{ok,File}->
		    S=File;
		_ ->
			S = []
	end,
	Exchangeconf=THIS:buildexchange(S,"",true),
	Cmd="PowerShell -PSConsoleFile "++ "\""++PscFilePath++"\" -Command "++"\""++"templates.applications/scripts.exchange2007/ExchangeTestCmdlets.ps1 "
    ++Server++" "++MailboxAlias++" "++UserDomain++Exchangeconf++"\"",
	CmdLine = command_line:new(),
	Rets = CmdLine:exec(Cmd),
	timer:sleep(2000),
	RR=string:tokens(Rets, "\r\n"),
	KeyValues=buildCounterKV(RR,[],[],[]),
	KeyValues.
%% 
%% 
buildCounterKV([],R,Init,SubR)->
	R++[{Init,SubR}];
buildCounterKV([H|E],R,Init,SubR)->
	case H of
		"--- TEST-MAPICONNECTIVITY_OUTPUT ---"->
			NewR=case Init of
					 [] ->
						 R;
					 _ ->
						 R++[{Init,SubR}]
				 end,
			buildCounterKV(E,NewR,"MAPI Connectivity",[]);
		"--- TEST-EXCHANGESEARCH_OUTPUT ---"->
			NewR=case Init of
					 [] ->
						 R;
					 _ ->
						 R++[{Init,SubR}]
				 end,
			buildCounterKV(E,NewR,"Exchange Search",[]);
		"--- TEST-MAILFLOW_OUTPUT ---"->
			NewR=case Init of
					 [] ->
						 R;
					 _ ->
						 R++[{Init,SubR}]
				 end,
			buildCounterKV(E,NewR,"Mail Flow",[]);
		"--- TEST-OWACONNECTIVITY_OUTPUT ---"->
			NewR=case Init of
					 [] ->
						 R;
					 _ ->
						 R++[{Init,SubR}]
				 end,
			buildCounterKV(E,NewR,"OWA Connectivity",[]);
		"--- TEST-WEBSERVICESCONNECTIVITY_OUTPUT ---"->
			NewR=case Init of
					 [] ->
						 R;
					 _ ->
						 R++[{Init,SubR}]
				 end,
			buildCounterKV(E,NewR,"Web Services Connectivity",[]);
		_A ->
			KV= string:tokens(H, " :"), 
	        case length(KV) of
				1 ->
					buildCounterKV(E,R,Init,SubR);
				_ ->
					Name1=lists:nth(1, KV),
					Tem=case Name1 of
							"Latency"->
								[{Name1,lists:nth(2, KV)++":"++lists:nth(3, KV)++":"++lists:nth(4, KV)}];
                             "MessageLatencyTime"->
								[{Name1,lists:nth(2, KV)++":"++lists:nth(3, KV)++":"++lists:nth(4, KV)}];
							_ ->
								[{Name1,lists:nth(2, KV)}]
						end,
					buildCounterKV(E,R,Init,SubR++Tem)
			end
	end.
%% updateValues([])->
%% 	ok;
%% updateValues([H|T])->
%% 	case H of
%% 		{"MAPI Connectivity",MLlist} ->
%% 			THIS:set_attribute(countersInError, THIS:getCounterSize()),
%% 			THIS:set_attribute(?CATEGORY,?ERROR_CATEGORY),
%% 			THIS:set_attribute(?STATE_STRING, Error),
%% 			stop;
%% 		{"Mail Flow", MLlist1} ->
%% 			ConvertString=convert1(State_String),
%% 			THIS:set_attribute(?STATE_STRING, util:replace(ConvertString,",","<br>") ),
%% 			updateValues(T);
%% 		{"countersInError", Counter_Errors} ->
%% 			THIS:set_attribute(countersInError, Counter_Errors),
%% 			updateValues(T);
%% 		{"pStatus",Status} ->
%% 			THIS:set_attribute(pStatus, Status),
%% 			updateValues(T);
%% 		{_,_}->
%% 			THIS:setCounterValue(H),
%% 			updateValues(T);
%% 		_->
%% 			updateValues(T)
%% 	end.
%% @spec getBrowseData(Params) -> Result
%% @type Param  = [term()]
%% @type Result = [{CounterValue, CounterName}]
%% @doc getBrowseData(Params) is the function called by schedule 
%%	CounterValue is the Counter's OID, which will be used to get value
%%	CounterName is the show name of the counter
getBrowseData(Params)->
	%%get argument	
	%%  <isHubTransportServer> <isClientAccessServer> <isEdgeServer> <isMailboxServer> <isUnifiedMessagingServer>
     Server = proplists:get_value(pServer, Params),
	 UserDomain = proplists:get_value(pUserDomain, Params),
	 MailboxAlias = proplists:get_value(pMailboxAlias, Params),
	 PscFilePath = proplists:get_value(pPscFilePath, Params),
     CR=perfexCommand(Server,PscFilePath),
	 IsMailboxServer=case lists:keyfind("IsMailboxServer", 1, CR) of
						 false ->
							 "False";
						 E1->
							element(2,E1)
					 end,
	 IsHubTransportServer=case lists:keyfind("IsHubTransportServer", 1, CR) of
						 false ->
							 "False";
						 E2->
							 element(2,E2)
					 end,
	 IsClientAccessServer=case lists:keyfind("IsClientAccessServer", 1, CR) of
						 false ->
							 "False";
						 E3->
							 element(2,E3)
					 end,
	 IsEdgeServer=case lists:keyfind("IsEdgeServer", 1, CR) of
						 false ->
							 "False";
						 E4->
							 element(2,E4)
					 end,
	 IsUnifiedMessagingServer=case lists:keyfind("IsUnifiedMessagingServer", 1, CR) of
						 false ->
							 "False";
						 E5->
							 element(2,E5)
					 end,
	 Filename="templates.applications/scripts.exchange2007/exchange.ps1",
	 case filelib:is_file(Filename) of
		true->
		    ok;
		_->
			case file:open(Filename, write) of
				{ok, SF} ->
					io:format(SF, "~s~n", ["{isHubTransportServer,\""++IsHubTransportServer++"\"}."]),
					io:format(SF, "~s~n", ["{isClientAccessServer,\""++IsClientAccessServer++"\"}."]),
					io:format(SF, "~s~n", ["{isEdgeServer,\""++IsEdgeServer++"\"}."]),
					io:format(SF, "~s ~n", ["{isMailboxServer,\""++IsMailboxServer++"\"}."]),
					io:format(SF, "~s~n", ["{isUnifiedMessagingServer,\""++IsUnifiedMessagingServer++"\"}."]),
					file:close(SF);
				_->
					error
			end
	end,
	R1= case IsMailboxServer of
		 "True"->
			 [{"MAPI Connectivity","MAPI Connectivity"},
			  {"MAPI-Result","MAPI Connectivity/Result"},
			  {"MAPI-Latency","MAPI Connectivity/Latency"},
			  {"Exchange Search","Exchange Search"},
			  {"ResultFound","Exchange Search/ResultFound"},
			  {"SearchTime","Exchange Search/SearchTime"}
			 ];
		 _ ->
			 []
	 end,
   R2=case IsHubTransportServer of
		 "True"->
			 case IsMailboxServer of
				 "True" ->
			 [{"Mail Flow","Mail Flow"},
			  {"TestMailflowResult","Mail Flow/TestMailflowResult"},
			  {"MessageLatencyTime","Mail Flow/MessageLatencyTime"}
			 ];
				 _ ->
					 []
			 end;
		 _ ->
			 []
	 end,
	R3= case IsClientAccessServer of
		 "True"->
			  [{"OWA Connectivity","OWA Connectivity"},
			  {"OWA-Result","OWA Connectivity/Result"},
			  {"OWA-Latency","OWA Connectivity/Latency"},
			  {"Web Services Connectivity","Web Services Connectivity"},
			  {"GetFolder","Web Services Connectivity/GetFolder"},
			  {"GetFolder-Result","Web Services Connectivity/GetFolder/Result"},
			  {"GetFolder-Latency","Web Services Connectivity/GetFolder/Latency"},
			  {"SyncFolderItems","Web Services Connectivity/SyncFolderItems"},
			  {"SyncFolderItems-Result","Web Services Connectivity/SyncFolderItems/Result"},
			  {"SyncFolderItems-Latency","Web Services Connectivity/SyncFolderItems/Latency"},
			  {"CreateItem","Web Services Connectivity/CreateItem"},
			  {"CreateItem-Result","Web Services Connectivity/CreateItem/Result"},
			  {"CreateItem-Latency","Web Services Connectivity/CreateItem/Latency"},
			  {"DeleteItem","Web Services Connectivity/DeleteItem"},
			  {"DeleteItem-Result","Web Services Connectivity/DeleteItem/Result"},
			  {"DeleteItem-Latency","Web Services Connectivity/DeleteItem/Latency"}
			 ];
		 _ ->
			 []
	 end,							 
	 BrowseD=R1++R2++R3,
	 BrowseD.
perfexCommand(Server,PscFilePath)->
	Cmd="PowerShell -PSConsoleFile "++ "\""++PscFilePath++"\" -Command "++"\""++"templates.applications/scripts.exchange2007/GetExchangeServer.ps1 "++Server++"\"",
	CmdLine = command_line:new(),
	Rets = CmdLine:exec(Cmd),
	RR=string:tokens(Rets, "\r\n"),
	buildCounterR(RR,[]).
buildCounterR([],R)->
	R;
buildCounterR([H|E],R)->
	KV= string:tokens(H, " :"), 
	case length(KV) of
		1 ->
			buildCounterR(E,R);
		_ ->
			Tem=[{lists:nth(1, KV),lists:nth(2, KV)}],
			buildCounterR(E,R++Tem)
	end.
%% 
%%www 
verify(Params)->
	Errs =
	case proplists:get_value(browse,Params) of
		undefined->
			[{browse,"must select at least one  counter"}];
		[]->
			[{browse,"must select at least one  counter"}];
		_->
			[]
	end ++
	case proplists:get_value(pServer,Params) of
		undefined->
			[{pServer,"Exchange Server is null"}];
		[]->
			[{pServer,"Exchange Server is null"}];
		_->
			[]
	end ++
		case proplists:get_value(pUserDomain,Params) of
		undefined->
			[{pUserDomain,"Exchange Domain is null"}];
		[]->
			[{pUserDomain,"Exchange Domain is null"}];
		_->
			[]
	end ++
		case proplists:get_value(pMailboxAlias,Params) of
		undefined->
			[{pMailboxAlias,"Mailbox is null"}];
		[]->
			[{pMailboxAlias,"Mailbox is null"}];
		_->
			[]
	end ++
	case BASE:verify(Params) of
		{error,Be}->
			Be;
		_->
			[]
	end,
	if
		length(Errs)>0->
			{error,Errs};
		true ->
			{ok,""}
	end.
%% @spec get_classifier(Flag) -> Threshold
%% where
%% Flag = (error|warning|good)
%% Threshold = [{Attribute,Opera,Value}]
%% Attribute = atom()
%% Opera = atom()
%% Value = (integer()|atom()|string())
%% @doc Set default threshold value.
get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{contersInError,'>',0}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{contersInError,'>',0}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier; 
		_->
			[{contersInError,'==',0}]
	end.	
getCostInLicensePoints()->
	{ok,{_,Counters}} = THIS:get_property(browse),
	length(Counters).

getLogProperties(This)->
	{ok,{_,Counters}} = THIS:get_property(browse),
	Temp = This:get_template_property(),
	[X#property.name || X<-Temp,X#property.state=:=true] ++ [X||{_,X}<- Counters].

getStatePropertyObjects()->
	[
	 #property{name=countersInError,title="counters in error",type=numeric,editable=true,configurable=false,state=true,order=100}  
	].

%% @spec get_template_property() -> PropertyList
%% where
%% PropertyList = [PropertyRecord]
%% PropertyRecord = record()
%% @doc Add properties to template property list.
get_template_property()->
	BASE:get_template_property() ++ 
	[
	 #property{name=pServer,title="Exchange Server",type=text,configurable=true,editable=true,state=false,description="MS Exchange Server", order=1},
	 #property{name=pUserDomain,title="Exchange Domain",type=text,configurable=true,editable=true,state=false,description="The domain to which both the mailbox owner and the MS Exchange Server belong",order=2},
	 #property{name=pMailboxAlias,title="Mailbox",type=text,configurable=true,editable=true,state=false,description="The alias of the mailbox",order=3},
	 #property{name=pPscFilePath,title="Exchange PS Console File Path",default="C:/Program Files/Microsoft/Exchange Server/Bin/ExShell.psc1",type=text,configurable=true,editable=true,state=false,description="The full path to the Exchange Management Shell PowerShell console file",order=4},
	 #property{name=browse,title="Counters", description="Current selection of counters.",type=browsable,editable=true,order=100},
	 #property{name=contersInError,title="counters in error",type=numeric,state=true,configurable=false}
	].



