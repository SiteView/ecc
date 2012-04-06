%% @author xingyu.cheng@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc historyreport_static
-module(historyreport_static).
-include("monitor.hrl").
-include("historyreport.hrl").
-include("monitor_template.hrl").
-define(TIMEOUT,5).


-define(MAIL_SERVER,mailServer).
-define(MAIL_USER,mailUser).
-define(MAIL_PASSWORD,mailPassword).
-define(AUTO_MAIL,autoEmail).
-define(AUTO_DAILY,autoDaily).
-define(AUTO_START,autoStartup).
-define(FROM_ADDRESS,fromAddress).
-define(MAIL_SERVER_BACKUP,mailServerBackup).
-define(MailEncode,mailEncode).

%%
%% Include files
%%

%%
%% Exported Functions
%%
%% -export([]).
-export([createHistoryReportObject/1,createTopNReportObject/1,createTimeComparisonReportObject/1, getWorstCategory/2, getThresholdList/1, getReportList/2, generateIndexPage/5, writeDatatoDb/0, test/1, test1/0, test2/2, queryReportData/8]).

%%
%% API Functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% static  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(A)->
	case A of
		"1" ->	
			api_siteview:getAllGroupsMonitors();
		"2" ->
			api_monitor:get_all_monitors();
		"3" ->
			api_siteview:getMonitorInfoForTuopu();
		"4" ->		
			rfc4627:encode(api_monitor:get_all_monitorsforjs());
		_->
			ok
	end.

test1()->
	11111.
test2(A,B)->
	B.

generateReportFromQueryID() ->
	ok.

returnGenerateReportFromQueryID() ->
	ok.

returnGenerateReportFromQueryID(S) ->
	ok.

generateReportFromRequest() ->
	ok.

returnReportFromRequest() ->
	ok.

setReportType() ->
	ok.

queryReportData_child([], Parms, Ret)->
	Ret;
queryReportData_child([H|T], Parms, Ret)->
	io:format("queryReportData_child1: ~p ~n", [H]),
	Obj = reportdata:new([]),
	Obj:readFromHashMap(Parms ++ [{monitors, H}]),
    io:format("queryReportData_child2: ~p ~n", [H]),
	Ret1 = Obj:createFromQuery(""),
	io:format("queryReportData_child3: ~p ~n", [H]),
	Obj:delete(),
	queryReportData_child(T, Parms, Ret ++ Ret1).

queryReportData(Ids, Compress, StartTime, EndTime, DstrNeed, DstrStatusNoNeed, Return_value_filter, ByCount)->
	io:format("queryReportData1: ~p ~n", [Ids]),
	Parms = [{comperss,Compress},{startTime, calendar:datetime_to_gregorian_seconds(StartTime)},{endTime, calendar:datetime_to_gregorian_seconds(EndTime)},{dstrNeed,DstrNeed}] ++
	[{dstrStatusNoNeed, DstrStatusNoNeed},{return_value_filter, Return_value_filter},{truedata, ByCount}, {count, 20}],
	MonitorStr = string:tokens(Ids, ","),
	Ret = queryReportData_child(MonitorStr, Parms, []),
	io:format("queryReportData 2 StartTime: ~p ~n", [StartTime]),
	io:format("queryReportData 2 EndTime : ~p ~n", [EndTime]),
	Ret.

%% filter_data([],_)->
%% %% 	io:format("filter_data: ~p ~n", ["2"]),
%% 	[];
%% filter_data(Data,[])->
%% %% 	io:format("filter_data: ~p ~n", ["3"]),
%% 	Data;
%% filter_data(Data,[H|T])->
%% %% 	io:format("filter_data: ~p ~n", ["222"]),
%% 	F=fun(X)->
%% 		case H of
%% 			{id,'in',V11}->
%% 				lists:member(atom_to_list(X#monitorlog.id), V11);
%% 			{id,'=',V1}->
%% %% 				io:format("filter_data: ~p ~n", ["22"]),
%% 				X#monitorlog.id=:=V1;
%% 			{id,'<',V2}->
%% 				X#monitorlog.id<V2;
%% 			{id,'>',V3}->
%% 				X#monitorlog.id>V3;
%% 			{name,'=',V4}->
%% 				X#monitorlog.name=:=V4;
%% 			{time,'>',V5}->
%% 				X#monitorlog.time>V5;
%% 			{time,'>=',V6}->
%% 				(X#monitorlog.time>V6) or (X#monitorlog.time =:= V6);
%% 			{time,'=',V7}->
%% 				X#monitorlog.time =:= V7;
%% 			{time,'<',V8}->
%% 				X#monitorlog.time < V8;
%% 			{time,'<=',V9}->
%% 				(X#monitorlog.time<V9) or (X#monitorlog.time=:=V9);
%% 			{category,'=',V10}->
%% 				X#monitorlog.category =:= V10;
%% 			_->
%% %% 				io:format("filter_data: ~p ~n", ["2"]),
%% 				false
%% 		end
%% 	end,
%% 	filter_data(lists:filter(F,Data),T).
%% 
%% allDatatoData([], Ret)->
%% 	Ret;
%% allDatatoData([H|T], Ret)->	
%% 	case H of
%% 		{eof, ChildData} ->
%% 			case is_list(ChildData) of
%% 				true ->
%% 					allDatatoData(T, Ret ++ ChildData);
%% 				_ ->
%% 					allDatatoData(T, Ret)
%% 			end;
%% 		_ ->
%% %% 			io:format("allDatatoData: ~p ~n", ["2"]),
%% 			allDatatoData(T, Ret)
%% 	end.
%% 
%% queryReportData_child([], Parms, Data, Ret)->
%% 	Ret;
%% queryReportData_child([H|T], Parms, Data, Ret)->	
%% %% 	io:format("queryReportData_child11: ~p ~n", [length(Data)]),	
%% %% 	F1=fun(X)->
%% %% 		io:format("filter_data: ~p ~n", [X]),
%% %% 		case X#monitorlog.id of
%% %% 			H->
%% %% 				io:format("filter_data: ~p ~n", ["1"]),
%% %% 				true;
%% %% 			_->
%% %% 				io:format("filter_data: ~p ~n", ["2"]),
%% %% 				false
%% %% 		end	
%% %% 	end,
%% %% 	io:format("queryReportData_child1: ~p ~n", [H]),
%% %% 	T1 = lists:filter(F1, Data),
%% %% 	filter_data(Data, [{id,'=',H}]),
%% %% 	filter_data(Data, [1,1]),
%% %% 	io:format("queryReportData_child22: ~p ~n", [filter_data(Data, [{id,'=',H}])]),
%% %% 	io:format("queryReportData_child2222dddd2: ~p ~n", [H]),
%% 	case Data of
%% 		[]->
%% 			Obj = reportdata:new();
%% 		_->
%% %% 			io:format("queryReportData_child22222: ~p ~n", [H]),
%% 			Obj = reportdata:new(filter_data(Data, [{id,'=',H}]))			
%% 	end,
%% 	
%% 	Obj:readFromHashMap(Parms ++ [{monitors, H}]),
%% %%     io:format("queryReportData_child2: ~p ~n", [H]),
%% 	Ret1 = Obj:createFromQuery(""),
%% %% 	io:format("queryReportData_child3: ~p ~n", [H]),
%% 	Obj:delete(),
%% 	queryReportData_child(T, Parms, Data, Ret ++ Ret1).
%% 
%% 
%% queryReportData(Ids, Compress, StartTime, EndTime, DstrNeed, DstrStatusNoNeed, Return_value_filter, ByCount)->
%% 	io:format("queryReportData1: ~p ~n", [Ids]),
%% 	Parms = [{comperss,Compress},{startTime, calendar:datetime_to_gregorian_seconds(StartTime)},{endTime, calendar:datetime_to_gregorian_seconds(EndTime)},{dstrNeed,DstrNeed}] ++
%% 	[{dstrStatusNoNeed, DstrStatusNoNeed},{return_value_filter, Return_value_filter},{truedata, ByCount}, {count, 20}],
%% 	MonitorStr = string:tokens(Ids, ","),
%% 	case length(MonitorStr) of
%% 		1->
%% 			Ret = queryReportData_child(MonitorStr, Parms, [], []);			
%% 		_->	
%% 			{_, AllData} = monitor_logger:q(StartTime,	EndTime, MonitorStr),			
%% %% 			{_, AllData} = report_proxy:q(StartTime,	EndTime, MonitorStr),		
%% %% 			io:format("queryReportData1ddddddddd: ~p ~n", [allDatatoData(AllData, [])]),			
%% 			Ret = queryReportData_child(MonitorStr, Parms, allDatatoData(AllData, []), [])
%% 	end,
%% %% 	Ret = queryReportData_child(MonitorStr, Parms, []),
%% 	io:format("queryReportData2: ~p ~n", [Ids]),
%% 	io:format("queryReportData 2 StartTime: ~p ~n", [StartTime]),
%% 	io:format("queryReportData 2 EndTime : ~p ~n", [EndTime]),
%% 	Ret.

queryReportDataOld(Ids, Compress, StartTime, EndTime, DstrNeed, DstrStatusNoNeed, Return_value_filter, ByCount)->
	Parms=[{monitors,Ids},{comperss,Compress},{startTime, calendar:datetime_to_gregorian_seconds(StartTime)},{endTime, calendar:datetime_to_gregorian_seconds(EndTime)},{dstrNeed,DstrNeed}] ++
	[{dstrStatusNoNeed, DstrStatusNoNeed},{return_value_filter, Return_value_filter},{truedata, ByCount}, {count, 20}],
	Obj = reportdata:new(),
	Obj:readFromHashMap(Parms),
    Ret = Obj:createFromQuery(""),
	Obj:delete(),
	Ret.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
createTopNReportObject(HashMap) ->
	Obj = topnreport:new(),
	Obj:readFromHashMap(HashMap),
	Obj.
%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
createTimeComparisonReportObject(HashMap) ->
	Obj = timecomparisonreport:new(),
	Obj:readFromHashMap(HashMap),
	Obj.
%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
createHistoryReportObject(HashMap) ->
	Obj = historyreport:new(),
%% 	io:format("createHistoryReportObject1:~p~n",[HashMap]),
	Obj:readFromHashMap(HashMap),
	case is_atom(Obj:get_app()) of
		true->
			put(hostname, Obj:get_app());	
		_->
			put(hostname, list_to_atom(Obj:get_app()))
    end,
	case proplists:get_value(id,HashMap) of
		undefined->
			pass;
		Id->
			siteview:remove_object(Id),
%% 			BASE:init(This,Data),
%% 			io:format("createHistoryReportObject2:~p~n",[Id]),
%% 			dbcs_base:set_app(proplists:get_value(?APP,HashMap)),
			siteview:set_object(Id,report,historyreport,Obj)
%% 			io:format("createHistoryReportObject3:~p~n",[siteview:get_object(Id)])
	end,
	Obj.
%% 	F = fun(X)->
%% 		AId = proplists:get_value(id,X),
%% 		case siteview:get_object(AId) of
%% 			[]->
%% 				Obj = historyreport:new(),
%% 				Obj:init(Obj,X);		   	
%% 			_->
%% 				pass
%% 		end
%% 	end,
%% 	lists:foreach(F, HashMap)


secondsToString() ->
	ok.

fileOut() ->
	ok.

generateIndexPage() ->
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getTableTitle1([], [], Datas)->
	Datas;
getTableTitle1([H|T], [H1|T1], Datas)->
	getTableTitle1(T, T1, Datas ++ ["<TH COLSPAN=2>"]  ++ to_list(H) ++ to_list(H1) ++ ["</TH>"]).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getTableTitle2([], [], Datas)->
	Datas;
getTableTitle2([H|T], [H1|T1], Datas)->	
	getTableTitle2(T, T1, Datas ++ ["<TD ALIGN=RIGHT>"++ gettext:key2str("Average","zh-cn")++"</TD><TD ALIGN=RIGHT>"++ gettext:key2str("Maximum","zh-cn")++"</TD>"]).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getTableRowData([], Cons, Date, Row)->
	Row;
getTableRowData([H|T], Cons, Date, Row)->
%% 	T1 = Row ++ to_list(Cons:getAverage(Date, H)) ++ to_list(Cons:getMaximum(Date, H)) ++ ["  "],
	case Cons:getCategory(Date, H) of
	good->
	 	T1 = Row ++ ["<TD ALIGN=RIGHT BGCOLOR=#88CC99>"] ++ to_list(Cons:getAverage(Date, H)) ++ ["</TD><TD ALIGN=RIGHT BGCOLOR=#88CC99>"]
		++ to_list(Cons:getMaximum(Date, H)) ++ ["</TD>"];		
	warning->
 		T1 = Row ++ ["<TD ALIGN=RIGHT BGCOLOR=#FFFF77>"] ++ to_list(Cons:getAverage(Date, H)) ++ ["</TD><TD ALIGN=RIGHT BGCOLOR=#FFFF77>"]
		++ to_list(Cons:getMaximum(Date, H)) ++ ["</TD>"];			
	error->
	 	T1 = Row ++ ["<TD ALIGN=RIGHT BGCOLOR=#EE7777>"] ++ to_list(Cons:getAverage(Date, H)) ++ ["</TD><TD ALIGN=RIGHT BGCOLOR=#EE7777>"]
		++ to_list(Cons:getMaximum(Date, H)) ++ ["</TD>"];		
	_->
	 	T1 = Row ++ ["<TD ALIGN=RIGHT BGCOLOR=#EE7777>"] ++ to_list(Cons:getAverage(Date, H)) ++ ["</TD><TD ALIGN=RIGHT BGCOLOR=#EE7777>"]
		++ to_list(Cons:getMaximum(Date, H)) ++ ["</TD>"]
	end,
	getTableRowData(T, Cons, Date, T1).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
to_list(X) when is_atom(X)->
	atom_to_list(X);
to_list(X) when is_float(X)->
	io_lib:format("~.2f", [X]);
to_list(X) when is_integer(X)->
    integer_to_list(X);
to_list(X) ->
  [X].

-include_lib("kernel/include/file.hrl").
file_time(File) ->
	case file:read_file_info(File) of
		{ok, Facts} ->
			Facts#file_info.ctime;
		_->
			 ok
	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
generateIndexPage(HistoryReport, ReportFile, ReportDirectory, Flag, S2) ->
	FileDateList1 = getReportList(ReportDirectory, ".html"),
	FileDateList = lists:sort(fun(A, B) -> file_time(ReportDirectory ++ A ++ ".html") =< file_time(ReportDirectory ++ B ++ ".html") end, FileDateList1),	
%% 	io:format("~p ~n", [FileDateList]),
	
	ImageList = getReportList(ReportDirectory, ".png"),
	removeOldReports(ReportDirectory, lists:reverse(FileDateList), ImageList, 1, 10),
	updateLatestReport(ReportDirectory, ReportFile, FileDateList),
	
	Historysummarycollector = historysummarycollector:new("", ReportDirectory),
	F = fun(X)->
	    Historysummarycollector:add(X)
	end,	
	lists:foreach(F, FileDateList),
	
	case file:open(ReportDirectory ++ "index.html", write) of
		{ok, S} ->
			io:format(S, "~s ~n", ["<html>"]),
			io:format(S, "~s ~n", ["<head>"]),
			io:format(S, "~s ~n", ["<META http-equiv='Content-Type' content='text/html; charset=UTF-8'>"]),
			io:format(S, "~s ~n", ["<link rel='stylesheet' href='/css/SiteView.css' type='text/css' media='screen' charset='utf-8'>"]),			
			io:format(S, "~s ~n", ["</head>"]),
			io:format(S, "~s ~n", ["<body BGCOLOR=#ffffff LINK=#1155bb ALINK=#1155bb VLINK=#1155bb>"]),
			
%% 			io:format("getMonitorNames : ~p ~n", [Historysummarycollector:getMonitorNames()])
			{ok,{_, Description}} = HistoryReport:get_property(description),
			{ok,{_, Title}} = HistoryReport:get_property(title),
			Caption = ["<H2>"] ++  [Title] ++ ["</H2>"] ++ ["<p>"] ++ [Description] ++ ["<p>"],			
			io:format(S, "~s ~n", [lists:flatten(Caption)]),
			case length(FileDateList) of
				0 ->
					io:format(S, "~s ~n", ["<CENTER><H4>No reports have been generated</H2></CENTER>"]);
				_ ->
					{ok,{_, ReportVirtual}} = HistoryReport:get_property(reportlistVirtual),			
					LatestReport = ["<A HREF="] ++ [ReportVirtual] ++ ["/latest.html"] ++ ["><B>"++ gettext:key2str("LatestReport","zh-cn")++"</B></A>"],
					io:format(S, "~s ~n", [lists:flatten(LatestReport)]),
					io:format(S, "~s ~n", ["<CENTER><CAPTION><B>"++ gettext:key2str("Report Summary","zh-cn")++"</B></CAPTION></CENTER>"]),					
					io:format(S, "~s ~n", ["<P><CENTER><A NAME=uptimeSummary> </A><TABLE class='table historylisttable' WIDTH=100% CELLSPACING=0>"]),
					Names = Historysummarycollector:getMonitorNames(),
					Ids = Historysummarycollector:getMonitorIDs(),
					io:format(S, "~s ~n", ["<TR BGCOLOR=#88AA99><TH>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</TH>"]),
					TableTitle1 = getTableTitle1(Names, Ids, [""]),
					io:format(S, "~s ~n", [lists:flatten(TableTitle1)]),
					io:format(S, "~s ~n", ["</TR>"]),
					
					io:format(S, "~s ~n", ["<TR BGCOLOR=#DDDDDD><TD><B>"++ gettext:key2str("Time","zh-cn")++"</B></TD>"]),
					TableTitle2 = getTableTitle2(Names, Ids, [""]),
					io:format(S, "~s ~n", [lists:flatten(TableTitle2)]),
					io:format(S, "~s ~n", ["</TR>"]),

					F1 = fun(X1)->
						io:format(S, "~s ~n", [lists:flatten(["<TR BGCOLOR=#DDDDDD><TD><A HREF="] ++ [ReportVirtual] ++ 
									  ["/"] ++[X1] ++ [".html"] ++ [">"] ++ [X1] ++ ["</A></TD>"])]),				
						TT = getTableRowData(Ids, Historysummarycollector,X1, [""]),
%% 		 				io:format("~p ~n", [lists:flatten(TT)]),
						io:format(S, "~s ~n", [lists:flatten(TT)]),
						io:format(S, "~s ~n", ["</TR>"])
					end,
					lists:foreach(F1, FileDateList),
					io:format(S, "~s ~n", ["</TABLE></CENTER><P>"]),
					
					{ok,{_, ID}} = HistoryReport:get_property(id),
					MakeReport = ["<A HREF="] ++ ["../../web/adhocReport?monitors=1&queryID="] ++ to_list(ID) ++ ["&parms=1"]++ ["><B>"++ gettext:key2str("MakeReport","zh-cn")++"</B></A>"],			
					io:format(S, "~s ~n", [lists:flatten(MakeReport)]),

					io:format(S, "~s ~n", ["</body>"]),
					io:format(S, "~s ~n", ["</html>"]),

					file:close(S)
			end;	
		_->
			io:format("eror   title    file: ~p ~n", [ReportDirectory ++ "index.html"])
	end,
	
	{ok,{_, EmailAddress}} = HistoryReport:get_property(email),
%% 	Email = "xingyu.cheng@dragonflow.com",
	case string:equal(EmailAddress, "") of
		true ->
			ok;
		_->
			{ok,{_, ReportName}} = HistoryReport:get_property(filename),	
			createReportZip(ReportDirectory, ReportName, ImageList),
			case file:list_dir(ReportDirectory ++ "statreport") of
				{ok, Filenames} ->
%% 					[ReportDirectory ++ X || X<- Filenames]
					FilenamesAll = (Filenames -- ["css"]) ++ ["css\\SiteView.css"] ++ ["css\\images\\tt_03.jpg"],
%% 					io:format("Filenames :~p ~n", [Filenames]),
%% 					io:format("Filenames11 :~p ~n", [F11]),
%% 					io:format("Filenames11 :~p ~n", [ReportDirectory]),
					zip:zip(ReportDirectory ++ "statreport.zip", FilenamesAll, [{cwd, ReportDirectory ++ "\\statreport"}]),
					{ok,{_, MailTemplate}} = HistoryReport:get_property(mailTemplate),
%% 					DeflultTitle = "deflult",
					{ok,{_, DeflultTitle}} = HistoryReport:get_property(title),
                    DeflultTitle1 =   iconv:convert(httputils:pageEncode(),"GB2312",DeflultTitle),
					Msg = HistoryReport:createMessage("templates.history", MailTemplate),
                   Msg1 =   Msg,
%%                   io:format("mailinfo: ~s ~n", [lists:flatten(Msg)]),
					{EMailTitle, EMailContent} = 
					case string:str(Msg1, "[Subject:") of
						0->
							{DeflultTitle1,Msg1};
						St->
							case string:str(Msg1, "]") of
								0->
									{DeflultTitle1,Msg1};
								En->
									if En < St->
											{DeflultTitle1,Msg1};                                            
										true ->
											{string:substr(Msg1,St + length("[Subject:"), En - St - length("[Subject:") ) ,
											 string:substr(Msg1,1,St-1) ++ string:substr(Msg1,En+1,length(Msg1) - En)}
									end
							end
					end,
                    
                    {ok,EmailSetting} = api_preferences:get_all('email_settings'),
                 %%    io:format("~n~n EmailSetting:~p ~n~n",[EmailSetting]),
                    case EmailSetting of
                        []  ->
                            ok;
                        _   ->

                        MailEncode = case api_preferences:get_prefs(email_settings,?MAIL_SERVER) of
                                                            {ok,[{_,V0}|_]}->
                                                                V0;
                                                            _->
                                                                ""
                                                        end,
                    %%    io:format("~n~n MailEncode:~p ~n~n",[MailEncode]),
                                case  MailEncode of
                                    []->  ok;
                                    _   ->
                                   %%     io:format("~n~n MailEncode:~p ~n EMailTitle:~p~n~n",[iconv:convert("utf-8",MailEncode, EMailTitle),iconv:convert("utf-8", MailEncode, EMailContent)]),
                                  MailServer = case api_preferences:get_prefs(email_settings,?MAIL_SERVER) of
                                                            {ok,[{_,V1}|_]}->
                                                                V1;
                                                            _->
                                                                ""
                                                        end,
                                 MailUser = case api_preferences:get_prefs(email_settings,?MAIL_USER) of
                                                        {ok,[{_,V2}|_]}->
                                                            V2;
                                                        _->
                                                            ""
                                                    end,
                                MailPasswd = case api_preferences:get_prefs(email_settings,?MAIL_PASSWORD) of
                                                            {ok,[{_,V4}|_]}->
                                                                V4;
                                                            _->
                                                                ""
                                                        end,
                                FromAddress = case api_preferences:get_prefs(email_settings,?FROM_ADDRESS) of
                                                                {ok,[{_,V5}|_]}->
                                                                    V5;
                                                                _->
                                                                    ""
                                                            end,
                                TOAddresslist = string:tokens(EmailAddress,","),
                                NewTitle = case DeflultTitle1 =:= EMailTitle of
                                                            true->
                                                                EMailTitle;
                                                            _->
                                                                EMailTitle ++ DeflultTitle1
                                                   end,
                                NewContent = EMailContent,
                                
                                io:format("~n~n ~p -> ~p: ~n EMailContent:~p -> ~p~n~n",[httputils:pageEncode(),MailEncode,EMailTitle,NewTitle]),
                                sendEMail(MailServer,MailUser,MailPasswd,FromAddress,TOAddresslist,NewTitle,NewContent, ReportDirectory ++ "statreport.zip")
                               end 
                     end;
				_->
					ok
			end
	end.
sendEMail(MailServer,MailUser,MailPasswd,FromAddress,[],Title,Content,Attachment)->ok;
sendEMail(MailServer,MailUser,MailPasswd,FromAddress,[ToAddress|TOAddresslist],Title,Content,Attachment)->
    SendResult = mail:smtp_mail_send(MailServer,25,MailUser,MailPasswd,FromAddress,ToAddress,Title,Content,Attachment,?TIMEOUT),
%%    io:format("~n~n SendResult:~p ~n~n",[SendResult]),
    sendEMail(MailServer,MailUser,MailPasswd,FromAddress,TOAddresslist,Title,Content,Attachment).
%% 	F4 = fun(X4)->
%% 		ets:delete(X4:get_tid())
%% 	end,
%% 	lists:foreach(F4, Historysummarycollector).



cleanAllTemporaryReportFiles(F) ->
	ok.
isFlipperQuickReport() ->
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
updateLatestReport(ReportDirectory, ReportFile, FileDateList) ->
	file:copy(ReportFile, ReportDirectory ++ "latest.html"),
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
removeOldReports(ReportDirectory, [], ImageList, I, MaxReports) ->
	ok;
removeOldReports(ReportDirectory, [H|T], ImageList, I, MaxReports) ->
	case I < MaxReports of
		true ->
			removeOldReports(ReportDirectory, T, ImageList, I+1, MaxReports);	
		_->
			TT = [ReportDirectory] ++ [H] ++ [".html"],	
			file:delete(TT),
			
			
			T1 = [ReportDirectory] ++ [H] ++ [".data"],
			file:delete(T1),
			
			F = fun(X) ->
			    case string:rstr(X, H) of
					0 ->
						continue;
					_ ->
						PngPath = [ReportDirectory] ++ [X] ++ [".png"],
						file:delete(PngPath)
				end
			end,
			lists:foreach(F, ImageList),
			removeOldReports(ReportDirectory, T, ImageList, I+1, MaxReports)	
	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
createReportZip(ReportDirectory, ReportName, ImageList) ->	
	case filelib:is_dir(ReportDirectory ++ "statreport") of
		true->
			case file:list_dir(ReportDirectory ++ "statreport") of
				{ok, Filenames} ->
					file:delete(ReportDirectory ++ "statreport.zip"),
					lists:foreach(fun(X) -> file:delete(ReportDirectory ++ "statreport\\" ++ X) end, Filenames);
				_ ->
					ok
			end;			
		_->
			file:make_dir(ReportDirectory ++ "statreport"),
			file:make_dir(ReportDirectory ++ "statreport\\css"),
			file:make_dir(ReportDirectory ++ "statreport\\css\\images")
	end,	
	
%% 	file:copy(ReportDirectory ++  ReportName ++ ".html", ReportDirectory ++  "statreport\\" ++ ReportName ++ ".html"),
	
	{ok, Bin} = file:read_file(ReportDirectory ++  ReportName ++ ".html"),
	ListContent = binary_to_list(Bin),
%% 	io:format("Content ~p ~n", [lists:flatten(ListContent)]),
	I = string:rstr(ListContent, "/css/SiteView.css"),
	S1 = string:sub_string(ListContent, 1, I - 1),
	S2 = string:sub_string(ListContent, I, length(ListContent)),
	S3 = S1 ++ "." ++ S2,	
%% 	io:format("~p ~n", [lists:flatten(S3)]),
%% 	io:format("~p ~n", [lists:flatten(S2)]),
	
	file:write_file(ReportDirectory ++  "statreport\\" ++ ReportName ++ ".html", S3),
	
	file:copy(ReportDirectory ++  "logo.jpg", ReportDirectory ++  "statreport\\logo.jpg"),
	file:copy(ReportDirectory ++  "SiteView.css", ReportDirectory ++  "statreport\\css\\SiteView.css"),
	file:copy(ReportDirectory ++  "tt_03.jpg", ReportDirectory ++  "statreport\\css\\images\\tt_03.jpg"),
%% 	file:copy(ReportDirectory ++  "logo.jpg", ReportDirectory ++  "statreport\\logo.jpg"),
	
	F = fun(X) ->
	    case string:rstr(X, ReportName) of
			0 ->
				continue;
			_ ->
				PngoldPath = [ReportDirectory] ++ [X] ++ [".png"],
				PngNewPath = [ReportDirectory] ++ ["statreport\\"] ++ [X] ++ [".png"],
				file:copy(PngoldPath, PngNewPath)
		end
	end,
	lists:foreach(F, ImageList).
  
cleanAllTemporaryReportFiles() ->
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getReportList(Path, Ext) ->
	case filelib:is_dir(Path) of
		true->
			ok;
		_->
			file:make_dir(Path)
%% 			Platform.chmod(reportDirectory, "rwx");
	end,
	
	case file:list_dir(Path) of
		{ok, Filenames} ->
			Html = [filename:basename([H|R], Ext) || [H|R] <- Filenames, H =:= $R, string:equal(filename:extension([H|R]), Ext)];
		_ ->
			Html = []
	end,
	Html.

%%
%% Local Functions
%%

accountToDirectory() ->
	ok.

dateToReportURLBase() ->
	ok.

dateToReportPathBase()->
	ok.

appendBinaryAttachment()->
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getWorstCategory(Category1, Category2)->
%%  categoryMap.add(WORST_CATEGORY, new String(ERROR_CATEGORY));
	CategoryMap = [{filtered, 0}, {nodata, 1}, {disabled, 2}, 
		{good, 3}, {warning, 4}, {error, 5}, {worst, 6}],

	{value,{_, I1}} = lists:keysearch(Category1, 1, CategoryMap),
	{value,{_, I2}} = lists:keysearch(Category2, 1, CategoryMap),
   	case I1 > I2 of
		true ->
			Category1;
		_ ->
			Category2 
	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getThresholdList(Monitor)->
	[{error, Monitor:get_classifier(error)},{warning, Monitor:get_classifier(warning)},
	{good, Monitor:get_classifier(good)}].

writeDatatoDb()->
	{_, AllData} = monitor_logger:q({{2009,11,1},{12,0,0}},{{2009,11,30},{12,0,0}}, ""),
	F = fun(X)->
%% 		io:format("~p ~n", [X]),
		case X of
			{eof, ChildData} ->
%% 				io:format("~p ~n", [ChildData]),
				F1 = fun(X1)->
%% 					io:format("~p ~n", [X1]),
					case is_list(X1) of
						true ->
							F2 = fun(X2)->
%% 								X2#monitorlog.id
%% 								io:format("~p ~n", [X2]),										 
								monitor_logger:logdb({X2#monitorlog.id, X2#monitorlog.time, X2})							 
							 end,
							 lists:foreach(F2, X1);
				 	 	 false ->
							ok
					end					
			    end,
%% 			    io:format("~p ~n", ["haha1111"]),
			    lists:foreach(F1, [ChildData]);
			_ ->
			    io:format("~p ~n", ["No Data Log:"]),
				io:format("~p ~n", [X])
		end
	end,
	lists:foreach(F, AllData),
	io:format("~p ~n", ["writeDatatoDb ok"]).
