%% Author: bin.liu
%% Created: 2009-12-2
%% Description: TODO: Add description to adhocReportPage
-module(adhocReportPage).
-compile([export_all]).

%%
%% API Functions
%%

createadhocReportFile(QueryID,Monitors,Account,Group,Detailed,StartDateTime,EndDateTime,Pparms)->
	case length(QueryID)>0 of
		 true ->
%% 			 Sdate="./wwwroot/htdocs/Reports-0/"++util:getSDateTime(), Sdate++".html"
			 Parms=[{startDay,today},{startHour,"now"},{window,""},{reportType,"barGraph"},{title,"0"}]
	                ++[{basic,false},{monitors,Monitors},{precision,""},{relative,-1},{reportPeriod,""}]
	                ++[{vmax,"default"},{realTime,true},{schedFilter,"all"},{statusFilter,"all"}]
	                ++[{description,""},{email,""},{attachReport,""},{emailData,""}]
                    ++[{tabfile,false},{xmlfile,false},{xmlEmailData,""},{disabled,false}]
                    ++[{warningNotIncluded,false},{upTimeIncludeWarning,false},{failureNotIncluded,false}]
                    ++[{schedule,""},{mailTemplate,"HistoryMail"},{bestCaseCalc,false},{showReportThresholdSummary,false}]
                    ++[{hideReportSummary,false},{showReportErrorTimeSummary,false},{showReportAlerts,""}]
                    ++[{format,""},{hideReportTables,false},{hideReportErrors,false},{hideReportWarnings,false}]
                    ++[{hideReportGoods,false},{noSlotFilter,true},{id,'0'}],
			 M = historyreport_static:createHistoryReportObject(Parms),
		     M:createFromQuery(""),
 			 {_,{_,Ret}}=M:get_property(reportURL),
			 M:delete();
		 false ->
%% 			 Sdate="./wwwroot/htdocs/Reports-0/"++util:getSDateTime(),
			 Parms=Pparms++[{reportPeriod,""},{realTime,true},{title,"0"},{id,'0'}],
			 M = historyreport_static:createHistoryReportObject(Parms),
		     M:createFromQuery(""),
 			 {_,{_,Ret}}= M:get_property(reportURL),
			 M:delete()
	end,
	Ret.

%%
%% Local Functions
%%

