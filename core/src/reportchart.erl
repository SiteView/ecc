%% Author: Administrator
%% Created: 2009-11-6
%% Description: TODO: Add description to reportchart
-module(reportchart).
-compile(export_al).
-define(Samples,samples).
%%
%%HistoryReportHTML
%%Files
%%CollectorList
%%AllPropertiesSame true or false
%%AllMonitorsSame   true or false
%%ImageCount
%%
imageLineGraph(HistoryReportHTML,Files,CollectorList,AllPropertiesSame,AllMonitorsSame,ImageCount,Width,Height )->
     LineCount =length(CollectorList),
	if LineCount=:=0 ->
		   Ret=null;
	   LineCount>0 ->
		 Names=  [ Samplecollector:getPropertyLabel() || Samplecollector<-CollectorList],
		 F=fun(Samplec) ->
				M=Samplec:getMonitorName(),
              if (AllPropertiesSame=/=true)->
                     Name=M++"("++ Samplec:getPropertyLabel()++")";
				  AllPropertiesSame=:=true ->
					  Name=M
               end,
		      Average=Samplec:getAverage(),
			  Maximum=Samplec:getMaximum(),
			  MaximumTime=Samplec:getMaximumTime(),
			  AveragedSamples=Samplec:samplesAreAveraged(),
				if (AllMonitorsSame=:=true)->
					   TotalTime=Samplec:getTotalTime(),
				       ErrorTime=Samplec:getErrorTime(),
					   ErrorPercentage=ErrorTime/TotalTime*100;
			        AllMonitorsSame=/=true ->
		               TotalTime=null
				end
		 end,
		 lists:foreach(F, CollectorList),
		 FirstCollector=hd(CollectorList),
		 IsMultipleMonitors=false,
		 if IsMultipleMonitors,LineCount=/=1 ->
				Title ="Report title";
			true ->
				Title=FirstCollector:getMonitorName()
		 end,
		 PropertyName=FirstCollector:getPropertyLabel(),
		 VertMax=1,
		 StartTime=FirstCollector:getStartTime(),
		 EndTime=FirstCollector:getEndTime(),
	 Ret=ok
	 end,  
	 Ret.
%%ReportObjectConfig,
%%default value
%%Imgcount=1
%%CollectorMap=[]
%%
htmlChart(HistoryReportHTML,Files,ConCollectorList,[H|E],CollectorMap,SimilarProperties,MultipleMonitors,Imgcount)->
	Samplecollector=H,
	case (Samplecollector:isSupportOnlyNumericCounters()) of
           true ->
			   case lists:keymember(Samplecollector:getIDString(), 1, CollectorMap) of
				   false ->
					 Ret=bulidNewCollectorList(Samplecollector,ConCollectorList,CollectorMap,[],SimilarProperties,MultipleMonitors,true,true,[],[]),
					 {_,Coll}=lists:keyfind(collectorList, 1, Ret),
				     {_,AllPropertiesSame}=ists:keyfind(allPropertiesSame, 1, Ret),
					 {_,AllMonitorsSame}=ists:keyfind(allMonitorsSame, 1, Ret),
					 {_,TCollectorMap}=ists:keyfind(collectorMap, 1, Ret),
					 case length(Coll)>0 of
						 true->
							imageLineGraph(HistoryReportHTML,Files,ConCollectorList,AllPropertiesSame,AllMonitorsSame,Imgcount,600,300),
							Tmpimagecount=Imgcount+1;
					      _->
	                       Tmpimagecount=Imgcount
					 end
						 
			   end
	end,
	htmlChart(HistoryReportHTML,Files,ConCollectorList,E,TCollectorMap,SimilarProperties,MultipleMonitors,Tmpimagecount);
htmlChart(_,_,_,[],_,_,_,_)->
	ok.
bulidNewCollectorList(SampleCollector,[H|E],CollectorMap,CollectorList,SimilarProperties,MultipleMonitors,AllPropertiesSame,AllMonitorsSame,LastMonitor,LastProperty) ->
	Coll=H,
	case (Coll:isSupportOnlyNumericCounters()) of
           true ->
			   case lists:keymember(Coll:getIDString(), 1, CollectorMap) of
				   false ->
					   case ((SampleCollector:getMonitor()=:=Coll:getMonitor()) orelse MultipleMonitors) of
                            true ->
                              case ((SampleCollector:getProperty()=:=Coll:getProperty()) orelse SimilarProperties) of
								  true ->
									  TmpCollectorList=CollectorList++Coll,
									  case AllPropertiesSame of true ->
									     case length(LastProperty)=/=0 andalso LastProperty=/=Coll:getProperty() of
                                           true ->
										    	 TmpAllPropertiesSame=false;
										    false->
											     TmpAllPropertiesSame=true
                                          end
									  end,
									   case AllMonitorsSame of true ->
							              case length(LastMonitor)=/=0 andalso LastMonitor=/=Coll:getMonitor() of
                                               true ->
											       TmpAllMonitorsSame=false;
										        false->
											       TmpAllMonitorsSame=true
                                               
                                           end
									  end,
									  TmpCollectorMap=CollectorMap++[{Coll:getIDString(),"graphed"}]
							  end
                       end
			   end 
   end,
	TmpLastMonitor=Coll:getMonitor(),
	TmpLastProperty=Coll:getProperty(),
	bulidNewCollectorList(SampleCollector,E,TmpCollectorMap,TmpCollectorList,SimilarProperties,MultipleMonitors,TmpAllPropertiesSame,TmpAllMonitorsSame,TmpLastMonitor,TmpLastProperty);
bulidNewCollectorList(_,[],CollectorMap,CollectorList,_,_,_,_,AllPropertiesSame,AllMonitorsSame) ->
	[{collectorList,CollectorList},{allPropertiesSame,AllPropertiesSame},{allMonitorsSame,AllMonitorsSame},{collectorMap,CollectorMap}].


%%
%% CollectorLookupMap={stringproperty.getName(), samplecollector}
%%GraphedCollectors={,"graphed"}
%%
realTimeHTMLChart(HistoryReportHTML,Files,CollectorLookupMap,[H|E],GraphedCollectors,ImageCount,SimilarProperties) ->
	SampleCollector=H,
	IDString=SampleCollector:getIDString(),
    case SampleCollector:isNumeric()=:=true andalso lists:keymember(IDString, 1, GraphedCollectors)=:=false of
		true ->
			if SimilarProperties ->
				   TmpCollectors=[],
			       TmpGraphedCollectors=[];
			   not SimilarProperties->
				   TmpGraphedCollectors=GraphedCollectors++[IDString,"graphed"],
				   TmpCollectors=[SampleCollector]
			end,
			case length(TmpCollectors)>0 of 
				true->
				   TemImageCount=ImageCount+1,
			      imageLineGraph(HistoryReportHTML,Files,TmpCollectors,false,false,TemImageCount,600,300);
				false ->
					TemImageCount=ImageCount
			end
	end,
	realTimeHTMLChart(HistoryReportHTML,Files,CollectorLookupMap,E,TmpGraphedCollectors,TemImageCount,SimilarProperties);
realTimeHTMLChart(_,_,_,[],_,_,_) ->
	ok.
buildata([H|E],CollectorLookupMap,Collectors,GraphedCollectors)->
	Stringproperty=H,
	SampleCollector2=lists:keyfind(Stringproperty:getName(), 1, CollectorLookupMap) ,
	case SampleCollector2=/=false of
		true ->
			dd
	end,
	ok.
getSamples()->
ok.
