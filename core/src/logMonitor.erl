-module(logMonitor,[BASE]).
-compile(export_all).
-extends(logMonitorBase).
-include("monitor_template.hrl").
-include("monitor.hrl").
-include_lib("kernel/include/file.hrl").

new() ->
    Base = logMonitorBase:new(),
    Base:set_property(pRulesFile,""),
    {?MODULE,Base}.
 

getScalarValues(Prop,Params)->
	case Prop of
		pResetFile->
			[{"Never","Never"},{"Always","Always"},{"First Time Only","First Time Only"}];
        pAlerting ->
            [{"for each log entry matched","for each log entry matched"},{"once, after all log entries have been checked","once, after all log entries have been checked"}];         
        _->    
			BASE:getScalarValues(Prop,Params)
	end.

verify(Params)->
    Errs = 
	case proplists:get_value(pLogFile,Params) of
    ""->
	    [{pLogFile,"file name missing."}];
    Filename->
        LocalCode = platform:getLocalCode(), 
        File_nameT = iconv:convert("utf-8",LocalCode,Filename),
        NoFileCheckExist = proplists:get_value(pNoFileCheckExist,Params),       
		Statu = check_file_exist(File_nameT),
        if Statu == error ->
            if  NoFileCheckExist  ->
                [];
            true ->                
                [{pLogFile,"The file does not exist or path error."}]
            end;     
        true ->
            Host =  proplists:get_value(machine,Params),
            SIZE = BASE:getEndPosition(Filename,Host),
            if SIZE >= 100000000 ->
                case proplists:get_value(pResetFile,Params) of
                "Always" ->
                    [{pResetFile,"Oversize log file,can not Select file checking option \"Always\"."}];   
                "First Time Only" ->
                    [{pResetFile,"Oversize log file,can not Select file checking option \"First Time Only\"."}];
                _ ->
                    []
                end;
            true ->
                if SIZE >= 50000000  ->
                   case proplists:get_value(pResetFile,Params) of
                   "Always" ->
                         [{pResetFile,"Oversize log file,can not Select file checking option \"Always\"."}]; 
                    _ ->
                        []
                    end;
                true ->
                    []
                end
            end                
		end
	end ++   
    case BASE:verify(Params) of
		{error,Be}->
			Be;
		_->
			[]
	end, 
	if length(Errs)>0 ->
	    {error,Errs};
    true ->
	    {ok,""}
	end.



                     

get_template_property()->
	BASE:get_template_property() ++ 
    [
        #property{name=pLogFile,title="Log File Pathname",type=text,order=1,description="the pathname of the log file to monitor\n<p> In order to monitor remote Unix files choose the 'Choose Server' link above.  For NT, you must specify the UNC path to the file.  For example, \\\\machinename\\sharename\\filename.log.\n<p>Optionally, use a <a href=/SiteView/docs/regexp.htm>regular expression</a> to insert date and time variables <br>(e.g s/ex$shortYear$$0month$$0day$.log/ to match IIS log files)"},
        #property{name=pResetFile,title="Check from Beginning",type=scalar,default="Never",description="Select file checking option as follows:\n <br><b>Never</b> - Check only newly added records (default)\n<br><b>First Time Only</b> - Check the whole file once, then only new records\n<br><b>Always</b> - Always check the whole file\n"},
        #property{name=pAlerting,title="Run Alerts",type=scalar,default="for each log entry matched",description="How alerts for this monitor are triggered by the following options:\n  <br>(1) For <b>'for each log entry matched and report status'</b> the monitor triggers alerts for every matching entry found based on the <b>Error If</b> and <b>Warning If</b> thresholds defined for the monitor. \n<br>(2) For <b>'once, after all log entries have been checked'</b>, the monitor counts up the number of matches and then triggers alerts based on the <b>Error If</b> and <b>Warning If</b> thresholds defined for the monitor.\n"},
        #property{name=pMatch,title="Content Match",type=text,order=3,description="enter the text to match in a log entry or a <a href=/SiteView/docs/regexp.htm>regular expression</a>. By default successful match makes monitor alert or error."},        
        #property{name=pRulesFile,title="Rules File Pathname",type=text,order=4,advance=true,description="Rules File Pathname"},
        #property{name=pValueLabels,title="Match Value Labels",type=text,order=5,advance=true,description="Labels for the values matched on the log output, separated by a \",\""},       
        #property{name=pNoFileCheckExist,title="No Error on File Not Found",advance = true,editable=true,configurable=true,type=bool,description="if the file is not found then don't error."},        
        #property{name=pLines,title="Lines",type=numeric,configurable=false,state=true},
        #property{name=pMatches,title="Matches",type=numeric,configurable=false,state=true},
        #property{name=pLastAlertsPerMinute,title="matches/min",type=numeric,configurable=false,state=true},
        #property{name=pLastLinesPerMinute,title="lines/min",type=numeric,configurable=false,state=true}
    ].
 

getStateProperties(This,Params) ->
    Temp = This:get_template_property(),
	T = [X || X<-Temp,X#property.state=:=true],
	case proplists:get_value(pValueLabels,Params) of
    undefined ->
        case This:get_property(pValueLabels) of
        {ok,{_,ValueLabels}} ->        
            LabelsLen  = length(ValueLabels),
            if LabelsLen > 0 ->      
                ValueList = string:tokens(ValueLabels,","),
                Len = length(ValueList),
                [Temp1] = lists:sublist(ValueList,1,1),
                Value1 = [{"value",textutils:trim(Temp1)}],
                if Len >= 2 ->
                    [Temp2] = lists:sublist(ValueList,2,1),
                    Value2= [{"value1",textutils:trim(Temp2)}] ++ Value1,
                    if Len >=3 ->
                        [Temp3] = lists:sublist(ValueList,3,1),
                        Value3 = [{"value2",textutils:trim(Temp3)}] ++ [{"value1",textutils:trim(Temp2)}] ++ Value1, 
                        if Len >= 4 ->
                            [Temp4] = lists:sublist(ValueList,4,1), 
                            Value4 = [{"value3",textutils:trim(Temp4)}] ++ [{"value2",textutils:trim(Temp3)}] ++ [{"value1",textutils:trim(Temp2)}] ++ Value1;
                        true ->
                            Value4= [{"value2",textutils:trim(Temp3)}] ++ [{"value1",textutils:trim(Temp2)}] ++ Value1                         
                        end;
                    true ->
                        Value4 = [{"value1",textutils:trim(Temp2)}] ++ Value1
                    end;
                true ->
                    Value4 = Value1 
                end;
            true ->    
                Value4 = [{"value", "value"},{"value2", "value2"},{"value3", "value3"},{"value4", "value4"}] 
            end,
            T ++ make_template(Value4);
        _ ->
            T 
        end;
    ValueLabels ->
        LabelsLen  = length(ValueLabels),
        if LabelsLen > 0 ->      
            ValueList = string:tokens(ValueLabels,","),
            Len = length(ValueList),
            [Temp1] = lists:sublist(ValueList,1,1),
            Value1 = [{"value",textutils:trim(Temp1)}],
            if Len >= 2 ->
                [Temp2] = lists:sublist(ValueList,2,1),
                Value2= [{"value1",textutils:trim(Temp2)}] ++ Value1,
                if Len >=3 ->
                    [Temp3] = lists:sublist(ValueList,3,1),
                    Value3 = [{"value2",textutils:trim(Temp3)}] ++ [{"value1",textutils:trim(Temp2)}] ++ Value1, 
                    if Len >= 4 ->
                        [Temp4] = lists:sublist(ValueList,4,1), 
                        Value4 = [{"value3",textutils:trim(Temp4)}] ++ [{"value2",textutils:trim(Temp3)}] ++ [{"value1",textutils:trim(Temp2)}] ++ Value1;
                    true ->
                        Value4= [{"value2",textutils:trim(Temp3)}] ++ [{"value1",textutils:trim(Temp2)}] ++ Value1                         
                    end;
                true ->
                    Value4 = [{"value1",textutils:trim(Temp2)}] ++ Value1
                end;
            true ->
                Value4 = Value1 
            end;
        true ->    
            Value4 = [{"value", "value"},{"value2", "value2"},{"value3", "value3"},{"value4", "value4"}] 
        end,
        T ++ make_template(Value4)                 
    end.

make_template(List) ->
    make_template_t(List,length(List),[]).
make_template_t(_L,0,Template) -> Template; 
make_template_t([A|B],Num,Te) ->
    {Name,Value} =  A, 
   Temp = #property{name=list_to_atom(Value),title=Value,type=text,state=true,configurable=false}, 
   make_template_t(B,Num-1,[Temp|Te]).

get_classifier(error)->
	Cls = case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			io:format("Classifier:~p~n",[Classifier]),
            Classifier;
		_->
			[{pMatches,'>',0}]
	end;    
get_classifier(warning)->
	Cls = case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[]
	end;
get_classifier(good)->
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[]
	end.
 
check_file_exist(LogFile) ->
    io:format("LogFile:~p~n",[LogFile]),    
    Index = string:str(LogFile,"\\\\"),
    if Index == 1 ->
        {Statu,Re} = file:read_file_info(LogFile),
        if Statu == error ->
            error;
        true ->
            if Re#file_info.type == directory ->
                error;
            true ->    
                ok
            end    
        end;
    true ->
        ok    
    end.        