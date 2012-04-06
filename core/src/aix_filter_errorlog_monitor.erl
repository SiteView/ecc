-module(aix_filter_errorlog_monitor,[BASE]).
-extends(server_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").


new() ->
    Base = server_monitor:new(),
	Base:set_attribute(pLastMeasurement,0),
	{?MODULE,Base}.

is_aix(Host)->
	case machine:getOS(Host) of
		14 ->
			true;
		_ ->
			false
	end.

update() ->
	{ok,{pLastMeasurement,LastMeasurement}} = THIS:get_attribute(pLastMeasurement),
    {ok,{_,Host}} = THIS:get_property(machine),
	{ok,{_,Filter_t}} = THIS:get_property(filtert),
	{ok,{_,Filter_c}} = THIS:get_property(filterc),
	{ok,{_,Filter_id}} = THIS:get_property(filterid),
	{ok,{_,Filter_time}} = THIS:get_property(filtertime),
	%~ {ok,{_,Filter_source}} = THIS:get_property(filtersource),
	Filter = [{t,3,Filter_t},{c,4,Filter_c},{id,1,Filter_id},{time,2,Filter_time},{source,5,""}],
    case is_aix(Host) of
		true ->
			case LastMeasurement of
				0 ->
					Command = "errpt";
				_ ->
					Command = lists:flatten(["errpt -s ",LastMeasurement])
			end,
			case siteview_commandline:exec(Host,Command) of
				{ok,[]} ->
					THIS:set_attribute(logs,0),
					THIS:set_attribute(?STATE_STRING,"no log happen");
				{ok,[_|Data_list]} ->
					Result = filter(Data_list,Filter),
					THIS:set_attribute(?STATE_STRING,Result);
				_ ->
					THIS:set_attribute(logs,"n/a"),
					THIS:set_attribute(?NO_DATA,true),
					THIS:set_attribute(?STATE_STRING,"Command is not right"),
					THIS:set_attribute(?CATEGORY,?NO_DATA)
			end,
			spawn(fun()->get_aix_date(Host) end);
			
		false ->
			THIS:set_attribute(logs,"n/a"),
			THIS:set_attribute(?NO_DATA,true),
			THIS:set_attribute(?STATE_STRING,"The Host is not a Aix machine"), 
			THIS:set_attribute(?CATEGORY,?NO_DATA)
    end.
 
verify(Params)->
    Errs = 
		%~ case proplists:get_value(machine,Params) of
			%~ ""->
				%~ [{host,"host missing"}];
			%~ _Host->
				%~ []
		%~ end,
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

get_template_property() ->
    BASE:get_template_property() ++
    [
		#property{name=filtert,title="Filter Condition T", description="Filter accroding to 'T' column,it will not filter if it's empty",type=text,editable=true,order=2},
		#property{name=filterc,title="Filter Condition C", description="Filter accroding to 'C' column,it will not filter if it's empty",type=text,editable=true,order=3},
		#property{name=filterid,title="Filter Condition Identifer", description="Filter accroding to 'identifier' column,it will not filter if it's empty",type=text,editable=true,order=4},
		#property{name=filtertime,title="Filter Condition Time", description="Filter accroding to 'timestamp' column,it will not filter if it's empty",type=text,editable=true,order=5},
		#property{name=filtersource,title="Filter Condition Resource Name", description="Filter accroding to 'resourse name' column,it will not filter if it's empty",type=text,editable=true,order=6}
		#property{name=logs,title="Logs",type=numeric,configurable=false,state=true}
    ].

get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{logs,'>=',5}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{logs,'>=',2}]
	
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{logs,'==',0}]
	
	end.
filter(Data_list,[{_Key,Column,Value}|Filter]) ->
	case length(Value) of
		0 ->
			filter(Data_list,Filter);
		_ ->
			Filter_list = filter(Data_list,Column,Value,[]),
			filter(Filter_list,Filter)
	end;
	
filter(Data_list,[]) ->
	%~ io:format("Data_list:~p~n",[Data_list]),
	case length(Data_list) of
		0 ->
			THIS:set_attribute(logs,0),
			"no log match the filter";
		_ ->
			THIS:set_attribute(logs,length(Data_list)),
			Sub_result = lists:sublist(Data_list,1,10),
			list_to_string(Sub_result,"")
	end.
	
filter([H|T],Column,Filter,Result) ->
	List = string:tokens(H," "),
	case lists:nth(Column,List) of
		Filter ->
			filter(T,Column,Filter,[H|Result]);
		_ ->
			filter(T,Column,Filter,Result)
	end;
	
filter([],_,_,Result) ->
	Result.
	
list_to_string([H|T],String) ->
	list_to_string(T,H++"<br/>"++String);
	
list_to_string([],String) ->
	String.

get_aix_date(Host) ->
	Date_command = "date +'%m%d%H%M%y'",
	case siteview_commandline:exec(Host,Date_command) of
		{ok,[Sys_date]} ->
			THIS:set_attribute(pLastMeasurement,Sys_date);
		_ ->
			THIS:set_attribute(pLastMeasurement,0)
	end.

      
    


