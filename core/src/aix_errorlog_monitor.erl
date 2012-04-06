-module(aix_errorlog_monitor,[BASE]).
-extends(server_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").


new() ->
    Base = server_monitor:new(),
	{?MODULE,Base}.

is_aix(Host)->
case machine:getOS(Host) of
	14 ->
		true;
	_ ->
		false
end.

update() ->
    {ok,{_,Host}} = THIS:get_property(machine),
	{ok,{_,Logcolumn}} = THIS:get_property(logcolumn),
    case is_aix(Host) of
		true ->
			Command = "errpt -a -j "++Logcolumn,
			case siteview_commandline:exec(Host,Command) of
				{ok,Data_list} ->
					% THIS:set_attribute(logs,length(Data_list)),
					% io:format("Data_list:~p~n",[Data_list]),
					THIS:set_attribute(logs,1),
					writeLog(Data_list),
					Result = format(Data_list,"",0),
					writeLog(Result),
					if 	Data_list =:= [] ->
							THIS:set_attribute(?STATE_STRING,"error");
						Data_list =/= [] ->
							THIS:set_attribute(?STATE_STRING,Result)
					end;
				_ ->
					THIS:set_attribute(logs,"n/a"),
					THIS:set_attribute(?NO_DATA,true),
					THIS:set_attribute(?STATE_STRING,"Command is not right"),
					THIS:set_attribute(?CATEGORY,?NO_DATA)
			end;
		false ->
			THIS:set_attribute(logs,"n/a"),
			THIS:set_attribute(?NO_DATA,true),
			THIS:set_attribute(?STATE_STRING,"The Host is not a Aix machine"), 
			THIS:set_attribute(?CATEGORY,?NO_DATA)
    end.
 
getScalarValues(Prop,Params)->
	case Prop of
		logcolumn ->
			case lists:keysearch(machine,1,Params) of
				{value,{machine,Host}}->
					case is_aix(Host) of
						true ->
							{ok,Command} = machine:getCommandString(errpt,Host),
							case siteview_commandline:exec(Host,Command) of
								{ok,[_|Data_list]} ->
									F = fun(Element) ->
											List = string:tokens(Element," "),
											{Element,hd(List)}
										end,
									Browse_data = lists:map(F,Data_list),
									Browse_data;
								_ ->
									[]
							end;
						false ->
							[]
					end;
				_->
					[{"Choose an AIX machine","Choose an AIX machine"}]
			end;
		_ -> BASE:getScalarValues(Prop,Params)
	end.
    
verify(Params)->
    io:format("Params, Params aix ~p~n",[Params]),
    Errs = 
	%~ case proplists:get_value(host,Params) of
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
		#property{name=logcolumn,title="Log Column", description="",type=scalar,editable=true,order=2},
		#property{name=logs,title="Logs",type=numeric,configurable=false,state=true}
    ].
	
get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{logs,'>=',2000}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{logs,'>=',1000}]
	
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{logs,'<',1000}]
	
	end.
	
	
format(_Data,Result,26) ->
	Result;
	
format([H|T],Result,Num) ->
	Labels_1 = ["LABEL","IDENTIFIER","Date","Sequence","Machine","Node","Class","Type","Resource"],
	Labels_2 = ["SOFTWARE PROGRAM","SIGNAL","USER'S","FILE","INODE","PROCESSOR","CORE","PROGRAM NAME","STACK","ADDITIONAL INFORMATION","REPORTABLE","INTERNAL ERROR","SYMPTOM CODE","COME FROM ADDRESS REGISTER"],
	% Labels_1 = ["LABEL","IDENTIFIER","Date","Sequence","Machine","Node","Class","Type","Resource"],
	% Labels_2 = ["USER'S"],
	F = fun(Element) ->
			case string:str(H,Element) of
				0 ->
					false;
				_ ->
					true
			end
		end,
	Label_1 = lists:filter(F,Labels_1),
	case length(Label_1) of
		0 ->
			Label_2 = lists:filter(F,Labels_2),
			case length(Label_2) of
				0 ->
					format(T,Result,Num);
				_ ->
					[Head|Tail] = T,
					format(Tail,Result++H++":"++Head++"<br/>",Num+1)
			end;
		_ ->
			format(T,Result++H++"<br/>",Num+1)
	end;

format([],Result,_Num) ->
	Result.
	
list_to_string([H|T],String) ->
	list_to_string(T,H++"<br/>"++String);
	
list_to_string([],String) ->
	String.
	
writeLog(Log) ->
	case file:open("gponInfo.txt",[append]) of
		{ok,File} -> io:format(File,"~n~p~n~n",[Log]),
		file:close(File);
		_ -> ok
end.	
	
	



      
    


