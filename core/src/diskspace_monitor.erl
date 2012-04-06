%%
%% Diskspace Monitor
%%

%% @doc diskspace monitor
%% @version{0.1}
%% @copyright 2009 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>
-module(diskspace_monitor,[BASE]).
-extends(server_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

-export([new/0,defaultTitle/1,update/0,getScalarValues/2,get_classifier/1,get_template_property/0,verify/1]).


%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for diskspace monitor
new()->
	Base = server_monitor:new(),
	%%Base:set_attribute(disk,""),
	%%Base:set_attribute(filesystem,0),
	Base:set_attribute(percentFull,0),
	Base:set_attribute(freeSpace,0),
	Base:set_attribute(totalSpace,0),
	{?MODULE,Base}.
	
diskProperty()->
	disk.

%% @spec defaultTitle(Params) -> List
%% Params = [term()]
%% List = string()
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	BASE:defaultTitle(Params) ++ "(" ++ case proplists:get_value(disk,Params) of undefined->"";V->V end ++ ")".



%% @spec update() -> Result
%% Result = term()
%% @doc update is the run function called by schedule to test the  diskspace monitor
update()->
	Prop = diskProperty(),
	{ok,{_,Disk}} = THIS:get_property(Prop),
	{ok,{machine,Machine}} = THIS:get_property(machine),
	%io:format("diskspace_monitor:~p,~p,~p~n",[Prop,Disk,Machine]),

	case platform:getDiskFull(Machine,Disk) of
	    [0,0,0] ->
	                THIS:set_attribute(percentFull,"n/a"),
			THIS:set_attribute(freeSpace,"n/a"),
			THIS:set_attribute(totalSpace,"n/a"),
			THIS:set_attribute(?STATE_STRING,"no data"),
			{error,get_data};
            [Full,Used,Total] ->
    		%io:format("Host:~p~nDisk Full:~p~nDisk Used:~p~nDisk Total:~p~n",[Machine,Full,Used,Total]),
				case Total of
					-1 ->
						THIS:set_attribute(percentFull,"n/a"),
						THIS:set_attribute(freeSpace,"n/a"),
						THIS:set_attribute(totalSpace,"n/a"),
						THIS:set_attribute(?STATE_STRING,"no data");
					_->

					THIS:set_attribute(percentFull,round(Full)),
					THIS:set_attribute(freeSpace,round((Total-Used)/(1024*1024))),
					THIS:set_attribute(totalSpace,round(Total/(1024*1024))),
					%io:format("diskspace_monitor:~p state:~p% full,~pMB free,~pMB total~n",[THIS:get_property(id),Full,round((Total-Used)/1000000),round(Total/1000000)]),
					THIS:set_attribute(?STATE_STRING,
						lists:flatten(io_lib:format("~p% full<br>~pMB free<br>~pMB total",[round(Full),round((Total-Used)/(1024*1024)),round(Total/(1024*1024))])))
				end;
            _ -> 
			THIS:set_attribute(percentFull,"n/a"),
			THIS:set_attribute(freeSpace,"n/a"),
			THIS:set_attribute(totalSpace,"n/a"),
			THIS:set_attribute(?STATE_STRING,"no data"),
			{error,get_data}
       end.   
	

%% @spec getScalarValues(Prop,Params) -> Result
%% Result = [term()]
%% @doc getScalarValues is the run function called by schedule to get drop-downlist box
getScalarValues(Prop,Params)->
	case THIS:diskProperty() of
		Prop->
			Machine = case lists:keysearch(machine,1,Params) of
							{value,{machine,Val}}->
								Val;
							_->
								""
						end,
			%%io:format("~p:getScalarValues,~p",[?MODULE,Machine]),
			io:format("Machine Machine ~p~n",[Machine]),
			lists:map(fun(X)->[Y|_]=string:tokens(X," "),{X,Y} end,platform:getDisks(Machine));
		_->
			BASE:getScalarValues(Prop,Params)
	end.


%% @spec verify(Params) -> {ok, []} | {error, Reason}
%%  Params = [term()]
%%  Reason = string()
%% @doc verify is the function called by schedule to verify the parameter of monitor inputed by user
verify(Params) ->
    Errs = 
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

%% @spec get_classifier(Param) -> List
%% Param = atom()
%% List = [Tuple]
%% Tuple = {Status, Logic, Value}
%% Status = 'error'|'warning'| 'good' 
%% Logic = '!=' | '==' | '>' | '<' | 'contain'
%% Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{percentFull,'>',98}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{percentFull,'>=',90}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{percentFull,'>=',0}]
	end.

%% @spec get_template_property() -> list()
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=percentFull,title="percentFull(%)",type=numeric,configurable=false,state=true,baselinable=true},
	#property{name=freeSpace,title="MB free(MB)",type=numeric,configurable=false,state=true,upIsBad=false,baselinable=true},
	#property{name=disk,title="Disk",type=scalar,editable=true,order=2}
	].


