%% 
%% @doc api function of monitor set
%% @version{1.0}
%% @copyright 2009 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>

-module(api_monitor_set).
%%-extends(api_siteview).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

-export([proxy_value/2,get_monitorset_list/0,get_monitorset/1,configure_monitorset/2,create_monitor_from_monitorset/2]).

%% @spec get_monitorset_list()-> [{File,Name,Descripition}]
%% where
%%	File = string()
%%	Name = string()
%%	Descripition = string()
%% @doc get monitor set list,return data is a tuple.
%%	<br>File is the monitor set file name </br>
get_monitorset_list()->
	monitor_set_template:get_template_list().

%% @spec get_monitorset(File)->({ok,MonitorSet} | {error,Err})
%% where
%%	File = string()
%%	MonitorSet = #monitor_set{}
%%	Err = atom()
%% @doc get detail information of a monitor set.
%% <br>File is the file name of monitor set,TemplateData is the Data of the monitor set,is a #monitor_set{} record.</br>
%%	<br>#monitor_set{} is defined in <a href="monitor.hrl">monitor.hrl</a></br>
%% <br>Typical error reasons:<br>
%% <dl>
%% <dt>enoent</dt><dd>The file does not exist.</dd>
%% <dt>eacces</dt><dd>Missing permission for reading the file or searching one of the parent directories.</dd>
%% <dt>eisdir</dt><dd>The named file is not a regular file. It may be a directory, a fifo, or a device.</dd>
%% <dt>parameter_error</dt><dd>input parameter error.</dd>
%% </dl>
get_monitorset(File) when is_list(File)->
	monitor_set_template:get_template(File);
get_monitorset(_)->{error,parameter_error}.

%% @spec configure_monitorset(File,Params)->({ok,MonitorSet} | {error,Err})
%% where
%%	File = string()
%%	Params = [{Key,Value}]
%%	Key = string()
%%	Value = string()
%%	MonitorSet = #monitor_set{}
%% @doc configure monitor set with input value
%%	<br>#monitor_set{} is defined in <a href="monitor.hrl">monitor.hrl</a></br>
configure_monitorset(File,Params)->
	monitor_set_template:configure_template(File,Params).
	
%% @spec create_monitor_from_monitorset(Parent,Data)-> ({ok,Result} | {error,Reason})
%% where
%%	Parent = atom()
%%	Data = [{atom(),term()}]
%%	Result = atom()
%%	Reason = atom()
%% @doc create monitor from monitor set's monitor data
%% <br>Parent is group id.</br>
%% Data is a member of #monitor_set.monitors,it is return by {@link configure_monitorset/2}
create_monitor_from_monitorset(Parent,Data)->
	case proplists:get_value(?CLASS,Data) of
		undefined->
			{error,data_error};
		Class ->
			M = Class:new(),
			Temp = M:get_template_property(),
            io:format("Temp: ~p~n", [Temp]),
            io:format("Data: ~p~n", [Data]),
			M:delete(),
			% Default = [{?CLASS,Class}] ++ template_default_value(Temp),
			% NewData = replace(Data,Default),
			% io:format("create_monitor_from_monitorset1:~p~n",[Data]),
            OtherTemp = other_property([error_classifier, warning_classifier, good_classifier],[]),
			NewData = [{?CLASS,Class}] ++ template_default_value(Temp, Data, []) ++ template_default_value(OtherTemp, Data, []) ++ proxy_value(Data, []),
			io:format("create_monitor_from_monitorset2:~p~n",[NewData]),
			case api_monitor:create(Parent,NewData) of
				{ok,Other}->
					{ok,Other};
				{error,{verify_error,Err}}->
					?ERROR_LOG2("create_monitor_from_monitorset:~p",[Err]),
					{error,verify_data_error};
				{error,Err}->
					{error,Err}
			end
	end.

% ----------------------------------------------------------------------------------
% get proxy value ------update by liangqing.meng
proxy_value([],Ret)->Ret;
proxy_value([{K,V}|T],Ret)->
	case K of
		'_proxy'->
			proxy_value(T,Ret++[{K,V}]);
		_->
			proxy_value(T,Ret)
end.

% ----------------------------------------------------------------------------------
% local functions
other_property([], Ret) ->
    Ret;
other_property([Name|T], Ret) when erlang:is_atom(Name) ->
    other_property(T, lists:append(Ret, [#property{name=Name}]));
other_property([Name|T], Ret) when erlang:is_record(Name, property) ->
    other_property(T, lists:append(Ret, [Name]));
other_property([Name|T], Ret) ->
    other_property(T, Ret).


	
template_default_value([],_,Ret)->Ret;
template_default_value([P=#property{}|T],Data,Ret)->
	case proplists:get_value(P#property.name,Data) of
		undefined->
			template_default_value(T,Data,Ret ++ property_default(P));
		Val->
			template_default_value(T,Data,Ret ++ property_value(P,Val))
	end;
template_default_value([_|T],Data,Ret)->
	template_default_value(T,Data,Ret).
	
property_value(P,Val)->
	case P#property.type of
		bool->
			[{P#property.name,val2bool(Val)}];
		frequency->
			[{P#property.name,val2num(Val)}];
		numeric->
			[{P#property.name,val2num(Val)}];
		_->
			[{P#property.name,Val}]
	end.
	
val2num(Val) when is_number(Val)->Val;
val2num(Val) when is_list(Val)->
	case string:to_integer(Val) of
		{error,_}->
			0;
		{N,_}->
			N;
		_->
			0
	end;
val2num(Val) when is_atom(Val)->
	case string:to_integer(atom_to_list(Val)) of
		{error,_}->
			0;
		{N,_}->
			N;
		_->
			0
	end.
	
val2bool(Val) when is_list(Val)->
	list_to_atom(Val);
val2bool(Val)->Val.

	
replace([],Targ)->Targ;
replace([P|T],Targ)->
	NewTarg = lists:keyreplace(element(1,P),1,Targ,P),
	replace(T,NewTarg).
	
template_default_value([])->[];
template_default_value([P|T])->
	property_default(P) ++ template_default_value(T).

property_default(P=#property{})->
	case P#property.default of
		""->
			case P#property.type of
				text->
					[{P#property.name,""}];
				bool->
					[{P#property.name,false}];
				frequency->
					[{P#property.name,0}];
				numeric->
					[{P#property.name,0}];
				server->
					[{P#property.name,""}];
				scalar->
					[{P#property.name,[]}];
				counter->
					[{P#property.name,[]}];
				browsable->
					[{P#property.name,[]}];
				schedule->
					[{P#property.name,"all"}];
				_->
					[{P#property.name,""}]
			end;
		_->
			[{P#property.name,P#property.default}]
	end.