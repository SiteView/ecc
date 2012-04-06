%% 
%% @doc monitor set template
%% @version{1.0}
%% @author Shi xianfang<xianfang.shi@dragonflow.com>
%%
-module(monitor_set_template).
-compile(export_all).

-include("monitor.hrl").

-define(MONITOR_SETS_DIR,"templates.sets").
-define(MONITOR_SETS_EXT,".mset").

%% @spec get_template_list()-> [Template]
%% where
%%	Template = {File,Title,Desc}
%%	File = string()
%%	Title = string()
%%	Desc = string()
%% @doc get all monitor set's brief information,return a list of monitor set,File is the filename of monitor set,Title is the title of the monitor set,Desc is descripition
get_template_list()->
	Files = [filename:basename(X)||X<-filelib:wildcard(?MONITOR_SETS_DIR ++ "/*" ++ ?MONITOR_SETS_EXT)],
	get_template_list(Files).


get_template_list([])-> [];
get_template_list([F|T])->
	case file:consult(?MONITOR_SETS_DIR ++ "/" ++ F) of
		{ok,[Ms|_]}->
			[{F,Ms#monitor_set.title,Ms#monitor_set.desc}];
		_->
			[]
	end
	 ++ get_template_list(T).

%% @spec get_template(File)-> ({ok,TemplateData} | {error,Err})
%% where
%%	File = string()
%%	TemplateData = #monitor_set{}
%%	Err = atom()
%% @doc get detail information of a monitor set.
%% File is the file name of monitor set,TemplateData is the Data of the monitor set,is a monitor_set record
get_template(File)->
	case file:consult(?MONITOR_SETS_DIR ++ "/" ++ File) of
		{ok,[Ms|_]}->
			{ok,Ms};
		{error,Err}->
			{error,Err};
		Else->
			{error,Else}
	end.

%% @spec configure_tempalte(File,Params)->({ok,MonitorSet} | {error,Err})
%% where
%%	File = string()
%%	Params = [{Key,Value}]
%%	Key = string()
%%	Value = string()
%%	MonitorSet = #monitor_set{}
%% @doc configure monitor set with input value
configure_template(File,Params)->
	case get_template(File) of
		{ok,Ms}->
			{ok,Ms#monitor_set{monitors=configure_monitors(Params,Ms#monitor_set.monitors)}};
		{error,Err}->
			{error,Err};
		Else->
			{error,Else}
	end.
	
configure_monitors(_,[])->[];
configure_monitors(Params,[M|T])->
	[configure_monitor(Params,M)] ++ configure_monitors(Params,T).

configure_monitor(_,[])->[];
configure_monitor(Params,[P|T])->
	configure_property(Params,P) ++ configure_monitor(Params,T).
	

configure_property([],P)-> [P];
configure_property([{AK,AV}|AT],{PK,PV}) when is_list(PV)->
	case regexp:match(PV,AK) of
		{match,_,_}->
			configure_property(AT,{PK,replace(PV,AK,AV)});
		_->
			configure_property(AT,{PK,PV})
	end;
configure_property([{AK,AV}|AT],{PK,PV}) when is_atom(PV)->
	Tmp = atom_to_list(PV),
	case regexp:match(Tmp,AK) of
		{match,_,_}->
			configure_property(AT,{PK,list_to_atom(replace(Tmp,AK,AV))});
		_->
			configure_property(AT,{PK,PV})
	end;
configure_property([_|AT],P)->
	configure_property(AT,P).
	
replace([],_,_)-> [];
replace(Src,Tag,V)->
	case lists:prefix(Tag,Src) of
		true ->
			V ++ lists:nthtail(length(Tag),Src);
		_->
			[H|T] = Src,
			[H] ++ replace(T,Tag,V)
	end.
	