%% 
%% @doc monitor solution template
%% @version{1.0}
%% @author Shi xianfang<xianfang.shi@dragonflow.com>
%%
-module(monitor_solution_template).
-compile(export_all).

-include("monitor.hrl").

-define(MONITOR_SETS_DIR,"templates.solutions").
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
%%	Err = string()
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
	% io:format("Params:~p~nMonitor:~p~n",[Params,M]),
	case proplists:get_value('FOREACH',M) of
		undefined->
			[configure_monitor(Params,M)] ++ configure_monitors(Params,T);
		ForApp->
			case proplists:get_value("selected_apps",Params) of
				""->
					configure_monitors(Params,T);
				undefined->
					configure_monitors(Params,T);
				Appstr->
					Apps = string:tokens(Appstr,"^"),
					MatchApps = find_app(ForApp,Apps,[]),
					AppId = "@" ++ ForApp ++ "{value}",
					AppName = "@" ++ ForApp ++ "{name}",
					F = fun({Id,Name})->
						configure_monitor(Params ++ [{AppId,Id},{AppName,Name}],M)
					end,
					lists:map(F,MatchApps) ++ configure_monitors(Params,T)
			end
	end.
	
%% find match type of appliations
find_app(_,[],R)->R;					
find_app(ForApp,[App|T],R)->
	[Id,Name|_] = string:tokens(App,"`"),
	case match_app(ForApp,Name) of
		true->
			find_app(ForApp,T, R ++ [{Id,Name}]);
		_->
			find_app(ForApp,T, R)
	end.

% match app's name
match_app(ForApp,Name)->
	PName = lists:last(string:tokens(Name,"/")),
	lists:prefix(ForApp,PName).
	
configure_monitor(_,[])->[];
configure_monitor(Params,[P|T])->
	configure_property(Params,P) ++ configure_monitor(Params,T).
	

%% use parameters to replace properties
configure_property([],P)-> [P];
configure_property([{AK,AV}|AT],{PK,[{_,_}|T]=PV}) when is_atom(AV)->
	F = fun({K,V})->
		{replace(K,AK,AV),replace(V,AK,atom_to_list(AV))}
	end,
	
	configure_property(AT,{PK,lists:map(F,PV)});
configure_property([{AK,AV}|AT],{PK,[{_,_}|T]=PV})->
	F = fun({K,V})->
		{replace(K,AK,AV),replace(V,AK,AV)}
	end,
	
	configure_property(AT,{PK,lists:map(F,PV)});
configure_property([{AK,AV}|AT],{PK,PV}) when is_list(PV) andalso is_atom(AV)->
	Pos = string:rstr(PV,AK),
	if
		Pos > 0->
			configure_property(AT,{PK,replace(PV,AK,atom_to_list(AV))});
		true ->
			configure_property(AT,{PK,PV})
	end;		
configure_property([{AK,AV}|AT],{PK,PV}) when is_list(PV)->
	Pos = string:rstr(PV,AK),
	if
		Pos > 0->
			configure_property(AT,{PK,replace(PV,AK,AV)});
		true ->
			configure_property(AT,{PK,PV})
	end;
configure_property([{AK,AV}|AT],{PK,PV}) when is_atom(PV) andalso is_atom(AV)->
	Tmp = atom_to_list(PV),
	Pos = string:rstr(Tmp,AK),
	if
		Pos > 0 ->
			configure_property(AT,{PK,list_to_atom(replace(Tmp,AK,atom_to_list(AV)))});
		true ->
			configure_property(AT,{PK,PV})
	end;
configure_property([{AK,AV}|AT],{PK,PV}) when is_atom(PV)->
	Tmp = atom_to_list(PV),
	Pos = string:rstr(Tmp,AK),
	if
		Pos > 0 ->
			configure_property(AT,{PK,list_to_atom(replace(Tmp,AK,AV))});
		true ->
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
	
get_applications(Type,Params)->

	% io:format("get_applications:~p~n",[Params]),
	case Type of
		"weblogic"->
			Server = proplists:get_value('$WebLogic Server:0$',Params),
			Usr = proplists:get_value('$WebLogic Username:2$',Params),
			Pwd = proplists:get_value('$WebLogic Password:3$',Params),
			Secure = case proplists:get_value('$Secure Server:5$',Params) of
						true->
							"on";
						_->
							"off"
					end,
			WeblogicJar = proplists:get_value('$WebLogic Jar File:4$',Params),
			WlCipherJar = proplists:get_value('$WebLogic Cipher File:6$',Params),
			Jvm = proplists:get_value('$Java VM:8$',Params),
			ClassPath = proplists:get_value('$Class Path:9$',Params),
			License = proplists:get_value('$WebLogic License File:7$',Params),
			Port = proplists:get_value('$WebLogic Port:1$',Params),
			M = weblogic_monitor:new(),
			Ret = M:getBrowseData([{server,Server},{usr,Usr},{pwd,Pwd},{secure,Secure},
									{weblogicJar,WeblogicJar},{port,Port},{wlCipherJar,WlCipherJar},
									{jvm,Jvm},{classpath,ClassPath},{license,License}]),
			M:delete(),
			filter_apps(Ret,[]);
		"websphere"->
			[]
	end.
	
filter_apps({error,Err},_)->{error,Err};	
filter_apps([],Apps)->Apps;
filter_apps([C|T],Apps)->
	Ts = string:tokens(element(2,C),"/"),
	if
		length(Ts)>2->
			filter_apps(T,Apps);
		length(Ts) == 1->
			Name = lists:nth(1,Ts),
			case string:rstr(Name,"ServerRuntime") of
				0->
					filter_apps(T,Apps);
				_->
					filter_apps(T,[C] ++ Apps)
			end;
		true->

			case [string:rstr(lists:nth(1,Ts),"ServerRuntime"),string:rstr(lists:last(Ts),"Runtime")] of
				[P1,P2] when P1>0 andalso P2 > 0->
					filter_apps(T,[C] ++ Apps);
				_->
					
					filter_apps(T,Apps)
			end
	end.