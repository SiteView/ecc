%% 
%% @doc api of global search
%% @version{1.0}
%% @copyright 2010 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>
%%
-module(api_search).
-compile(export_all).

-include("monitor.hrl").

-export([search/1,search/2,get_allreport/0,getreport/1]).

%% @spec search(Tag,Type)-> ([Results] | {error,parameter_error})
%% where
%%	Type = string()
%%	Results = [Item]
%%	Item = [{Key,Value}]
%%	Key = atom()
%%	Value = term()
%% @doc global search,return all items match of Tag and Type
%%  <br>Type:</br>
%%	<dl>
%%		<dt>"machine"</dt><dd>search machines.</dd>
%%		<dt>"monitor"</dt><dd>search monitors</dd>
%%		<dt>"alert"</dt><dd>search alerts</dd>
%%		<dt>"report"</dt><dd>search reports</dd>
%%	</dl>
search(Tag,Type) when is_list(Tag) andalso is_list(Type)->
	case Type of
		"machine"->
			lists:nth(1,search(Tag));
		"monitor"->
			lists:nth(2,search(Tag));
		"alert"->
			lists:nth(3,search(Tag));
		"report"->
			lists:nth(4,search(Tag));
		_->
			{error,parameter_error}
	end;
search(_,_)->{error,parameter_error}.

%% @spec search(Tag)-> [Results]
%% where
%%	Results = [Item]
%%	Item = [{Key,Value}]
%%	Key = atom()
%%	Value = term()
%% @doc global search,return all items match of Tag
search("")->
	[[],[],[],[]];
search(Tag)->
	case index_store:find(Tag) of
		{ok,Is}->
			Ret = 
			lists:foldl(fun(X,[Machs,Monitors,Alerts,Reports,Ids])->
				case X of
					{_,Id,machine,_}->
						case lists:member(Id,Ids) of
							true->
								[Machs ,Monitors,Alerts,Reports,Ids];
							_->
								Mach = dbcs_machine:get_machineById(atom_to_list(Id)),
								if
									Mach#machine.id =/= undefined->
										[Machs ++ [[{id,Mach#machine.id},{name,Mach#machine.name},{os,Mach#machine.os},{host,Mach#machine.host}]],
										Monitors,Alerts,Reports,Ids++[Id]];
									true->
										[Machs ,Monitors,Alerts,Reports,Ids]
								end
						end;
					{_,Id,monitor,_}->
						case lists:member(Id,Ids) of
							true->
								[Machs ,Monitors,Alerts,Reports,Ids];
							_->
								case siteview:get_object(Id) of
									[]->
										[Machs ,Monitors,Alerts,Reports,Ids];
									[M]->
										Host = M:getHostname(),
										Name = M:get_name(),
										{ok,{_,Id}} = M:get_property(id),
										{ok,{_,Class}} = M:get_property(?CLASS),
										{ok,{_,Category}} = M:get_attribute(?CATEGORY),
										Md = [{id,Id},{name,Name},{host,Host},{class,Class},{?CATEGORY,Category}],
										[Machs ,Monitors++[Md],Alerts,Reports,Ids++[Id]]
								end
						end;
					{_,Id,rule,_}->
						case lists:member(Id,Ids) of
							true->
								[Machs ,Monitors,Alerts,Reports,Ids];
							_->
								case dbcs_rule:get_rule(Id) of
									{error,_}->
										[Machs ,Monitors,Alerts,Reports,Ids];
									Rule->
										Id = proplists:get_value(id,Rule),
										Name = proplists:get_value(name,Rule,""),
										Action = proplists:get_value(action,Rule),
										Cate = proplists:get_value(category,Rule),
										Action = proplists:get_value(action,Rule),
										Target = proplists:get_value(target,Rule,""),
										Tgs = string:tokens(Target,"<>,"),
										Rd = [{id,Id},{name,Name},{target,[list_to_atom(Y)||Y<-Tgs]},{action,Action},{category,Cate}],
										[Machs ,Monitors,Alerts++[Rd],Reports,Ids++[Id]]
								end
						end;
					{_,Id,report,_}->
						case lists:member(Id,Ids) of
							true->
								[Machs ,Monitors,Alerts,Reports,Ids];
							_->
								case getreport(Id) of
									{error,_}->
										[Machs ,Monitors,Alerts,Reports,Ids];
									[]->
										[Machs ,Monitors,Alerts,Reports,Ids];
									Report->
										T1=proplists:get_value("Title", Report, ""),
                                        T2=string:tokens(T1, "|"),
                                        Title=lists:nth(1, T2),
%% 										Title = proplists:get_value("Title",Report,""),
										Desc = proplists:get_value("Descript",Report,""),
										Id = proplists:get_value(id,Report),
										Sate1=proplists:get_value("Deny",Report,"No"),
										Cate =case Sate1 of
												  "Yes"->disable;
												  "No"->good
											  end,
										TeType=proplists:get_value("Type",Report,""),
										Type=case TeType of
												 ""->"Statistics Report";
												 _ ->"TopN Report"
											 end,
												 
										[Machs ,Monitors,Alerts,Reports++[[{id,Id},{name,Title},{class,Type},{description,Desc},{category,Cate}]],Ids++[Id]]
								end
						end;
					_->
						[Machs ,Monitors,Alerts,Reports,Ids]
				end
			end,[[],[],[],[],[]], Is),
			lists:sublist(Ret,1,4);
		_->
			[[],[],[],[]]
	end.				


search2("")->
	[[],[],[],[]];
search2(Tag)->
	Machs = search_machine(Tag),
	Monitors = search_monitor(Tag,Machs),
	Alerts = search_alert(Tag,Monitors),
	Reports = search_report(Tag,Monitors),
	[Machs,Monitors,Alerts,Reports].
	
search_machine(Tag)->
	All = dbcs_machine:get_all(),
	F = fun(X,R)->
		case re:run(X#machine.host,Tag,[]) of
			{match,_}->
				R ++ [[{id,X#machine.id},{name,X#machine.name},{os,X#machine.os},{host,X#machine.host}]];
			_->
				case re:run(X#machine.name,Tag,[]) of
					{match,_}->
						R ++ [[{id,X#machine.id},{name,X#machine.name},{os,X#machine.os},{host,X#machine.host}]];
					_->
						R
				end
		end
	end,
	lists:foldl(F,[],All).
	
search_monitor(Tag,Machs)->
	Hosts = lists:map(fun(X)->proplists:get_value(host,X,"") end,Machs),
	io:format("hosts:~p~n",[Hosts]),
	All = siteview:get_object_by_type(monitor),
	F = fun(X,R)->
		Host = X:getHostname(),
		Name = X:get_name(),
		{ok,{_,Id}} = X:get_property(id),
		{ok,{_,Class}} = X:get_property(?CLASS),
		case lists:member(Host,Hosts) of
			true->
				{ok,{_,Category}} = X:get_attribute(?CATEGORY),
				R ++ [[{id,Id},{name,Name},{host,Host},{class,Class},{?CATEGORY,Category}]];
			_->
				NMatch = [nomatch == re:run(Host,Tag), nomatch == re:run(Name,Tag)],
				case lists:member(false,NMatch) of
					true->
						{ok,{_,Category}} = X:get_attribute(?CATEGORY),
						R ++ [[{id,Id},{name,Name},{host,Host},{class,Class},{?CATEGORY,Category}]];
					_->
						R
				end
		end
	end,
	lists:foldl(F,[],All).
	
search_alert(Tag,Monitors)->
	MIds = lists:map(fun(X)-> proplists:get_value(id,X) end,Monitors),
	All = dbcs_rule:get_all(),
	F = fun(X,R)->
		Name = proplists:get_value(name,X,""),
		Id = proplists:get_value(id,X),
		Target = proplists:get_value(target,X,""),
		Tgs = string:tokens(Target,"<>,"),
		Contas = lists:foldl(fun(X1,R1)->
					R1 ++ [lists:member(list_to_atom(X1),MIds)]
				end,[],Tgs),
		case lists:member(true,Contas) of
			true->
				Action = proplists:get_value(action,X),
				Cate = proplists:get_value(category,X),
				R ++ [[{id,Id},{name,Name},{target,[list_to_atom(Y)||Y<-Tgs]},{action,Action},{category,Cate}]];
			_->
				case re:run(Name,Tag) of
					{match,_}->
						Action = proplists:get_value(action,X),
						Cate = proplists:get_value(category,X),
						R ++ [[{id,Id},{name,Name},{target,[list_to_atom(Y)||Y<-Tgs]},{action,Action},{category,Cate}]];
					_->
						R
				end
		end
	end,
	lists:foldl(F,[],All).
		
search_report(Tag,Monitors)->
	MIds = lists:map(fun(X)-> proplists:get_value(id,X) end,Monitors),
	io:format("mids:~p ~n", [[MIds]]),
	All = get_allreport(),
	F = fun(X,R)->
		Title = proplists:get_value(title,X,""),
		Desc = proplists:get_value(description,X,""),
		Id = proplists:get_value(id,X),
		Target = proplists:get_value(target,X,""),
		Tgs = string:tokens(Target,","),
		Contas = lists:foldl(fun(X1,R1)->
					R1 ++ [lists:member(list_to_atom(X1),MIds)]
				end,[],Tgs),
		case lists:member(true,Contas) of
			true->
				R ++ [[{id,Id},{title,Title},{description,Desc}]];
			_->
				case re:run(Title,Tag) of
					{match,_}->
						R ++ [[{id,Id},{title,Title},{description,Desc}]];
					_->
						case re:run(Desc,Tag) of
							{match,_}->
								R ++ [[{id,Id},{title,Title},{description,Desc}]];
							_->
								R
						end
				end
		end
	end,
	lists:foldl(F,[],All).
buildreport([],R)->
	R;
buildreport([H|E],R)->
	Id=element(1, H),
	Data=element(2,H),
	T1=proplists:get_value("Title", Data, ""),
    T2=string:tokens(T1, "|"),
    Title=lists:nth(1, T2),
	Description=proplists:get_value("Descript", Data, ""),
	Target=proplists:get_value("Descript", Data, ""),
	GroupRight=proplists:get_value("GroupRight", Data, ""),
    Newdata=[[{id,Id},{title,Title},{description,Description},{target,GroupRight}]],
    buildreport(E,R++Newdata).
    
get_allreport()->
Alltj=case api_preferences:get_prefs('general','reportset.ini') of
		  {ok,[{'reportset.ini',Tj}]} ->
			 Tj;
		  _ ->
			  []
	  end,
%% {ok,[{'reportset.ini',Alltj}]}=api_preferences:get_prefs('general','reportset.ini'),
A=buildreport(Alltj,[]),
Alltopn=case api_preferences:get_prefs('general','topnreportset.ini') of
	{ok,[{'topnreportset.ini',Topn}]} ->
		Topn;
	_ ->
		[]
		end,
		
%% {ok,[{'topnreportset.ini',Alltopn}]}=api_preferences:get_prefs('general','topnreportset.ini'),
B=buildreport(Alltopn,[]),
A++B.

getreport(Id)->
 Alltj=case api_preferences:get_prefs('general','reportset.ini') of
		  {ok,[{'reportset.ini',Tj}]} ->
			 Tj;
		  _ ->
			  []
	  end,
Alltopn=case api_preferences:get_prefs('general','topnreportset.ini') of
	{ok,[{'topnreportset.ini',Topn}]} ->
		Topn;
	_ ->
		[]
		end,
Data=Alltj++Alltopn,
 case Data of
	 [] ->
		 [];
	 _ ->
          Newdata=[element(2, X) ||X<-Data,element(1, X)=:=Id],
          [{id,Id}]++lists:nth(1, Newdata)
 end.
