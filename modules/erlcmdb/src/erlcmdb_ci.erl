%% @doc erl cmdb ci module
%% @version{0.1}
%% @copyright 2009 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>
%%

-module(erlcmdb_ci).
-include("erlcmdb.hrl").
-include("../../core/include/dbcs_common.hrl").
-compile(export_all).

base_property(id)->
	true;
base_property(_)->
	false.


%% 
%%
ci_to_db(Type,Device)->
	{value,{id,Id}} = lists:keysearch(id,1,Device),
	Advance = [dbcs_base:term2db(K,V)||{K,V}<- Device,base_property(K)=:=false],
	case [is_atom(Id)] of
		[true]->
			{content,list_to_atom(Type), Id, <<"ci">>,null,null,null,null,?Author,null,null,null,null,null,Advance};
		_->
			{}
	end.

db_to_ci(DeviceData)->
	case DeviceData of
		{_,App,Id,_,_,_,_,_,_,_,_,_,_,_,Adv}->
			[{id,Id},{app_,App}] ++ [dbcs_base:db2term(K,T,V)||{K,T,V}<-Adv];
		_->
			[]
	end.
	
%% @spec find_by_id(Id)->(Result|{error,Reason})
%% where
%%	Id = atom()
%%	Reason = atom()
%%	Result = [{atom(),term()}]
%% @doc find a configure item by id
find_by_id(Id) when is_atom(Id)->
	Qc = #query_condition{type="Server",id=Id},
	find(Qc);
find_by_id(_)->{error,parameter_error}.


find_by_id(Type,Id) when is_atom(Id)->
	Qc = #query_condition{type=Type,id=Id},
	find(Qc);
find_by_id(_,_)->{error,parameter_error}.


find(Type,Where)when is_list(Type) andalso is_list(Where)->
	Qc = #query_condition{type=Type,conditions=Where},
	find(Qc);
find(_,_)->{error,parameter_error}.


find(Type,Where,Order)when is_list(Type) andalso is_list(Where) andalso is_list(Order)->
	Qc = #query_condition{type=Type,conditions=Where,orderby=Order},
	find(Qc);
find(_,_,_)->{error,parameter_error}.

find(Type,Where,From,Count)when is_list(Type) andalso is_list(Where) andalso is_integer(From) 
									andalso is_integer(Count) ->
	Qc = #query_condition{type=Type,conditions=Where,firstResult=From,maxResult=Count},
	find(Qc);
find(_,_,_,_)->{error,parameter_error}.

find(Type,Where,From,Count,Order)when is_list(Type) andalso is_list(Where) andalso is_integer(From) 
									andalso is_integer(Count) andalso is_list(Order)->
	Qc = #query_condition{type=Type,conditions=Where,firstResult=From,maxResult=Count,orderby=Order},
	find(Qc);
find(_,_,_,_,_)->{error,parameter_error}.

%% @spec find(Where)->(Result|{error,Reason})
%% where
%%	Where = (list() | #query_condition{})
%%	Result = [{atom(),term()}]
%%	Reason = atom()
%% @doc find a configure item by query conditions
find(Where)when is_list(Where)->
%	io:format("find: ~p ~n", [Where]),
	Qc = #query_condition{type="Server",conditions=Where},
	find(Qc);
find(Qc=#query_condition{})->
%	io:format("find1: ~p ~n", [Qc]),
	Fr=
	case Qc#query_condition.firstResult of
		undefined->
			case Qc#query_condition.orderby of
				undefined->
					[];
				""->
					[];
				Order->
					"order=my." ++ Order
			end;
		First->
			"from="++integer_to_list(First) ++
			case Qc#query_condition.maxResult of
				undefined->
					"";
				Max->
					"& to=" ++ integer_to_list(First+Max)
			end ++
			case Qc#query_condition.orderby of
				undefined->
					"";
				""->
					[];
				Order->
					"&order=my."++Order
			end
	end,
%	io:format("find2: ~p ~n", [Fr]),
	{DbCon,ApCon} = filter_db_conditions(Qc#query_condition.conditions),
%	io:format("find3 DbCon: ~p ~n", [DbCon]),
%	io:format("find3 ApCon: ~p ~n", [ApCon]),
	CL = 
	case Qc#query_condition.id of
		undefined->
			[];
		Id->
			["id=" ++ atom_to_list(Id)]
	end ++
	lists:map(fun(X)->"my." ++ X end,DbCon),
	Qstr = string:join(CL," & "),
%	io:format("find5 Qstr: ~p ~n", [Qstr]),
	% case dbcs_device:get_device_match(Qstr) of
	% io:format("Qstr:~p,Order:~p~n",[Qstr,Fr]),
	case db_ecc:get_data2(?DBName,Qc#query_condition.type,Qstr,Fr) of
		{error,Err}->
%			io:format("find6 Err: ~p ~n", [Err]),
			{error,Err};
		Ret->
%% 			io:format("find6 Ret: ~p ~n", [Ret]),
			filter_result([db_to_ci(X)||X<-Ret],ApCon)
	end;
%% 	io:format("find6 Qstr1: ~p ~n", [Qstr]);
find(_)->
	{error,parameter_error}.


filter_db_conditions(Conditions)->
	F = fun(X)->
		[H|_]=string:tokens(X,"="),
		case string:rstr(H,".") of
			0->
				true;
			_->
				false
		end
	end,
	lists:partition(F,Conditions).
filter_result(Ret,[])->
% 	io:format("filter_result: ~p ~n", ["hahah"]),
	Ret;
filter_result([],_)->[];
filter_result([Ret|T],Cons)->
	filter_result_ci(Ret,Cons) ++ filter_result(T,Cons).

filter_result_ci(_,[])->[];
filter_result_ci(Ci,[Con|T])->
%	io:format("filter_result_ci CI: ~p ~n", [Ci]),
%	io:format("filter_result_ci Con: ~p ~n", [Con]),
	[P,O,V] = parse_exp(Con),
%	io:format("filter_result_ci P: ~p ~n", [P]),
%	io:format("filter_result_ci O: ~p ~n", [O]),
%	io:format("filter_result_ci V: ~p ~n", [V]),
	case check_ci_value(string:tokens(P,"."),O,V,Ci) of
		true ->
			[Ci];
		_->
			filter_result_ci(Ci,T)
	end.
	
check_ci_value([],_,_,_)->false;
check_ci_value([P|T],O,V,Ci)->
	L = lists:filter(fun(X)->element(1,X)== list_to_atom(P) end, Ci),
	case L  of
		[]->
			false;
		_->
			case T of
				[]->
					lists:any(fun(X)->check_value(element(2,X),O,V) end,L);
				_->
					lists:any(fun(X)->check_ci_value(T,O,V,element(2,X)) end,L)
			end
	end.

	
check_value(Val,"=",V)->
	Val==V;
check_value(Val,">",V)->
	Val > V;
check_value(Val,"<",V)->
	Val < V;
check_value(Val,"=<",V)->
	Val =< V;
check_value(Val,">=",V)->
	Val >= V;
check_value(Val,"!=",V)->
	Val =/= V;
check_value(Val,"like",V)->
	case regexp:match(Val,V) of
		{match,_,_}->
			true;
		_->
			false
	end;
check_value(_,_,_)->false.


parse_exp(Str)->
	case string:rstr(Str," like ") of
		0->
			case string:rstr(Str,">=") of
				0->
					case string:rstr(Str,"=<") of
						0->
							case string:rstr(Str,"!=") of
								0->
									case string:rstr(Str,">") of
										0->
											case string:rstr(Str,"<") of
												0->
													case string:rstr(Str,"=") of
														0->
															["","",""];
														Pos->
															[string:left(Str,Pos-1),"=",string:right(Str,length(Str)-Pos)]
													end;
												Pos->
													[string:left(Str,Pos-1),"<",string:right(Str,length(Str) - Pos)]
											end;
										Pos->
											[string:left(Str,Pos-1),">",string:right(Str,length(Str)-Pos)]
									end;
								Pos->
									[string:left(Str,Pos-1),"!=",string:right(Str,length(Str)-Pos-1)]
							end;
						Pos->
							[string:left(Str,Pos-1),"=<",string:right(Str,length(Str)-Pos-1)]
					end;
				Pos->
					[string:left(Str,Pos-1),">=",string:right(Str,length(Str)-Pos-1)]
			end;
		Pos->
			[string:left(Str,Pos-1),"like",string:right(Str,length(Str)-Pos-5)]
	end.
	
update(Type,Id,Ci)->
	% NewCi = proplists:delete(id,Ci) ++ [{id,Id},{time,erlang:localtime()}],
	[Old|_] = find_by_id(Type,Id),
	NewCi = lists:ukeymerge(1,lists:ukeysort(1,proplists:delete(id,Ci)),lists:ukeysort(1,proplists:delete(id,Old)))
			++ [{id,Id},{time,erlang:localtime()}],
	
	case db_ecc:update_data(?DBName,Type,"id=" ++ atom_to_list(Id),ci_to_db(Type,NewCi)) of
		{ok,Ret}->
			{ok,db_to_ci(Ret)};
		Else->
			Else
	end.
	
	
delete(Type,Ci) when is_atom(Ci)->
	db_ecc:delete_data(?DBName,Type,"id="++atom_to_list(Ci));
delete(_,_)->{error,parameter_error}.
	
create(Type,Ci)->
	erlcmdb:start(),
	case erlcmdb_template:get_template(Type) of
		undefined->
			{error,unknow_ci_type};
		_->
			Nc = proplists:delete(type,Ci),
			case proplists:get_value(id,Nc) of
				undefined->
					case db_ecc:insert_data(?DBName,Type,ci_to_db(Type,[{id,dbcs_base:uuid()}] ++ Nc)) of
						{ok,Ret}->
							{ok,db_to_ci(Ret)};
						Else->
							Else
					end;
				_->
					case db_ecc:insert_data(?DBName,Type,ci_to_db(Type,Nc)) of
						{ok,Ret}->
							{ok,db_to_ci(Ret)};
						Else->
							Else
					end
			end
	end.
	
get_ci_by_type(Type) when is_list(Type)->
	Ret = db_ecc:get_data(?DBName,Type,""),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_ci};
				_->
					[Device|_] = Ret,
					db_to_ci(Device)
			end
	end;
get_ci_by_type(_)->{error,parameter_error}.
	
%%
%%  find1 ref function  
%% 
find1(Where)when is_list(Where)->
%	io:format("find000: ~p ~n", [Where]),
	Qc = #query_condition{type="Server",conditions=Where},
	find1(Qc);
find1(Qc=#query_condition{})->
%	io:format("find11: ~p ~n", [Qc]),
	Fr=
	case Qc#query_condition.firstResult of
		undefined->
			case Qc#query_condition.orderby of
				undefined->
					[];
				""->
					[];
				Order->
					"order=my." ++ Order
			end;
		First->
			"from="++integer_to_list(First) ++
			case Qc#query_condition.maxResult of
				undefined->
					"";
				Max->
					"& to=" ++ integer_to_list(First+Max)
			end ++
			case Qc#query_condition.orderby of
				undefined->
					"";
				""->
					[];
				Order->
					"&order=my."++Order
			end
	end,
	io:format("find12: ~p ~n", [Fr]),
	{DbCon,ApCon} = filter_db_conditions(Qc#query_condition.conditions),
	io:format("find13 DbCon: ~p ~n", [DbCon]),
	io:format("find13 ApCon: ~p ~n", [ApCon]),
	CL = 
	case Qc#query_condition.id of
		undefined->
			[];
		Id->
			["id=" ++ atom_to_list(Id)]
	end ++
	lists:map(fun(X)->"my." ++ X end,DbCon),
	Qstr = string:join(CL," & "),
	io:format("find15 Qstr: ~p ~n", [Qstr]),
	% case dbcs_device:get_device_match(Qstr) of
	% io:format("Qstr:~p,Order:~p~n",[Qstr,Fr]),
	case db_ecc:get_data2(?DBName,Qc#query_condition.type,Qstr,Fr) of
		{error,Err}->
%% 			io:format("find6 Err: ~p ~n", [Err]),
			{error,Err};
		Ret->
			ListResult=[db_to_ci(X)||X<-Ret],
%%  			io:format("find6 Ret: ~p ~n", ["find6"]),
			
		case ApCon of
			[]->
				filter_result1(ListResult,ApCon);
			_ ->
				
			   case string:str(hd(ApCon), "port") of
				  0 ->
					 filter_result1(ListResult,ApCon);
				  _ ->
				     Listports=buildListbyPort(ListResult,[]),
					 filter_result1(Listports,ApCon)
			   end
			end
		
	end;
%% 	io:format("find6 Qstr1: ~p ~n", [Qstr]);
find1(_)->
	{error,parameter_error}.

buildsubList(_,[])->
	[];
buildsubList(Main,[H|E])->
	[Main++[H]]++buildsubList(Main,E).
buildListbyPort([],Ret)->
	Ret;
buildListbyPort([H|E],Ret)->
  Ports=proplists:get_value(ports, H, []),
  Main=lists:dropwhile(fun(X)->element(1,X)=:=ports end, H),
%%   io:format(" Main:~p ~n", [Main]),
  Temp=buildsubList(Main,Ports),
%%   io:format(" Temp:~p ~n", [Temp]),
  buildListbyPort(E,Ret++Temp).

filter_result1(Ret,[])->
 	io:format("filter_result: ~p ~n", ["hahah1"]),
	Ret;
filter_result1([],_)->[];
filter_result1([Ret|T],Cons)->
	filter_result_ci1(Ret,Cons) ++ filter_result1(T,Cons).

filter_result_ci1(_,[])->[];
filter_result_ci1(Ci,[Con|T])->
%% 	io:format("filter_result_ci CI: ~p ~n", [Ci]),
%% 	io:format("filter_result_ci Con: ~p ~n", [Con]),
	[P,O,V] = parse_exp(Con),
%% 	io:format("filter_result_ci P: ~p ~n", [P]),
%% 	io:format("filter_result_ci O: ~p ~n", [O]),
	V1=util:delSubstr(V," "),
%% 	io:format("filter_result_ci V: ~p ~n", [V1]),
	case check_ci_value1(string:tokens(P,". "),O,V1,Ci) of
		true ->
			[Ci];
		_->
			filter_result_ci1(Ci,T)
	end.
	
%% get_L([])->[];
%% get_L([H|E])->
%% 	element(2,H)++get_L(E).
check_ci_value1([],_,_,_)->false;
check_ci_value1([P|T],O,V,Ci)->
	L = proplists:get_value(list_to_atom(P), Ci,[]),
%% 	case length(Ci) of
%% 		1 ->
%% 			L = proplists:get_value(list_to_atom(P), Ci,[]);
%% 		0 ->
%% 			L=[];
%% 		_ ->
%% 			L=[]
%% 	end,				
%% 	lists:filter(fun(X)->element(1,X)== list_to_atom(P) end, Ci),
%%  	io:format("checkcivalueP: ~p ~n", [P]),
%% 	io:format("checkcivalueL: ~p ~n", [L]),
%% 	io:format("checkcivalueT: ~p ~n", [T]),
	case L  of
		[]->
			false;
		_->
			case T of
				[]->
					check_value(L,O,V);
				_->
					check_ci_value1(T,O,V,L)
			end
	end.
%% 
%% end find1 
%% 