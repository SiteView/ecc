%% ---
%%content store  database access
%%
%%---
-module(db_ecc).
-export([open_db/1,close_db/1,get_data/3,get_data/4,get_data2/4,update_data/4,insert_data/3,delete_data/3,is_open/1,domain/1,get_data_stat/4]).

-include("config.hrl"). 
% -include("log.hrl").

-define(ERROR_LOG2(X,Y),void).

domain(undefined) -> "localhost";
domain("localhost") -> "localhost";
domain(Host) when is_atom(Host) -> atom_to_list(Host);
domain(Host) ->
  case string:str(Host,".") of
      0 -> "localhost";
	  Pos ->
			case re:run(Host,"^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+$") of
				{match,_}->
					"localhost";
				_->
					lists:sublist(Host,1,Pos-1)
			end
      %{ok,lists:sublist(Host,1,Pos-1),lists:nthtail(Pos,Host)}
      %lists:sublist(Host,1,Pos-1)
  end.
 
open_db(Name) ->
	case net_adm:ping(Name) of
		pong->
			true;
		_->
			false
	end.

is_open(Name)->
	open_db(Name).


close_db(_)->
	ok.
	
print_stack()->
	try 
		throw(stack_trace)
	catch 
	   _:_->
		  erlang:get_stacktrace() 
	end.
	
get_data(DbName, Table, [])->
    AppName = domain(get(hostname)),
	?ERROR_LOG2("DBLOG--get_data:~p,where:~p~n",[Table,[]]),
%%     io:format("hostname:~p~n",[AppName]),
	%case rpc_proxy:call(DbName, content, get, [[{application,Table},{content,Where},"from=0&to=100000"]]) of
    case rpc_proxy:call(DbName, content, get, [AppName,Table,"", "from=0&to=100000"]) of
%%     case content:get(AppName,Table,"", "from=0&to=100000") of
		{ok,{_,_,_,R}}->
			R;
		Else->
			?ERROR_LOG2("DBLOG--get_data AppName:~p Table:~p, Where:~p, Else:~p~n",[AppName,Table, "", Else]),
			Else
	end;

get_data(DbName, Table, Where)->
    AppName = domain(get(hostname)),
	?ERROR_LOG2("DBLOG--get_data:~p,where:~p~n",[Table,Where]),
	%case rpc_proxy:call(DbName, content, get, [[{application,Table},{content,Where},"from=0&to=100000"]]) of
    case rpc_proxy:call(DbName, content, get, [AppName, Table, Where,"from=0&to=100000"]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			?ERROR_LOG2("DBLOG--get_data AppName:~p Table:~p, Where:~p, Else:~p~n",[AppName,Table, Where, Else]),
			Else
	end.
get_data(DbName, Table, [],Order) when is_list(Order),length(Order)>0->
    AppName = domain(get(hostname)),
	?ERROR_LOG2("DBLOG--get_data:~p,where:~p,order:~p~n",[Table,[],Order]),
%%     io:format("hostname:~p~n",[AppName]),
	case rpc_proxy:call(DbName, content, get, [AppName,Table, "", "from=0&to=100000" ++ "&" ++ Order]) of
    %case rpc_proxy:call(DbName, content, get, [[{application,Table},{content,Where},"from=0&to=100000" ++ "&" ++ Order]]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			?ERROR_LOG2("DBLOG--get_data AppName:~p Table:~p, Where:~p, Else:~p~n",[AppName,Table, "", Else]),
			Else
	end;
get_data(DbName, Table, Where,Order) when is_list(Order),length(Order)>0->
    AppName = domain(get(hostname)),
	?ERROR_LOG2("DBLOG--get_data:~p,where:~p,order:~p~n",[Table,Where,Order]),
%%     io:format("hostname:~p~n",[AppName]),
	case rpc_proxy:call(DbName, content, get, [AppName, Table, Where,"from=0&to=100000" ++ "&" ++ Order]) of
    %case rpc_proxy:call(DbName, content, get, [[{application,Table},{content,Where},"from=0&to=100000" ++ "&" ++ Order]]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			?ERROR_LOG2("DBLOG--get_data AppName:~p Table:~p, Where:~p, Else:~p~n",[AppName,Table, Where ,Else]),
			Else
	end;
get_data(DbName, Table, [],Order) when is_list(Order)->
    AppName = domain(get(hostname)),
	?ERROR_LOG2("DBLOG--get_data:~p,where:~p,order:~p~n",[Table,[],Order]),
%%     io:format("hostname:~p~n",[AppName]),
	case rpc_proxy:call(DbName, content, get, [AppName, Table, "", "from=0&to=100000"]) of
    %case rpc_proxy:call(DbName, content, get, [[{application,Table},{content,Where},"from=0&to=100000"]]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			?ERROR_LOG2("DBLOG--get_data AppName:~p Table:~p, Where:~p, Else:~p~n",[AppName,Table, "", Else]),
			Else
	end;
get_data(DbName, Table, Where,Order) when is_list(Order)->
	?ERROR_LOG2("DBLOG--get_data:~p,where:~p,order:~p~n",[Table,Where,Order]),
    AppName = domain(get(hostname)),
	case rpc_proxy:call(DbName, content, get, [AppName, Table, Where,"from=0&to=100000"]) of
    %case rpc_proxy:call(DbName, content, get, [[{application,Table},{content,Where},"from=0&to=100000"]]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			?ERROR_LOG2("DBLOG--get_data AppName:~p Table:~p, Where:~p, Else:~p~n",[AppName,Table, Where, Else]),
			Else
	end;
get_data(_, _, _,_)->{error,parameter_error}.


get_data2(DbName, Table, [],Order) when is_list(Order)->
    AppName = domain(get(hostname)),
	?ERROR_LOG2("DBLOG--get_data2:~p,where:~p,order:~p~n",[Table,[],Order]),
    % io:format("hostname:~p~n",[AppName]),
%%     io:format("get_data DbName:~p~n",[DbName]),
%% 	io:format("get_data Table:~p~n",[Table]),
%% 	io:format("get_data Order:~p~n",[Order]),	
	case rpc_proxy:call(DbName, content, get, [AppName, Table, "", Order]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			?ERROR_LOG2("DBLOG--get_data AppName:~p Table:~p, Where:~p, Else:~p~n",[AppName,Table, "", Else]),
			Else
	end;
get_data2(DbName, Table, Where,Order) when is_list(Order) andalso is_list(Where)->
    AppName = domain(get(hostname)),
	?ERROR_LOG2("DBLOG--get_data2:~p,where:~p,order:~p~n",[Table,Where,Order]),
    % io:format("hostname:~p~n",[AppName]),
%%     io:format("get_data DbName:~p~n",[DbName]),
%% 	io:format("get_data Table:~p~n",[Table]),
%% 	io:format("get_data Where:Where~p~n",[Where]),
%% 	io:format("get_data Order:~p~n",[Order]),
	case rpc_proxy:call(DbName, content, get, [AppName, Table, Where, Order]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			?ERROR_LOG2("DBLOG--get_data AppName:~p Table:~p, Where:~p, Else:~p~n",[AppName,Table, Where, Else]),
			Else
	end;
get_data2(_, _, _,_)->{error,parameter_error}.


%% @spec get_data_stat(DbName, Table, Where,Order) -> (Result)
%% DbName = string()
%% Table = string()
%% Where = string()
%% Order = string()
%% Result = #content{}
%% @doc get label by id
%% @doc According to id for label
%%
get_data_stat(DbName, Table, [],Order) when is_list(Order)->
    AppName = domain(get(hostname)),
	?ERROR_LOG2("DBLOG--get_data_stat:~p,where:~p,order:~p~n",[Table,[],Order]),
	case rpc_proxy:call(DbName, content, get, [AppName, Table, "", Order]) of
		{ok,R}->
			R;
		Else->
			Else
	end;
get_data_stat(DbName, Table, Where,Order) when is_list(Order) andalso is_list(Where)->
    AppName = domain(get(hostname)),
	?ERROR_LOG2("DBLOG--get_data_stat:~p,where:~p,order:~p~n",[Table,Where,Order]),
	case rpc_proxy:call(DbName, content, get, [AppName, Table, Where, Order]) of
		{ok,R}->
			R;
		Else->
			?ERROR_LOG2("DBLOG--get_data AppName:~p Table:~p, Where:~p, Else:~p~n",[AppName,Table, Where, Else]),
			Else
	end;
get_data_stat(_, _, _,_)->{error,parameter_error}.

insert_data(_,_,{})->{error,parameter_error};
insert_data(DbName,Table,Data)->
    AppName = domain(get(hostname)),
	?ERROR_LOG2("DBLOG--insert_data:~p~n",[Table]),
%%     io:format("hostname:~p~n",[{AppName,DbName,Table,Data}]),
    Newdata = Data#content{xn_type = list_to_binary(Table),application=list_to_atom(AppName)},
    %io:format("____2______:~p~n",[Newdata]),
	%rpc_proxy:call(DbName,content,create,[[{application,Table}],Data]).
    rpc_proxy:call(DbName,content,create,[AppName, Table, Newdata]).
 
insert_data(AppName,DbName,Table,Data)->
	?ERROR_LOG2("DBLOG--insert_data:~p~n",[Table]),
    Newdata = Data#content{xn_type = list_to_binary(Table),application=list_to_atom(AppName)},
    rpc_proxy:call(DbName,content,create,[AppName, Table, Newdata]). 

update_data(_,_,_,{})->{error,parameter_error};
update_data(DbName,Table,Where,Data)->
    AppName = domain(get(hostname)),
	?ERROR_LOG2("DBLOG--update_data:~p,where:~p~n",[Table,Where]),
%%     io:format("hostname:~p~n",[AppName]),
    Newdata = Data#content{xn_type = list_to_binary(Table),application=list_to_atom(AppName)},
    %io:format("_____1____:~p~n",[Newdata]),
	Result = rpc_proxy:call(DbName,content,update,[AppName, Table, Where, Newdata]),
    %io:format("_____1____:~p~n",[Result]),
    Result.

delete_data(DbName,Table,Where)->
    AppName = domain(get(hostname)),
	?ERROR_LOG2("DBLOG--delete_data:~p,where:~p~n",[Table,Where]),
%%     io:format("hostname:~p~n",[AppName]),
	rpc_proxy:call(DbName,content,delete,[AppName, Table, Where]).