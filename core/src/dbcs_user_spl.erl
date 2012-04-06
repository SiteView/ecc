%%
%% @doc simple user and right manager base on content store
%%
%%
-module(dbcs_user_spl).
-compile(export_all).

-define(Table,"user_spl").
-define(TableRight,"user_spl_right").
-include("user_spl.hrl").
-include("dbcs_common.hrl").

base_property(id)->
	true;
base_property(_)->
	false.
	
user_to_db(User)->
	{value,{id,Id}} = lists:keysearch(id,1,User),
	Advance = [dbcs_base:term2db(K,V)||{K,V}<- User,base_property(K)=:=false],
	{content,list_to_atom(?Table), list_to_atom(Id), <<?Table>>,null,null,null,null,?Author,null,null,null,null,null,Advance}.

db_to_user(UserData)->
	case UserData of
		{_,_,Id,_,_,_,_,_,_,_,_,_,_,_,Adv}->
			[{id,atom_to_list(Id)}] ++ [dbcs_base:db2term(K,T,V)||{K,T,V}<-Adv];
		_->
			[]
	end.
	
	
get_maxIdByuser() ->
	Ret = db_ecc:get_data(?DBName, ?Table, "id != administrator & type = " ++ ?Table),
	
	case is_list(Ret) of
		false ->
			error;
		true ->
			case length(Ret) of
				0 ->
					"login0";
				_ ->
					CurIdList = [Id || {content, _, Id, _, _, _, _, _, _, _, _, _, _, _, _} <- Ret],
					MaxId = lists:max(CurIdList),
					case atom_to_list(MaxId) of
						"login" ++ Sn ->
							"login" ++ integer_to_list(list_to_integer(Sn) +1 );
						_->
							"login0"
					end
			end
	end.

create_user(UserData)when is_list(UserData) ->
	case proplists:get_value(name,UserData) of
		undefined->
			{error,user_data_error};
		Name->
			case get_user_match("my.name=" ++ Name) of
				[]-> 
					%%case get_maxIdByuser() of  
						%%error->
							%%{error,make_user_id_error};
						%%UId->
                    UId = dbcs_base:uuid2(),
                    Data = [{id,UId}] ++ UserData,
                    db_ecc:insert_data(?DBName,?Table,user_to_db(Data));
					%%end;
				_->
					{error,user_existed}
			end
	end;
create_user(_)->{error,parameter_error}.    
    
create_user(app,AppName,UserData)when is_list(UserData) ->
	case proplists:get_value(name,UserData) of
		undefined->
			{error,user_data_error};
		Name->
			case get_user_match("my.name=" ++ Name) of
				[]-> 
					%%case get_maxIdByuser() of  
						%%error->
							%%{error,make_user_id_error};
						%%UId->
                    UId = dbcs_base:uuid2(),
                    Data = [{id,UId}] ++ UserData,
                    db_ecc:insert_data(AppName,?DBName,?Table,user_to_db(Data));
					%%end;
				_->
					{error,user_existed}
			end
	end;    
create_user(_,_,_)->{error,parameter_error}.


create_user(Id,UserData)when is_list(UserData) ->
	case proplists:get_value(name,UserData) of
		undefined->
			{error,user_data_error};
		Name->
			case get_user_match("my.name=" ++ Name) of
				[]->
					Data = [{id,Id}] ++ UserData,
					db_ecc:insert_data(?DBName,?Table,user_to_db(Data));
				_->
					{error,user_existed}
			end
	end;
create_user(_,_)->{error,parameter_error}.

get_user(Id) when is_atom(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=user_spl & id="++atom_to_list(Id)),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_existed};
				_->
					[User|_] = Ret,
					db_to_user(User)
			end
	end;
get_user(Id) when is_list(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=user_spl & id="++Id),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_existed};
				_->
					[User|_] = Ret,
					db_to_user(User)
			end
	end;
get_user(_)->
	{error,parameter_error}.
	
%% @doc update user information
%% @spec update_user(User)->({error,Reason} | {ok,Result})
%% where
%%		User = list()
%%		Result = tuple()
update_user(User)->
	{value,{id,Id}} = lists:keysearch(id,1,User),
	case get_user(Id) of
		{error,Err}->
			{error,Err};
		Old->
			ND = lists:ukeymerge(1,lists:ukeysort(1,User),lists:ukeysort(1,Old)),
			case db_ecc:update_data(?DBName,?Table,"id=" ++ Id,user_to_db(ND)) of
				{ok,Ret}->
					{ok,db_to_user(Ret)};
				Err->
					Err
			end
	end.
	
%% @doc query user with compsite condition
%% @spec  get_user_match(Where)-> (Users | {error,Reason})
%% where
%%		Where = string()
%%		Users = list()
get_user_match(Where)->
	Ret = db_ecc:get_data(?DBName,?Table,Where),
	case Ret of
		{error,Err}->
			{error,Err};
		_->
			[db_to_user(X)||X <- Ret]
	end.
	
%% @doc get all user
%% @spec get_all()->({error,Reason} | Users)
%% where
%%		Users = list()
get_all()->
     
	Ret = db_ecc:get_data(?DBName,?Table,""),
	case Ret of
		{error,_}->
			[];
		_->
			[db_to_user(X)||X <- Ret]
	end.
	
validate_user(Login,Pass)->
	ok.
	
get_user_by_name(Name)->
	case get_user_match("my.name=" ++ Name) of
		{error,Err}->
			{error,Err};
		[]->
			{error,not_existed};
		[User|_]->
			{ok,User};
		_->
			{error,database_error}
	end.
	
%% @doc delete a user
%% @spec remove_user(Id)->({error,Reason} | {ok,Result})
%% where
%%		Id = (atom() | string())
%%		Result = tuple()
remove_user(Id)when is_atom(Id)->
	db_ecc:delete_data(?DBName,?Table,"id="++atom_to_list(Id));
remove_user(Id)when is_list(Id)->
	db_ecc:delete_data(?DBName,?Table,"id="++Id);
remove_user(Id)when is_number(Id)->
	db_ecc:delete_data(?DBName,?Table,lists:flatten(io_lib:format("id=~p",[Id])));
remove_user(_)->
	{error,parameter_error}.
	
	
reset() ->
	Users = get_all(),
	lists:foreach(fun(X) ->
					Id = proplists:get_value(id,X),
					remove_user(Id)
				  end, Users),
	UData = [{name, "admin"},
			 {password,"admin"},
			 {title, "Administrator"}
			],
	create_user(?ADMIN, UData).
	
	
right_to_db(Right)->
	{value,{id,Id}} = lists:keysearch(id,1,Right),
	Advance = [dbcs_base:term2db(K,V)||{K,V}<- Right,base_property(K)=:=false],
	{content,list_to_atom(?TableRight), Id, <<?TableRight>>,null,null,null,null,?Author,null,null,null,null,null,Advance}.

db_to_right(RightData)->
	case RightData of
		{_,_,Id,_,_,_,_,_,_,_,_,_,_,_,Adv}->
			[{id,Id}] ++ [dbcs_base:db2term(K,T,V)||{K,T,V}<-Adv];
		_->
			[]
	end.
	
get_right(UId,GId)->
	Where = lists:flatten(io_lib:format("my.uid=~s & my.gid='~s'",[UId,GId])),
	Ret = db_ecc:get_data(?DBName,?TableRight,Where),
	case Ret of
		{error,Err}->
			{error,Err};
		_->
			[db_to_right(X)||X <- Ret]
	end.
	
get_right(UId)->
	Where = lists:flatten(io_lib:format("my.uid=~s",[UId])),
	Ret = db_ecc:get_data(?DBName,?TableRight,Where),
	case Ret of
		{error,Err}->
			{error,Err};
		_->
			[db_to_right(X)||X <- Ret]
	end.
	
add_right(UId,GId,Rights)->
	case get_right(UId,GId) of
		{error,Err}->
			{error,Err};
		[_|_]->	
			{error,existed};
		_->
			Id = dbcs_base:uuid(),
			Data = [{id,Id},{uid,UId},{gid,GId},{rights,Rights}],
			case db_ecc:insert_data(?DBName,?TableRight,right_to_db(Data)) of
				{ok,Ret}->
					{ok,db_to_user(Ret)};
				Err->
					Err
			end
	end.
	
remove_right(UId,GId)->
	case get_right(UId,GId) of
		{error,Err}->
			{error,Err};
		[]->
			{error,not_existed};
		[D|_]->
			Id = proplists:get_value(id,D),
			Where = lists:flatten(io_lib:format("id=~s",[Id])),
			db_ecc:delete_data(?DBName,?TableRight,Where)
	end.
	
remove_right(UId)->
	case get_right(UId) of
		{error,Err}->
			{error,Err};
		[]->
			{error,not_existed};
		Ret->
			F = fun(X,R)->
				Id = proplists:get_value(id,X),
				Where = lists:flatten(io_lib:format("id=~s",[Id])),
				Ret2 = db_ecc:delete_data(?DBName,?TableRight,Where),
				case R of
					{ok,_}->
						Ret2;
					_->
						R
				end
			end,
			lists:foldl(F,{ok,remove},Ret)
	end.

	
update_right(UId,GId,Rights)->
	case get_right(UId,GId) of
		{error,Err}->
			{error,Err};
		[]->	
			{error,not_existed};
		[D|_]->
			Data = lists:keyreplace(rights,1,D,{rights,Rights}),
			Id = proplists:get_value(id,D),
			Where = lists:flatten(io_lib:format("id=~s",[Id])),
			% io:format("update_right:~p,~p~n",[Where,Data]),
			case db_ecc:update_data(?DBName,?TableRight,Where,right_to_db(Data)) of
				{ok,Ret}->
					{ok,db_to_user(Ret)};
				Err->
					Err
			end
	end.