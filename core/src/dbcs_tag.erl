%% Author: Administrator
%% Created: 2010-3-15
%% Description: TODO: Add description to dbcs_tag
-module(dbcs_tag).

-define(Table,"tag").
-define(TableClass,"tagclass").

-export([create_tag/1,get_tag/1,update_tag/1,get_all/0,remove_tag/1,get_tag_1_ids/0,get_tag_childs_nextid/1]).
-export([create_tagclass/1,remove_tagclass/1,update_tagclass/1,get_all_tagclass/0,get_tagclass/1,get_tag_by_class/1]).
-export([get_object_by_tag/1,create_subtag/2,get_subtags/1,get_tags/0]).

-define(APP,app_).
-include("dbcs_common.hrl").

base_property(id)->
	true;
base_property(?APP)->
	true;
base_property(_)->
	false.
	
tag_to_db(Tag)->
	{value,{id,Id}}=lists:keysearch(id, 1, Tag),
	Advance=[dbcs_base:term2db(K,V)||{K,V}<- Tag,base_property(K)=:=false],
	case is_atom(Id) of
		true ->
			{content,list_to_atom(?Table), Id, <<"tag">>,null,null,null,null,?Author,null,null,null,null,null,Advance};
		_ ->{}
	end.
db_to_tag(TagData)->
	case TagData of
		{_,App,Id,_,_,_,_,_,_,_,_,_,_,_,Adv}->
			[{id,Id},{?APP, App}] ++ [dbcs_base:db2term(K,T,V)||{K,T,V}<-Adv];
		_->
			[]
	end.
	
db_to_tagclass(TagData)->
	case TagData of
		{_,App,Id,_,_,_,_,_,_,_,_,_,_,_,Adv}->
			[{id,Id},{?APP, App}] ++ [dbcs_base:db2term(K,T,V)||{K,T,V}<-Adv];
		_->
			[]
	end.
	

tagclass_to_db(Tag)->
	{value,{id,Id}}=lists:keysearch(id, 1, Tag),
	Advance=[dbcs_base:term2db(K,V)||{K,V}<- Tag,base_property(K)=:=false],
	case is_atom(Id) of
		true ->
			{content,list_to_atom(?Table), Id, <<"tagclass">>,null,null,null,null,?Author,null,null,null,null,null,Advance};
		_ ->{}
	end.


%%dbcs_tag		Result = tuple()
create_tag(Tag)when is_list(Tag) ->
	Name = proplists:get_value(name,Tag),
	case get_tag_match("my.name=" ++ Name) of
		[]->
			case db_ecc:insert_data(?DBName,?Table,tag_to_db(Tag++[{parent,'0'}])) of
				{ok,Ret}->
					{ok,db_to_tag(Ret)};
				{error,Err}->
					{error,Err};
				Err2->
					{error,Err2}
			end;
		[_|_]->
			{error,existed};
		Else->
			Else
	end;
create_tag(_)->{error,parameter_error}.

create_subtag(ParentId,Tag)when is_list(Tag) ->
	Name = proplists:get_value(name,Tag),
	case get_tag_match("my.name=" ++ Name) of
		[]->
			case db_ecc:insert_data(?DBName,?Table,tag_to_db(Tag++[{parent,ParentId}])) of
				{ok,Ret}->
					{ok,db_to_tag(Ret)};
				{error,Err}->
					{error,Err};
				Err2->
					{error,Err2}
			end;
		[_|_]->
			{error,existed};
		Else->
			Else
	end;
create_subtag(_,_)->{error,parameter_error}.

get_tag(Id)when is_atom(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"id="++atom_to_list(Id)),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_tag};
				_->
					[Tag|_] = Ret,
					db_to_tag(Tag)
			end
	end;
get_tag(Id)when is_list(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"id="++Id),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_tag};
				_->
					[Tag|_] = Ret,
					db_to_tag(Tag)
			end
	end;
get_tag(_)->
	{error,id_error}.
	
get_tags()->
	get_subtags('0').
	
get_subtags(ParentId) when is_atom(ParentId)->
	Ret = db_ecc:get_data(?DBName,?Table,"my.parent="++atom_to_list(ParentId)),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					[];
				_->
					[db_to_tag(Tag)||Tag<-Ret]
			end
	end;
get_subtags(_)->
	{error,parameter_error}.
	
update_tag(Tag)->
	Name = proplists:get_value(name,Tag),
	Id = proplists:get_value(id,Tag),
	case get_tag_match("id!="++ atom_to_list(Id) ++"&my.name=" ++ Name) of
		[]->
			Old = get_tag(Id),
			ND = lists:ukeymerge(1,lists:ukeysort(1,Tag),lists:ukeysort(1,Old)),
			case db_ecc:update_data(?DBName,?Table,"id=" ++ atom_to_list(Id),tag_to_db(ND)) of
				{ok,Ret}->
					{ok,db_to_tag(Ret)};
				{error,Err}->
					{error,Err};
				Err2->
					{error,Err2}
			end;
		[_|_]->
			{error,existed};
		Else->
			Else
	end.


get_tag_match(Where)->
	Ret = db_ecc:get_data(?DBName,?Table,Where),
	[db_to_tag(X)||X <- Ret].

get_tag_childs_nextid(Parentid) ->
	PTag= get_tag(Parentid),
	{value,{_,Valuesid}} =lists:keysearch(valuesid, 1, PTag),
	case Valuesid of 
		"" ->
			Newid=atom_to_list(Parentid)++"."++"1";
%% 			Newid=list_to_atom(TNewid);
		_ ->
%% 			{value,{_,Maxid}}=lists:keysearch(id, 1,Last),
            Vlist=string:tokens(Valuesid, ","), 
			MaxSid=lists:last(Vlist),
			ListId=string:tokens(MaxSid, "."),
			Maxend=lists:last(ListId),
			Newid=list_to_integer(Maxend)+1
%% 			Newid=atom_to_list(Parentid)++"."++integer_to_list(Maxendint)
%% 			Newid=list_to_atom(TNewid)
	end,
	Newid.
		
get_ids(Ret,[])->
	Ret;
get_ids(Ret,[H|E])->
	{value,{_,Id}}=lists:keysearch(id, 1, H),
	Sid=atom_to_list(Id),
	Intid=list_to_integer(Sid),
	case Ret of
		"" ->
			Result=[Intid];
		_ ->
			Result=Ret++[Intid]
	end,
	get_ids(Result,E).
			
get_tag_1_ids() ->
	Alldata=get_all(),
	get_ids([],Alldata).

	
get_all()->
	Ret = db_ecc:get_data(?DBName,?Table,""),
	[db_to_tag(X)||X <- Ret].
	
	
get_all_tagclass()->
	Ret = db_ecc:get_data(?DBName,?TableClass,""),
	[db_to_tagclass(X)||X <- Ret].	

get_tagclass(Id)->
	Ret = db_ecc:get_data(?DBName,?TableClass,"id="++atom_to_list(Id)),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_tagclass};
				_->
					[Tag|_] = Ret,
					db_to_tagclass(Tag)
			end
	end.
	
get_tagclass_match(Where)->
	Ret = db_ecc:get_data(?DBName,?TableClass,Where),
	[db_to_tagclass(X)||X <- Ret].

remove_tag(Id)when is_atom(Id)->
	case remove_child_tag(Id) of
		ok->
			db_ecc:delete_data(?DBName,?Table,"id="++atom_to_list(Id));
		Else->
			Else
	end;
remove_tag(_)->
	{error,parameter_error}.
	
remove_child_tag([])->ok;
remove_child_tag([Tag|T])->
	Id = proplists:get_value(id,Tag),
	case remove_child_tag(Id) of
		ok->
			remove_child_tag(T);
		Else->
			Else
	end;
remove_child_tag(Id)->
	case get_subtags(Id) of
		{error,Err}->
			{error,Err};
		SubTags->
			case remove_child_tag(SubTags) of
				ok->
					case db_ecc:delete_data(?DBName,?Table,"id="++atom_to_list(Id)) of
						{ok,_}->
							ok;
						Else->
							Else
					end;
				Err->
					Err
			end
	end.
			
	
get_tag_by_class(ClassId)->
	get_tag_match("my.tagclass=" ++ atom_to_list(ClassId)).
	
create_tagclass(Data) when is_list(Data)->
	Name = proplists:get_value(name,Data),
	case get_tagclass_match("my.name="++Name) of
		[]->
			case db_ecc:insert_data(?DBName,?TableClass,tagclass_to_db(Data)) of
				{ok,Ret}->
					{ok,db_to_tagclass(Ret)};
				{error,Err}->
					{error,Err};
				Err2->
					{error,Err2}
			end;
		[_|_]->
			{error,existed};
		Else->
			Else
	end;
create_tagclass(_)->{error,parameter_error}.


remove_tagclass(Id)->
	Tags=get_tag_match("my.tagclass=" ++ atom_to_list(Id)),
	RemoveTag = 
	lists:foldl(fun(X,R)->
		TempId = proplists:get_value(id,X),
		case R of
			false->
				false;
			_->
				case remove_tag(TempId) of
					{ok,_}->
						true;
					_->
						false
				end
		end
	end,true,Tags),
	case RemoveTag of
		true->
			db_ecc:delete_data(?DBName,?TableClass,"id="++atom_to_list(Id));
		false->
			{error,not_empty}
	end.
	
update_tagclass(Tag)->
	Id = proplists:get_value(id,Tag),
	Name = proplists:get_value(name,Tag),
	case get_tagclass_match("id!="++atom_to_list(Id)++ "&my.name="++Name) of
		[]->
			{value,{id,Id}} = lists:keysearch(id,1,Tag),
			case db_ecc:update_data(?DBName,?TableClass,"id=" ++ atom_to_list(Id),tagclass_to_db(Tag)) of
				{ok,Ret}->
					{ok,db_to_tagclass(Ret)};
				{error,Err}->
					{error,Err};
				Err2->
					{error,Err2}
			end;
		[_|_]->
			{error,existed};
		Else->
			Else
	end.

get_object_by_tag(TagId)->
	tag_store:start_link(),
	case tag_store:q([{tagid,'=',TagId}]) of
		{ok,Tags}->
			lists:foldl(fun(X,R)->
				OId = element(2,X),
				OType = element(3,X),
				Object=
				case OType of
					monitor->
						dbcs_monitor:get_monitor(OId);
					group->
						dbcs_group:get_group(OId);
					alert->
						dbcs_rule:get_rule(OId);
					report->
						dbcs_report:get_report(OId);
					_->
						{error,unknow_object_type}
				end,
				R ++ [Object]
			end,[], Tags);
		Else->
			Else
	end.
