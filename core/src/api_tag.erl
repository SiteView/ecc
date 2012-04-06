%% Author: Administrator
%% Created: 2010-3-15
%% Description: TODO: Add description to api_tag
-module(api_tag).
%% -compile(export_all).
-export([create/3,get/1,get_all/0,delete/1,update/3,getTagAndValuesData/1,getallTagAndValuesData/0]).

-export([get_all_class/0,
		create_tagclass/2,
		update_tagclass/3,
		remove_tagclass/1,
		create_tag/3,
		update_tag/4,
		remove_tag/1,
		get_tag_by_class/1,
		get_tagclass/1,
		get_tag/1,
		get_object_by_tag/1,
		tag/3,
		get_tag_by_objectid/1,
		create_tag/1,
		create_subtag/2,
		get_subtags/1,
		update_subtag/2,
		get_tags/0
		]).

get_all_class()->
	dbcs_tag:get_all_tagclass().
	
get_tagclass(Id)->
	dbcs_tag:get_tagclass(Id).
	
create_tagclass(Name,Desc)->
	Id = dbcs_base:uuid(),
	dbcs_tag:create_tagclass([{id,Id},{name,Name},{desc,Desc}]).
	
update_tagclass(Id,Name,Desc)->
	dbcs_tag:update_tagclass([{id,Id},{name,Name},{desc,Desc}]).
	
remove_tagclass(Id)->
	dbcs_tag:remove_tagclass(Id).
	
create_tag(Class,Name,Value)->
	Id = dbcs_base:uuid(),
	dbcs_tag:create_tag([{id,Id},{tagclass,Class},{name,Name},{value,Value}]).
	
create_tag(Name)->
	Id = dbcs_base:uuid(),
	dbcs_tag:create_tag([{id,Id},{name,Name}]).
	
create_subtag(ParentId,Name)->
	Id = dbcs_base:uuid(),
	dbcs_tag:create_subtag(ParentId,[{id,Id},{name,Name}]).
	
get_subtags(ParentId)->
	dbcs_tag:get_subtags(ParentId).
	
update_subtag(Id,Name)->
	dbcs_tag:update_tag([{id,Id},{name,Name}]).
	
update_tag(Id,Class,Name,Value)->
	dbcs_tag:update_tag([{id,Id},{tagclass,Class},{name,Name},{value,Value}]).
	
remove_tag(Id)->
	dbcs_tag:remove_tag(Id).
	
get_tag_by_class(Class)->
	dbcs_tag:get_tag_by_class(Class).
	
get_tag(Id)->
	dbcs_tag:get_tag(Id).
	
get_tags()->
	dbcs_tag:get_tags().
	
get_tag_by_objectid(OId)->
	tag_store:start_link(),
	case tag_store:q([{oid,'=',OId}]) of
		{ok,TagIns}->
			{ok,lists:map(fun(X)->element(4,X) end,TagIns)};
		Else->
			Else
	end.
	
tag(OId,OType,TagId)->
	tag_store:start_link(),
	tag_store:tag(OId,OType,TagId).

get_object_by_tag(TagId)->
	dbcs_tag:get_object_by_tag(TagId).

%%  V=[{name,"hi"},{description,"hi you"}].
%%  V1=[[{name,"h1"},{description,"h1"}],[{name,"h2"},{description,"h2"}]].
%%  api_tag:create(0,V,V1).
create('',_,_)->{error,error_parameter};
create(ParentId,TagData,TagvaluesData) when is_integer(ParentId),is_list(TagData),is_list(TagvaluesData)->
	NewId=getnextid(ParentId),	
	Sid=integer_to_list(NewId),
	Atomid=list_to_atom(Sid),
	ValuesData=getValuesData(1,TagvaluesData,NewId,"","","",""),
	Tag=[{id,Atomid}]++TagData++ValuesData,
%% 	io:format("Tag:~p ~n", [Tag]),
     dbcs_tag:create_tag(Tag) ;
create(_,_,_)->
	{error,parameter_error}.
	
getValuesData(I,[],_,Valueid,Valuesname,Valuesdescription,Valuesorder) ->
	case I of
		1 ->
			[{valuesid,""},{valuesname,""},{valuesdescription,""},{valuesorder,""}];
		_ ->
			[{valuesid,Valueid},{valuesname,Valuesname},{valuesdescription,Valuesdescription},{valuesorder,Valuesorder}]
	end;
getValuesData(I,[H|E],Pid,Valueid,Valuesname,Valuesdescription,Valuesorder)->
	SNewid=integer_to_list(Pid)++"."++integer_to_list(I),
	Sorder=integer_to_list(I),
	{value,{_,Sname}}=lists:keysearch(name, 1, H),
	{value,{_,SDescription}}=lists:keysearch(description, 1, H),
	case Valuesname of
		""->
			TValueid=SNewid,
			TValuesname=Sname,
			TValuesdescription=SDescription,
			TValuesorder=Sorder;
		_ ->
			TValueid=Valueid++","++SNewid,
			TValuesname=Valuesname++","++Sname,
			TValuesdescription=Valuesdescription++","++SDescription,
			TValuesorder=Valuesorder++","++Sorder
	end,
	getValuesData(I+1,E,Pid,TValueid,TValuesname,TValuesdescription,TValuesorder).
buildValueData(_,[],I,OrderI,Valueid,Valuesname,Valuesdescription,Valuesorder)->
	case OrderI of
		1 ->
			[{valuesid,""},{valuesname,""},{valuesdescription,""},{valuesorder,""}];
		_ ->
			[{valuesid,Valueid},{valuesname,Valuesname},{valuesdescription,Valuesdescription},{valuesorder,Valuesorder}]
	end;
buildValueData(ParentId,[H|E],I,OrderI,Valueid,Valuesname,Valuesdescription,Valuesorder) ->
	IdTuple=lists:keysearch(id, 1, H),
	case IdTuple of
		false ->
			J=I+1,
			Id=integer_to_list(ParentId)++"."++integer_to_list(I);
		{value,{_,TId}}->
			J=I,
			Id=TId
	end,
	Sorder=integer_to_list(OrderI),
	{value,{_,Sname}}=lists:keysearch(name, 1, H),
	{value,{_,SDescription}}=lists:keysearch(description, 1, H),		
	case Valuesname of
		""->
			TValueid=Id,
			TValuesname=Sname,
			TValuesdescription=SDescription,
			TValuesorder=Sorder;
		_ ->
			TValueid=Valueid++","++Id,
			TValuesname=Valuesname++","++Sname,
			TValuesdescription=Valuesdescription++","++SDescription,
			TValuesorder=Valuesorder++","++Sorder
	end,		
	buildValueData(ParentId,E,J,OrderI+1,TValueid,TValuesname,TValuesdescription,TValuesorder).
	
update(Id,TagData,TagvaluesData)when is_atom(Id),is_list(TagData),is_list(TagvaluesData)->
	Nextid=getnextid(Id),
	{Idint,_}=string:to_integer(atom_to_list(Id)),
	ValuesData=buildValueData(Idint,TagvaluesData,Nextid,1,"","","",""),
	Tag=[{id,Id}]++TagData++ValuesData,
%% 	io:format("updatetag:~p ~n", [Tag]);
 	 dbcs_tag:update_tag(Tag);
update(_,_,_)->
	{error,notoftype}.
	
getnextid(ParentId)->
	case ParentId of
		0 ->
			Ids=dbcs_tag:get_tag_1_ids(),
			case Ids of
				[] ->
					Maxid=0;
				_ ->
					Maxid=lists:max(Ids)
			end,
			Nextid=Maxid+1;
		_ ->
			Nextid=dbcs_tag:get_tag_childs_nextid(ParentId)
	end,
	Nextid.
	
get(Id) ->
	dbcs_tag:get_tag(Id).
valuesData(I,_,_,_,_) when I=:=0->
	[];
valuesData(I,Valuesids,Valuesnames,Valuesdescriptions,Valuesorders)  ->
	
	Id=lists:nth(I, Valuesids),
	Name=lists:nth(I, Valuesnames),
	Description=lists:nth(I, Valuesdescriptions),
	Order=lists:nth(I, Valuesorders),
	[[{id,Id},{name,Name},{description,Description},{order,Order}]]++valuesData(I-1,Valuesids,Valuesnames,Valuesdescriptions,Valuesorders).
	
getTagAndValuesData(TTag) ->
	{value,{_,Id}}=lists:keysearch(id, 1, TTag),
	{value,{_,Name}}=lists:keysearch(name, 1, TTag),
    {value,{_,Description}}=lists:keysearch(description, 1, TTag),	
	{value,{_,Valuesid}}=lists:keysearch(valuesid, 1, TTag),
	{value,{_,Valuesname}}=lists:keysearch(valuesname, 1, TTag),
	{value,{_,Valuesdescription}}=lists:keysearch(valuesdescription, 1, TTag),
	{value,{_,Valuesorder}}=lists:keysearch(valuesorder, 1, TTag),
	Tag=[{id,Id},{name,Name},{description,Description}],
	Valuesids=string:tokens(Valuesid,"," ),
	Valuesnames=string:tokens(Valuesname,"," ),
	Valuesdescriptions=string:tokens(Valuesdescription,"," ),
	Valuesorders=string:tokens(Valuesorder,"," ),
	case Valuesids of
		[] ->
			ValuesData="";
		_ ->
			I=length(Valuesids),
			ValuesData=valuesData(I,Valuesids,Valuesnames,Valuesdescriptions,Valuesorders)
	end,
	[{tag,Tag},{valuesdata,ValuesData}].

buildall([])->
	[];
buildall([H|E])->
	R=getTagAndValuesData(H),
	[R]++buildall(E).
getallTagAndValuesData() ->
	Tags=get_all(),
	buildall(Tags).
get_all()->
  dbcs_tag:get_all().
delete(Id)  ->
	dbcs_tag:remove_tag(Id).