%% ---
%% page_siteview
%%
%%---
-module(page_group).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").
%% -include("../../core/src/monitor.hrl").
%% -include("../../core/src/monitor_template.hrl").

respond(Req,Path,Raw_path)->
	io:format("page_group:~p,~p~n",[Path,Raw_path]),
	case Raw_path of
		"oper=edit&id="++Id->
			group_edit(Id);
		"oper=add&parent="++Id->
			group_add(Id);
		"oper=update"->
			group_update(Req,Path,Raw_path);
		"oper=new&parent=" ++ ParentId->
			group_new(Req,Path,Raw_path,ParentId);
		"oper=delete&id=" ++ Id->
			group_delete(Req,Path,Raw_path,Id);
		"oper=remove"->
			group_remove(Req,Path,Raw_path);
		"id=" ++ Id ->
			group_list(Id)
	end.
	
printfullpath([])->"";
printfullpath([Obj|_])->
	printfullpath(Obj);
printfullpath({error,_})->"";
printfullpath({ok,{parent,Parent}})->
	printfullpath(Parent);
printfullpath(Obj)->
	{ok,{id,Id}} = Obj:get_property(id),
	case Obj:get_property(name) of
		{ok,{name,Name}}->
			printfullpath(Obj:get_owner()) ++ ":" ++ "<a href=\"/group?id=" ++ atom_to_list(Id) ++ "\">" ++ atom_to_list(Name) ++ "</a>";
		_->
			printfullpath(Obj:get_owner()) ++ ":" ++ "<a href=\"/group?id=" ++ atom_to_list(Id) ++ "\">"  ++ atom_to_list(Id) ++ "</a>"
	end.
	

group_list(Id)->
	G = api_group:childs(list_to_atom(Id)),
	"<html><head><title>siteview</title></head>"
	++"<body>" ++ "<p>Group:" ++ printfullpath(api_siteview:find_object(list_to_atom(Id))) ++ "</p>"
	++"<table width=600px border=1><tr><td width=100px>state</td><td width=200px>status</td><td width=200px>name</td><td width=100px>Edit</td><td width=100px>Del</td>" ++
	obj2page(G) ++"</table>" ++ 
	"<p>Add to Group</p><p><a href=\"/monitortemplate?parent=" ++ Id ++ "\">Monitor</a></p>" ++
	"<p><a href=\"/group?oper=add&parent=" ++ Id ++ "\">Subgroup</a></p>" ++
	"</body></html>".

obj2page([])->"";
obj2page([H|T])->
	{value,{id,Id}} = lists:keysearch(id,1,H),
	RI = api_group:get_run_info(Id),
	{value,{class,Class}} = lists:keysearch(class,1,H),
	"<tr>" ++
	case lists:keysearch(category,1,RI) of
		{value,{category,Cate}}->
			"<td>"++atom_to_list(Cate) ++"</td>";
		_->
			"<td>no data</td>"
	end ++
	case lists:keysearch(state_string,1,RI) of
		{value,{state_string,SS}}->
			"<td>"++ SS ++"</td>";
		_->
			"<td>no status</td>"
	end ++
	case lists:keysearch(name,1,H) of
		{value,{name,Name}} when Class=:=group->
			"<td><a href=\"/group?id="++atom_to_list(Id) ++ "\">" ++ atom_to_list(Name) ++"</a></td>";
		{value,{name,Name}}->
			"<td>" ++ atom_to_list(Name) ++"</td>";
		_->
			case Class of
				group->
					"<td><a href=\"/group?id="++atom_to_list(Id) ++ "\">" ++atom_to_list(Id) ++ "</a></td>";
				_->
					"<td>" ++atom_to_list(Id) ++ "</td>"
			end
	end ++
	case lists:keysearch(class,1,H) of
		{value,{class,group}}->
			"<td><a href=\"/group?oper=edit&id="++atom_to_list(Id) ++ "\">Edit</a></td>";
		{value,{class,device}}->
			"<td>Edit</td>";
		_->
			"<td><a href=\"/monitortemplate?oper=edit&id="++atom_to_list(Id) ++ "\">Edit</a></td>"
	end ++
	case lists:keysearch(class,1,H) of
		{value,{class,group}}->
			"<td><a href=\"/group?oper=delete&id="++atom_to_list(Id) ++ "\">X</a></td>";
		_->
			"<td><a href=\"/monitor?oper=delete&id="++atom_to_list(Id) ++ "\">X</a></td>"
	end ++
	"<tr>" ++obj2page(T).
	
group_add(ParentId)->
	MT = api_monitor_template:get_template(monitor_group),
	[Parent|_] = api_group:find_group(list_to_atom(ParentId)),
	ParentName = case Parent:get_property(name) of
					{ok,{name,Name}}->
						atom_to_list(Name);
					_->
						ParentId
				end,
	{Adv,_} = lists:partition(fun(X)->(X#property.advance=:=true) and (X#property.configurable=:= true)  end,MT),
	{Base,_} = lists:partition(fun(X)->(X#property.advance=:=false) and (X#property.configurable=:= true) end, MT),
	"<html><head><title>siteview</title></head>" ++
	"<body>"++
	"<h2>Edit Group To:<a href=\"/group?id=" ++ ParentId ++ "\">" ++ ParentName ++ "<a></h2>" ++
	"<form action=/group?oper=new&parent=" ++ ParentId ++ " method=POST>" ++
	"<input type=hidden name=class value=group>" ++
	page_monitortemplate:print_propertys("monitor_group",Base,[],"",[]) ++
	"<p><input type=submit VALUE=\"Add\">  Group</p>" ++
	"<hr>" ++
	"<H3>Advanced Options</H3>" ++
	page_monitortemplate:print_propertys("monitor_group",Adv,[],"",[]) ++
	"<p><input type=submit VALUE=\"Add\">  Group</p>" ++
	"</form>"++
	"</body></html>".

group_edit(Id)->
	MT = api_monitor_template:get_template(monitor_group),
	Gi = api_group:info(list_to_atom(Id)),
	Name = page_monitortemplate:get_property(name,Gi,text),
	{Adv,_} = lists:partition(fun(X)->(X#property.advance=:=true) and (X#property.configurable=:= true)  end,MT),
	{Base,_} = lists:partition(fun(X)->(X#property.advance=:=false) and (X#property.configurable=:= true) end, MT),
	"<html><head><title>siteview</title></head>" ++
	"<body>"++
	"<h2>Edit Group To:<a href=\"/group?id=" ++ Id ++ "\">" ++ Name ++ "<a></h2>" ++
	"<form action=/group?oper=update method=POST>" ++
	"<input type=hidden name=id value=" ++ Id ++ ">" ++
	page_monitortemplate:print_propertys("monitor_group",Base,[],"",Gi) ++
	"<p><input type=submit VALUE=\"Update\">  Group</p>" ++
	"<hr>" ++
	"<H3>Advanced Options</H3>" ++
	page_monitortemplate:print_propertys("monitor_group",Adv,[],"",Gi) ++
	"<p><input type=submit VALUE=\"Update\">  Group</p>" ++
	"</form>"++
	"</body></html>".


group_update(Req,Path,Raw_path)->
	Ret = Req:parse_post(),
	io:format("group_update:~p~n",[Ret]),
	{value,{"id",Id}} = lists:keysearch("id",1,Ret),
	Parent= api_siteview:get_parent_id(list_to_atom(Id)),
	Temp = api_monitor_template:get_template(monitor_group),
	{Unit,Props} = lists:partition(fun({K,V})->case regexp:match(K,"temp_unit") of {match,1,_}->true;_->false end end,Ret),
	NR = page_monitor:filter_props(Ret,Temp,Unit),
	R = case api_group:update(NR) of
			{ok,_}->
				"Update Success.";
			_->
				"Update fail!"
		end,
	"<html><head><title>siteview</title></head>"++
	"<body><p>" ++ R ++ "</p><p><a href=\"/group?id=" ++ atom_to_list(Parent) ++  "\">·µ»Ø</a></p></body></html>".
	
group_delete(Req,Path,Raw_path,Id)->
	"<html><head><title>Delete Group</title></head>"++
	"<h2>Delete Group: <a href=\"/group?id="++ Id ++ "\">"++ atom_to_list(api_siteview:get_object_name(list_to_atom(Id))) ++"</a>" ++"</h2>" ++
	"<p>Are you sure you want to delete the group \"" ++ atom_to_list(api_siteview:get_object_name(list_to_atom(Id))) ++ "\"? </p>"  ++
	"<form action=/group?oper=remove method=POST>" ++
	"<input type=hidden name=id value=" ++ Id ++ ">" ++
	"<p><input type=submit value=\"Delete Group\"></p>" ++
	"</form></body></html>".
	
group_remove(Req,Path,Raw_path)->
	Ret = Req:parse_post(),
	{value,{"id",Id}} = lists:keysearch("id",1,Ret),
	Parent= api_siteview:get_parent_id(list_to_atom(Id)),
	
	R = case api_group:delete(list_to_atom(Id)) of
			{ok,_}->
				"Delete Success.";
			{error,Reason}->
				"Delete Fail:" ++ atom_to_list(Reason)
		end,
	"<html><head><title>siteview</title></head>"++
	"<body><p>" ++ R ++ "</p><p><a href=\"/group?id=" ++ atom_to_list(Parent) ++  "\">Return Back</a></p></body></html>".
	
group_new(Req,Path,Raw_path,ParentId)->
	Ret = Req:parse_post(),
	Temp = api_monitor_template:get_template(monitor_group),
	{Unit,Props} = lists:partition(fun({K,V})->case regexp:match(K,"temp_unit") of {match,1,_}->true;_->false end end,Ret),
	NR = page_monitor:filter_props(Ret,Temp,Unit),
	R = case api_group:create(list_to_atom(ParentId),NR) of
			{ok,_}->
				"Create Success.";
			{error,Reason}->
				"Create Fail:" ++ atom_to_list(Reason)
		end,
	"<html><head><title>siteview</title></head>"++
	"<body><p>" ++ R ++ "</p><p><a href=\"/group?id=" ++ ParentId ++  "\">back</a></p></body></html>".