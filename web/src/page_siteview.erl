%% ---
%% page_siteview
%%
%%---
-module(page_siteview).
-compile(export_all).

respond(Req,Path)->
	SV = siteview:get_current_siteview(),
	G = SV:get_childs(),
	"<html><head><title>siteview</title></head>"
	++"<body><table width=240px border=1><tr><td>state</td><td>name</td>" ++
	obj2page(G) ++"</table></body></html>".

obj2page([])->"";
obj2page([H|T])->
	{ok,{id,Id}}=H:get_property(id),
	"<tr>" ++
	case H:get_attribute(category) of
		{ok,{category,Cate}}->
			"<td>"++atom_to_list(Cate) ++"</td>";
		_->
			"<td>nodata</td>"
	end ++
	case H:get_property(name) of
		{ok,{name,Name}}->
			"<td><a href=\"/group?id="++atom_to_list(Id) ++ "\">" ++ atom_to_list(Name) ++"</a></td>";
		_->
			"<td><a href=\"/group?id="++atom_to_list(Id) ++ "\">" ++atom_to_list(Id) ++ "</a></td>"
	end ++
	"<tr>" ++obj2page(T).


val2string(V) when is_float(V)-> float_to_list(V);
val2string(V) when is_integer(V)->integer_to_list(V);
val2string(V) when is_atom(V) ->atom_to_list(V);
val2string(V)->V.