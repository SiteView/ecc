%% ---
%% page_monitortemplate
%%
%%---
-module(page_monitortemplate).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").
%%-include("../core/src/monitor_template.hrl").
%%-include("../core/src/monitor.hrl").

respond(Req,Path,Raw_path)->
	Params = string:tokens(Raw_path,"&"),
	case length(Params) of
		0->
			ok;
		1->
			[P1|T] = Params,
			respond(Req,Path,Raw_path,P1,getParams(T));
		2->
			[P1,P2|_] = Params,
			respond(Req,Path,Raw_path,P1,P2,[]);
		_->
			[P1,P2|T] = Params,
			respond(Req,Path,Raw_path,P1,P2,getParams(T))
	end.

getParams([])->[];
getParams([P|T])->
	case regexp:match(P,"=") of
		nomatch->
			[] ++ getParams(T);
		{match,I,_}->
			[{list_to_atom(string:substr(P,1,I-1)),string:substr(P,I+1,length(P)-I)}] ++ getParams(T)
	end.
	

respond(Req,Path,Raw_path,"parent="++Id,Params)->
	monitortemplate_list(Id,Params);
respond(_,_,_,_,_)->"".

%%dispaly monitor add template
respond(Req,Path,Raw_path,"key="++Key,"parent="++ParentId,Params)->
	Ret = Req:parse_post(),
	NR = [{list_to_atom(X),Y} || {X,Y}<-Ret],
	io:format("respond:~p~n",[NR]),
	monitortemplate(Key,ParentId,NR ++ Params,Raw_path);
respond(Req,Path,Raw_path,"oper=edit","id="++Id,Params)->
	monitor_edit(Req,Path,Raw_path,Id,Params);
respond(_,_,_,_,_,_)->"".

%%template list
monitortemplate_list(Id,_)->
	M = api_group:info(list_to_atom(Id)),
	Name = case lists:keysearch(name,1,M) of
			{value,{name,V}}->
				atom_to_list(V);
			_->
				Id
			end,
	"<html><head><title>siteview</title></head>"
	++"<body>" ++ "<p>Add Monitor to Group:" ++ Name  ++ "</p><hr>"
	++ "<p>List of Monitor Types</p>" ++
	print_templates(api_monitor_template:get_templates(),Id) ++ 
	"</body></html>".

print_templates([],_)->"";
print_templates([M|T],Id)->
	"<p><a href=\"monitortemplate?key=" ++ atom_to_list(element(1,M)) ++ "&parent=" ++ Id ++ print_template_param(element(3,M)) ++ "\">"
	++ element(2,M) ++"</a></p>" ++ print_templates(T,Id).

print_template_param([])->"";
print_template_param([P|T])->
	{K,V} = P,
	"&" ++ atom_to_list(K) ++ "=" ++ atom_to_list(V) ++ "&" ++ print_template_param(T);
print_template_param(_)->"".

monitortemplate(Key,ParentId,Params,Raw_path)->
	MT = api_monitor_template:get_template(list_to_atom(Key)),
	[Parent|_] = api_group:find_group(list_to_atom(ParentId)),
	ParentName = case Parent:get_property(name) of
					{ok,{name,Name}}->
						atom_to_list(Name);
					_->
						ParentId
				end,
%%	F = fun(X)->case lists:keysearch(advance,1,element(2,X)) of
%%					{value,{advance,Advance}}->
%%						Advance =:= true;
%%					_->
%%						false
%%				end
%%			end,
	F = fun(X)->(X#property.advance=:=true) and (X#property.configurable=:= true)  end,
	{Adv,_} = lists:partition(F,MT),
	{Base,_} = lists:partition(fun(X)->(X#property.advance=:=false) and (X#property.configurable=:= true) end, MT),
	{Sta,_} = lists:partition(fun(X)->(X#property.state=:=true) and (X#property.configurable=:= false) end,MT),
	
	"<html><head><title>siteview</title></head>"
	++"<body>" ++ "<p>Add " ++ api_monitor_template:get_template_name(list_to_atom(Key)) ++ " to Group:" ++ ParentName ++ "</p>" ++
	"<FORM ACTION=/monitor/add?parent=" ++ ParentId ++ "&type=" ++ Key ++ " method=POST>" ++
	"<input name=class type=hidden value=" ++ Key ++ ">"++
	print_propertys(Key,Base,Params,Raw_path,[])
	++ "<input type=submit value=\"Add\"> Monitor<hr><p>Advanced Options</p>" ++
	print_propertys(Key,Adv,Params,Raw_path,[])
	++ "<input type=submit value=\"Add\"> Monitor" 
	++ "</FORM></body></html>".

print_classifier(Props)->ok.
	%%<p>"error condition£º"

print_propertys(Key,Props,Params,Raw_path,Data)->
%%	F = fun(X,Y)->
%%			Ox = case lists:keysearch(order,1,element(2,X)) of
%%					{value,{order,O1}}->
%%						O1;
%%					_->
%%						99
%%				end,
%%			Oy = case lists:keysearch(order,1,element(2,Y)) of
%%					{value,{order,O2}}->
%%						O2;
%%					_->
%%						99
%%				end,
%%			Ox >= Oy
%%		end,
	F = fun(X,Y)->(X#property.order < Y#property.order) or (X#property.order =:= Y#property.order)  end,
	OrderProps = lists:sort(F,Props),
	"<table width=800px><tr><td width=100px></td><td width=300px></td><td width=200px></td><td width=100px></td><td width=100px></td></tr>"++
	print_property(Key,OrderProps,Params,Raw_path,Data)
	++"</table>".
	
%%
%%get data property value from page 
get_property(Prop,Data,Type)->
	 case lists:keysearch(Prop,1,Data) of
			{value,{Prop,V}}->
				case Type of
					bool->
						case V of
							true->
								" checked";
							_->
								""
						end;
					text->
						atom_to_list(V);
					scalar->
						atom_to_list(V);
					frequency->
						V;
					server->
						atom_to_list(V)
				end;
			_->
				case Type of
					bool->
						"";
					text->
						"";
					scalar->
						"";
					frequency->
						0;
					server->
						""
				end
		end.

print_options([],_)->"";
print_options([O|T],Sel)->
	case  element(2,O) of
		Sel->
			"<option value=\"" ++ element(2,O) ++"\" selected>" ++ element(1,O) ++"</option>" ++ print_options(T,Sel);
		_->
			"<option value=\"" ++ element(2,O) ++"\">" ++ element(1,O) ++"</option>" ++ print_options(T,Sel)
	end.

print_property(_,[],_,_,_)->"";
print_property(Key,[P=#property{type=bool}|T],Params,Raw_path,Data)->
	"<tr><td>" ++ P#property.title ++ ":</td><td>" ++
	io_lib:format("<input name=~p type=checkbox",[P#property.name])
	++ case P#property.editable of
			true ->
				"";
			_->
				" disabled"
		end 
	++ get_property(P#property.name,Data,bool)
	++"></td></tr>" ++ print_property(Key,T,Params,Raw_path,Data);
print_property(Key,[P=#property{type=text}|T],Params,Raw_path,Data)->
	"<tr><td>" ++ P#property.title ++ ":</td><td>" ++
	io_lib:format("<input name=~p type=text",[P#property.name])
	++ case P#property.editable of
			true ->
				"";
			_->
				" disabled"
		end
	++ " value =" ++  get_property(P#property.name,Data,text)
	++"></td></tr>" ++ print_property(Key,T,Params,Raw_path,Data);
print_property(Key,[P=#property{type=scalar}|T],Params,Raw_path,Data)->
	Scalar = api_monitor_template:get_scalar_property(list_to_atom(Key),P#property.name,Params),
	"<tr><td>" ++ P#property.title ++ ":</td><td>" ++
	io_lib:format("<select name=~p >",[P#property.name]) ++ print_options(Scalar,get_property(P#property.name,Data,scalar))
	++ "</select>"
	++"</td></tr>" ++ print_property(Key,T,Params,Raw_path,Data);
print_property(Key,[P=#property{type=frequency}|T],Params,Raw_path,Data)->
	Val = get_property(P#property.name,Data,frequency),
	Unit = if
				Val=:= 0 ->
					seconds;
				Val rem 86400 =:= 0 ->
					days;
				Val rem 3600 =:=0->
					hours;
				Val rem 60 =:=0->
					minutes;
				true->
					seconds
			end,
	NV = case Unit of
			minutes->
				integer_to_list(Val div 60);
			hours->
				integer_to_list(Val div 3600);
			days->
				integer_to_list(Val div 86400);
			_->
				""
		end,
	
	"<tr><td>" ++ P#property.title ++ ":</td><td>" ++
	io_lib:format("<input name=~p type=text",[P#property.name])
	++ case P#property.editable of
			true ->
				"";
			_->
				" disabled"
		end
	++ " value =" ++  NV
	++">" 
	++ case Unit of
			minutes->
				io_lib:format("<select name=temp_unit_~p><option value=minutes selected>minutes</option><option value=hours>hours</option><option value=days>days</option></select>",[P#property.name]);
			hours->
				io_lib:format("<select name=temp_unit_~p><option value=minutes>minutes</option><option value=hours selected>hours</option><option value=days>days</option></select>",[P#property.name]);
			days->
				io_lib:format("<select name=temp_unit_~p><option value=minutes >minutes</option><option value=hours>hours</option><option value=days selected>days</option></select>",[P#property.name]);
			_->
				io_lib:format("<select name=temp_unit_~p><option value=minutes>minutes</option><option value=hours>hours</option><option value=days>days</option></select>",[P#property.name])
		end
	++"</td></tr>" ++ print_property(Key,T,Params,Raw_path,Data);
print_property(Key,[P=#property{type=server}|T],Params,Raw_path,Data)->
	Name = P#property.name,
	Url = "/server?returnUrl=" ++ mochiweb_util:quote_plus("/monitortemplate?"++ Raw_path),
	Val = case lists:keysearch(Name,1,Params) of
				{value,{Name,V}}->
					V;
				_->
					get_property(P#property.name,Data,server)
						
			end,
	DispVal = case Val of
				""->
					"this server";
				_->
					Val
				end,
	"<tr><td>" ++ P#property.title ++ ":</td><td>" ++
	"<TABLE><TR><TD ALIGN=\"left\" VALIGN=\"top\"><TABLE border=1 cellspacing=0><TR><TD>" ++
	DispVal ++
	io_lib:format("</TD></TR></TABLE></TD><TD><input type=hidden name=~p value=~s>",[P#property.name,Val]) ++
	io_lib:format("<a href=~p>choose server</a></TD></TR><TR><TD ALIGN=\"left\" VALIGN=\"top\"><FONT SIZE=-1>",[Url]) ++
	P#property.description ++
	"</FONT></TD></TR></TABLE>"
	++"</td></tr>" ++ print_property(Key,T,Params,Raw_path,Data).
	
monitor_edit(Req,Path,Raw_path,Id,Params)->
	Ret = Req:parse_post(),
	NP = [{list_to_atom(X),Y} || {X,Y}<-Ret],
	Gi = api_group:info(list_to_atom(Id)),
	{value,{?CLASS,Class}} = lists:keysearch(?CLASS,1,Gi),
	MT = api_monitor_template:get_template(Class),
	TeName = api_monitor_template:get_template_name(Class),
	Name = page_monitortemplate:get_property(?NAME,Gi,text),
	{Adv,_} = lists:partition(fun(X)->(X#property.advance=:=true) and (X#property.configurable=:= true)  end,MT),
	{Base,_} = lists:partition(fun(X)->(X#property.advance=:=false) and (X#property.configurable=:= true) end, MT),
	"<html><head><title>siteview</title></head>" ++
	"<body>"++
	"<h2>Edit "++ TeName ++ " in" ++ page_group:printfullpath(api_siteview:find_object(api_siteview:get_parent_id(list_to_atom(Id)))) ++"</h2>" ++
	"<form action=/monitor?oper=update method=POST>" ++
	"<input type=hidden name=id value=" ++ Id ++ ">" ++
	"<input type=hidden name=class value=" ++ atom_to_list(Class) ++ ">" ++
	page_monitortemplate:print_propertys(atom_to_list(Class),Base,NP++Params++[{K,page_siteview:val2string(V)}||{K,V}<-Gi],Raw_path,Gi) ++
	"<p><input type=submit VALUE=\"Update\">  Monitor</p>" ++
	"<hr>" ++
	"<H3>Advanced Options</H3>" ++
	page_monitortemplate:print_propertys(atom_to_list(Class),Adv,NP++Params++[{K,page_siteview:val2string(V)}||{K,V}<-Gi],Raw_path,Gi) ++
	"<p><input type=submit VALUE=\"Update\">  Monitor</p>" ++
	"</form>"++
	"</body></html>".

