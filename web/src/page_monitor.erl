%%
%% page_monitor
%%
%%
-module(page_monitor).
-compile(export_all).
-include("monitor_template.hrl").
%% -include("../../core/src/monitor_template.hrl").

respond(Req,Path,Raw_path)->
	Params = string:tokens(Raw_path,"&"),
	case length(Params) of
		0->
			ok;
		1->
			[P1|_] = Params,
			respond(Req,Path,Raw_path,P1);
		2->
			[P1,P2|_] = Params,
			respond(Req,Path,Raw_path,P1,P2);
		_->
			ok
	end.

getParams([])->[];
getParams([P|T])->
	case regexp:match(P,"=") of
		nomatch->
			[] ++ getParams(T);
		{match,I,_}->
			[{list_to_atom(string:substr(P,1,I-1)),list_to_atom(string:substr(P,I+1,length(P)-I))}] ++ getParams(T)
	end.
	
	
respond(Req,Path,Raw_path,"oper=update")->
	Ret = Req:parse_post(),
	io:format("monitor_add:~p~n",[Ret]),
	{value,{"class",Type}}=lists:keysearch("class",1,Ret),
	{value,{"id",Id}} = lists:keysearch("id",1,Ret),
	Parent= api_siteview:get_parent_id(list_to_atom(Id)),
	Temp = api_monitor_template:get_template(list_to_atom(Type)),
	Default = template_to_default_props(Temp),
	NewData = lists:ukeymerge(1,lists:ukeysort(1,Ret),lists:ukeysort(1,Default)),
	{Unit,Props} = lists:partition(fun({K,V})->case regexp:match(K,"temp_unit") of {match,1,_}->true;_->false end end,NewData),
	NR = filter_props(NewData,Temp,Unit),
	R = case api_monitor:update(NR) of
			{ok,_}->
				"Update Success.";
			{error,Reason}->
				"Update Fail:" ++ atom_to_list(Reason)
		end,
	"<html><head><title>siteview</title></head>"++
	"<body><p>" ++ R ++ "</p><p><a href=\"/group?id=" ++ atom_to_list(Parent) ++  "\">Return Back</a></p></body></html>";
respond(Req,Path,Raw_path,"oper=remove")->
	Ret = Req:parse_post(),
	{value,{"id",Id}}=lists:keysearch("id",1,Ret),
	Parent= api_siteview:get_parent_id(list_to_atom(Id)),
	R = case api_monitor:delete(list_to_atom(Id)) of
			{ok,_}->
				"Delete Success.";
			{error,Reason}->
				"Delete Fail:" ++ atom_to_list(Reason)
		end,
	"<html><head><title>siteview</title></head>"++
	"<body><p>" ++ R ++ "</p><p><a href=\"/group?id=" ++ atom_to_list(Parent) ++  "\">Return Back</a></p></body></html>";
respond(_,_,_,_)->not_found.
	

respond(Req,Path,Raw_path,"parent="++Parent,"type=" ++ Type)->
	Ret = Req:parse_post(),
	%%io:format("monitor_add:~p~n",[Ret]),
	Temp = api_monitor_template:get_template(list_to_atom(Type)),
	Default = template_to_default_props(Temp),
	NewData = lists:ukeymerge(1,lists:ukeysort(1,Ret),lists:ukeysort(1,Default)),
	{Unit,Props} = lists:partition(fun({K,V})->case regexp:match(K,"temp_unit") of {match,1,_}->true;_->false end end,NewData),
	NR = filter_props(NewData,Temp,Unit),
	R = case api_monitor:create(list_to_atom(Parent),NR) of
			{ok,_}->
				"Add Success.";
			{error,Reason}->
				"Add Fail:" ++ atom_to_list(Reason)
		end,
	"<html><head><title>siteview</title></head>"++
	"<body><p>" ++ R ++ "</p><p><a href=\"/group?id=" ++ Parent ++  "\">Return Back</a></p></body></html>";

respond(Req,Path,Raw_path,"oper=delete","id=" ++ Id)->
	monitor_delete(Id);
respond(_,_,_,_,_)->not_found.

monitor_delete(Id)->
	"<html><head><title>Delete Monitor</title></head>"++
	"<h2>Delete Monitor</h2>" ++
	"<p>Are you sure you want to delete the monitor\"" ++ atom_to_list(api_siteview:get_object_name(list_to_atom(Id))) ++ 
	"\"from the <a href=\"/group?id=" ++ atom_to_list(api_siteview:get_parent_id(list_to_atom(Id))) ++ "\">"++  
	atom_to_list(api_siteview:get_parent_name(list_to_atom(Id))) ++ "</a> Group</p>" ++ 
	"<form action=/monitor?oper=remove method=POST>" ++
	"<input type=hidden name=id value=" ++ Id ++ ">" ++
	"<p><input type=submit value=\"Delete Monitor\"></p>" ++
	"</form></body></html>".
	

template_to_default_props([])->[];
template_to_default_props([P|T])->
	V =case P#property.type of
		bool->
			{atom_to_list(P#property.name),""};
		frequency->
			{atom_to_list(P#property.name),0};
		_->
			{atom_to_list(P#property.name),""}
		end,
	[V] ++ template_to_default_props(T).
	
filter_props([],_,_)->[];
filter_props([{K,V}|T],Temp,Unit)->
	case K of
		"id"->
			[{list_to_atom(K),list_to_atom(V)}] ++ filter_props(T,Temp,Unit);
		"class"->
			[{list_to_atom(K),list_to_atom(V)}] ++ filter_props(T,Temp,Unit);
		_->
			NK = list_to_atom(K),
			case find_property_temp(NK,Temp) of
				{ok,Te}->
					case Te#property.type of
						frequency->
							[{NK,get_number(V) * find_property_unit(K,Unit)}] ++ filter_props(T,Temp,Unit);
						bool->
							case V of
								"on" ->
									[{NK,true}] ++ filter_props(T,Temp,Unit);
								_->
									[{NK,false}] ++ filter_props(T,Temp,Unit)
							end;
						numeric->
							[{NK,get_number(V)}] ++ filter_props(T,Temp,Unit);
						_->
							[{NK,list_to_atom(V)}] ++ filter_props(T,Temp,Unit)
					end;
				_->
					filter_props(T,Temp,Unit)
			end
	end.

	
find_property_temp(K,[])->{error,not_found};		
find_property_temp(K,[Temp|T])->
	case Temp#property.name of
		K->
			{ok,Temp};
		browse->
			%io:format("find_property_temp:~p~n",[K]),
			case string:rstr(atom_to_list(K),"browser_counter") of
				1->
					{ok,Temp};
				_->
					find_property_temp(K,T)
			end;
		_->
			find_property_temp(K,T)
	end.

find_property_unit(K,[])->1;
find_property_unit(K,[U|T])->
	{UK,UV} = U,
	case UK of
		"temp_unit_" ++ K->
			case UV of
				"minutes"->
					60;
				"hours"->
					3600;
				"days"->
					86400
			end;
		_->
			find_property_unit(K,T)
	end.

get_number(V)->
	case string:to_float(V) of
		{F,[]}->
			F;
		_->
			case string:to_integer(V) of
				{I,[]}->
					I;
				_->
					0
			end
	end.

