-module(local_group_info).
-compile(export_all).
-define(maxLength,10).

-export([info/0]).

%%-record(group,{siteview,id,title,category,statestring,level}).
%%-record(monitor,{gid,id,title,category,statestring}).

info() ->
    Contents = local_group_info:parse_siteview(),
    "<!--CURRENTSTATE\n"++
    Contents++
    "ENDCURRENTSTATE-->\n".

%%注意最后显示的顺序
parse_siteview() ->
    [SV|_] = api_siteview:get_nodes(),
    Siteview = element(1,SV),
	BasicGroups = api_group:childs(Siteview),
    Groups = parse_group(BasicGroups,atom_to_list(Siteview),[]),
    create_content(Groups,"").
    
parse_group([],_,Groups) ->Groups;
parse_group([F|R],Siteview,Groups) ->
    Gid = proplists:get_value(id,F),
    GroupLevel = proplists:get_value(level,F,0),
    Group = [Siteview,atom_to_list(Gid),proplists:get_value(name,F),atom_to_list(proplists:get_value(category,F)),proplists:get_value(state_string,F),integer_to_list(GroupLevel)],
    {Sub_Groups,Sub_Monitors} = filter(api_group:childs(Gid),Siteview,GroupLevel+1,[],[]),
    parse_group(Sub_Groups++R,Siteview,Groups++[[Group]++Sub_Monitors]).

filter([],_,_,Monitors,Groups)->{Groups,Monitors};
filter([F|R],Siteview,Level,Monitors,Groups) ->
    Class = proplists:get_value(class,F),
    if
        Class==group ->
            filter(R,Siteview,Level,Monitors,Groups++[F++[{level,Level}]]);
        true ->
            Monitor = [atom_to_list(proplists:get_value(parent,F)),atom_to_list(proplists:get_value(id,F)),proplists:get_value(name,F),atom_to_list(proplists:get_value(category,F)),""],
            filter(R,Siteview,Level,Monitors++[Monitor],Groups)
    end.

create_content([],Content) ->Content;
create_content([Group|R],Content) ->
    create_content(R,Content++create_each_group(Group,"")).
    
create_each_group([],Groupinfo)->Groupinfo;
create_each_group([F|R],Groupinfo) ->
    Info = [X++"\t"||X<-F],
    create_each_group(R,Groupinfo++lists:flatten(Info)++"\n").



    