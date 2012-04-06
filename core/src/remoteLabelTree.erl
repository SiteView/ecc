-module(remoteLabelTree).
-compile(export_all).
-include("monitor.hrl").
-include("remoteMachine.hrl").

%%这个模块用于构造label树的相应的更、删、改、查。
%%  -record(machine_label,{
%%                                   id,
%%                                   name="undefined",  %label name
%%                                   type="nt",         %标签类型, 实际上属于标签
%%                                   index,   %引用计数 
%%                                   syslabel="false", %% 是否为系统标签
%%                                   hide="false",     %% 是否隐藏
%%                                   value="nt"          %% 标签值
%%                                   parentid="",              %%父亲id
%%                                   childrenid=[]        %%孩子id , childrenid=[{id,type}] ,type= "machine" | "label"
%%                                   }).


%%用户标签根 Parentid = UserDefine
create_Label(Parentid,Label)->
    case remoteMachineTag:getTagById(Parentid) of
    {error,Error}-> {error,Error};
    PLabel->
        Maxchild = PLabel#machine_label.maxchild,
        TreeIndex = 
        case string:substr(Maxchild,string:rstr(Maxchild,":")+1,string:len(Maxchild)) of
            []-> PLabel#machine_label.treeindex ++ ":1";
            M-> PLabel#machine_label.treeindex++":"++ integer_to_list(list_to_integer(M)+1)
        end,
        NLabel = Label#machine_label{treeindex=TreeIndex},
        NPLabel = PLabel#machine_label{maxchild=TreeIndex},
        case remoteMachineTag:update_label(NPLabel#machine_label.id,NPLabel) of
            {error,Err}-> {error,Err};
             _-> 
            case  remoteMachineTag:create_label(NLabel) of
            {ok,_}->
                {ok,NLabel#machine_label.id};
            Err->Err
            end
        end
   end.

%%删除树节点，同时删除子节点
removeLabelNode([])->{ok,"remove label succeed "};
removeLabelNode("[]")->{ok,"ok"};
removeLabelNode(Lid)->
    case remoteMachineTag:getTagById(Lid) of
    {error,Err}->{error,Err};
    L->
        Treeindex = L#machine_label.treeindex,
        Labels = remoteMachineTag:getLabel_likeindex(Treeindex),  %%找到所有子节点
        [ remoteMachineTag:remove_label(CLid#machine_label.id)||CLid<-Labels,string:str(CLid#machine_label.treeindex,L#machine_label.treeindex)=:=1],
        {ok,Lid}
    end.

    
%%更新节点
updateLabelNode([])->{ok,"update sucess"};
updateLabelNode([Label = #machine_label{}|Next])->
    case  remoteMachineTag:update_label(Label#machine_label.id,Label) of
    {ok,_}->    updateLabelNode(Next);
    Err->  Err
    end;
updateLabelNode(_Other)->{error,"unknow format"}.

%%获得一个标签下的子标签
get_children_label(Lid)->
    case remoteMachineTag:getTagById(Lid) of
    {error,Err}->{error,Err};
    L->
        Treeindex = L#machine_label.treeindex,
        Labels = remoteMachineTag:getLabel_likeindex(Treeindex++":"),  %%找到所有子节点.
        [ Label ||Label<- Labels ,string:str(Label#machine_label.id, L#machine_label.treeindex)=:=1]
    end.
    
%%查询一个标签下的全部设备.
get_allMachinesbylabel(Ids,Lid, Index, Count, Sort, SortType)->
%%  读根节点
    Labelid = 
        case is_list(Lid) of
        true->  Lid;
        _->atom_to_list(Lid)
        end,
    case remoteMachineTag:getTagById(Labelid) of
    []->[];
    {error,_}->[];
    Label->
        case Label#machine_label.type =/=?SYSTAG_USERDEFINE of
        true->        
            remoteMachineTag:get_Machine_ByTag(Ids,Labelid,Index, Count, Sort, SortType);
        false->
            Treeindex = Label#machine_label.treeindex,
            TLabels =remoteMachineTag:getLabel_likeindex(Treeindex),
            io:format("~n Label:~p, TLabels~p",[Label,TLabels]),
            Labels = [ Label1#machine_label.id ||Label1 <- TLabels,string:str(Label1#machine_label.treeindex,Treeindex)=:=1],
            remoteMachineTag:getMachineByUserDefineTags(Labels,Index, Count, Sort, SortType)
        end
    end. 
    
%%labelid 列表转化 label列表
id2Label(Ids)->
    remoteMachineTag:getLabel_byIds(Ids).

label2id([])->[];
label2id([Label|Next])->
    [Label#machine_label.id]++label2id(Next).
%%test()->
%%    writeDefaultTag().

    

    
    