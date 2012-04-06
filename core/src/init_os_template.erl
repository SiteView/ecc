%%
%% init os template
%%

%% @author lei.lin@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc init os template
-module(init_os_template).
-compile(export_all).

-define(LOGDIR,"logs").

-export([start/0]).

%% @spec start() -> ok
%% @doc  start init  ets tables from os template config file
start() ->
    init_template().
    
%%
init_template() ->
    OsNum = platform:getOs(),
    case file:list_dir("template.os") of
    {ok,TemplateFileList} ->
        write_ets(TemplateFileList,OsNum);   
    {error,_Reason} ->
        io:format("init os template ets tables false!")
        %log:error("init_os_template","1","init os template ets tables false," ++ Reason)
    end.
    
    
write_ets(FileList,OsNum) ->
    write_ets_t(FileList,length(FileList),OsNum).
write_ets_t(_L,0,_OsNum) -> ok;    
write_ets_t(Li,Num,OsNum) ->
    [A|B] = Li,
    Index = string:str(A,".config"),
    if Index > 0 ->
        case OsNum of
        1 ->
            FilePath = "template.os" ++ "\\" ++ A;
        _ ->
            FilePath = "template.os" ++ "/" ++ A
        end,
        case file:consult(FilePath) of
        {error,_Reason} ->
            write_ets_t(B,Num-1,OsNum);
        {ok,Res} ->
            [Ets|_] = string:tokens(A,"."),
            EtsTable = list_to_atom(Ets),
            case ets:info(EtsTable) of
            undefined ->
                ets:new(EtsTable,[public,named_table]),         
		        insertEts_from_List(EtsTable,Res),
                write_ets_t(B,Num-1,OsNum);
            _ ->
                write_ets_t(B,Num-1,OsNum)
            end;
        _ ->
            write_ets_t(B,Num-1,OsNum)
        end;
    true ->
        write_ets_t(B,Num-1,OsNum) 
    end.    
        
insertEts_from_List(Tab,List) ->
insertEts_from_List_t(Tab,List,length(List)).
insertEts_from_List_t(_T,_L,0) ->ok;
insertEts_from_List_t(T,L,N) ->
    [A|B] = L,
	ets:insert(T,A),
	insertEts_from_List_t(T,B,N-1).        