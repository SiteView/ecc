%%
%% file_monitor
%%

%% @author lei.lin@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc file monitor
-module(file_monitor,[BASE]).
-compile(export_all).
-extends(atomic_monitor).
-include("monitor.hrl").
-include("monitor_template.hrl").
-include_lib("kernel/include/file.hrl").

-export([new/0,update/0,getScalarValues/2,get_classifier/1,get_template_property/0,set_content_match/2]).

%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for directory monitor

new()->
	Base = atomic_monitor:new(),
	Base:set_attribute(status,"200"),
	Base:set_attribute(file_age,0),
	Base:set_attribute(size,0),
	Base:set_attribute(match_content,""),
	%%case ets:info(file) of
	%%undefined ->
	%%    ets:new(file,[public,named_table]);
	%%_ ->
    %%    nothing
    %%end,		
	{?MODULE,Base}.
		
%% @spec update() -> Result
%% Result = term()
%% @doc update is the run function called by schedule to test the  directory monitor
update() ->
    {ok,{_,_File_name}} = THIS:get_property(file_name),
	{ok,{_,Change}} = THIS:get_property(change),
	{ok,{_,Encoder}} = THIS:get_property(encoder),
    LocalCode = platform:getLocalCode(),
	File_nameT = iconv:convert("utf-8",LocalCode,_File_name),
	case File_nameT of
	[] ->
	    File_name = _File_name;
	_ ->
        File_name = File_nameT
    end,		
	%io:format("File_name:~s~n",[File_name]),	
	{Statu,Data} = file:read_file_info(File_name),
	case Statu of
	    ok ->
		    THIS:set_attribute(status,"200"),
            if  Data#file_info.size  < 20000000 ->            
		        {ok,Con} = file:read_file(File_name),        
		        MatchBool = match_count(Con,LocalCode),
		        if MatchBool ->
                    case Change of	
                    "no content checking" ->
		                Size = erlang:element(2,Data),
			            Time =  erlang:element(7,Data),
			            Old = (calendar:datetime_to_gregorian_seconds(erlang:localtime()) - calendar:datetime_to_gregorian_seconds(Time)) / 60,			
			            List1 = string:tokens(float_to_list(Old),"+"),
			            [List2] = lists:sublist(List1,2,1),
			            [List3] = lists:sublist(List1,1,1),
			            List4 = string:tokens(List3,"."),
			            [B] = lists:sublist(List4,1,1),
			            [E] = lists:sublist(List4,2,1), 
			            E1 = lists:sublist(E,1,list_to_integer(List2)),
			            Old2 = list_to_integer(B ++ E1),
			            THIS:set_attribute(file_age,Old2),			
                        THIS:set_attribute(size,Size),
			            Stat = integer_to_list(Size) ++ "bytes<br>" ++ B ++ E1 ++ "minutes old",
                        THIS:set_attribute(?STATE_STRING,Stat);
                    "compare to last contents" ->
                        File = ets:lookup(file,File_name),
				        case File of
                        [] ->
                            ets:insert(file,{File_name,Con});
                        [{_,D}] ->
                            if D == Con ->
		                        Size = erlang:element(2,Data),
			                    Time =  erlang:element(7,Data),
			                    Old = (calendar:datetime_to_gregorian_seconds(erlang:localtime()) - calendar:datetime_to_gregorian_seconds(Time)) / 60,			
			                    List1 = string:tokens(float_to_list(Old),"+"),
			                    [List2] = lists:sublist(List1,2,1),
			                    [List3] = lists:sublist(List1,1,1),
			                    List4 = string:tokens(List3,"."),
			                    [B] = lists:sublist(List4,1,1),
			                    [E] = lists:sublist(List4,2,1), 
			                    E1 = lists:sublist(E,1,list_to_integer(List2)),
			                    Old2 = list_to_integer(B ++ E1),
			                    THIS:set_attribute(file_age,Old2),			
                                THIS:set_attribute(size,Size),
			                    Stat = integer_to_list(Size) ++ "bytes<br>" ++ B ++ E1 ++ "minutes old",
                                THIS:set_attribute(?STATE_STRING,Stat),
                                ets:insert(file,{File_name,Con});
					        true ->
		                        Size = erlang:element(2,Data),
			                    Time =  erlang:element(7,Data),
			                    Old = (calendar:datetime_to_gregorian_seconds(erlang:localtime()) - calendar:datetime_to_gregorian_seconds(Time)) / 60,			
			                    List1 = string:tokens(float_to_list(Old),"+"),
			                    [List2] = lists:sublist(List1,2,1),
			                    [List3] = lists:sublist(List1,1,1),
			                    List4 = string:tokens(List3,"."),
			                    [B] = lists:sublist(List4,1,1),
			                    [E] = lists:sublist(List4,2,1), 
			                    E1 = lists:sublist(E,1,list_to_integer(List2)),
			                    Old2 = list_to_integer(B ++ E1),
						        THIS:set_attribute(status,"400"), 
			                    THIS:set_attribute(file_age,Old2),			
                                THIS:set_attribute(size,Size),
			                    %Stat = integer_to_list(Size) ++ "bytes<br>" ++ B ++ E1 ++ "minutes old",
                                THIS:set_attribute(?STATE_STRING,"can't match last contents"),
                                ets:insert(file,{File_name,Con})                         
                            end;				  
                        _ ->
				            ets:insert(file,{File_name,Data})
                        end;
                    "reset saved contents" ->
			            io:format("reset saved contents"),
			            File = ets:lookup(file,File_name),
				        case File of
                        [] ->
                            ets:insert(file,{File_name,Con});
                        [{_,D}] ->
                            if D == Con ->
		                        Size = erlang:element(2,Data),
			                    Time =  erlang:element(7,Data),
			                    Old = (calendar:datetime_to_gregorian_seconds(erlang:localtime()) - calendar:datetime_to_gregorian_seconds(Time)) / 60,			
			                    List1 = string:tokens(float_to_list(Old),"+"),
			                    [List2] = lists:sublist(List1,2,1),
			                    [List3] = lists:sublist(List1,1,1),
			                    List4 = string:tokens(List3,"."),
			                    [B] = lists:sublist(List4,1,1),
			                    [E] = lists:sublist(List4,2,1), 
			                    E1 = lists:sublist(E,1,list_to_integer(List2)),
			                    Old2 = list_to_integer(B ++ E1),
			                    THIS:set_attribute(file_age,Old2),			
                                THIS:set_attribute(size,Size),
			                    Stat = integer_to_list(Size) ++ "bytes<br>" ++ B ++ E1 ++ "minutes old",
                                THIS:set_attribute(?STATE_STRING,Stat);
					        true ->
		                        Size = erlang:element(2,Data),
			                    Time =  erlang:element(7,Data),
			                    Old = (calendar:datetime_to_gregorian_seconds(erlang:localtime()) - calendar:datetime_to_gregorian_seconds(Time)) / 60,			
			                    List1 = string:tokens(float_to_list(Old),"+"),
			                    [List2] = lists:sublist(List1,2,1),
			                    [List3] = lists:sublist(List1,1,1),
			                    List4 = string:tokens(List3,"."),
			                    [B] = lists:sublist(List4,1,1),
			                    [E] = lists:sublist(List4,2,1), 
			                    E1 = lists:sublist(E,1,list_to_integer(List2)),
			                    Old2 = list_to_integer(B ++ E1),
						        THIS:set_attribute(status,"401"),
			                    THIS:set_attribute(file_age,Old2),			
                                THIS:set_attribute(size,Size),
			                    %Stat = integer_to_list(Size) ++ "bytes<br>" ++ B ++ E1 ++ "minutes old",
                                Res  = file:write_file(File_name,D),  
						        case Res of
						        ok ->
						            THIS:set_attribute(?STATE_STRING,"Reset saved contents");
						        {error,enoent} ->
                                    THIS:set_attribute(?STATE_STRING,"A component of the file name does not exist.");
						        {error,enotdir} ->
						            THIS:set_attribute(?STATE_STRING,"A component of the file name is not a directory.");   
                               {error,enospc} ->
						            THIS:set_attribute(?STATE_STRING,"There is a no space left on the device.");
                               {error,eacces} ->
						            THIS:set_attribute(?STATE_STRING,"Missing permission for writing the file or searching one of the parent directories.");
                               {error,eisdir} ->
                                    THIS:set_attribute(?STATE_STRING,"The named file is a directory.")						
						        end
                            end;						
                        _ ->
				            ets:insert(file,{File_name,Data})
                        end;				
                    _ ->
                        File = ets:lookup(file,File_name),
				        case File of
                        [] ->
                            ets:insert(file,{File_name,Con});
                        [{_,D}] ->
                            if D == Con ->
		                        Size = erlang:element(2,Data),
			                    Time =  erlang:element(7,Data),
			                    Old = (calendar:datetime_to_gregorian_seconds(erlang:localtime()) - calendar:datetime_to_gregorian_seconds(Time)) / 60,			
			                    List1 = string:tokens(float_to_list(Old),"+"),
			                    [List2] = lists:sublist(List1,2,1),
			                    [List3] = lists:sublist(List1,1,1),
			                    List4 = string:tokens(List3,"."),
			                    [B] = lists:sublist(List4,1,1),
			                    [E] = lists:sublist(List4,2,1), 
			                    E1 = lists:sublist(E,1,list_to_integer(List2)),
			                    Old2 = list_to_integer(B ++ E1),
			                    THIS:set_attribute(file_age,Old2),			
                                THIS:set_attribute(size,Size),
			                    Stat = integer_to_list(Size) ++ "bytes<br>" ++ B ++ E1 ++ "minutes old",
                                THIS:set_attribute(?STATE_STRING,Stat),
                                ets:insert(file,{File_name,Con});
					        true ->
		                        Size = erlang:element(2,Data),
			                    Time =  erlang:element(7,Data),
			                    Old = (calendar:datetime_to_gregorian_seconds(erlang:localtime()) - calendar:datetime_to_gregorian_seconds(Time)) / 60,			
			                    List1 = string:tokens(float_to_list(Old),"+"),
			                    [List2] = lists:sublist(List1,2,1),
			                    [List3] = lists:sublist(List1,1,1),
			                    List4 = string:tokens(List3,"."),
			                    [B] = lists:sublist(List4,1,1),
			                    [E] = lists:sublist(List4,2,1), 
			                    E1 = lists:sublist(E,1,list_to_integer(List2)),
			                    Old2 = list_to_integer(B ++ E1),
						        THIS:set_attribute(status,"402"),
			                    THIS:set_attribute(file_age,Old2),			
                                THIS:set_attribute(size,Size),
			                    %Stat = integer_to_list(Size) ++ "bytes<br>" ++ B ++ E1 ++ "minutes old",
                                THIS:set_attribute(?STATE_STRING,"can't match last contents"),
                                ets:insert(file,{File_name,Con})                         
                            end;				  
                        _ ->
				            ets:insert(file,{File_name,Con})
                        end                               			
                    end;
                true ->
                    THIS:set_attribute(content_match,0), 
                    THIS:set_attribute(status,"201"),
                    THIS:set_attribute(size,Data#file_info.size),
                    THIS:set_attribute(file_age,round((calendar:datetime_to_gregorian_seconds(erlang:localtime()) - calendar:datetime_to_gregorian_seconds(Data#file_info.ctime)) / 60)),                     
                    THIS:set_attribute(?STATE_STRING,"content match error")  
                end; 
            true ->
                THIS:set_attribute(content_match,-1), 
                THIS:set_attribute(status,"200"),
                THIS:set_attribute(size,Data#file_info.size),
                THIS:set_attribute(file_age,round((calendar:datetime_to_gregorian_seconds(erlang:localtime()) - calendar:datetime_to_gregorian_seconds(Data#file_info.ctime)) / 60)),
                THIS:set_attribute(?STATE_STRING, integer_to_list(Data#file_info.size) ++" "++ "bytes<br>" ++ integer_to_list(round((calendar:datetime_to_gregorian_seconds(erlang:localtime()) - calendar:datetime_to_gregorian_seconds(Data#file_info.ctime)) / 60)) ++" "++"minutes old")               
            end;  
		error ->
		    case Data of
			enoent ->
			    THIS:set_attribute(match_content,""),
			    THIS:set_attribute(status,"404"),
			    THIS:set_attribute(?STATE_STRING,"The file does not exist."), 
			    THIS:set_attribute(?CATEGORY,?NO_DATA);
			eacces ->
			    THIS:set_attribute(match_content,""),
			    THIS:set_attribute(status,"405"),
			    THIS:set_attribute(?STATE_STRING,"Missing search permission for one of the parent directories of the file."), 
			    THIS:set_attribute(?CATEGORY,?NO_DATA);
			enotdir ->
			    THIS:set_attribute(match_content,""),
			    THIS:set_attribute(status,"406"),
			    THIS:set_attribute(?STATE_STRING,"A component of the file name is not a directory."), 
			    THIS:set_attribute(?CATEGORY,?NO_DATA); 
            _ ->	
                THIS:set_attribute(match_content,""),			
                THIS:set_attribute(status,"407"),
		        THIS:set_attribute(?CATEGORY,?NO_DATA),
		        THIS:set_attribute(?STATE_STRING,atom_to_list(Data))
            end				
	end.


%% @spec verify(Params) -> {ok, []} | {error, Reason}
%% Params = [term()]
%% Reason = string()
%% @doc verify is the function called by schedule to verify the parameter of monitor inputed by user
verify(Params)->
    Errs = 
	case proplists:get_value(file_name,Params) of
    ""->
	    [{file_name,"file name missing."}];
    Filename->
        File_nameT = iconv:convert("utf-8","gbk",Filename),
		{Statu,Re} = file:read_file_info(File_nameT),
        if Statu == error ->
            [{file_name,"The file does not exist or path error."}];
        true ->
            []        
		end
	end ++
    case BASE:verify(Params) of
		{error,Be}->
			Be;
		_->
			[]
	end, 
	if length(Errs)>0 ->
	    {error,Errs};
    true ->
	    {ok,""}
	end.


%% @spec get_template_property() -> List
%% List = [term()]
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
get_template_property() ->
    BASE:get_template_property() ++ 
    [
	    #property{name=file_name,title="File Name",type=text,order=1,description="Enter the full path or UNC name of the file (e.g. e:\\\\mydir\\\\myfile.log). Optionally, use a regular expression to insert date and time variables (e.g s/C:\\\\firstdir\\\\$shortYear$$0month$$0day$/)"},	
        #property{name=encoder ,title="Encoder",default="acsii",type=scalar,editable=true,advance=true,order=2,description="Please select your encoding format."},         
        #property{name=match_content,title="Match Content",type=text,advance=true,order=1,description="optional text to match against content of the file. By default successful match makes monitor good."},	
		#property{name=change,title="Check for Content Changes",default="no content checking",type=scalar,editable=true,advance=true,order=2,description="generate error if the content of the file changes - resetting the saved contents updates the contents checked against during the next monitor run"},        
        #property{name=status,title="status",type=text,order=3,configurable=false,state=true},
		#property{name=file_age,title="file age(minutes)",type=numeric,order=4,configurable=false,state=true},		
		#property{name=size,title="size(bytes)",type=numeric,order=5,configurable=false,state=true},
		#property{name=content_match,title="Content match",type=numeric,order=6,configurable=false,state=true} 
    ].


%% @spec getScalarValues(Prop,Params) -> Result
%% Result = [term()]
%% @doc getScalarValues is the run function called by schedule to get drop-downlist box
getScalarValues(Prop,Params)->
    case Prop of
	change ->
	    [{"no content checking","no content checking"},{"compare to last contents","compare to last contents"},{"compare to saved contents","compare to saved contents"},{"reset saved contents","reset saved contents"}];
	encoder ->
	    [{"ACSII","acsii"},{"UTF-8","utf-8"},{"Unicode","unicode"},{"GB2313","gb2313"},{"Big5","big5"},{"GBK","gbk"}]; 
	_ ->
	    BASE:getScalarValues(Prop,Params)
	end.	
	

%% @spec get_classifier(Param) -> List
%% Param = atom()
%% List = [Tuple]
%% Tuple = {Status, Logic, Value}
%% Status = 'error'|'warning'| 'good' 
%% Logic = '!=' | '==' | '>' | '<' | 'contain'
%% Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error)->
	Cls = case THIS:get_property(error_classifier) of
				{ok,{error_classifier,Classifier}}->
					Classifier;
				_->
					[{status,'!=',200}]
			end;
get_classifier(warning)->
	Cls =case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[]
	end;
get_classifier(good)->
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'==',200}]
	end.


match_count(Data,Encod) ->
    {ok,{_,MatchC}} = THIS:get_property(match_content), 
	Soc = binary_to_list(Data), 
    Match = iconv:convert("utf-8",Encod,MatchC), 
    case regexp:matches(Soc,Match) of
    {match,ListNum} ->        
        THIS:set_attribute(content_match,length(ListNum)),
        if length(ListNum) == 0 ->
            false;
        true ->            
            true
        end;    
    _ ->
        THIS:set_attribute(content_match,-1),
        false        
    end.

%% @spec set_content_match(Data,Encode) -> ok
%% Data = string()
%% Encode = string()
%% @doc  get the number of files matching the string
%{ok,{error_classifier,[{content_match,contains,"test"}]}}
set_content_match(Data,Encod) ->
    if Encod == "utf-8" ->
	    Soc = binary_to_list(Data);
	true ->	
	    Soc = iconv:convert(Encod,"utf-8",binary_to_list(Data))
	end,
    case  THIS:get_property(error_classifier) of
	{ok,{error_classifier,[{content_match,T1,Content1}]}} ->
		case regexp:match(Soc,Content1) of
		nomatch ->
		    io:format("~n2~n"),
			case THIS:get_property(warning_classifier) of
			{ok,{warning_classifier,[{content_match,T2,Content2}]}} ->
			    case regexp:match(Soc,Content2) of
				nomatch ->
					case THIS:get_property(good_classifier) of
				    {ok,{good_classifier,[{content_match,T3,Content3}]}} ->
						case regexp:match(Soc,Content3) of
						nomatch ->
							THIS:set_attribute(content_match,"");
					    _ ->
                            THIS:set_attribute(content_match,Content3)								
					    end;
					_ ->
                        THIS:set_attribute(content_match,"")
                    end;									
				_->
				    
					case THIS:get_property(good_classifier) of
					{ok,{good_classifier,[{content_match,T3,Content3}]}} ->
					    case regexp:match(Soc,Content3) of
						nomatch ->
						    THIS:set_attribute(content_match,Content2);
						_ ->
                            THIS:set_attribute(content_match,Content2 ++ Content3)
                        end;						
					_ ->
					    THIS:set_attribute(content_match,Content2)
					end
                end;
            _ ->
                case THIS:get_property(good_classifier) of
				{ok,{good_classifier,[{content_match,T4,Content3}]}} ->
				    case regexp:match(Soc,Content3) of 
					nomatch ->
					    THIS:set_attribute(content_match,""); 
					_ ->
					    THIS:set_attribute(content_match,Content3) 
					end;
                _ ->
                    THIS:set_attribute(content_match,"") 
                end				
			end;			
        _ ->
		    io:format("~n3~n"),
			io:format("Content1~p~n",[Content1]),
		   
			case THIS:get_property(warning_classifier) of
			{ok,{warning_classifier,[{content_match,T2,Content2}]}} ->
			    case regexp:match(Soc,Content2) of
				nomatch ->
				    case THIS:get_property(good_classifier) of
				    {ok,{good_classifier,[{content_match,T3,Content3}]}} ->
						case regexp:match(Soc,Content3) of
                        nomatch ->
						     THIS:set_attribute(content_match,Content1);
                        _ ->
                       		THIS:set_attribute(content_match,Content1 ++ Content3)			
                        end;
                    _->
                         THIS:set_attribute(content_match,Content1)
                    end;					
				_ ->
				    case THIS:get_property(good_classifier) of
				    {ok,{good_classifier,[{content_match,T3,Content3}]}} ->
						case regexp:match(Soc,Content3) of
                        nomatch ->
						     THIS:set_attribute(content_match,Content1 ++ Content2);
                        _ ->
                       		THIS:set_attribute(content_match,Content1 ++ Content3 ++ Content2)			
                        end;
                    _->
					    io:format("this is test"),
                        THIS:set_attribute(content_match,Content1 ++ Content2)
                    end									    
				end;
			_ ->
			    case THIS:get_property(good_classifier) of 
				{ok,{good_classifier,[{content_match,T3,Content3}]}} ->
			        case regexp:match(Soc,Content3) of
                    nomatch ->
						THIS:set_attribute(content_match,Content1);
                    _ ->
                        THIS:set_attribute(content_match,Content1 ++ Content3)			
                    end;
                _->
                   THIS:set_attribute(content_match,Content1)
			    end  
			end
		end;
    _Other ->
	    io:format("Other ~n~p",[_Other]),
        case THIS:get_property(warning_classifier) of
	    {ok,{warning_classifier,[{content_match,T5,Content2}]}} ->
		    io:format("Content2 ~p~n",[Content2]),
		    case regexp:match(Soc,Content2) of
			nomatch ->
			    case THIS:get_property(good_classifier) of
				{ok,{good_classifier,[{content_match,T6,Content3}]}} ->
				    case regexp:match(Soc,Content3) of
					nomatch ->
						THIS:set_attribute(content_match,"");
					_ ->
                        THIS:set_attribute(content_match,Content3)								
				    end;
				_->
					THIS:set_attribute(content_match,"")
                end;
			_ ->
			    io:format("test content match"),
			    
				case THIS:get_property(good_classifier) of
				{ok,{good_classifier,[{content_match,T6,Content3}]}} ->
				    case regexp:match(Soc,Content3) of
					nomatch ->
					    THIS:set_attribute(content_match,Content2);
					_ ->
                        THIS:set_attribute(content_match,Content2 ++ Content3)
                    end;					
				_ ->
				    THIS:set_attribute(content_match,Content2)
				end
            end;			
        _Other2 ->
		    io:format("Other2 ~p~n",[_Other2]),
            case THIS:get_property(good_classifier) of
 			{ok,{good_classifier,[{content_match,Ts7,Content3}]}} ->
			    case regexp:match(Soc,Content3) of
                nomatch ->
               	    THIS:set_attribute(content_match,"");
                _ ->
                    THIS:set_attribute(content_match,Content3)			
                end ;
            _ ->
                THIS:set_attribute(content_match,"")
            end				
        end
    end.		
	
defaultTitle(Params)->
	Path = proplists:get_value(file_name,Params),
	if
		length(Path)>0->
			BASE:defaultTitle(Params) ++":" ++ Path;
		true ->
			BASE:defaultTitle(Params)
	end.    