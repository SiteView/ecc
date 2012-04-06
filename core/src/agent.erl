
%% @author lianbing.wang@dragonflow.com
%% @copyright 2010 siteview
%% @version 1.0
%% @doc agent 
-module(agent).
-export([test/2,get_info/3,get_errors/1,get_state_info/2,get_counters/2]).
-export([request/3,parse_json/1,build/1]).

test(Address,Port) ->
    %io:format("input:~p~n",[Port]),
    Available = case request(Address,Port,"[\"Test\",{}]") of
	error ->
	    false;
	timeout ->
	    false;
	_ ->
	    true
    end,
    io:format("Available: ~p~n",[Available]),
    Available.

get_info(_,_,[]) ->
    [];
get_info(Address,Port,[Info|T]) ->
    g_info(Address,Port,[Info|T],[]).

get_errors([]) ->
    [];
get_errors([{State,Info}|T]) ->
    io:format("~p~n",[State]),
    io:format("~p~n",[Info]),
    lists:filter(fun({S,_}) -> S == error end,[{State,Info}|T]).

get_state_info([],_) ->
    [];
get_state_info(Infos,_Split) ->
    io:format("~p~n",[Infos]),
    List = [KeyValues || {ok,[KeyValues]} <- Infos],
    output(List).
    %lists:concat(g_state_info(Infos,_Split,[])).

output([]) ->
    [];
output([{Key,Value}|T]) ->
    lists:concat([atom_to_list(Key) ++ " " ++ to_string(Value)] ++ [ "," ++ atom_to_list(_Key) ++ " " ++ to_string(_Value) || {_Key,_Value} <- T]).


request(Address,Port,Request) when is_list(Port) ->
   r(Address,list_to_integer(Port),Request);
request(Address,Port,Request) ->
   r(Address,Port,Request).

r(Address,PortNum,Request) ->
     Value = case gen_udp:open(0) of
	{ok,Socket} ->
	    case gen_udp:send(Socket,Address,PortNum,Request) of
		{error, Reason} ->
		    {error,Reason};
		ok ->
		    Receive = receive
			{udp,Socket,_,_,Bin} ->
			    {ok,Bin}
		    after 2000 ->
			    timeout
		    end,
		    gen_udp:close(Socket),
		    Receive
	    end;
	_ ->
	    error
    end,
    Value.

g_info(_,_,[],_Acc) ->
    lists:reverse(_Acc);
g_info(Address,Port,[Info|T],Acc) ->
    Request = "[\"" ++ Info ++ "\",{}]",
    case request(Address,Port,Request) of
	{ok,Response} ->
	    g_info(Address,Port,T,[extract(Response) | Acc]);
	_ ->
	    g_info(Address,Port,T,Acc)
    end.

parse_json(String)->
     case json:decode_string(String) of
	 {ok,Content} ->
	     Content
     end.

extract(Response) ->  
    case parse_json(Response) of
	{array,["error",Error]} ->
		{error,Error};
	{array,["ok",{array,[{struct,KeyValueList}]}]} ->
		{ok,KeyValueList}
    end.

get_counters(Address,Port) ->
    %io:format("~p ~p ~n",[Address,list_to_integer(Port)]),
    Result = case test(Address,list_to_integer(Port)) of
		   true ->
		        case request(Address,Port,"[\"Counters\",{}]") of
			       {ok,Response} ->
				   %~ io:format("Response Json:~p~n",[parse_json(Response)]),
				   case parse_json(Response) of
				       {array,Counters} ->
					   %~ io:format("Counters:~p~n",[Counters]),
					   build(Counters);
				       _ ->
					   []
				   end;
			       _ ->
				   []
			   end;
		       %[{"CpuInfoList","Cpu"}];		 
		   false ->
		       []%[{"0.1","b"},{"0.2","b/c"}]
	       end,
    io:format("~p~n",[Result]),
    Result.

to_string(Value) when is_integer(Value) ->
    integer_to_list(Value);
to_string(Value) when is_float(Value) ->
    float_to_list(Value);
to_string(Value) ->
    Value.

build([])->
    [];
build([H|T])->
    {array,Counter} = H,    
    [Cmd,Text,Childs] = Counter,
    
    [{Cmd,Text}] ++ parent_child(Text,Childs) ++ build(T).

parent_child(_,[]) ->
    [];
parent_child(_,{array,[]}) ->
    [];
parent_child(ParentText,{array,[ChildsH|ChildsT]}) ->
    %io:format("Child H:~p~n",[ChildsH]),
    %io:format("Child T:~p~n",[ChildsT]),
    {array,[Command,Text,Childs]} = ChildsH, 
    case Text =:= "Size" orelse Text =:= "Free" orelse Text =:= "Used" of
	true ->
		CurrentText = ParentText ++ "/" ++ Text ++ "(KB)";
	_ ->
		CurrentText = ParentText ++ "/" ++ Text
     end,
    [{Command, CurrentText}] ++ parent_child(CurrentText,Childs) ++ parent_child(ParentText,{array,ChildsT}) .
    

    
		
