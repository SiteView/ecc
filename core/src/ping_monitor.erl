%%
%% ping monitor
%%

%% @author lei.lin@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc ping monitor
-module(ping_monitor,[BASE]).
-compile(export_all).
-extends(atomic_monitor).
-include("monitor.hrl").
-include("monitor_template.hrl").

-export([new/0,defaultTitle/1,update/0,get_classifier/1,get_template_property/0,verify/1]).

%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for ping monitor
new()->
	Base = atomic_monitor:new(),
	{?MODULE,Base}.

%% @spec update() -> Result
%% Result = term()
%% @doc update is the run function called by schedule to test the  ping monitor
update()->
    THIS:set_attribute(roundTripTime,"n/a"),
	% THIS:set_attribute(percentgood,"n/a"),
	THIS:set_attribute(packetsgood,"n/a"),
    {ok,{_,Hostname}} = THIS:get_property(hostname),
	{ok,{_,Timeout}} = THIS:get_property(timeout),
	{ok,{_,Size}} = THIS:get_property(size),		
    {Osfamily, Osname} = os:type(),
	    case Osfamily of 
		    win32 -> 
			    Cmd = "ping -n 4 -l " ++ integer_to_list(Size) ++ " -w " ++ integer_to_list(Timeout) ++ "  " ++Hostname,
		        %io:format("Cmd:~p~n",[Cmd]),
				{Timer,Data} = timer:tc(os,cmd,[Cmd]),
				%%io:format("Data Data Data Data Data:~p~n",[Data]),
                %Data = os:cmd(Cmd),
				Li = string:tokens(Data,"\r\r\n"),
				Num = length(Li),
                [String] = lists:sublist(Li,Num,1),
		
				List = string:tokens(String,","),
				N = length(List),
				[Lasts] = lists:sublist(List,N,1),
				%%io:format("Lasts Lasts Lasts Lasts Lasts:~p~n",[Lasts]),
				Index1 = string:str(Lasts,"Average"),
				Index2 = string:str(Lasts,"Æ½¾ù"),
				if Index1 > 0 ->
				    if Index2 > 0 ->					
					    Bool = true;
					true ->
                        Bool = true
                    end;
                true ->
				    if Index2 > 0 ->
					    Bool = true;
					true ->
					    Bool = false
                    end					
				end,
                %io:format("Bool:~p~n",[Bool]),				
				if  Bool ->
				    Av = string:tokens(Lasts,"="),
                        [Time] = lists:sublist(Av,length(Av),length(Av)),
                        Mi = lists:sublist(Time,2,length(Time)-3),
                    	THIS:set_attribute(roundTripTime,list_to_integer(Mi)),
                        [Pack] = lists:sublist(Li,Num-2,1),
                        %io:format("Pack:~p~n",[Pack]),						
                		_P = string:tokens(Pack,","),
						Length = length(_P),
						if Length == 1 ->
						    BM = 1,
						    P = string:tokens(Pack,"£¬");
						true ->
						    BM =2,
                            P = _P
                        end,							
						[Received] = lists:sublist(P,2,1),
						%io:format("Received:~p~n",[Received]),
						[Loss] = lists:sublist(P,3,1),
						%io:format("Loss:~p~n",[Loss]),
						Con = string:tokens(Loss," "),
						[Rn] = lists:sublist(string:tokens(Received,"="),2,1),
						ReceNum = lists:sublist(Rn,2,length(Rn)-1),
						%io:format("ReceNum~p~n",[ReceNum]),
                        THIS:set_attribute(round_trip_time,list_to_integer(Mi)),
						% THIS:set_attribute(packetsgood,list_to_integer(ReceNum)),						
						THIS:set_attribute(packetsgood,(list_to_integer(ReceNum)/4) * 100),						
						if BM == 1  ->
						    _Lasts = iconv:convert("gb2312","utf-8",Lasts),
			                THIS:set_attribute(?STATE_STRING,_Lasts);
						true ->
                            THIS:set_attribute(?STATE_STRING,Lasts)
                        end;						  
				true ->
                    THIS:set_attribute(round_trip_time,"n/a"),
                    THIS:set_attribute(percentgood,"n/a"),
                    THIS:set_attribute(packetsgood,"n/a"),
                    THIS:set_attribute(?NO_DATA,true),
                    THIS:set_attribute(?CATEGORY,?NO_DATA),
					THIS:set_attribute(?STATE_STRING,"timeout")
                end;					
            unix ->
			    Cmd = "ping -c 4 -s " ++ integer_to_list(Size)  ++ "  " ++Hostname,
                %Data =  os:cmd(Cmd),
				{Time1,Data} = timer:tc(os,cmd,[Cmd]),
                Li = string:tokens(Data,"\n"),
				[Stat] = lists:sublist(Li,7,1),
				Stat2 = string:tokens(Stat,","),
				[Rece] = lists:sublist(Stat2,2,1),
				[Re] = lists:sublist(string:tokens(Rece," "),1,1),
				% THIS:set_attribute(packetsgood,list_to_integer(Re)),
				[Perg] = lists:sublist(Stat2,length(Stat2)-1,1),
				[Pg] = lists:sublist(string:tokens(Perg," "),1,1),
				[LossN] = string:tokens(Pg,"%"),   				
				THIS:set_attribute(packetsgood,100-list_to_integer(LossN)),
				[Time] = lists:sublist(Stat2,length(Stat2),1),
				Ti = string:tokens(Time," "),
				[Ms] = lists:sublist(Ti,length(Ti),1),
				[Mt] = string:tokens(Ms,"ms"),
				THIS:set_attribute(roundTripTime,list_to_integer(Mt)),
				THIS:set_attribute(round_trip_time,list_to_integer(Mt)),
				%THIS:set_attribute(?CATEGORY,good),
				THIS:set_attribute(?STATE_STRING,Stat)
        end.


%% @spec verify(Params) -> {ok, []} | {error, Reason}
%% Params = [term()]
%% Reason = string()
%% @doc verify is the function called by schedule to verify the parameter of monitor inputed by user
verify(Params)->
    Errs = 
	case proplists:get_value(timeout,Params) of
    ""->
	    [{timeout,"timeout missing."}];
    Time->
		if
			not is_number(Time) ->
				[{timeout,"timeout must be a number."}];
			true->
				[]
		end
	end ++
	case proplists:get_value(hostname,Params) of
    ""->
		[{hostname,"Host Name missing"}];
    Host->
	    case string:rstr(Host," ") of
		    0->
			    [];
			_->
			    [{hostname,"no spaces are allowed"}]
	    end
	end ++
    case proplists:get_value(size,Params) of
    "" ->
        [{size,"size missing."}];
    Size ->
       if not is_number(Size) ->
          
		    [{size,"packet size must be a positive number"}];
       true ->
            if Size > 60000 ->
                [{size,"packet size must be a number between 1 and 60000"}];
            true ->
                []
            end
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

%% @spec defaultTitle(Params) ->string()
%%  Params = [term()]
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	Host = proplists:get_value(hostname,Params),
	if
		length(Host)>0->
			BASE:defaultTitle(Params) ++":" ++ Host;
		true ->
			BASE:defaultTitle(Params)
	end.
	
getHostname()->
	case THIS:get_property(hostname) of
		{ok,{_,V}}->
			V;
		_->
			BASE:getHostname()
	end.
	
%% @spec get_template_property() ->list()
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
get_template_property() ->
    BASE:get_template_property() ++ 
    [
	    #property{name=hostname,title="Host Name",type=text,order=1,description="Monitoring of the host name"},
		#property{name=timeout,title="Timeout",type=numeric,advance=true,default= 3000,order=1,description="the time out, in seconds, to wait for the response",baselinable=true},
		#property{name=size,title="Size",type=numeric,advance=true,order=2,default=32,description="Packet size"},
        % #property{name=percentgood,title="percentGood",type=numeric,order=3,configurable=false,state=true,upIsBad=false,baselinable=true},
        #property{name=packetsgood,title="% packets good%",type=numeric,order=4,configurable=false,state=true,upIsBad=false,baselinable=true},
        #property{name=round_trip_time,title="round trip time(milliseconds)",type=numeric,order=5,configurable=false,state=true,baselinable=true}		
	].

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
					[{packetsgood,'==',0}]
			end;
get_classifier(warning)->
	Cls =case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{packetsgood,'<=',75}]
	end;
	
get_classifier(good)->
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{percentgood,'>',75}]
	end.