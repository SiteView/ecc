%%handle.erl
%% @author linlei <lei.lin@dragonflow.com>
%% @copyright 2009.

-module(handle).
-behaviour(gen_server). 


-include("tt.hrl").
-include("handle.hrl").

-export([start/0]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export([handler/3,get/0]).

-record(state,{pid,info,messageid,ip}).

start() ->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).
	
init([]) ->
    {ok,AttemperPid} = test:start(),     
    {ok,#state{pid = AttemperPid}}.
	
% external interface
handler(Ip,Header,Rec) ->
    %io:format("Ip:~p~n",[Ip]),
    %io:format("Header:~p~n",[Header]),
	%io:format("Rec:~p~n",[Rec]),
    gen_server:call(?MODULE,{Ip,Header,Rec}).


handle_call({_Ip,_Header,[{'cwmp:Fault', AnyAttribs, FaultCode, FaultString, SetParameterValuesFault}]},_From,State) ->
    ID = random:uniform(10000),
	_Id = integer_to_list(ID),
	Header =  #'soap:Header'{choice=[#'cwmp:ID'{'P:mustUnderstand'="1",'#text'=_Id},#'cwmp:HoldRequests'{'P:mustUnderstand'="1",'#text'="false"}]}, 

	%send the error msg to inquiry process, wiat for reponse
    MessageId = State#state.messageid,	
	CPEId = (State#state.info)#'cwmp:Inform'.'DeviceId',
    ?QUERY_SERVER ! {acs_response,self(),CPEId, MessageId, {'cwmp:Fault', AnyAttribs, FaultCode, FaultString, SetParameterValuesFault}},
	receive
	    {acs_response_response, _Response} ->
		    ?QUERY_SERVER ! {acs_request,self(),(State#state.info)#'cwmp:Inform'.'DeviceId'},
			    receive
					{acs_request_response,MessageId, undefined} ->
                        Reply =  {ok,undefined},
		                State1 = State; 				
				    {acs_request_response,MessageId, Request} ->
					    State1 = State#state{messageid = MessageId},
		                Reply = {ok,Header,[processrecord(Request)]};
                    {error, _Reason} ->
					    State1 = State, 
                        Reply =  {ok,undefined};						
					_ ->
					    State1 = State,
                        Reply =  {ok,undefined}
	                after ?TIMEOUT ->
		                State1 = State, 
	                    Reply =  {ok,undefined}					
                end;						
		{error, _Reason} ->
		    State1 = State,
            Reply =  {ok,undefined};
		_ ->
            State1 = State,
            Reply =  {ok,undefined}		
	    after ?TIMEOUT ->
		    State1 = State,
	        Reply =  {ok,undefined}
    end, 
    %Reply = {ok,undefined},	
    {reply,Reply,State1};



%  process CPE RPCMethodsResponse 
handle_call({_Ip,_Header,[{'cwmp:GetRPCMethodsResponse', AnyAttribs, MethodList}]},_From,State) ->
    ID = random:uniform(10000),
	_Id = integer_to_list(ID),
	Header =  #'soap:Header'{choice=[#'cwmp:ID'{'P:mustUnderstand'="1",'#text'=_Id},#'cwmp:HoldRequests'{'P:mustUnderstand'="1",'#text'="false"}]}, 

    MessageId = State#state.messageid, 
	CPEId = (State#state.info)#'cwmp:Inform'.'DeviceId',
    ?QUERY_SERVER ! {acs_response,self(),CPEId, MessageId, {'cwmp:GetRPCMethodsResponse', AnyAttribs, MethodList}},
	receive
	    {acs_response_response, _Response} ->
		    ?QUERY_SERVER ! {acs_request,self(),(State#state.info)#'cwmp:Inform'.'DeviceId'},
			    receive
					{acs_request_response,MessageId, undefined} ->
                        Reply =  {ok,undefined},
		                State1 = State; 				
				    {acs_request_response,MessageId, Request} ->
					    State1 = State#state{messageid = MessageId},
		                Reply = {ok,Header,[processrecord(Request)]};
                    {error, _Reason} ->
					    State1 = State, 
                        Reply =  {ok,undefined};						
					_ ->
					    State1 = State,
                        Reply =  {ok,undefined}
	                after ?TIMEOUT ->
		                State1 = State, 
	                    Reply =  {ok,undefined}						
                end;						
		{error, _Reason} ->
		    State1 = State,
            Reply =  {ok,undefined};
		_ ->
            State1 = State,
            Reply =  {ok,undefined}
        after ?TIMEOUT ->
		    State1 = State,
	        Reply =  {ok,undefined}
    end,
	%io:format("Header:~p~n",[Header]),
    io:format("MethodList:~p~n",[MethodList]), 	
	%Reply = {ok,[#'cwmp:GetParameterNames'{'ParameterPath' = "",'NextLevel' = false}]},
    %Reply = {ok,undefined},	
    {reply,Reply,State1};    


%process TransferComplete msg
handle_call({_Ip,_Header,[{'cwmp:TransferComplete', AnyAttribs, CommandKey, FaultStruct, StartTime, CompleteTime}]},_From,State) ->
    ID = random:uniform(10000),
	_Id = integer_to_list(ID),
	Header =  #'soap:Header'{choice=[#'cwmp:ID'{'P:mustUnderstand'="1",'#text'=_Id},#'cwmp:HoldRequests'{'P:mustUnderstand'="1",'#text'="false"}]}, 

    MessageId = State#state.messageid, 
	CPEId = (State#state.info)#'cwmp:Inform'.'DeviceId',
    ?QUERY_SERVER ! {acs_response,self(),CPEId, MessageId, {'cwmp:TransferComplete', AnyAttribs, CommandKey, FaultStruct, StartTime, CompleteTime}},
    receive
	    {acs_response_response, _Response} ->
		    ?QUERY_SERVER ! {acs_request,self(),(State#state.info)#'cwmp:Inform'.'DeviceId'},
			    receive
					{acs_request_response,MessageId, undefined} ->
                        Reply =  {ok,undefined},
		                State1 = State; 				
				    {acs_request_response,MessageId, Request} ->
					    State1 = State#state{messageid = MessageId},
		                Reply = {ok,Header,[processrecord(Request)]};
                    {error, _Reason} ->
					    State1 = State,
                        Reply =  {ok,undefined};						
					_ ->
					    State1 = State,
                        Reply =  {ok,undefined}
	                after ?TIMEOUT ->
		                State1 = State, 
	                    Reply =  {ok,undefined}									
                end;						
		{error, _Reason} ->
		    State1 = State,
            Reply =  {ok,undefined};
		_ ->
            State1 = State,
            Reply =  {ok,undefined}			
	    after ?TIMEOUT ->
		    State1 = State, 
	        Reply =  {ok,undefined}
    end,
    %Reply = {ok,[#'cwmp:TransferCompleteResponse'{}]},
	%Reply = {ok,undefined},
	{reply,Reply,State1};

%process GetParameterNames response message
handle_call({_Ip,_Header,[{'cwmp:GetParameterNamesResponse', AnyAttribs, ParameterList}]},_From,State) ->
    ID = random:uniform(10000),
	_Id = integer_to_list(ID),
	Header =  #'soap:Header'{choice=[#'cwmp:ID'{'P:mustUnderstand'="1",'#text'=_Id},#'cwmp:HoldRequests'{'P:mustUnderstand'="1",'#text'="false"}]}, 

    MessageId = State#state.messageid, 
	CPEId = (State#state.info)#'cwmp:Inform'.'DeviceId',	
    ?QUERY_SERVER ! {acs_response,self(),CPEId, MessageId, {'cwmp:GetParameterNamesResponse', AnyAttribs, ParameterList}},
    receive
	    {acs_response_response, _Response} ->
		    ?QUERY_SERVER ! {acs_request,self(),(State#state.info)#'cwmp:Inform'.'DeviceId'},
			    receive
					{acs_request_response,MessageId, undefined} ->
                        Reply =  {ok,undefined},
		                State1 = State; 				
				    {acs_request_response,MessageId, Request} ->
					    State1 = State#state{messageid = MessageId},
		                Reply = {ok,Header,[processrecord(Request)]};
                    {error, _Reason} ->
					    State1 = State,
                        Reply =  {ok,undefined};						
					_ ->
					    State1 = State,
                        Reply =  {ok,undefined}
	                after ?TIMEOUT ->
		                State1 = State, 
	                    Reply =  {ok,undefined}									
                end;						
		{error, _Reason} ->
		    State1 = State,
            Reply =  {ok,undefined};
		_ ->
            State1 = State,
            Reply =  {ok,undefined}			
	    after ?TIMEOUT ->
		    State1 = State, 
	        Reply =  {ok,undefined}
    end,
	%io:format("ParameterList:~p~n",[ParameterList]),
    InfoStruct = ParameterList#'cwmp:ParameterInfoList'.'ParameterInfoStruct',
    %io:format("ParameterList#'cwmp:ParameterInfoList'.'ParameterInfoStruct~p~n",[InfoStruct]),
    io:format("processparameterinfostruct(InfoStruct):~p~n",[processparameterinfostruct(InfoStruct)]),    
    %State2 = State#state{info = {'cwmp:GetParameterNamesResponse', AnyAttribs, ParameterList}},
    %Reply = {ok,[#'cwmp:GetParameterValues'{'ParameterNames' = #'cwmp:ParameterNames'{anyAttribs = [{{"xsi:type", []},"SOAP-ENC:Array"},{{"SOAP-ENC:arrayType", []},"xsd:string[1]"}],'string' = ["DeviceInfoModule.HardwareVersionModule"]}}]},	
    %Reply = {ok,undefined},
	{reply,Reply,State1};
    

%-record('cwmp:XFileEvent', {anyAttribs, 'DeviceId', 'Event', 'OUI'}).
%-record('cwmp:XFileEventResponse', {anyAttribs}).
%-record('cwmp:Inform', {anyAttribs, 'DeviceId', 'Event', 'MaxEnvelopes', 'CurrentTime', 'RetryCount', 'ParameterList'}).
%% process Inform request
handle_call({Ip,Header,[{'cwmp:Inform',AnyAttribs,DeviceId, Event, MaxEnvelopes, CurrentTime, RetryCount, ParameterList}]},_From,State) ->
	EventList = Event#'cwmp:EventList'.'EventStruct',
	EventCodeList = geteventcode(EventList),
	Boot = lists:any(fun(X) -> X == "1 BOOT" end,EventCodeList),
	Value = lists:any(fun(X) -> X == "4 VALUE CHANGE" end,EventCodeList),
    io:format("Boot:~p~n",[Boot]),
    io:format("Value:~p~n",[Value]),
    if Boot == true ->
		    %io:format("test");
		    ?QUERY_SERVER ! {acs_post,#'cwmp:Boot'{'DeviceId' = DeviceId,'Ip' = Ip,'MaxEnvelopes' = MaxEnvelopes, 'CurrentTime' = CurrentTime, 'RetryCount' = RetryCount}};
	    true ->
	        io:format("Not boot~n")
	end,
    if 	Value == true ->
	        %io:format("test1");
            ?QUERY_SERVER ! {acs_post,#'cwmp:ValueChange'{'DeviceId' = DeviceId,'Ip' = Ip,'MaxEnvelopes' = MaxEnvelopes, 'CurrentTime' = CurrentTime, 'RetryCount' = RetryCount,'ParameterList' = ParameterList}};
	    true ->
		    io:format("Not valuechange~n")
	end,
	%io:format("EventCodeList:~p~n",[EventCodeList]),
	Xeventlist = get_x_event_code(EventCodeList),
	%io:format("Xeventlist:~p~n",[Xeventlist]),
	Xlength = length(Xeventlist),
	%io:format("Xlength:~p~n",[Xlength]),
	if Xlength == 0 ->
	        io:format("Not X event~n");
		true ->
		    process_x_event(Xeventlist,DeviceId) 
    end,			
	State2 = State#state{info = {'cwmp:Inform', AnyAttribs, DeviceId, Event, MaxEnvelopes, CurrentTime, RetryCount, ParameterList}},
	if Header /= undefined ->
	        _Header = #'soap:Header'{choice= Header},
            Reply = {ok,_Header,[#'cwmp:InformResponse'{'MaxEnvelopes' = "0"}]};
	   	true ->
		    Reply = {ok,Header,[#'cwmp:InformResponse'{'MaxEnvelopes' = "0"}]}
	end,		
    {reply,Reply,State2};




%process CPE call ACS GetRPCMethods method
handle_call({_Ip,Header,[{'cwmp:GetRPCMethods', _AnyAttribs}]},_From,State) ->
	if Header /= undefined ->
	        _Header = #'soap:Header'{choice= Header},
			Reply = {ok,_Header,[#'cwmp:GetRPCMethodsResponse'{anyAttribs= [{{"xsi:type", []},"SOAP-ENC:Array"},{{"SOAP-ENC:arrayType", []},"xsd:string[3]"}],'MethodList' = #'cwmp:MethodList'{'string' = ?ACS_METHODS}}]};
		true ->
            Reply = {ok,Header,[#'cwmp:GetRPCMethodsResponse'{anyAttribs= [{{"xsi:type", []},"SOAP-ENC:Array"},{{"SOAP-ENC:arrayType", []},"xsd:string[3]"}],'MethodList' = #'cwmp:MethodList'{'string' = ?ACS_METHODS}}]}
	end,		
	%State1  = State#state{info = {'cwmp:GetRPCMethodsResponse', AnyAttribs, ?ACS_METHODS}},
    {reply,Reply,State};   



% process GetParameterValuesResponse��send the paramters to shceduling process and wait for scheduler
handle_call({_Ip,_Header,[{'cwmp:GetParameterValuesResponse', AnyAttribs, ParameterList}]},_From,State) ->
    ID = random:uniform(10000),
	_Id = integer_to_list(ID),
	Header =  #'soap:Header'{choice=[#'cwmp:ID'{'P:mustUnderstand'="1",'#text'=_Id},#'cwmp:HoldRequests'{'P:mustUnderstand'="1",'#text'="false"}]}, 

    MessageId = State#state.messageid, 
	CPEId = (State#state.info)#'cwmp:Inform'.'DeviceId',	
    ?QUERY_SERVER ! {acs_response,self(),CPEId, MessageId, {'cwmp:GetParameterValuesResponse', AnyAttribs, ParameterList}},
    receive
	    {acs_response_response, _Response} ->
		    ?QUERY_SERVER ! {acs_request,self(),(State#state.info)#'cwmp:Inform'.'DeviceId'},
			    receive
					{acs_request_response,MessageId, undefined} ->
                        Reply =  {ok,undefined},
		                State1 = State; 				
				    {acs_request_response,MessageId, Request} ->
					    State1 = State#state{messageid = MessageId},
		                Reply = {ok,Header,[processrecord(Request)]};
                    {error, _Reason} ->
					    State1 = State,
                        Reply =  {ok,undefined};						
					_ ->
					    State1 = State,
                        Reply =  {ok,undefined}
	                after ?TIMEOUT ->
		                State1 = State, 
	                    Reply =  {ok,undefined}									
                end;						
		{error, _Reason} ->
		    State1 = State,
            Reply =  {ok,undefined};
		_ ->
            State1 = State,
            Reply =  {ok,undefined}			
	    after ?TIMEOUT ->
		    State1 = State, 
	        Reply =  {ok,undefined}
    end,
	io:format("ParameterList:~p~n",[ParameterList]),
	%State4  = State#state{info ={'cwmp:GetParameterValuesResponse', AnyAttribs, ParameterList}},
	%Reply = {ok,[#'cwmp:SetParameterValues'{'ParameterList' = #'cwmp:ParameterValueList'{anyAttribs = [{{"xsi:type", []},"SOAP-ENC:Array"},{{"SOAP-ENC:arrayType", []},"cwmp:ParameterValueStruct[1]"}],'ParameterValueStruct' = [#'cwmp:ParameterValueStruct'{'Name' = "tttt",'Value' = "1.9.256.9"}]}, 'ParameterKey' = ""}]},
	%Reply = {ok,undefined},
    {reply,Reply,State1};	



handle_call({_Ip,_Header,[{'cwmp:SetParameterValuesResponse', AnyAttribs, SetStatus}]},_From,State) ->
    ID = random:uniform(10000),
	_Id = integer_to_list(ID),
	Header =  #'soap:Header'{choice=[#'cwmp:ID'{'P:mustUnderstand'="1",'#text'=_Id},#'cwmp:HoldRequests'{'P:mustUnderstand'="1",'#text'="false"}]}, 

    MessageId = State#state.messageid, 
	CPEId = (State#state.info)#'cwmp:Inform'.'DeviceId',	
    ?QUERY_SERVER ! {acs_response,self(),CPEId, MessageId, {'cwmp:SetParameterValuesResponse', AnyAttribs, SetStatus}},
    receive
	    {acs_response_response, _Response} ->
		    ?QUERY_SERVER ! {acs_request,self(),(State#state.info)#'cwmp:Inform'.'DeviceId'},
			    receive
					{acs_request_response,MessageId, undefined} ->
                        Reply =  {ok,undefined},
		                State1 = State; 				
				    {acs_request_response,MessageId, Request} ->
					    State1 = State#state{messageid = MessageId},
		                Reply = {ok,Header,[processrecord(Request)]};
                    {error, _Reason} ->
					    State1 = State,
                        Reply =  {ok,undefined};						
					_ ->
					    State1 = State,
                        Reply =  {ok,undefined}
	                after ?TIMEOUT ->
		                State1 = State, 
	                    Reply =  {ok,undefined}									
                end;						
		{error, _Reason} ->
		    State1 = State,
            Reply =  {ok,undefined};
		_ ->
            State1 = State,
            Reply =  {ok,undefined}			
	    after ?TIMEOUT ->
		    State1 = State, 
	        Reply =  {ok,undefined}
    end,
	io:format("SetStatus~p~n",[SetStatus]),
	%State5  = State#state{info ={'cwmp:SetParameterValuesResponse', AnyAttribs, SetStatus}},
	%Reply = {ok,[#'cwmp:GetParameterAttributes'{'ParameterNames' = #'cwmp:ParameterNames'{anyAttribs = [{{"xsi:type", []},"SOAP-ENC:Array"},{{"SOAP-ENC:arrayType", []},"xsd:string[1]"}],'string' = ["DeviceInfoModule.HardwareVersionModule"]}}]},
	%Reply = {ok,undefined},
    {reply,Reply,State1};	


%-record('cwmp:SetParameterAttributesResponse', {anyAttribs}).
handle_call({_Ip,_Header,[{'cwmp:SetParameterAttributesResponse', AnyAttribs}]},_From,State) ->
    ID = random:uniform(10000),
	_Id = integer_to_list(ID),
	Header =  #'soap:Header'{choice=[#'cwmp:ID'{'P:mustUnderstand'="1",'#text'=_Id},#'cwmp:HoldRequests'{'P:mustUnderstand'="1",'#text'="false"}]}, 

    MessageId = State#state.messageid, 
	CPEId = (State#state.info)#'cwmp:Inform'.'DeviceId',	
    ?QUERY_SERVER ! {acs_response,self(),CPEId, MessageId, {'cwmp:SetParameterAttributesResponse', AnyAttribs}},
    receive
	    {acs_response_response, _Response} ->
		    ?QUERY_SERVER ! {acs_request,self(),(State#state.info)#'cwmp:Inform'.'DeviceId'},
			    receive
					{acs_request_response,MessageId, undefined} ->
                        Reply =  {ok,undefined},
		                State1 = State; 				
				    {acs_request_response,MessageId, Request} ->
					    State1 = State#state{messageid = MessageId},
		                Reply = {ok,Header,[processrecord(Request)]};
                    {error, _Reason} ->
					    State1 = State,
                        Reply =  {ok,undefined};						
					_ ->
					    State1 = State,
                        Reply =  {ok,undefined}
	                after ?TIMEOUT ->
		                State1 = State, 
	                    Reply =  {ok,undefined}						
                end;						
		{error, _Reason} ->
		    State1 = State,
            Reply =  {ok,undefined};
		_ ->
            State1 = State,
            Reply =  {ok,undefined}			
	    after ?TIMEOUT ->
		    State1 = State, 
	        Reply =  {ok,undefined}
    end,
	io:format("SetParameterAttributesResponse AnyAttribs:~p~n",[AnyAttribs]),
    %State1 = State#state{info = {'cwmp:SetParameterAttributesResponse', AnyAttribs}},
	%Reply = {ok,Header,undefined},
	{reply,Reply,State1};
	
%-record('cwmp:GetParameterAttributesResponse', {anyAttribs, 'ParameterList'}).
handle_call({_Ip,_Header,[{'cwmp:GetParameterAttributesResponse', AnyAttribs, ParameterList}]},_From,State) ->
    ID = random:uniform(10000),
	_Id = integer_to_list(ID),
	Header =  #'soap:Header'{choice=[#'cwmp:ID'{'P:mustUnderstand'="1",'#text'=_Id},#'cwmp:HoldRequests'{'P:mustUnderstand'="1",'#text'="false"}]}, 

    MessageId = State#state.messageid, 
	CPEId = (State#state.info)#'cwmp:Inform'.'DeviceId',	
    ?QUERY_SERVER ! {acs_response,self(),CPEId, MessageId, {'cwmp:GetParameterAttributesResponse', AnyAttribs, ParameterList}},
    receive
	    {acs_response_response, _Response} ->
		    ?QUERY_SERVER ! {acs_request,self(),(State#state.info)#'cwmp:Inform'.'DeviceId'},
			    receive
					{acs_request_response,MessageId, undefined} ->
                        Reply =  {ok,undefined},
		                State1 = State; 
				    {acs_request_response,MessageId, Request} ->
					    State1 = State#state{messageid = MessageId},
		                Reply = {ok,Header,[processrecord(Request)]};
                    {error, _Reason} ->
					    State1 = State,
                        Reply =  {ok,undefined};						
					_ ->
					    State1 = State,
                        Reply =  {ok,undefined}
	                after ?TIMEOUT ->
		                State1 = State, 
	                    Reply =  {ok,undefined}						
                end;						
		{error, _Reason} ->
		    State1 = State,
            Reply =  {ok,undefined};
		_ ->
            State1 = State,
            Reply =  {ok,undefined}			
	    after ?TIMEOUT ->
		    State1 = State, 
	        Reply =  {ok,undefined}
    end,
	io:format("ParameterList:~p~n",[ParameterList]),
    %State1 = State#state{info ={'cwmp:GetParameterAttributesResponse', AnyAttribs, ParameterList}},
	%Reply = {ok,[#'cwmp:Reboot'{'CommandKey' = "test"}]},
	%Reply = {ok,undefined},
	{reply,Reply,State1};

%-record('cwmp:AddObjectResponse', {anyAttribs, 'InstanceNumber', 'Status'}).
handle_call({_Ip,_Header,[{'cwmp:AddObjectResponse', AnyAttribs, InstanceNumber, Status}]},_From,State) ->
    ID = random:uniform(10000),
	_Id = integer_to_list(ID),
	Header =  #'soap:Header'{choice=[#'cwmp:ID'{'P:mustUnderstand'="1",'#text'=_Id},#'cwmp:HoldRequests'{'P:mustUnderstand'="1",'#text'="false"}]}, 

    MessageId = State#state.messageid, 
	CPEId = (State#state.info)#'cwmp:Inform'.'DeviceId',	
    ?QUERY_SERVER ! {acs_response,self(),CPEId, MessageId, {'cwmp:AddObjectResponse', AnyAttribs, InstanceNumber, Status}},
    receive
	    {acs_response_response, _Response} ->
		    ?QUERY_SERVER ! {acs_request,self(),(State#state.info)#'cwmp:Inform'.'DeviceId'},
			    receive
					{acs_request_response,MessageId, undefined} ->
                        Reply =  {ok,undefined},
		                State1 = State; 	
				    {acs_request_response,MessageId, Request} ->
					    State1 = State#state{messageid = MessageId},
		                Reply = {ok,Header,[processrecord(Request)]};
                    {error, _Reason} ->
					    State1 = State,
                        Reply =  {ok,undefined};							
					_ ->
					    State1 = State,
                        Reply =  {ok,undefined}
	                after ?TIMEOUT ->
		                State1 = State, 
	                    Reply =  {ok,undefined}						
                end;						
		{error, _Reason} ->
		    State1 = State,
            Reply =  {ok,undefined};
		_ ->
            State1 = State,
            Reply =  {ok,undefined}			
	    after ?TIMEOUT ->
		    State1 = State, 
	        Reply =  {ok,undefined}
    end,
	io:format("AddObjectResponse InstanceNumber:~p~nStatus:~p~n",[InstanceNumber, Status]),
    %State1 = State#state{info ={'cwmp:AddObjectResponse', AnyAttribs, InstanceNumber, Status}},
	%Reply = {ok,undefined},
	{reply,Reply,State1};

%-record('cwmp:DeleteObjectResponse', {anyAttribs, 'Status'}).
handle_call({_Ip,_Header,[{'cwmp:DeleteObjectResponse', AnyAttribs, Status}]},_From,State) ->
    ID = random:uniform(10000),
	_Id = integer_to_list(ID),
	Header =  #'soap:Header'{choice=[#'cwmp:ID'{'P:mustUnderstand'="1",'#text'=_Id},#'cwmp:HoldRequests'{'P:mustUnderstand'="1",'#text'="false"}]}, 

    MessageId = State#state.messageid, 
	CPEId = (State#state.info)#'cwmp:Inform'.'DeviceId',	
    ?QUERY_SERVER ! {acs_response,self(),CPEId, MessageId, {'cwmp:DeleteObjectResponse', AnyAttribs, Status}},
    receive
	    {acs_response_response, _Response} ->
		    ?QUERY_SERVER ! {acs_request,self(),(State#state.info)#'cwmp:Inform'.'DeviceId'},
			    receive
				    {acs_request_response,MessageId, undefined} ->
                        Reply =  {ok,undefined},
		                State1 = State; 		
				    {acs_request_response,MessageId, Request} ->
					    State1 = State#state{messageid = MessageId},
		                Reply = {ok,Header,[processrecord(Request)]};
                    {error, _Reason} ->
					    State1 = State,
                        Reply =  {ok,undefined};						
					_ ->
					    State1 = State,
                        Reply =  {ok,undefined}
	                after ?TIMEOUT ->
		                State1 = State, 
	                    Reply =  {ok,undefined}						
                end;						
		{error, _Reason} ->
		    State1 = State,
            Reply =  {ok,undefined};
		_ ->
            State1 = State,
            Reply =  {ok,undefined}			
	    after ?TIMEOUT ->
		    State1 = State, 
	        Reply =  {ok,undefined}
    end,
	%Reply = {ok,undefined},
	{reply,Reply,State1};

%-record('cwmp:DownloadResponse', {anyAttribs, 'Status', 'StartTime', 'CompleteTime'}).
handle_call({_Ip,_Header,[{'cwmp:DownloadResponse', AnyAttribs, Status, StartTime, CompleteTime}]},_From,State) ->
    ID = random:uniform(10000),
	_Id = integer_to_list(ID),
	Header =  #'soap:Header'{choice=[#'cwmp:ID'{'P:mustUnderstand'="1",'#text'=_Id},#'cwmp:HoldRequests'{'P:mustUnderstand'="1",'#text'="false"}]}, 

    MessageId = State#state.messageid, 
	CPEId = (State#state.info)#'cwmp:Inform'.'DeviceId',	
    ?QUERY_SERVER ! {acs_response,self(),CPEId, MessageId, {'cwmp:DownloadResponse', AnyAttribs, Status, StartTime, CompleteTime}},
    receive
	    {acs_response_response, _Response} ->
		    ?QUERY_SERVER ! {acs_request,self(),(State#state.info)#'cwmp:Inform'.'DeviceId'},
			    receive
					{acs_request_response,MessageId, undefined} ->
                        Reply =  {ok,undefined},
		                State1 = State; 	
				    {acs_request_response,MessageId, Request} ->
					    State1 = State#state{messageid = MessageId},
		                Reply = {ok,Header,[processrecord(Request)]};
                    {error, _Reason} ->
					    State1 = State,
                        Reply =  {ok,undefined};							
					_ ->
					    State1 = State,
                        Reply =  {ok,undefined}
	                after ?TIMEOUT ->
		                State1 = State, 
	                    Reply =  {ok,undefined}						
                end;						
		{error, _Reason} ->
		    State1 = State,
            Reply =  {ok,undefined};
		_ ->
            State1 = State,
            Reply =  {ok,undefined}			
	    after ?TIMEOUT ->
		    State1 = State, 
	        Reply =  {ok,undefined}
    end,
	io:format("DownloadResponse Status:~p~nStartTime~p~nCompleteTime~p~n",[Status, StartTime, CompleteTime]),
    %State1 = State#state{info = {'cwmp:DownloadResponse', AnyAttribs, Status, StartTime, CompleteTime}},
	%Reply = {ok,undefined},
	{reply,Reply,State1};

%-record('cwmp:RebootResponse', {anyAttribs}).
handle_call({_Ip,_Header,[{'cwmp:RebootResponse', AnyAttribs}]},_From,State) ->
    ID = random:uniform(10000),
	_Id = integer_to_list(ID),
	Header =  #'soap:Header'{choice=[#'cwmp:ID'{'P:mustUnderstand'="1",'#text'=_Id},#'cwmp:HoldRequests'{'P:mustUnderstand'="1",'#text'="false"}]},
    MessageId = State#state.messageid, 
	CPEId = (State#state.info)#'cwmp:Inform'.'DeviceId',	
    ?QUERY_SERVER ! {acs_response,self(),CPEId, MessageId, {'cwmp:RebootResponse', AnyAttribs}},
    receive
	    {acs_response_response, _Response} ->
		    ?QUERY_SERVER ! {acs_request,self(),(State#state.info)#'cwmp:Inform'.'DeviceId'},
			    receive
		            {acs_request_response,MessageId, undefined} ->
                        Reply =  {ok,undefined},
		                State1 = State; 				
				    {acs_request_response,MessageId, Request} ->
					    State1 = State#state{messageid = MessageId},
		                Reply = {ok,Header,[processrecord(Request)]};
                    {error, _Reason} ->
					    State1 = State,
                        Reply =  {ok,undefined};					
					_ ->
					    State1 = State,
                        Reply =  {ok,undefined}
	                after ?TIMEOUT ->
		                State1 = State, 
	                    Reply =  {ok,undefined}						
                end;						
		{error, _Reason} ->
		    State1 = State,
            Reply =  {ok,undefined};
		_ ->
            State1 = State,
            Reply =  {ok,undefined}			
	    after ?TIMEOUT ->
		    State1 = State, 
	        Reply =  {ok,undefined}
    end,
	io:format("RebootResponse"),
    %State1 = State#state{info = {'cwmp:RebootResponse', AnyAttribs}},
	%Reply = {ok,[#'cwmp:Download'{'CommandKey' = "cmdkey_download123", 'FileType' = "1 Firmware Upgrade Image", 'URL' = "http://192.168.250.54/autoprovision/mx8/Rel_1.9.286.tar.gz", 'Username' = "newrock", 'Password' = "1234567", 'FileSize' = "582125", 'TargetFileName' = "target_file_name", 'DelaySeconds' = "2", 'SuccessURL' = "", 'FailureURL' = ""}]},
	%Reply = {ok,undefined},
	{reply,Reply,State1};


%  process emtpy msg, including query query_server 
handle_call({_Ip,undefined,undefined},_From,State) ->
    ID = random:uniform(10000),
	_Id = integer_to_list(ID),
	%[Pid] = pg2:get_members(mogile_tracker),
	%io:format("Pid~p~n",[Pid]),
	_Header =  #'soap:Header'{choice=[#'cwmp:ID'{'P:mustUnderstand'="1",'#text'=_Id},#'cwmp:HoldRequests'{'P:mustUnderstand'="1",'#text'="false"}]},
    ?QUERY_SERVER ! {acs_request,self(), (State#state.info)#'cwmp:Inform'.'DeviceId'},
    receive
		{acs_request_response,_MessageId, undefined} ->
            Reply =  {ok,undefined},
		    State2 = State; 	
	    {acs_request_response,MessageId, Request} ->
	        Reply = {ok,_Header,[processrecord(Request)]},
		    State2 = State#state{messageid = MessageId};		
		{error,_Reason} ->
            Reply  = {ok,undefined},
            State2 = State
	    after ?TIMEOUT ->
	        Reply =  {ok,undefined},
		    State2 = State
    end,			
	% read all parameter names
    %Reply = {ok,[#'cwmp:GetParameterNames'{'ParameterPath' = "",'NextLevel' = false}]},
	
	% read CPE support methods
	%Reply = {ok,_Header,[#'cwmp:GetRPCMethods'{}]},
	
	%read parameter value
	%Reply = {ok,[#'cwmp:GetParameterValues'{'ParameterNames' = #'cwmp:ParameterNames'{anyAttribs = [{{"xsi:type", []},"SOAP-ENC:Array"},{{"SOAP-ENC:arrayType", []},"xsd:string[1]"}],'string' = ["DeviceInfoModule.HardwareVersionModule"]}}]},
    
	%set parameter value
	%Reply = {ok,[#'cwmp:SetParameterValues'{'ParameterList' = #'cwmp:ParameterValueList'{anyAttribs = [{{"xsi:type", []},"SOAP-ENC:Array"},{{"SOAP-ENC:arrayType", []},"cwmp:ParameterValueStruct[1]"}],'ParameterValueStruct' = [#'cwmp:ParameterValueStruct'{'Name' = "tttt",'Value' = "1.9.256.9"}]}, 'ParameterKey' = ""}]},    

    % read parameter attributes
	%Reply = {ok,[#'cwmp:GetParameterAttributes'{'ParameterNames' = #'cwmp:ParameterNames'{anyAttribs = [{{"xsi:type", []},"SOAP-ENC:Array"},{{"SOAP-ENC:arrayType", []},"xsd:string[1]"}],'string' = ["DeviceInfoModule.HardwareVersionModule"]}}]},
	
	% set paramter attributes
	%-record('cwmp:SetParameterAttributesResponse', {anyAttribs}).
    %-record('cwmp:SetParameterAttributes', {anyAttribs, 'ParameterList'}).
	%-record('cwmp:SetParameterAttributesList', {anyAttribs, 'P:offset', 'P:arrayType', 'href', 'id', 'SetParameterAttributesStruct'}).
    %-record('cwmp:SetParameterAttributesStruct', {anyAttribs, 'Name', 'NotificationChange', 'Notification', 'AccessListChange', 'AccessList'}).
	%-record('cwmp:AccessList', {anyAttribs, 'P:offset', 'P:arrayType', 'href', 'id', 'string'}).
	%-record('cwmp:ParameterAttributeList', {anyAttribs, 'P:offset', 'P:arrayType', 'href', 'id', 'ParameterAttributeStruct'}).
    %-record('cwmp:ParameterAttributeStruct', {anyAttribs, 'Name', 'Notification', 'AccessList'}).
	%Reply = {ok,[#'cwmp:SetParameterAttributes'{'ParameterList' = #'cwmp:SetParameterAttributesList'{anyAttribs = [{{"xsi:type", []},"SOAP-ENC:Array"},{{"SOAP-ENC:arrayType", []},"cwmp:SetParameterAttributesStruct[1]"}],'SetParameterAttributesStruct' = [#'cwmp:SetParameterAttributesStruct'{'Name' = "DeviceInfoModule.HardwareVersionModule",'NotificationChange' = false,'Notification'= "0",'AccessListChange' = false,'AccessList' = [#'cwmp:AccessList'{anyAttribs = [{{"xsi:type", []},"SOAP-ENC:Array"},{{"SOAP-ENC:arrayType", []},"xsd:string[1]"}],'string'= ["ttt"]}]}]}}]},

	%add an object
	%-record('cwmp:AddObject', {anyAttribs, 'ObjectName', 'ParameterKey'}).
	%Reply = {ok,[#'cwmp:AddObject'{'ObjectName' = "11", 'ParameterKey' = "22"}]},
	
	%delete object
	%-record('cwmp:DeleteObject', {anyAttribs, 'ObjectName', 'ParameterKey'}).
	%Reply = {ok,[#'cwmp:AddObject'{'ObjectName' = "11", 'ParameterKey' = "22"}]},
	
	%restart CPE
	%-record('cwmp:Reboot', {anyAttribs, 'CommandKey'}).
	%Reply = {ok,[#'cwmp:Reboot'{'CommandKey' = "test"}]},
	
	%request to download files
	%-record('cwmp:Download', {anyAttribs, 'CommandKey', 'FileType', 'URL', 'Username', 'Password', 'FileSize', 'TargetFileName', 'DelaySeconds', 'SuccessURL', 'FailureURL'}).
	%Reply = {ok,[#'cwmp:Download'{'CommandKey' = "cmdkey_download123", 'FileType' = "1 Firmware Upgrade Image", 'URL' = "http://192.168.250.54/autoprovision/mx8/Rel_1.9.286.tar.gz", 'Username' = "newrock", 'Password' = "1234567", 'FileSize' = "582125", 'TargetFileName' = "target_file_name", 'DelaySeconds' = "2", 'SuccessURL' = "", 'FailureURL' = ""}]},
	%[#'cwmp:ID'{'P:mustUnderstand'="1",'#text'="1234"},]
	
    %Header = #'soap:Header'{choice=[#'cwmp:ID'{'P:mustUnderstand'="1",'#text'="1234"}]},
    %Reply = {ok,Header,[#'cwmp:GetRPCMethodsResponse'{anyAttribs= [{{"xsi:type", []},"SOAP-ENC:Array"},{{"SOAP-ENC:arrayType", []},"xsd:string[3]"}],'MethodList' = #'cwmp:MethodList'{'string' = ?ACS_METHODS}}]},  	
    %Reply = {ok,undefined},
	{reply,Reply,State2};	

handle_call(stop,_From,State) ->		    
	{stop,normal,stopped,State};
	
handle_call({_Ip,_Header,_Others},_From,State) ->
    io:format("Others:::::::::~n"),
    Reply = {ok,undefined}, 
    {reply,Reply,State}.	
	
handle_cast(_Msg,State) ->{noreply,State}.
handle_info(_Info,State) ->{noreply,State}.
terminate(_Reason,_State) ->ok.
code_change(_OldVsn, State, _Extra) ->  
    {ok, State}.  


%read X event code
get_x_event_code(EventCodeList) ->
    get_x_event_code_t(EventCodeList,length(EventCodeList),[]).
get_x_event_code_t(_EventCodeList,0,List) -> List;
get_x_event_code_t(EventCodeList,Num,List) ->
    [A|B] = EventCodeList,
	F =  lists:sublist(A,1,1),
	if F == "X" ->
	        L = lists:append(List,[A]);
		true ->
            L = List
	end,
    get_x_event_code_t(B,Num-1,L).	
	
    %[E] =  lists:sublist(EventCodeList,Num,1),
    %F =  lists:sublist(E,1,1),
    %if F == "X" ->
    %        L = lists:append(List,E);
    %    true ->
    %        L = List
    %end,
    %get_x_event_code_t(EventCodeList,Num-1,L).	

%-record('cwmp:XFileEvent', {anyAttribs, 'DeviceId', 'Event', 'OUI'}).

process_x_event(Xeventlist,DeviceId) ->
    process_x_event_t(Xeventlist,DeviceId,length(Xeventlist)).
process_x_event_t(_Xeventlist,_DeviceId_t,0) ->io:format("process_x_event over");
process_x_event_t(Xeventlist,DeviceId_t,Num) ->
    [E] =  lists:sublist(Xeventlist,Num,1),
	Xl = string:tokens(E," "),
	L = length(Xl), 
	if L == 3 ->
	        [Event_t] = lists:sublist(Xl,3,1),
            [OUI_t]   = lists:sublist(Xl,2,1),
			%io:format("test");
            ?QUERY_SERVER ! {acs_post,#'cwmp:XFileEvent'{'DeviceId' = DeviceId_t, 'Event' =Event_t, 'OUI' = OUI_t}};
        true ->
            io:format("X event error")
    end,
    process_x_event_t(Xeventlist,DeviceId_t,Num-1).	

%-record('cwmp:EventStruct', {anyAttribs, 'EventCode', 'CommandKey'}).

geteventcode(EventList) ->
    getevent_t(EventList,length(EventList),[]).
getevent_t(_List,0,_Ec) -> _Ec;	
getevent_t(List,Len,Ec) ->
    [A|B] = List,
    Ec_t = lists:append(Ec,[A#'cwmp:EventStruct'.'EventCode']),
    getevent_t(B,Len-1,Ec_t).
	
%-record('cwmp:HoldRequests',{mustUnderstand,'HoldRequests'}).
%Ϊsoap��header���HoldRequestsΪtrue
%{ok,#'soap:Header'{choice=[#'cwmp:ID'{mustUnderstand="1",'#text'="1234"}]},[#'cwmp:deleteObjectResponse'{'Status'="1"}]}.


%-record('cwmp:ParameterInfoStruct', {anyAttribs, 'Name', 'Writable'}).
processparameterinfostruct(List) ->
    processparameterinfostruct_t(List,length(List),[]).
processparameterinfostruct_t(_List,0,Res) -> Res;
processparameterinfostruct_t(List,Num,R) ->
    [A|B] = List,
	_R = lists:append(R, [{A#'cwmp:ParameterInfoStruct'.'Name',A#'cwmp:ParameterInfoStruct'.'Writable'}]),
	processparameterinfostruct_t(B,Num-1,_R).

%-record('cwmp:MonitorGetParameterAttributes', {anyAttribs, 'DeviceId', 'Ip', 'ParameterNames'}).
%-record('cwmp:MonitorSetParameterAttributes', {anyAttribs, 'DeviceId', 'Ip', 'ParameterList'}).
%-record('cwmp:MonitorGetParameterValues', {anyAttribs, 'DeviceId', 'Ip', 'ParameterNames'}).
%-record('cwmp:MonitorSetParameterValues', {anyAttribs, 'DeviceId', 'Ip', 'ParameterList', 'ParameterKey'}).\
processrecord(Record) ->
    case Record of
	    {'cwmp:MonitorGetParameterAttributes', AnyAttribs, _DeviceId, _Ip, ParameterNames} ->
		    {'cwmp:GetParameterAttributes', AnyAttribs, ParameterNames};
		{'cwmp:MonitorSetParameterAttributes', AnyAttribs, _DeviceId, _Ip, ParameterList} ->
            {'cwmp:SetParameterAttributes', AnyAttribs, ParameterList};
        {'cwmp:MonitorGetParameterValues', AnyAttribs, _DeviceId, _Ip, ParameterNames} ->
            {'cwmp:GetParameterValues', AnyAttribs,ParameterNames};
        {'cwmp:MonitorSetParameterValues', AnyAttribs, _DeviceId, _Ip, ParameterList, ParameterKey} ->			
		    {'cwmp:SetParameterValues', AnyAttribs, ParameterList, ParameterKey}
	end.
	
%get CPE
get() ->
    http:request("http://221.133.234.54").