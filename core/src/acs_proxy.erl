%% ---
%% acs_proxy
%%
%%---
-module(acs_proxy).
-compile(export_all).
-define(TIMOUT,10000).
-include("common.hrl").

start()->
	ets:new(?MODULE,[set,named_table,protected]),
	case whereis(?MODULE) of
		undefined->
			case register(?MODULE,spawn(fun()->loop() end)) of
				true->
					{ok,started};
				_->
					{error,register_error}
			end;
		Else->
			{error,Else}
	end.


get_message_id()->
	random:uniform(1000000).

stop()->
	?MODULE ! {self(),stop},
	receive
		Ret->
			Ret
	end.

setParameterValues(Params)->
	CpeId = proplists:get_value(cpeid),
	Ip = proplists:get_value(ip),
	case ets:lookup(?MODULE, {CpeId,{set,value}}) of
		[]->
			ets:insert({CpeId,{set,value}},{CpeId,Ip,get_message_id(),method,data_conversion:setParameterValues(Params)}),
			?MODULE ! {self(),{CpeId,Ip,get_message_id(),method,data_conversion:setParameterValues(Params)}},
			receive
				{?MODULE,{ok,Val}}->
					R = data_conversion:setParameterValuesResponse(Val),
					dbcs_device:update_device(Val), %%If it is set up successfully saved to the database   
					ets:delete(?MODULE,{CpeId,{set,value}}),
					{ok,R};
				{?MODULE,{error,Reason}}->
					{error,Reason}
			end;
		_->
			{error,request_not_complete}
	end.


setParameterAttributes(Params)->
	CpeId = proplists:get_value(cpeid),
	Ip = proplists:get_value(ip),
	case ets:lookup(?MODULE, {CpeId,{set,attribute}}) of
		[]->
			ets:insert({CpeId,{set,attribute}},{CpeId,Ip,get_message_id(),method,data_conversion:setParameterAttributes(Params)}),
			?MODULE ! {self(),{CpeId,Ip,get_message_id(),method,data_conversion:setParameterAttributes(Params)}},
			receive
				{?MODULE,{ok,Val}}->
					R = data_conversion:setParameterAttributesResponse(Val),
					dbcs_device:update_device(Val), %%If it is set up successfully saved to the database
					ets:delete(?MODULE,{CpeId,{set,attribute}}),
					{ok,R};
				{?MODULE,{error,Reason}}->
					{error,Reason}
			end;
		_->
			{error,request_not_complete}
	end.

getParameterValues(Params)->
	CpeId = proplists:get_value(cpeid),
	Ip = proplists:get_value(ip),
	case ets:lookup(?MODULE, {CpeId,{get,values}}) of
		[]->
			ets:insert({CpeId,{get,values}},{CpeId,Ip,get_message_id(),method,data_conversion:getParameterValues(Params)}),
			?MODULE ! {self(),{CpeId,Ip,get_message_id(),method,data_conversion:getParameterValues(Params)}},
			receive
				{?MODULE,{ok,Val}}->
					R = data_conversion:getParameterValuesResponse(Val),
					dbcs_device:update_device(Val), %%If it is set up successfully saved to the database
					ets:delete(?MODULE,{CpeId,{get,values}}),
					{ok,R};
				{?MODULE,{error,Reason}}->
					{error,Reason}
			end;
		_->
			{error,request_not_complete}
	end.

getParameterNames()->
	ok.


asc_req(Msg)->
	AscPid = acs_helper:get_acs_pid(),
	AscPid ! {client_conf,self(),Msg},
	receive
		{conf_response,MsgId,{ok,Val}}->
			{ok,Val};
		{conf_response,MsgId,{error,Reason}}->
			{error,Reason}
	after ?TIMOUT->
		{error,timeout}
	end.

loop()->
	receive
		{From,{CpeId,Ip,MessageId,method,Req}}->
			From ! {self(),rpc:call(node(),acs_proxy,asc_req,[{CpeId,Ip,MessageId,method,Req}])},
			loop();
		{From,stop}->
			{ok,stopped}
	end.