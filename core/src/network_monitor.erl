%% @author Cheng kaiyang <kaiyang.cheng@dragonflow.com>
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc Network Monitor.
%% 
%% Description: Only for windows planform
-module(network_monitor,[BASE]).
-extends(atomic_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

-export([new/0,isError/1,update/0,get_classifier/1,get_template_property/0]).

%% @spec new() -> ok
%% @doc Start the monitor instance.
new()->
	Obj = atomic_monitor:new(),
	Obj:set_attribute(lastBytesIn,0),
	Obj:set_attribute(lastBytesOut,0),
	Obj:set_attribute(lastPacketErrors,0),
	Obj:set_attribute(lastMeasurement,0),
	{?MODULE,Obj}.
	
%% @spec isError(Data) -> bool()
%% where
%% Data = list()
%% @doc check result from command.
isError(L) when length(L)==4 ->
	Al1 = lists:nth(1,L),
	Al2 = lists:nth(2,L),
	Al3 = lists:nth(3,L),
	Al4 = lists:nth(4,L),
	((Al1 =:= -1) or (Al2 =:= -1) or (Al3 =:= -1) or (Al4 =:= -1));
isError(_) ->true.
	
%% @spec update() -> ok
%% @doc Run the monitor.
%% 
%% Description: Get data from platform module
%% calculate bytein per sec,byteout per sec
update() ->
	{ok,{_,LastBytesIn}} = THIS:get_attribute(lastBytesIn),
	{ok,{_,LastBytesOut}} = THIS:get_attribute(lastBytesOut),
	{ok,{_,LastPacketErrors}} = THIS:get_attribute(lastPacketErrors),
	{ok,{_,LastMeasurement}} = THIS:get_attribute(lastMeasurement),
    % last 
    {LBytesIn, LBytesOut, LPacketErrors, LMeasurement, Success1} = if
        LastMeasurement == 0 ->
            NetStats = platform:getNetStats(),
            LM = httputils:currentTimeMillis(),
            case isError(NetStats) of
                true ->
                    {LastBytesIn, LastBytesOut, LastPacketErrors, LM, false};
                _ ->
                    platform:sleep(2000),
                    {lists:nth(1, NetStats), lists:nth(2, NetStats), lists:nth(3, NetStats), LM, true}
            end;
        true ->
            {LastBytesIn, LastBytesOut, LastPacketErrors, LastMeasurement, true}
    end,
    % now
    Result = platform:getNetStats(),
    BytesIn = lists:nth(1, Result),
    BytesOut = lists:nth(2, Result),
    PacketErrors = lists:nth(3, Result),
    Connections = lists:nth(4, Result),
    Measurement = httputils:currentTimeMillis(),
    
    Interval = Measurement - LMeasurement,
    
    {BytesPerSecondIn, BytesPerSecondOut, PacketErrorsPerSecond, Success2} = 
        case ((not isError(Result)) and (Interval>0)) of
            true ->
                Inv = Interval/1000,
                if
                    (((BytesIn - LBytesIn)<0) or ((BytesOut - LBytesOut)<0) or ((PacketErrors - LPacketErrors)<0)) ->
                        add_error({erlang:localtime(), [{bytein, [LBytesIn, BytesIn]}, {byteout, [LBytesOut, BytesOut]}, {packeterror, [LPacketErrors, PacketErrors]}, {time, [LMeasurement, Measurement]}]}),
                        {-1, -1, -1, false};
                    true ->
                        {(BytesIn - LBytesIn) / Inv, (BytesOut - LBytesOut) / Inv, (PacketErrors - LPacketErrors) / Inv, Success1}
                end;
            _ ->
                {-1, -1, -1, false}
        end,
    if
        Success2 ->
            THIS:set_attribute(lastBytesIn, BytesIn),
			THIS:set_attribute(lastBytesOut, BytesOut),
			THIS:set_attribute(lastPacketErrors, PacketErrors),
			THIS:set_attribute(lastMeasurement, Measurement),
			THIS:set_attribute(connections, Connections),
			THIS:set_attribute(bytesInPerSecond, BytesPerSecondIn),
			THIS:set_attribute(bytesOutPerSecond, BytesPerSecondOut),
			THIS:set_attribute(packetErrorsPerSecond, PacketErrorsPerSecond),
			THIS:set_attribute(?STATE_STRING,httputils:bytesToString(BytesPerSecondIn/1, 2)++"/s in/sec., "++httputils:bytesToString(BytesPerSecondOut/1, 2)++"/s out/sec., "++httputils:bytesToString(PacketErrorsPerSecond, 2)++" packet errors/sec., "++integer_to_list(Connections)++" current connections.");
		true ->
			Er = isError(Result),
			if
				Er ->
					THIS:set_attribute(?STATE_STRING,"net statistics not found");
				true ->
					THIS:set_attribute(?STATE_STRING,"no data")
			end,
			THIS:set_attribute(lastBytesIn, 0),
			THIS:set_attribute(lastBytesOut, 0),
			THIS:set_attribute(lastPacketErrors, 0),
			THIS:set_attribute(lastMeasurement, 0),
			THIS:set_attribute(connections, 0),
			THIS:set_attribute(bytesInPerSecond, 0),
			THIS:set_attribute(bytesOutPerSecond, 0),
			THIS:set_attribute(packetErrorsPerSecond, 0),
			THIS:set_attribute(?NO_DATA,true),
			THIS:set_attribute(?CATEGORY,?NO_DATA)
	end.
	
add_error(Error) ->
    Result = case THIS:get_attribute(error_params) of
        {ok,{_, OE}} ->
            [Error|OE];
        _ ->
            []
    end,
    THIS:set_attribute(error_params, Result).
    
%% @spec get_classifier(Flag) -> Threshold
%% where
%% Flag = (error|warning|good)
%% Threshold = [{Attribute,Opera,Value}]
%% Attribute = atom()
%% Opera = atom()
%% Value = (integer()|atom()|string())
%% @doc Set default threshold value.
get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{packetErrorsPerSecond, '>=', 70}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{packetErrorsPerSecond, '>=', 30}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{packetErrorsPerSecond, '>=', 0}]
	end.
	

%% @spec get_template_property() -> PropertyList
%% where
%% PropertyList = [PropertyRecord]
%% PropertyRecord = record()
%% @doc Add properties to template property list.
get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=bytesOutPerSecond,title="bytes/sec sent(bytes/sec)",type=numeric,state=true,configurable=false},
	#property{name=bytesInPerSecond,title="bytes/sec received(bytes/sec)",type=numeric,state=true,configurable=false},
	#property{name=connections,title="Active connections(connections)",type=numeric,state=true,configurable=false},
	#property{name=packetErrorsPerSecond,title="packet errors/sec(errors/sec)",type=numeric,state=true,configurable=false}
	].
	