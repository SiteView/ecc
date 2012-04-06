%% 
%% @author Cheng kaiyang <kaiyang.cheng@dragonflow.com>
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc Port Monitor.
%% 
%% Description: Using gen_tcp,gen_udp erlang module for sokect programming,
%% Verifies that a connection can be made to a network port and measures the length of 
%% time it takes to make the connection. Optionally, it can look for a string of text to be returned or send a string of text 
%% once the connection is made.
%% Requirement: UDP requires additional configuration
-module(port_monitor,[BASE]). 
-extends(atomic_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

-export([new/0,verify/1,defaultTitle/1,update/0,getPortInfo/1,getScalarValues/2,port_tcp/5,port_udp/5,get_classifier/1,get_template_property/0]).

-define(DEFAULT_TIMEOUT,60000).

%% @spec new() -> ok
%% @doc Start the monitor instance.
new()->
	Base = atomic_monitor:new(),
	Base:set_attribute(portType,"tcp"),
	Base:set_attribute(flag,false),
	Base:set_attribute(connect_time,0),
	Base:set_attribute(round_trip_time,0),
	Base:set_attribute(port,""),
	Base:set_attribute(name,""),
	Base:set_attribute(udp,false),
	Base:set_attribute(udpPort,0),
	Base:set_attribute(sendString,""),
	Base:set_attribute(matchString,""),
	{?MODULE,Base}.
	
%% @spec verify(Params) -> ErrorList
%% where
%% Params = [{atom(),integer()|string()}]
%% ErrorList = [string()]
%% @doc page check.
verify(Params)->
	Errs =
	case proplists:get_value(host,Params) of
		""->
			[{host,"Host Name is missing"}];
		Host->
			case string:str(Host," ") of
				0 ->
					[];
				_->
					[{host,"no spaces are allowed"}]
			end
	end ++
	case proplists:get_value(port,Params) of
		"" ->
			[];
		Port1->
			try list_to_integer(Port1) of
				Port ->
					case is_number(Port) of
						false->
							[{port,"port must be a number"}];
						_->
							[]
					end
			catch
			error:X->X,
			[{port,"port must be a number"}]
			end
	end ++
	case proplists:get_value(time,Params) of
		""->
			[];
		Time->
			if
				not is_number(Time) ->
					[{time,"timeout must be a number."}];
				Time<1->
					[{tme,"time out must be greater than 0"}];
				true->
					[]
			end
	end ++
	case BASE:verify(Params) of
		{error,E}->
			E;
		_->
			[]
	end,
	if
		length(Errs)>0 ->
			{error,Errs};
		true ->
			{ok,""}
	end.
	
%% @spec defaultTitle(Params) -> Title
%% where
%% Params = [PagePropertis]
%% PagePropertis = {PropertyName,PropertyValue}
%% PropertyName = atom()
%% PropertyValue = (integer()|string())
%% Title = string()
%% @doc create a default title.
defaultTitle(Params)->
	Port = proplists:get_value(port,Params),
	Host = proplists:get_value(host,Params),
	PortInfo = getPortInfo(Port),
	if
		PortInfo ->
			{ok,{_,Name}} = THIS:get_attribute(name),
			Name++" on "++Host;
		true ->
			"Port "++Port++" on "++Host
	end.
	
%% @spec update() -> ok
%% @doc Run the monitor.
%%
%% Description: read port information and decide how to process the port
update() ->
	THIS:set_attribute(connect_time,0),
	THIS:set_attribute(round_trip_time,0),
	THIS:set_attribute(portType,"TCP"),
	THIS:set_attribute(flag,false),
	{ok,{_,Host}} = THIS:get_property(host),
	{ok,{_,Port1}} = THIS:get_property(port),
	Port = list_to_integer(Port1),
	{ok,{_,Time}} = THIS:get_property(time),
	Timeout = if
		Time=:=0 ->
			?DEFAULT_TIMEOUT;
		true ->
			Time*1000
	end,
	{ok,{_,Mes}} = THIS:get_property(message_send),
	{ok,{_,Mesr}} = THIS:get_property(message_receive),
	PortInfo = getPortInfo(Port1),
	if
		PortInfo->
			{ok,{_,Flag1}} = THIS:get_attribute(udp),
			THIS:set_attribute(flag,Flag1),
			if
				((Mes=:="") and (Mesr=:="")) ->
					{ok,{_,S2}} = THIS:get_attribute(sendString),
					{ok,{_,S3}} = THIS:get_attribute(matchString),
					SendString = S2,
					MatchString = S3;
				((Mes=:="") and (Mesr=/="")) ->
					{ok,{_,S2}} = THIS:get_attribute(sendString),
					SendString = S2,
					MatchString = Mesr;
				true ->
					SendString = Mes,
					MatchString = Mesr
			end;
		true ->
			SendString = Mes,
			MatchString = Mesr
	end,
	{ok,{_,Flag2}} = THIS:get_attribute(flag),
	if
		Flag2 ->
			THIS:set_attribute(portType,"UDP"),
			port_udp(Host,Port,Timeout,SendString,MatchString);
		true ->
			ok
	end,
	port_tcp(Host,Port,Timeout,SendString,MatchString).
	
initportinfo() ->
	THIS:set_attribute(port,""),
	THIS:set_attribute(name,""),
	THIS:set_attribute(udp,false),
	THIS:set_attribute(udpPort,0),
	THIS:set_attribute(sendString,""),
	THIS:set_attribute(matchString,"").
	
%% @spec getPortInfo(Port) -> Flag
%% where
%% Flag = bool()
%% Port = string()
%% @doc get the port information.
getPortInfo(Port) ->
	initportinfo(),
	List = getPortMonitors(),
	for1(List,Port,false).
	
for1(_,_,true)->true;
for1([],_,FF)->FF;
for1([F|R],Port,FF)->
	I = length(tuple_to_list(F)),
	if
		I>4 ->
			Flag = true,
			K = string:str(element(5,F),"UDP:"),
			if
				K=/=0 ->
					J = list_to_integer(string:strip((string:sub_string(element(5,F),K+string:len("UDP:"),string:len(element(5,F))))));
				true ->
					J = 0
			end;
		true ->
			Flag = false,
			J = 0
	end,
	if
		((I>=2) and (Port=:=element(1,F))) ->
			THIS:set_attribute(port,element(1,F)),
			THIS:set_attribute(name,element(2,F)),
			THIS:set_attribute(udp,Flag),
			THIS:set_attribute(udpPort,J),
			if
				I>=4 ->
					THIS:set_attribute(sendString,element(3,F)),
					THIS:set_attribute(matchString,element(4,F));
				true ->
					THIS:set_attribute(sendString,""),
					THIS:set_attribute(matchString,"")
			end,
			for1(R,Port,true);
		true ->
			for1(R,Port,FF)
	end.
	
	
getPortMonitors()->
	[{"7","Echo","Echo test\r\n","Echo test"},{"13","Daytime"},{"21","FTP ","","220"},{"23","Telnet"},{"25","SMTP","","220"},{"70","Gopher","\r\n","1"},{"110","POP3","","+OK"},{"119","News","","200"},{"143","IMAP4"},{"1494","Citrix","","ICA"},{"6667","IRC"},{"1645","RADIUS","\x01S\x00\x2cSiteViewRADIUS1\x01\x06test\x02\x12password01234567","\x02S\x00","UDP"}].

%% @spec getScalarValues(Property,Params) -> ValueList
%% where
%% Property = atom()
%% Params = [{PropertyName,PropertyValue}]
%% PropertyName = atom()
%% PropertyValue = string()
%% ValueList = [{Scalarname,Scalarvalue}]
%% Scalarname = string()
%% Scalarvalue = string()
%% @doc Set scalar properties value.
getScalarValues(Prop,Params)->
	case Prop of
		port->
			AS = getPortMonitors(),
			buildoptions(AS,[]);
		_->
			BASE:getScalarValues(Prop,Params)
	end.
	
buildoptions([],Result) ->Result;
buildoptions([F|R],Result) ->
	I = length(tuple_to_list(F)),
	if
		I>=2 ->
			S = element(1,F),
			S1 = element(2,F),
			if
				I>=5 ->
					buildoptions(R,Result++[{S1++"("++S++" udp)",S}]);
				true ->
					buildoptions(R,Result++[{S1++"("++S++")",S}])
			end;
		true ->
			buildoptions(R,Result)
	end.

%% @spec port_tcp(Host,Port,Timeout,MessageSend,MessageMatch) -> ok
%% where
%% Host = string()
%% Port = string()
%% Timeout = integer()
%% MessageSend = string()
%% MessageMatch = string()
%% @doc Process tcp port.
port_tcp(Host,Port,T,M,Mr) ->
	{T1,_}=statistics(wall_clock),
	{ok,{_,PortType}} = THIS:get_attribute(portType),
    case gen_tcp:connect(Host,Port,[binary,{packet,0}],T) of
		{ok, Socket}->
			{T2,_}=statistics(wall_clock),
			THIS:set_attribute(connect_time,(T2-T1)/1000),
			if
				((M=/="") or (Mr=/="")) ->
					gen_tcp:send(Socket,M),
					%%io:format("Port Monitor sent to "++Host++":"++integer_to_list(Port)++"("++PortType++"), ~p~n",[M]),
					R=receive
						{tcp,Socket,Bin}->
							{ok,Bin}
						after 5000 ->
							{error,"time out"}
						end,
					case R of
						{ok,R1}->
							Receive = binary_to_list(R1),
							{T3,_}=statistics(wall_clock),
							THIS:set_attribute(round_trip_time,(T3-T1)/1000),
							case string:str(Receive,Mr) of
								0->
									THIS:set_attribute(status,400),
									THIS:set_attribute(?STATE_STRING,"expected: "++Mr++", got: "++Receive);
								_ ->
									THIS:set_attribute(status,200),
                                    THIS:set_attribute(?STATE_STRING,"times = "++httputils:floatToString((T3-T1)/1000,2)++" sec")
							end;
						_->
							THIS:set_attribute(status,400),
							THIS:set_attribute(?STATE_STRING,"time out")
					end;
				true ->
					THIS:set_attribute(round_trip_time,(T2-T1)/1000),
					THIS:set_attribute(status,200),
                    THIS:set_attribute(?STATE_STRING,"times = "++httputils:floatToString((T2-T1)/1000,2)++" sec")
			end,
			gen_tcp:close(Socket);
		_->
			THIS:set_attribute(status,400),
			THIS:set_attribute(?STATE_STRING,"connect error")
	end.
	
%% @spec port_udp(Host,Port,Timeout,MessageSend,MessageMatch) -> ok
%% where
%% Host = string()
%% Port = string()
%% Timeout = integer()
%% MessageSend = string()
%% MessageMatch = string()
%% @doc Process udp port.
port_udp(Host,Port,T,M,Mr)->
	{T1,_}=statistics(wall_clock),
	{ok,{_,PortType}} = THIS:get_attribute(portType),
	{ok,Socket} = gen_udp:open(Port,[binary]),
	gen_udp:send(Socket,Host,Port,M),
	%%io:format("Port Monitor sent to "++Host++":"++integer_to_list(Port)++"("++PortType++")~n"),
	{T2,_}=statistics(wall_clock),
	THIS:set_attribute(connect_time,(T2-T1)/1000),
	%%io:format("Port Monitor (" ++ PortType ++ ") matching:"++Mr++"~n"),
	receive
		{udp,Socket,_,_,Bin} ->
			{T3,_}=statistics(wall_clock),
			THIS:set_attribute(round_trip_time,(T3-T1)/1000),
			%%io:format("Port Monitor (" ++ PortType ++ ") got:"++binary_to_list(Bin)++"~n"),
			case string:str(binary_to_list(Bin),Mr) of
				0->
					THIS:set_attribute(status,400),
					THIS:set_attribute(?STATE_STRING,"expected: "++Mr++", got: "++Bin);
				_->
					THIS:set_attribute(status,200),
                    THIS:set_attribute(?STATE_STRING,"times = "++httputils:floatToString((T3-T1)/1000,2)++" sec")
			end;
		Error->
			%%io:format("error is:~p~n",[Error]),
			THIS:set_attribute(status,400),
			THIS:set_attribute(?STATE_STRING,"unkown error")
		after T->
			THIS:set_attribute(status,400),
			THIS:set_attribute(?STATE_STRING,"time out")
		end,
	gen_udp:close(Socket).

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
			[{status,'!=',200}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'!=',200}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'==',200}]
	end.

getHostname()->
	case THIS:get_property(host) of
		{ok,{_,V}}->
			V;
		_->
			BASE:getHostname()
	end.
	
%% @spec get_template_property() -> PropertyList
%% where
%% PropertyList = [PropertyRecord]
%% PropertyRecord = record()
%% @doc Add properties to template property list.
get_template_property()->
	BASE:get_template_property() ++
	  [
		#property{name=host,title="Host Name",type=text,order=1,description="the IP address or host name that will be connected to (examples: 206.168.191.21 or demo.siteview.com)"},
		#property{name=port,title="Port Number",type=scalar,order=2,allowother=true,description="the port that will be connected to.",default="7"},
		#property{name=time,title="Timeout",type=numeric,advance=true,optional=true,order=3,description="the time out, seconds, to wait for connection and reply",default=60,baselinable=true},
		#property{name=message_send,title="Send String",type=text,advance=true,optional=true,order=4,description="Override the default string that is be sent to the port after connecting"},
		#property{name=message_receive,title="Match String",type=text,advance=true,optional=true,order=5,description="Override the default string that is matched against the port's response"},
		#property{name=status,title="Status",type=numeric,state=true,configurable=false}
	  ].