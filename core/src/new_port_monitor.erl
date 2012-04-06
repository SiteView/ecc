%%
%% new port monitor
%%
%%
-module(new_port_monitor,[BASE]). 
-extends(port_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

-define(DEFAULT_TIMEOUT,60000).

new()->
	{?MODULE,port_monitor:new()}.

getScalarValues(Prop,Params)->
    Host = proplists:get_value(host,Params),
    AutoDiscovery = proplists:get_value(autoDiscovery,Params),
    AS = getPortMonitors(),
	case Prop of
		port->
            if
                (AutoDiscovery=:="true") ->
                    Ports = autoDiscovery(Host),
                    buildoptions(filterPorts(Ports,AS,[]),[]);
                true ->
                    buildoptions(AS,[])
            end;
		_->
			BASE:getScalarValues(Prop,Params)
	end.

filterPorts([],_,Ports)->Ports;
filterPorts([F|R],Standard,Ports) ->
    Port = element(1,F),
    {Exist,OP} = findPort(Port,Standard),
    Result = if
        Exist ->
            OP;
        true ->
            F
    end,
    filterPorts(R,Standard,Ports++[Result]).


findPort(_,[])->{false,[]};
findPort(Port,[F|R]) ->
    SP = element(1,F),
    if
        Port=:=SP ->
            {true,F};
        true ->
            findPort(Port,R)
    end.

getPortMonitors()->
	[{"7","Echo","Echo test\r\n","Echo test"},{"13","Daytime"},{"21","FTP ","","220"},{"23","Telnet"},{"25","SMTP","","220"},{"70","Gopher","\r\n","1"},{"110","POP3","","+OK"},{"119","News","","200"},{"143","IMAP4"},{"1494","Citrix","","ICA"},{"6667","IRC"},{"1645","RADIUS","\x01S\x00\x2cSiteViewRADIUS1\x01\x06test\x02\x12password01234567","\x02S\x00","UDP"}].

autoDiscovery(Host)->
    Result = nmap_scan:os_scan(Host),
    if
        length(Result)>0 ->
            Result1 = lists:nth(1,Result),
            Ports = proplists:get_value(ports,Result1,[]),
            processPort(Ports,[]);
        true ->
            []
    end.
    
processPort([],Ports)->Ports;
processPort([{_,Info}|R],Ports)->
    Port = proplists:get_value(portid,Info),
    Protocol = proplists:get_value(protocol,Info),
    Type = proplists:get_value(service,Info),
    Result = if
        Protocol=:="tcp" ->
            [{Port,Type}];
        true ->
            [{Port,Type,"","","UDP"}]
    end,
    processPort(R,Ports++Result);
processPort([_|R],Ports)->
    processPort(R,Ports).
    
    
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

deleteProperty(Properties,Type) ->
    lists:filter(fun(#property{name=Name}) -> if Name=:=Type->false;true ->true end end ,Properties).

get_template_property()->
    deleteProperty(BASE:get_template_property(),port) ++
	  [
        #property{name=autoDiscovery,title="AutoDiscovery",type=bool,order=2,description="chose this option the monitor will autodiscover ports in specified host, if not use default ports(CAUTION the process will cost a few minutes)"},
        #property{name=port,title="Port Number",type=refreshscalar,order=3,description="the port that will be connected to."}
	  ].