%% 
%% server_monitor
%%

-module(server_monitor,[BASE]).
-extends(atomic_monitor).
-compile(export_all).
-include("monitor_template.hrl").
-include("monitor.hrl").

new()->
	Base = atomic_monitor:new(),
	Base:set_attribute(os,platform:getLocalPlatform()),
	Base:set_attribute(machine,""),
	{?MODULE,Base}.


getPlatform()->
	%%THIS:get_attribute(os).
	{ok,{_,Machine}} = THIS:get_property(machine),
	case machine:getMachine(Machine) of
		[]->
			machine:osToString(platform:getOs());
		[M|_]->
			M#machine.os
	end.
	
addRemote(Monitor)->ok.

removeRemote(Monitor)->ok.
	
remoteCommandLineAllowed()->
	true.
	
machineNameFromID(Id)->
	case platform:isNTRemote(Id) of
		true->
			machine:getMachineFromMachineID(Id);
		_->
			case platform:isCommandLineRemote(Id) of
				true->
					machine:getMachineName(Id);
				_->
					machine:getMachineFromMachineID(Id)
			end
	end.


startMonitor(This)->
	THIS:addRemote(THIS),
	BASE:startMonitor(This).
	
stopMonitor(This)->
	BASE:stopMonitor(This),
	THIS:removeRemote(THIS).
	
	
getServerProperty()->
	machine.
	
	
getHostname()->
	case THIS:get_property(machine) of
		{ok,{_,[]}}->
			"";
		{ok,{_,Machine}}->
			case machine:getMachineByHost(Machine) of
				[]->
					"";
				[Mach|_]->
					Mach#machine.host
			end;
		_->
			BASE:getHostname()
	end.

verify(Params)->
	BASE:verify(Params).

defaultTitle(Params)->
	Host = proplists:get_value(machine,Params),
	if
		length(Host)>0->
			BASE:defaultTitle(Params) ++":" ++ Host;
		true ->
			BASE:defaultTitle(Params) ++":" ++ "this server"
	end.

get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=machine,title="Host",type=server,editable=true,order=1,description="Monitoring of the host name"}
	].
