

-module(alertPlugin).
-compile(export_all).
-record(alertlog,{id,type,name="",monitor,receiver="",title="",time,result="",content="",measurement}).

%% this is a alter plug ,insert in Action:logAlert() method£¬
%%    if the alert occur . before alter from main node will invoke this plug£¬
%%    the method hava not return (expand don't deal with this)
action_logAlertExtPoint_ErlangPlugin(OneAlert)-> 
	%%As the rpc principle, the following sentence appears content to extend point where the master node
%% 	io:format("AlertPlugin Args:~n~p~n~n",[OneAlert]), 
	
	%% if diaplay content to console , imitate under statement¡£
%% 	extension_node:io_format_local("AlertPlugin Args:~n~p~n~n",[OneAlert]), 

	Id= 	OneAlert#alertlog.id,
	Type= 	OneAlert#alertlog.type,
	Name= 	OneAlert#alertlog.name,
	Mnt= 	OneAlert#alertlog.monitor,
	Rece= 	OneAlert#alertlog.receiver,
	Title= 	OneAlert#alertlog.title,
	Time= 	OneAlert#alertlog.time,
	Result= OneAlert#alertlog.result,
	Cnt= 	OneAlert#alertlog.content,
	Msmt= 	OneAlert#alertlog.measurement,
	
	extension_node:io_format_local("~n~nget one alert~n",[]),
	extension_node:io_format_local("AlertId: ~p~n",[Id]),
	extension_node:io_format_local("Type: ~p~n",[Type]),
	extension_node:io_format_local("Name: ~p~n",[Name]),
	extension_node:io_format_local("Monitor: ~p~n",[Mnt]),
	extension_node:io_format_local("Receiver: ~p~n",[Rece]),
	extension_node:io_format_local("Title: ~p~n",[Title]),
	extension_node:io_format_local("Time: ~p~n",[Time]),
	extension_node:io_format_local("Result: ~p~n",[Result]),
	extension_node:io_format_local("Content: ~p~n",[Cnt]),
	extension_node:io_format_local("Measurement: ~p~n",[Msmt]),
%%   	
%% get one alert
%%  AlertId: '9609507BA7328691B0903CAF2E412507F58E8398'
%%  Type: "Sound alert sent"
%%  Name: "sound"
%%  Monitor: '0.1.12'
%%  Receiver: "localhost"
%%  Title: "elecc Alert, warning,Disk.12,21% full<br>82640MB free<br>104856MB total"
%%  Time: {{2010,11,24},{17,42,34}}
%%  Result: "ok"
%%  Content: []
%%  Measurement: undefined
	
	ok.



