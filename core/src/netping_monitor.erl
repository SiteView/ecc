%% ---
%%erlang node ping monitor
%%
%%---
-module(netping_monitor,[BASE]).
-extends(atomic_monitor).
-export([update/0,new/0,get_template_property/0,get_counter_attribute/0,get_classifier/1]).
-include("monitor.hrl").
-include("monitor_template.hrl").

%%run(T)->
%%	{ok,{id,Id}} = THIS:get_attribute(id),
%%	io:format("netping_monitor:~p runing~n",[Id]),
%%	
%%	receive
%%		_->
%%			ok
%%	after 20000 ->
%%		run(T)
%%	end.

new()->
	Obj = atomic_monitor:new(),
	{?MODULE,Obj}.


update() ->
	Val = BASE:get_property(hostnode),
	case Val of
		{ok,{hostnode,Node}} when is_list(Node)->
			case net_adm:ping(list_to_atom(Node)) of 
				pong->
					io:format("net_ping monitor:~p ,ping:~p, result:pong~n",[THIS:get_property(id),Node]),
					THIS:set_attribute(ping,"pong"),
					%%THIS:set_attribute(?CATEGORY,good),
					THIS:set_attribute(?STATE_STRING,"ping result:pong");
				_->
					io:format("net_ping monitor:~p ,ping:~p, result:pang~n",[THIS:get_property(id),Node]),
					THIS:set_attribute(ping,"pang"),
					%%THIS:set_attribute(?CATEGORY,error),
					THIS:set_attribute(?STATE_STRING,"ping result:pang")
			end;
		{ok,{hostnode,Node}} when is_atom(Node)->
			case net_adm:ping(Node) of 
				pong->
					io:format("net_ping monitor:~p ,ping:~p, result:pong~n",[THIS:get_property(id),Node]),
					THIS:set_attribute(ping,"pong"),
					%%THIS:set_attribute(?CATEGORY,good),
					THIS:set_attribute(?STATE_STRING,"ping result:pong");
				_->
					io:format("net_ping monitor:~p ,ping:~p, result:pang~n",[THIS:get_property(id),Node]),
					THIS:set_attribute(ping,"pang"),
					%%THIS:set_attribute(?CATEGORY,error),
					THIS:set_attribute(?STATE_STRING,"ping result:pang")
			end;
		_->
			io:format("net_ping monitor:~p , no ping host, result:no data~n",[THIS:get_property(id)]),
			THIS:set_attribute(ping,"pang"),
			THIS:set_attribute(?CATEGORY,nodata),
			THIS:set_attribute(?NO_DATA,true)
	end.


get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{ping,'==',"pang"}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{ping,'==',"pang"}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{ping,'==',"pong"}]
	end.

get_template_property()->
	BASE:get_template_property() ++
	  [
		#property{name=hostnode,title="Erlang Node",type=text,editable=true,order=2},
		#property{name=ping,title="ping",type=text,order=1,configurable=false,state=true}
%%	  {hostnode,[{title,'Erlang 节点'},{type,text},{order,2}]},
%%	  {name,[{title,'名字'},{type,text},{order,1}]}
	  ].

get_counter_attribute()->
	[ping].