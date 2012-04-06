-module(snmp_read_arp).
-export([get_ip_from_arp/2]).
-include_lib("snmp/include/snmp_types.hrl").

-define(TIMEOUT,3000).


get_ip_from_arp(IpSrc,[])->
	[ipmask2cidr(X)||X<-read_arp(IpSrc,["public"])];
get_ip_from_arp(IpSrc,Communitys)->
	[ipmask2cidr(X)||X<-read_arp(IpSrc,Communitys)].

read_arp([],_)->[];	
read_arp([Ip|T],Comnunitys)->
	read_arp_s(Ip,Comnunitys) ++ read_arp(T,Comnunitys).
	
read_arp_s(_,[])->[];
read_arp_s(Ip,[C|T])->
	Tip = ipstr2tuple(Ip),
	case test_community(Tip,C) of
		ok->
			Ips = read_row(Tip,[1,3,6,1,2,1,4,20,1,1],[1,3,6,1,2,1,4,20,1,1]),
			Mask = read_row(Tip,[1,3,6,1,2,1,4,20,1,3],[1,3,6,1,2,1,4,20,1,3]),
			lists:zip(Ips,Mask);
		_->
			read_arp_s(Ip,T)
	end.

read_row(Tip,Oid,Prefix)->
	case snmp_ex2_manager:sync_get_next(Tip,[Oid]) of
		{ok,{noError,_,[Vb = #varbind{}|_]},_}->
			% io:format("vb:~p~n",[Vb]),
			case lists:prefix(Prefix,Vb#varbind.oid) of
				true->
					[Vb#varbind.value] ++ read_row(Tip,Vb#varbind.oid,Prefix);
				_->
					[]
			end;
		_->
			[]
	end.
	
	
test_community(Tip,Community)->
	snmp_ex2_manager:unagent(Tip),
	snmp_ex2_manager:agent(Tip,[{community,Community},{timeout,?TIMEOUT}]),
	case snmp_ex2_manager:sync_get_next(Tip,[[1,3,6]]) of
		{ok,_,_}->
			ok;
		_->
			error
	end.
	
% ------------------------------------------------------
ipstr2tuple(Ipstr)->
	list_to_tuple([list_to_integer(X)||X<-string:tokens(Ipstr,".")]).
	
tuple2ipstr(Tuple)->
	string:join([integer_to_list(X)||X<-tuple_to_list(Tuple)],".").
	
ipmask2cidr({Ip,Mask})->
	Ip2 = lists:zipwith(fun(X,Y)-> X band Y end,Ip,Mask),
	W = lists:sum([get_bitnum(X)||X<-Mask]),
	Ipstr = string:join([integer_to_list(X)||X<-Ip2],"."),
	Ipstr ++ "/" ++ integer_to_list(W).
	
get_bitnum(0)->0;
get_bitnum(N)->
	1 + get_bitnum(N div 2).
