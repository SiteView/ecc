%%% ******************************************************************************
%%%  Copyright (c) 2004 Vlad Dumitrescu and others.
%%%  All rights reserved. This program and the accompanying materials
%%%  are made available under the terms of the Eclipse Public License v1.0
%%%  which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
%%%
%%%  Contributors:
%%%      Vlad Dumitrescu
%%% ******************************************************************************/
%%% File    : erlide_backend.erl
%%% Author  :  Vlad Dumitrescu
%%% Description :
%%% Created : 12 Aug 2004 by  Vlad Dumitrescu
%%% Version: $Revision: 1.2 $

-module(erlide_objectlist).

-export([
		 init/1,		 
		 object_list/0,
		 object_list_init/0,
		 get_object_info/1
		]).
-include("object.hrl").

init(_EventSinkPid) ->
	object_list_init(),
	ok.

%% taken from distel, for testing
%% original author: Luke Gorrie

object_list() ->	
%% 	[pinfo(Pid) || Pid <- processes()].
	[info(Object) || Object <- object:get_all()].


info(Object) ->
	{memory,PropMem} = erlang:process_info(object:property_server_of(Object), memory),
	{memory,ExeMem} = erlang:process_info(object:executorof(Object), memory),
    {object:executorof(Object), 
     object:get(Object, name),
     object:getClass(Object), 
     PropMem+ExeMem, 
	 object:get_defined_attrs(Object),
     object:get(Object, ?PROPERTY_STATUS)
    }.

pinfo(Pid) ->
    {Pid, 
     name(Pid), 
     initial_call(Pid), 
     reductions(Pid), 
     messages(Pid)
    }.

name(Pid) ->
	case process_info(Pid, registered_name) of
		{registered_name, Regname} ->
			Regname;
		_ ->
			lists:flatten(io_lib:format("~p", [Pid]))
	end.

initial_call(Pid) ->
    case process_info(Pid, initial_call) of
        {initial_call, {M, F, A}} ->
            lists:flatten(io_lib:format("~s:~s/~p", [M, F, A]));
        Other ->
            lists:flatten(io_lib:format("~p", [Other]))
    end.

reductions(Pid) ->
	{reductions, NrReds} = process_info(Pid, reductions),
	NrReds.

messages(Pid) ->
	{message_queue_len, Len} = process_info(Pid, message_queue_len),
	Len.


get_object_info(Pid) ->
	case (catch erlang:process_info(Pid)) of
		{'EXIT', Reason} ->
			{error, Reason};
		Result ->
			Result
	end.

object_list_init() ->
	case get(object_list_init) of
		true ->
			ok;
		_ ->
			put(object_list_init, true),
			spawn(fun() ->
						  object_list_updater() 
				  end)
	end.

object_list_updater() ->
	receive
		stop -> ok;
		_ -> object_list_updater()
		after 5000 ->
			erlide_jrpc:event(objectlist, {erlang:now(), self()}),
			object_list_updater()
	end.


%% end distel

