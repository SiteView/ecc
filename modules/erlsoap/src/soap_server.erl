%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.0, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.erlang.org/license/EPL1_0.txt
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is erlsoap-0.2
%%%
%%% The Initial Developer of the Original Code is Ericsson 
%%% Telecommunicatie BV. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%
%%%-------------------------------------------------------------------
%%% File    : soap_server.erl
%%% Author  : Erik Reitsma <Erik.Reitsma@etm.ericsson.se>
%%% Description : 
%%%
%%%-------------------------------------------------------------------
-module(soap_server).
-vsn("0.1").

-behaviour(application).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start/2,
	 stop/1
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}   
%%--------------------------------------------------------------------
start(_Type, StartArgs) ->
    case mod_soap:start(StartArgs) of
	{ok, Pid} -> 
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% Func: stop/1
%% Returns: any 
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
