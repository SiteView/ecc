%
% object.hrl
%
% ----------------------------------------------------------------------
%
%  eXAT, an erlang eXperimental Agent Tool
%  Copyright (C) 2005-07 Corrado Santoro (csanto@diit.unict.it)
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>
%
%

-record (object, {class,context, property_server, executor}).  %do not change the order of the fields, used in search

-define(PROPERTY_STATUS, '__status__').
-define(TERMINATING, '__terminating__').
-define(JOINING_SYNC, '__joining__').
-define(INIT_STATUS, '__init__').
-define(END_STATUS, '__end__').
-define(BOUND_AGENT, '__agent__').
-define(ACL_QUEUE, '__acl_queue__').
-define(NO_AGENT, '__no_agent__').
-define(MULTISYNC, '__multisync__').

-define(ACTION(X), action(Self,X)).
-define(EVENT(X), event(Self,X)).
-define(PATTERN(X), pattern(Self,X)).

%% MACROS for simplifying the object 
-define(VALUE(X), object:get(Self,X)).
-define(SETVALUE(Key,Value),object:set(Self,Key,Value)).
-define(NEWATTRMONITOR(Key),object:call(Self,create_monitor,[Key])).
-define(ATTRMONITOR(Key),object:get(object:call(Self,get_monitor_object,[Key]), Key)).
-define(QUEVALUE(X), object:getTimedValue(Self,X)).
-define(SETQUEVALUE(Key,Value),object:setTimedValue(Self,Key,Value)).

% call super class's function if no clause matched.
-define(SUPERCLAUSE(X),X(Self,Other) -> object:super(Self, X, [Other]))  .

-define(POOLNAME,'pool').
-define(POOLOBJ,'resource_pool_object').
-define(LOGOBJ,'log_analyzer_object').
-define(LOGNAME,'logger').
-define(PROGRESSNAME,'report_progress').

-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
