%% ---
%%应用程序例程
%%
%%---
-module(svecc_logapp).
-behaviour(application).

-export([start/2,stop/1]).

%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%% {ok, Pid, State} |
%% {error, Reason}
%% Description: This function is called whenever an application
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, StartArgs) ->
%% 	erlide_log:log("******************* svecc_logapp OK*************************"),
	erlang:set_cookie(node(),'3ren'),
	
    % error_logger:add_report_handler(ejabberd_logger_h, "./error_logs/logger_log.txt"),
	% ejabberd_loglevel:set(4),
	% inets:start(),
	% init_os_template:start(),
	svecc_logsup:start_link(StartArgs).

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored.
%%--------------------------------------------------------------------
stop(_State) ->
	ok.
