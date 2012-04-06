-module(app_init).
-export([start/0, stop/0]).
%% Called during application startup.
%% Put other initialization code here.

	
start() ->    
	application:start(quickstart_mochiweb),
	application:start(svecc),
	application:start(crypto),
	application:start(gettext),
	gettext:recreate_db(),
	extension_sup:start(),
	application:start(ssh),
	application:start(test),
	application:start(esyslog).

stop() ->
        application:stop(quickstart_mochiweb),
	application:stop(svecc),
	application:stop(crypto),
	application:stop(gettext),

	application:stop(ssh),
	application:stop(test),
	application:stop(esyslog).
    
