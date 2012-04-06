-module(start_svecc).   
-export([start/0, start/1]).


start()->
	application:start(crypto),
	application:start(gettext),
	application:start(ssh),
	gettext:recreate_db(),
	application:start(quickstart_mochiweb),
	application:start(svecc),
	extension_sup:start(),
	application:start(esyslog),	
	io:format("start_svecc reached end!").

start(_)->
	start().