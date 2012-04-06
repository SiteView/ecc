-module(ecc).

-export([start/0]).

start() ->	
%% 	object:start(),
	content_store:start(),
%%	application:start(quickstart_mochiweb),
	application:start(svecc),
	application:start(crypto),
	application:start(ssh),
%% 	application:start(gettext),
%%  	gettext:recreate_db(),
	
	ok.