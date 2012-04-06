-module(proxy_web).
-export([start/0]).
-include("yaws.hrl").

start()->
	MDir = code:lib_dir("proxy_web"),
	yaws:start_embedded(MDir++"/www",[{port,8100},{authdirs,[{"/",#auth{dir="/",realm ="The realm",users=[{"admin","pass"}],mod=proxy_web_auth}}]}],
					[{logdir,MDir++"/logs"}]).