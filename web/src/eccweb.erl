-module(eccweb).
-behaviour(application).

-export([start/2,stop/1]).



%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for test.
start(_Type, _StartArgs) ->
    eccweb_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for test.
stop(_State) ->
    ok.
