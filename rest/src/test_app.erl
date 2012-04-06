%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the test application.

-module(test_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for test.
start(_Type, _StartArgs) ->
    %io:format("application:start   is: ~p    ~p  ~n",[_Type,_StartArgs]),
    test_deps:ensure(),
    test_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for test.
stop(_State) ->
    ok.
