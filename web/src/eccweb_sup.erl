%% ---
%%your comment
%%
%%---
-module(eccweb_sup).
-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	config:init(),
    Webipaddress = config:getconfig(webipaddress),
    Webport = config:getconfig(webport),
    io:format("webipaddress:~p~nwebport:~p~n",[Webipaddress,Webport]),
    %%Ip = case os:getenv("MOCHIWEB_IP") of false -> "127.0.0.1"; Any -> Any end,   
    WebConfig = [
         {ip, Webipaddress},%%Ip},
                 {port, Webport},
                 {docroot, local_path(["www"])}],
    Web = {eccweb_main,
           {eccweb_main, start, [WebConfig]},
           permanent, 5000, worker, dynamic},
	%JSW={elecc_json_server,{elecc_json_server,start_httpd,[]},permanent,5000,worker,dynamic},

    %Processes = [Web,JSW],
	Processes = [Web],
    {ok, {{one_for_one, 10, 10}, Processes}}.


local_path(Components) ->
    local_path(Components, ?MODULE).

local_path(Components, Module) ->
    filename:join([get_base_dir(Module) | Components]).

get_base_dir(Module) ->
    {file, Here} = code:is_loaded(Module),
    filename:dirname(filename:dirname(Here)).