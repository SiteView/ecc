%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for test.

-module(test_web).
-author('author <author@example.com>').
-include("log.hrl").
-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).


get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.




%% Internal API


raw_path([]) -> [];
raw_path({ok,[_,Raw_path]}) -> Raw_path;
raw_path({ok,_}) -> [];
raw_path(Raw_path) -> raw_path({ok,string:tokens(Raw_path,"?")}).


log_print(Hostname,Method,Path,[]) -> ?Log({"[~p][~p]:~p~n",[Method,Hostname,Path]});
log_print(Hostname,Method,Path,Raw_path) -> ?Log({"[~p][~p]:~p?~p~n",[Method,Hostname,Path,common:urldecode(Raw_path)]}).
  
  
  
loop(Req, _) ->   
    Path = Req:get(path),
    Method = string:to_lower(atom_to_list(Req:get(method))),
    Raw_path = raw_path(Req:get(raw_path)),
	Host = mochiweb_headers:get_value(host,Req:get(headers)),
    log_print(Host,Method,Path,Raw_path),
    put(hostname,Host),
    match:match(Req,Method,Host,Path,Raw_path).
    %?TimeSpan1(match,match,[Req,Method,Host,Path,Raw_path]).

  


   
