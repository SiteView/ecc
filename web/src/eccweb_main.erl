%% ---
%%your comment
%%
%%---
-module(eccweb_main).
-export([start/1, stop/0, loop/2]).
-include("log.hrl").

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

%% Internal API
raw_path([]) -> [];
raw_path({ok,[_,Raw_path]}) -> Raw_path;
raw_path({ok,_}) -> [];
raw_path(Raw_path) -> raw_path({ok,string:tokens(Raw_path,"?")}).


log_print(Hostname,Method,Path,[]) -> ?Log({"[~p][~p]:~p~n",[Method,Hostname,Path]});
log_print(Hostname,Method,Path,Raw_path) -> ?Log({"[~p][~p]:~p?~p~n",[Method,Hostname,Path,web_common:urldecode(Raw_path)]}).


get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

loop(Req, DocRoot) ->   
    Path = Req:get(path),
    Method = string:to_lower(atom_to_list(Req:get(method))),
    Raw_path = raw_path(Req:get(raw_path)),
	Host = mochiweb_headers:get_value(host,Req:get(headers)),
    log_print(Host,Method,Path,Raw_path),
	case regexp:match(Path,"/elecc/rest") of
		{match,1,_}->
			?TimeSpan1(match,match,[Req,Method,Host,Path,Raw_path]);
		_->
			case Path of
				"/siteview"->
					Req:respond(web_common:respond(page_siteview:respond(Req,Path)));
				"/group" ++ Id->
					Req:respond(web_common:respond(page_group:respond(Req,Path,Raw_path)));
				"/monitortemplate" ++ Key->
					Req:respond(web_common:respond(page_monitortemplate:respond(Req,Path,Raw_path)));
				"/monitor" ++ Id->
					Req:respond(web_common:respond(page_monitor:respond(Req,Path,Raw_path)));
				"/server"++Id->
					Req:respond(web_common:respond(page_server:respond(Req,Path,Raw_path)));
				_->
				ok
			end
	end.
	%%"/" ++ File = Path,
	%%Req:serve_file(File, DocRoot).