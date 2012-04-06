%% ---
%%your comment
%%
%%---
-module(elecc_json_server).

-include("rfc4627.hrl").
-include("mod_jsonrpc.hrl").

-behaviour(gen_server).

-export([start/0, start_httpd/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

start() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    mod_jsonrpc:register_service
      (Pid,
       mod_jsonrpc:service(<<"elecc_json">>,
			   <<"urn:uuid:afe1b4b5-23b0-4964-a74a-9168535c96b2">>,
			   <<"1.0">>,
			   [
			   #service_proc{name = <<"siteview_childs">>,
					  idempotent = true,
					  params = []},

				#service_proc{name = <<"group_childs">>,
					  idempotent = true,
					  params = [#service_proc_param{name = <<"group">>,
									type = <<"str">>}]}
				]
			)
		).

start_httpd() ->
    httpd:start("web/ebin/httpd.conf"),
    Ret = mod_jsonrpc:start(),
    start(),
	Ret.

%---------------------------------------------------------------------------

init(_Args) ->
    {ok, no_state}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

handle_call({jsonrpc, <<"siteview_childs">>, _ModData, []}, _From, State) ->
	SV = siteview:get_current_siteview(),
	{reply, {result, [{obj,X:get_properties()}||X<-SV:get_childs()]}, State};

handle_call({jsonrpc, <<"group_childs">>, _ModData, [Value]}, _From, State) ->
	%%io:format("handle_call:~p,~p,~p~n",[_ModData,Value,_From]),
    {reply, {result, [{obj,X}||X<-api_group:childs(list_to_atom(binary_to_list(Value)))]}, State}.

handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in test_jsonrpc: ~p", [Request]),
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in test_jsonrpc: ~p", [Info]),
    {noreply, State}.
