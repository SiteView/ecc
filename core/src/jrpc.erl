
%% ---
%% java rpc
%%
%%---
-module(jrpc).
-export([call/4]).

call(Token,Mod,Fun,Params)->
	{Token,catch(apply(Mod,Fun,Params))}.
