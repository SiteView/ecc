%% Author: cxy
%% Created: 2012-2-29
%% Description: TODO: Add description to niftest
-module(niftest).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([init/0, hello/0]).

%%
%% API Functions
%%



%%
%% Local Functions
%%


init() ->
      erlang:load_nif("./niftest", 0).

hello() ->
      "NIF library not loaded".