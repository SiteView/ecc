%%
%% @author SiteView
%% @copyright Dragonflow Networks, Inc.
%% @version 1.0.0
%% @title The resource pool manager
%% @doc The resource pool needs started during system initializing phase.  The resource pool module implement managing the limited system resource for different monitor type.  
%% Limiting the vary large parallel process by queue the resource request when exeeding the resourse limits.

%% when monitor request resource, the resource counter increased by one, if less than limits, assert the resource allocated
%% if exeed the limit, put it into a queue.  Once finishing monitoring, the resource counter decrease by one or taking a queue
%% item  
%% 
%% 
%% 
%% @end
-module(pool).
-compile(export_all).
-include("object.hrl").
-include_lib("eunit/include/eunit.hrl").

%%
%% if (X is female) and (X is Y's parent) then (X is Y's mother)
%%
mother (Engine, {female, X}, {parent, X, Y}) ->
  eresye:assert (Engine, {mother, X, Y}).


%%
%% if (X is male) and (X is Y's parent) then (X is Y's father)
%%
father (Engine, {male, X}, {parent, X, Y}) ->
  eresye:assert (Engine, {father, X, Y}).



%%
%% if (Y and Z have the same parent X) and (Z is female)
%%    then (Z is Y's sister)
%%
sister (Engine, {parent, X, Y}, {parent, X, Z}, {female, Z}) when Y =/= Z ->
  eresye:assert (Engine, {sister, Z, Y}).



%%
%% if (Y and Z have the same parent X) and (Z is male)
%%    then (Z is Y's brother)
%%
brother (Engine, {parent, X, Y}, {parent, X, Z}, {male, Z}) when Y =/= Z ->
  eresye:assert (Engine, {brother, Z, Y}).


%%
%% if (X is Y's father) and (Y is Z's parent)
%%    then (X is Z's grandfather)
%%
grandfather (Engine, {father, X, Y}, {parent, Y, Z}) ->
  eresye:assert (Engine, {grandfather, X, Z}).


%%
%% if (X is Y's mother) and (Y is Z's parent)
%%    then (X is Z's grandmother)
%%
grandmother (Engine, {mother, X, Y}, {parent, Y, Z}) ->
  eresye:assert (Engine, {grandmother, X, Z}).



start () ->
  eresye:start (?POOLNAME),
  lists:foreach (fun (X) ->
                     eresye:add_rule (?POOLNAME, {?MODULE, X})
                 end,
                 [mother, father,
                  brother, sister,
                  grandfather, grandmother]),

  eresye:assert (relatives,
                 [{male, bob},
                  {male, corrado},
                  {male, mark},
                  {male, caesar},
                  {female, alice},
                  {female, sara},
                  {female, jane},
                  {female, anna},
                  {parent, jane, bob},
                  {parent, corrado, bob},
                  {parent, jane, mark},
                  {parent, corrado, mark},
                  {parent, jane, alice},
                  {parent, corrado, alice},
                  {parent, bob, caesar},
                  {parent, bob, anna},
                  {parent, sara, casear},
                  {parent, sara, anna}]),
  ok.
