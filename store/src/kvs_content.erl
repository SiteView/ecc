-module(kvs_content).
-export([put/1, get/1]).

-include("kvs_define.hrl").

put(Content)->
	kvs_table:insert(Content).

get(Condition)->
	kvs_search:execute(Condition).

