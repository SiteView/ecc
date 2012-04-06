-module(kvs_test).
-compile(export_all).

-include("./src/kvs_define.hrl").

%%-record(log, {time, facility, level, machine, program, pid, message, master_decoder, sub_decoder, rule_id, rule_match,rule_description, my}).
test()->
	Record = #log{
				id = null,
				time = 20100402001108, 
				facility= <<"mail">>, 
				level= notice, 
				machine= <<"developer">>, 
				program= <<"amavis">>,
				pid= 13263,
				message= <<"(13263-11) Passed CLEAN, [124.205.10.235] [124.205.10.235] <yun.zeng@dragonflow.com> -> <jieyao.li@dragonflow.com>,<xiao.tan@dragonflow.com>, Message-ID: <!&!AAAAAAAAAAAYAAAAAAAAAF/gx0/BM8pLrIRvKCytXUvigAAAEAAAAEzh9smvMbJKv3t9Mt1YsVgBAAAAAA==@dragonflow.com>, mail_id: lxM0Vc1BvRlW, Hits: -7.612, size: 18652, queued_as: 4B87F8200CC, 4906 ms\n">>,
				master_decoder = <<"amavis">>,
				sub_decoder = <<"amavis-passed-clean">>,
				rule_id = 5001, 
				rule_match = [
							{<<"srcip">>,<<"124.205.10.235">>},
							{<<"from">>,<<"yun.zeng@dragonflow.com">>},
							{<<"to">>,<<"jieyao.li@dragonflow.com">>}
						      ],
				rule_description = <<"">>,
				my=[
					{<<"srcip">>,<<"124.205.10.235">>},
					{<<"from">>,<<"yun.zeng@dragonflow.com">>},
					{<<"to">>,<<"jieyao.li@dragonflow.com">>}
					]
				},

	Records = [
				Record#log{time=20100329104500, level=notice},
				Record#log{time=20100329104600, facility= <<"mail">>}, 
				Record#log{time=20100329104700, level=error}, 
				Record#log{time=20100330114500}, 
				Record#log{time=20100330124500}, 
				Record#log{time=20100330134500, level=warning}, 
				Record#log{time=20100331144500}, 
				Record#log{time=20100331154500, facility= <<"ftp">>}, 
				Record#log{time=20100331164500}, 
				Record#log{time=20100331174500, facility= <<"ftp">>}, 
				Record#log{time=20100319184500, level=notice}, 
				Record#log{time=20100319194500}, 
				Record#log{time=20100401204500}
			 ],
	
	lists:foreach(fun(X)-> io:format("Record:~p~n", [X]), kvs_content:put(X) end, Records),

	io:format("********************************~n"),
	io:format("********************************~n"),

	Condition1 = [{time, [20100319184500, 20100320184500]}, {level, notice}],
	io:format("Condition1:~p~n", [Condition1]),

	io:format("---------------------------------------------~n"),

	Result1 = kvs_content:get(Condition1),
	io:format("Result1:~p~n", [Result1]),

	io:format("********************************~n"),
	io:format("********************************~n"),


	Condition2 = [{time, [20100329104500, 20100401174500]}, {facility, <<"ftp">>}],
	io:format("Condition2:~p~n", [Condition2]),

	io:format("---------------------------------------------~n"),

	Result2 = kvs_content:get(Condition2),
	io:format("Result2:~p~n", [Result2]).
