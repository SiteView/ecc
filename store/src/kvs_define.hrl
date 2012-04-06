-ifdef(debug).
-define(Trace(X, Y), kvs_common:trace(X, Y, ?MODULE, ?LINE)).
-else.
-define(Trace(X, Y), void).
-endif.

-define(INTERVAL, 10000).
-define(BASE, 32).
-define(DAY, 3).
-define(TIMES, 3).

-record(log, {id, time, facility, level, machine, program, pid, message, master_decoder, sub_decoder, rule_id, rule_match,rule_description, my}).
-record(index, {id=0, time=1, facility=1, level=1, machine=1, program=1, pid=0, message=0, master_decoder=1, sub_decoder=1, rule_id=1, rule_match=0,rule_description=0, my=2}).
-record(rollup, {id=0, time=0, facility=0, level=0, machine=1, program=0, pid=0, message=0, master_decoder=0, sub_decoder=0, rule_id=0, rule_match=0,rule_description=0, my=0}).

-record(storage, {id, index_name, index_table, data_name, data_table, lasted_time=now()}).