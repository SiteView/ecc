-record(etop_info, 
	{now = {0, 0, 0},
	 n_procs = 0,
	 wall_clock = {0, 0},
	 runtime = {0, 0},
	 run_queue = 0,
	 alloc_areas = [],
	 memi = [{total, 0},
		 {processes, 0}, 
		 {ets, 0},
		 {atom, 0},
		 {code, 0},
		 {binary, 0}],
	 procinfo = []
	}).

-record(etop_proc_info,
	{pid,
	 mem=0,
	 class=0,
	 name,
	 runtime=0,
	 cf,
	 mq=0}).

-record(pool_info,
	{name,
	 counter=0,
	 max,
	 queue}).