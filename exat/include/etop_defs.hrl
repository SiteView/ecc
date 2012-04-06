-define(SYSFORM,
	" ~-72w~10s~n"
	" Load:  cpu  ~8w               Memory:  total    ~8w    binary   ~8w~n"
	"        procs~8w                        processes~8w    code     ~8w~n"
	"        runq ~8w                        atom     ~8w    ets      ~8w~n").

-record(opts, {node=node(), port = 8415, accum = false, intv = 5000, lines = 10, 
	       width = 700, height = 340, sort = runtime, tracing = on,
	       %% Other state information
	       out_mod=etop_gui, out_proc, server, host, tracer, store, 
	       accum_tab, remote}).
