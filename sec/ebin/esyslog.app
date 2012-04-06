{application, esyslog, [
	{description,  "Erlang Syslog Server"},
	{mod, {esyslog, []}},
	{env, [
		{log, true},
        {port, 514},
        {ipaddress,"0.0.0.0"},
		{couchdb, {"localhost", 5984}}
	]}
]}.
