-module(test).
-compile(export_all).

-include("../include/define.hrl").

auth(OS, Host, User, Password)->
	put(os, OS),
	put(host, Host),
	put(user, User),
	put(password, Password).
	
auth()->
	case get(os) of
		undefined->
			error;
		OS->
			case get(host) of
				undefined->
					error;
				Host->
					case get(user) of
						undefined->
							error;
						User->
							case get(password) of
								undefined->
									error;
								Password->
									{OS, Host, User, Password}
							end
					end
			end
	end.
	
cmd(Query)->
	CMD = case auth() of
		error->
			lists:append([?WMIC, " -d -1 -U administrator%kennyy //192.168.6.198 \"", Query, "\""]);
		{_, Host, User, Password}->
			lists:append([?WMIC, " -d -1 -U ", User, "%", Password, " //", Host, " \"", Query, "\""])
	end,
	Result = os:cmd(CMD),
	case Result of
		"NTSTATUS: " ++ Rest1->
			{error, Rest1};
		"/bin/sh: " ++ Rest2->
			{error, Rest2};
		_ ->
			{ok, lists:map(fun(X)->list_to_tuple(string:tokens(X, "|")) end, string:tokens(Result, "\n"))}
	end.

cpu()->
	case auth() of
		error->
			wmi:cpu(?WinXP, "192.168.6.198", "administrator", "kennyy");
		{OS, Host, User, Password}->
			wmi:cpu(OS, Host, User, Password)
	end.


memory()->
	case auth() of
		error->
			wmi:memory(?WinXP, "192.168.6.198", "administrator", "kennyy");
		{OS, Host, User, Password}->
			wmi:memory(OS, Host, User, Password)
	end.

disk()->
	case auth() of
		error->
			wmi:disk(?WinXP, "192.168.6.198", "administrator", "kennyy");
		{OS, Host, User, Password}->
			wmi:disk(OS, Host, User, Password)
	end.

disk(Disk)->
	case auth() of
		error->
			wmi:disk(?WinXP, "192.168.6.198", "administrator", "kennyy", Disk);
		{OS, Host, User, Password}->
			wmi:disk(OS, Host, User, Password, Disk)
	end.

service()->
	case auth() of
		error->
			wmi:service(?WinXP, "192.168.6.198", "administrator", "kennyy");
		{OS, Host, User, Password}->
			wmi:service(OS, Host, User, Password)
	end.
	
service(Service)->
	case auth() of
		error->
			wmi:service(?WinXP, "192.168.6.198", "administrator", "kennyy", Service);
		{OS, Host, User, Password}->
			wmi:service(OS, Host, User, Password, Service)
	end.
	
process()->
	case auth() of
		error->
			wmi:process(?WinXP, "192.168.6.198", "administrator", "kennyy");
		{OS, Host, User, Password}->
			wmi:process(OS, Host, User, Password)
	end.

process(Process)->
	case auth() of
		error->
			wmi:process(?WinXP, "192.168.6.198", "administrator", "kennyy", Process);
		{OS, Host, User, Password}->
			wmi:process(OS, Host, User, Password, Process)
	end.


network()->
	case auth() of
		error->
			wmi:network(?WinXP, "192.168.6.198", "administrator", "kennyy");
		{OS, Host, User, Password}->
			wmi:network(OS, Host, User, Password)
	end.
	
network(Network)->
	case auth() of
		error->
			wmi:network(?WinXP, "192.168.6.198", "administrator", "kennyy", Network);
		{OS, Host, User, Password}->
			wmi:network(OS, Host, User, Password, Network)
	end.


directory()->
	case auth() of
		error->
			wmi:directory(?WinXP, "192.168.6.198", "administrator", "kennyy", "F:", "false", "");
		{OS, Host, User, Password}->
			wmi:directory(OS, Host, User, Password, "F:", "false", "")
	end.
	
directory(Path, Recursive, Match)->
	case auth() of
		error->
			wmi:directory(?WinXP, "192.168.6.198", "administrator", "kennyy", Path, Recursive, Match);
		{OS, Host, User, Password}->
			wmi:directory(OS, Host, User, Password, Path, Recursive, Match)
	end.
