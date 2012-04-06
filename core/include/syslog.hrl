-define(SYSLOG_HOST,syslogHost).
-define(SYSLOG_PORT,syslogPort).
-define(SYSLOG_FACILITY,syslogFacility).
-define(SYSLOG_FACILITYS,[
        {"kernel messages", "0"},
        {"random user-level messages", "8"},
		{"mail system","16"},
		{"system daemons","24"},
        {"security/authorization messages","32"},
        {"messages generated internally by syslogd","40"},
        {"line printer subsystem","48"},
        {"network news subsystem","56"},
        {"UUCP subsystem","64"},
        {"clock daemon","72"},
        {"security/authorization messages (private)","80"},
        {"ftp daemon","88"}
		]).
-define(SYSLOG_PROGRAM,syslogProgram).
-define(SYSLOG_LEVEL, syslogLevel).
-define(SYSLOG_LEVELS, [
        {"system is unusable", "0"},
        {"action must be taken immediately", "1"},
		{"critical conditions","2"},
		{"error conditions","3"},
        {"warning conditions","4"},
        {"normal but significant condition","5"},
        {"informational","6"},
        {"debug-level messages","7"}
		]).
-define(SYSLOG_MESSAGE, syslogMessage).