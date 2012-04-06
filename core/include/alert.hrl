-record(mail_alert,{
		sendto,				%%
		other="",			%%other address
		template="Default",	%%mail template
		duty
		}).

-record(sms_alert,{
		phone,
		type,				%%send message type
        url,                  %%web send message url 
        username,     %%web send message username
        password,      %%web send message password
        other="",
		template="Default",	
		cd,			%%send parament
		duty
		}).


-record(script_alert,{
		server,				
		script,				
		params,				
		template="Default"	
		}).

-record(snmptrap_alert,{
		msgprefix,			
		trapto,				
		template="Default"	
		}).

-record(sound_alert,{
		file			
		}).
        
-record(syslog_alert,{
        host,
        port,
        facility,
        program,
        level,
        template="Default"
        }).

-record(database_alert,{
		url,
		sql,
		dbuser,
		dbpasswd,
		dbdriver,
		dbBackup
		}).

-record(disable_alert,{
		oper,				%%operation disable/enable
		target,				%%target ，list()
        distarget           %%display target
		}).

-record(eventlog_alert,{
		template="Default"	
		}).

-record(post_alert,{
		url,				%%post form url
        proxy,
        challengeResponse,
        username,
        password,
        proxyUserName,
        proxyPassword,
		template="Default"	
		}).


-record(alertlog,{id,type,name="",monitor,receiver="",title="",time,result="",content="",alert_level,measurement,groupid,responsetime={{0,0,0},{0,0,0}},responder="",responsecontent="",cleartime={{0,0,0},{0,0,0}},times=1}).