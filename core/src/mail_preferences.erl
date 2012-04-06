%% @doc mail_preferences
%%
%%
-module(mail_preferences).
-extends(preferences).
-compile(export_all).

-define(MAIL_SERVER,mailServer).
-define(MAIL_USER,mailUser).
-define(MAIL_PASSWORD,mailPassword).
-define(AUTO_MAIL,autoEmail).
-define(AUTO_DAILY,autoDaily).
-define(AUTO_START,autoStartup).
-define(FROM_ADDRESS,fromAddress).
-define(MAIL_SERVER_BACKUP,mailServerBackup).

-define(TIMEOUT,5).

test(To)->
	Sever =
	case preferences:get(email_settings,?MAIL_SERVER) of
		{ok,[{_,V1}|_]}->
			V1;
		_->
			""
	end,
	User = 
	case preferences:get(email_settings,?MAIL_USER) of
		{ok,[{_,V2}|_]}->
			V2;
		_->
			""
	end,
	Password=
	case preferences:get(email_settings,?MAIL_PASSWORD) of
		{ok,[{_,V4}|_]}->
			V4;
		_->
			""
	end,
	From = 
	case preferences:get(email_settings,?FROM_ADDRESS) of
		{ok,[{_,V5}|_]}->
			V5;
		_->
			""
	end,
	case mail:smtp_mail_send(Sever,25,User,Password,From,To,"mail test","This is a test.  This is only a test.","",?TIMEOUT) of
		ok->
			{ok,"ok"};
		{error,Err}->
			{error,Err};
		Err2->
			{error,Err2}
	end.
