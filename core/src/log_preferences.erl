%% @doc log_preferences
%%
%%
-module(log_preferences).
-extends(preferences).
-compile(export_all).

-define(LOG_KEEP_DAYS,logKeepDays).
-define(LOG_TOTAL_LIMIT,logTotalLimit).
-define(LOG_JDBC_URL,logJdbcURL).
-define(LOG_JDBC_DRIVER,logJdbcDriver).
-define(LOG_JDBC_USER,logJdbcUser).
-define(LOG_JDBC_PASSWORD,logJdbcPassword).
-define(LOG_JDBC_URL_BACKUP,logJdbcURLBackup).


verify(Prop,Params)->
	case Prop of
		?LOG_KEEP_DAYS ->
			case proplists:get_value(?LOG_KEEP_DAYS,Params) of
				""->
					{error,[{?LOG_KEEP_DAYS,"is empty"}]};
				P->
					case re:run(P,"^[0-9]+$") of
						{match,_}->
							{ok,""};
						_->
							{error,[{?LOG_KEEP_DAYS,"The entry is not a valid number."}]}
					end
			end;
		?LOG_TOTAL_LIMIT ->
			case re:run(proplists:get_value(?LOG_TOTAL_LIMIT, Params),"^[0-9]+$") of
				{match,_}->
					{ok,""};
				_->
					{error,[{?LOG_TOTAL_LIMIT,"The entry is not a valid number."}]}
			end;
		_->
			{ok,""}
	end.