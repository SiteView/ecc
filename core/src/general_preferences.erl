%% @doc general_preferences
%%
%%
-module(general_preferences).
-extends(preferences).
-compile(export_all).

-define(LICENSE,license).
-define(LICENSE_FOR_X,license_for_x).
-define(LOCAL_ENABLED,locale_enabled).
-define(BACKUPS2KEEP,backups2keep).
-define(ALERT_ICON_LINK,alert_icon_link).
-define(REPORT_ICON_LINK,report_icon_link).
-define(GROUPS_PER_ROW,groups_per_row).
-define(DISPLAY_GAUGES,display_gauges).
-define(SUSPEND_MONITORS,suspend_monitors).
-define(AUTHORIZED_IP,authorized_ip).
-define(REQUIRE_IP_LOGIN_IN,check_address_and_login).
-define(HTTP_PORT,http_port).
-define(IS_I18N,is_i18n).
-define(CREATE_STATIC_HTML,create_static_html).
-define(WEBSERVER_ADDRESS,webserver_address).

verify(Prop,Params)->
	case Prop of
		?HTTP_PORT->
			case proplists:get_value(?HTTP_PORT,Params) of
				""->
					{error,[{?HTTP_PORT,"is empty"}]};
				"0"->
					{error,[{?HTTP_PORT,"HTTP Port must be a number greater than 0"}]};
				P->
					case re:run(P,"^[1-9]+[0-9]*$") of
						{match,_}->
							{ok,""};
						_->
							{error,[{?HTTP_PORT,"HTTP Port must be a number greater than 0"}]}
					end
			end;
		?AUTHORIZED_IP->
			case re:run(proplists:get_value(?AUTHORIZED_IP, Params),"^[\s]*[1-9,\.\*]*[\s]*$") of
				{match,_}->
					{ok,""};
				_->
					{error,[{?AUTHORIZED_IP,"The IP Address allowed access contained illegal characters"}]}
			end;
		_->
			{ok,""}
	end.