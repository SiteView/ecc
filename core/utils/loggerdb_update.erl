-module(loggerdb_update).
-export([up20110301/2]).

up20110301(Start,End) when Start > End-> ok;
up20110301(Start,End)->
	case pgsql:connect(localhost, "root", "root", [{database,"postgres"}]) of
		{ok, C}->
			{Y,M,D} = Start,
			StartDate = lists:flatten(io_lib:format("~w-~w-~w",[Y,M,D])),
			io:format("~nupdate table siteview_~s~n",[StartDate]),
			Sql = lists:flatten(io_lib:format("select id,time,measurement from \"siteview_~s\";",[StartDate])),
			case pgsql:equery(C,Sql) of
				{ok,_,Rows}->
					case pgsql:squery(C,lists:flatten(io_lib:format("ALTER TABLE \"siteview_~s\" DROP COLUMN measurement;",[StartDate]))) of
						{ok,_,_}->
							case pgsql:squery(C,lists:flatten(io_lib:format("ALTER TABLE \"siteview_~s\" ADD COLUMN measurement bytea;",[StartDate]))) of
								{ok,_,_}->
									pgsql:squery(C, "BEGIN"),
									lists:map(fun(X)->
										Id = binary_to_list(element(1,X)),
										Time = element(2,X),
										Mes = base64:decode(element(3,X)),
										io:format("."),
										USql = lists:flatten(io_lib:format("update \"siteview_~s\" set measurement = $1 where id=$2 and time=$3;",[StartDate])),
										{ok,_}=pgsql:equery(C,USql,[Mes,Id,Time])
									end,Rows),
									pgsql:squery(C, "COMMIT"),
									up20110301(sv_datetime:next_date(Start),End);
								Else->
									io:format("error:~p~n",[Else])
							end;
						Else->
							io:format("error:~p~n",[Else])
					end;
				Else->
					io:format("error:~p~n",[Else])
			end;
						
		_->
			{error,"connect db fail!"}
	end.