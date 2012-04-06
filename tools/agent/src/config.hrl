

%%% LOGGING %%%
-ifndef(debug_print).
-define(debug_print, true).
-define(LOG(Msg, Args), case application:get_env(quickstart_mochiweb, loginfo) of
			    {ok,false} -> ok;
                             _ -> error_logger:info_msg(lists:concat(["[",?MODULE,"](",?LINE,"):",Msg]), Args)
			    
			end).
-define(ERROR(Msg, Args), case application:get_env(quickstart_mochiweb, errorinfo) of
				{ok,false} -> ok;
				_ -> error_logger:error_msg(lists:concat(["[",?MODULE,"](",?LINE,"):",Msg]), Args)			    
			end).
-endif.
