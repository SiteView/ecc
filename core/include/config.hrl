-record(content, {application, id, xn_type, title, summary, published, updated, author, xn_private, 
					link, content, xn_width, xn_height, my}).
-record(application, {id, title, published, updated, author, description,
                        viewSourceUrl, privateSource, runOwnAds,
                        xn_premiumService,  xn_tag, xn_category, xn_domain,
                        xn_layoutVersion, xn_active, link, seqenceNumber, storeLimit, contentCount = 0}).    
-record(profile, {id, title, summary, published, updated, author, link, emailVerified, xn_relationship,
    xn_zipcode, xn_gender, xn_location, xn_country, xn_birthdate, xn_email, xn_password, xn_masterkey, point = 0}).     



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
