-define(Server, <<"3ren">>).
-define(Storage, 10000000).
-define(MaxCreateApplicationCount, 10).

-define(Begin, 'begin').
-define(And, 'and').
-define(Or, 'or').

-ifdef(debug).
-define(Trace(X, Y), common:trace(X, Y, ?MODULE, ?LINE)).
-else.
-define(Trace(X, Y), void).
-endif.

-record(predicate, {id, table, field, index, operator, function}).
					
-record(content, {application, id, xn_type, title, summary, published, updated, author, xn_private, 
					link, content, xn_width, xn_height, my}).

-record(document, {id, application, published, updated, author, my}).
					
-record(application, {id, title, published, updated, author, description,
                        viewSourceUrl, privateSource, runOwnAds,
                        xn_premiumService,  xn_tag, xn_category, xn_domain,
                        xn_layoutVersion, xn_active, link, seqenceNumber, storeLimit, contentCount = 0}).

-record(profile, {id, title, summary, published, updated, author, link, emailVerified, xn_relationship,
    xn_zipcode, xn_gender, xn_location, xn_country, xn_birthdate, xn_email, xn_password, xn_masterkey, point = 0}).

