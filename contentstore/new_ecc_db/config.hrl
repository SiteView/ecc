-define(Server, <<"3ren">>).
-define(Storage, 10000000).
-define(MaxCreateApplicationCount, 100).

-define(Begin, 'begin').
-define(And, 'and').
-define(Or, 'or').

-ifdef(debug).
-define(Trace(X, Y), common:trace(X, Y, ?MODULE, ?LINE)).
-else.
-define(Trace(X, Y), void).
-endif.

-record(predicate, {id, table, field, index, operator, function}).

-record(activity, {application, id, type, author, visibility, created, queues, my}).

-record(alias, {application, id, label, profile}).

%-record(application, {id, title, published, updated, author, description,
%                        viewSourceUrl, privateSource, runOwnAds,
%                        xn_premiumService,  xn_tag, xn_category, xn_domain,
%                        xn_layoutVersion, xn_active, link, seqenceNumber, storeLimit, contentCount = 0}).

-record(application, {id, title, published, updated, author, description,
                        privateSource, runOwnAds, xn_premiumService,  xn_tag, xn_category, xn_domain,
                        xn_layoutVersion, xn_active, link, seqenceNumber, storeLimit, contentCount = 0}).

%-record(contact, {masterID, friendID, createdDate, updatedDate, owner, relationship, contactRelationship, 
%                    screenName, profileUrl, photoUrl, email, name}).
-record(contact, {masterID, friendID, createdDate, updatedDate, owner, relationship, 
                    contactRelationship, screenName, photoUrl, email, name}).

%-record(content, {application, id, xn_type, title, summary, published, updated, author, xn_private, 
%					link, content, xn_width, xn_height, my}).
-record(content, {application, id, xn_type, title, summary, published, updated, author, xn_private, 
					content, xn_width, xn_height, my}).

-record(invitation, {application, id, profile, published, updated, author, category, xn_recipient, xn_useCount, xn_expiration, xn_maxCount}).

-record(map, {application, domain, flag}).

-record(message, {application, id, published, updated, author, titile, xn_type, xn_from, xn_to, 
                    folder, state, saveFlag, summary, link}).

%-record(profile, {id, title, summary, published, updated, author, link, emailVerified, xn_relationship,
%    xn_zipcode, xn_gender, xn_location, xn_country, xn_birthdate, xn_email, xn_password, xn_masterkey, point = 0}).

-record(profile, {id, title, summary, published, updated, link, emailVerified, xn_relationship,
    xn_zipcode, xn_gender, xn_location, xn_country, xn_birthdate, xn_email, xn_password, xn_maxApps, xn_masterkey, point = 0}).

-record(shape, {application, name, xn_version, xn_mimetype, xn_searchable, attributes}).
%-record(attribute, {title, xn_type, xn_indexing}).


-record(tag, {application, id, createdDate, updatedDate, author, value}).

