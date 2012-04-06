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

%%-record(content, {id, xn_type, xn_id, title, published, updated, author, xn_private, xn_application, link, my}).
%-record(content, {id, xn_type, xn_id, title, summary, published, updated, author, xn_private, 
%					xn_application, link, content, xn_width, xn_height, my}).
					
-record(content, {application, id, xn_type, title, summary, published, updated, author, xn_private, 
					link, content, xn_width, xn_height, my}).
					
-record(application, {id, title, published, updated, author, description,
                        viewSourceUrl, privateSource, runOwnAds,
                        xn_premiumService,  xn_tag, xn_category, xn_domain,
                        xn_layoutVersion, xn_active, link, seqenceNumber, storeLimit, contentCount = 0}).
%-record(profile, {id, title, summary, published, updated, author, link, xn_zipcode, xn_gender, xn_location, xn_age, xn_contry, xn_presence}).
%-record(profile, {id, xn_id, title, summary, published, updated, author, link, emailVerified, xn_relationship,
%    xn_zipcode, xn_gender, xn_location, xn_country, xn_birthdate, xn_email, xn_password, xn_masterkey}).
-record(profile, {id, title, summary, published, updated, author, link, emailVerified, xn_relationship,
    xn_zipcode, xn_gender, xn_location, xn_country, xn_birthdate, xn_email, xn_password, xn_masterkey, point = 0}).

%-record(tag, {id, application, createdDate, updatedDate, author, target, value}).
-record(tag, {application, id, createdDate, updatedDate, author, value}).

-record(role, {id, application, profile, createdDate, updatedDate, author, name}).
%%id = {application, profile, name}

%-record(message, {id, application, published, updated, author, titile, xn_type, xn_from, xn_to, folder, state, saveFlag, summary, link}).
-record(message, {application, id, published, updated, author, titile, xn_type, xn_from, xn_to, folder, state, saveFlag, summary, link}).

%-record(invitation, {id, application, profile, published, updated, author, category, xn_recipient, xn_useCount, xn_expiration, xn_maxCount}).
-record(invitation, {application, id, profile, published, updated, author, category, xn_recipient, xn_useCount, xn_expiration, xn_maxCount}).

-record(job, {id, application, published, updated, author, link, tasks}).

%-record(contact, {id, appID, masterID, friendID, createdDate, updatedDate, owner, relationship, contactRelationship, screenName, profileUrl, photoUrl, email, name}).
%%contact : id = {masterID, friendID}
-record(contact, {masterID, friendID, createdDate, updatedDate, owner, relationship, contactRelationship, screenName, profileUrl, photoUrl, email, name}).

-record(shape, {application, name, xn_version, xn_mimetype, xn_searchable, attributes}).
%-record(attribute, {title, xn_type, xn_indexing}).

-record(affiliation, {id, application, profile, published, updated, author}).
%%contact : id = {application, profile}

%-record(alias, {id, application, alias_id, label, profile}).
%%contact : id = {application, alias_id}
-record(alias, {application, id, label, profile}).

-record(dictionary, {key, value}).

-record(map, {application, domain, flag}).

-record(token, {id, email = [], password = [], expire}).

%-record(service, {id, name, type, description, price, discount, content}).
%
%-record(subscribe, {application, id, published, updated, description, start_time, end_time, amount, customize}).

