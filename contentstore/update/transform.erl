-module(transform).

-export([update/0]).

-include_lib("stdlib/include/qlc.hrl").
-include("./src/config.hrl").

-record(activity, {application, id, type, author, visibility, created, queues, my}).

-record(alias, {application, id, label, profile}).

-record(contact, {masterID, friendID, createdDate, updatedDate, owner, relationship, 
                    contactRelationship, screenName, photoUrl, email, name}).

-record(invitation, {application, id, profile, published, updated, author, category, xn_recipient, xn_useCount, xn_expiration, xn_maxCount}).

-record(map, {application, domain, flag}).

-record(message, {application, id, published, updated, author, titile, xn_type, xn_from, xn_to, 
                    folder, state, saveFlag, summary, link}).

-record(shape, {application, name, xn_version, xn_mimetype, xn_searchable, attributes}).
%-record(attribute, {title, xn_type, xn_indexing}).

-record(tag, {application, id, createdDate, updatedDate, author, value}).

update(Content)->
	Document = converter:content2document(Content),
	Table = erlang:binary_to_atom(Content#content.xn_type, utf8),
	io:format("~p--->~p~n", [Table,Document#document.id]),
	case cache:get(fragments, Table) of
		undefined->
			mnesia:create_table(Table, [{disc_copies, [node()]}, {index, [application]}, {record_name, document}, {attributes, record_info(fields, document)}]),
			cache:set(fragments, Table, Table);
		_ ->
			nothing
	end,
	mnesia:dirty_write(Table, Document).


update() ->

	lists:foreach(fun(X)-> cache:delete(fragments, X) end, [activity, alias, contact, content, invitation, map, message, shape, tag]),

	mnesia:delete_table(contact),
	mnesia:delete_table(shape),
	mnesia:delete_table(tag),

	mnesia:del_table_index(invitation, id),
	mnesia:delete_table(invitation),

	mnesia:del_table_index(message, id),
	mnesia:delete_table(message),

	mnesia:del_table_index(alias, id),
	mnesia:delete_table(alias),

	mnesia:delete_table(map),

	mnesia:del_table_index(activity, id),
	mnesia:del_table_index(activity, author),
	mnesia:delete_table(activity),

	Contents = ets:tab2list(content),
	lists:foreach(fun(X)->update(X) end, Contents),

	mnesia:del_table_index(content, application),
	mnesia:delete_table(content).