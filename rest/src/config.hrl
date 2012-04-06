
%-record(predicate, {id, table, field, index, operator, function}).

%-record(activity, {application, id, type, author, visibility, created, queues, my}).

%-record(alias, {application, id, label, profile}).

-record(profile, {id,name, password, ldapserver, ldapsecurity, desc, title, email, disabled,groups,rights}).

-record(content, {application, id, xn_type, title, summary, published, updated, author, xn_private, 
					link, content, xn_width, xn_height, my}).