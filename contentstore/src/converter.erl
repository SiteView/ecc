-module(converter).
-export([content2document/1, document2content/2]).

-include("config.hrl").

content2document_1(Content)->
	#document{
		id = {Content#content.application, Content#content.id},
		application = Content#content.application,
		published = Content#content.published,
		updated = Content#content.updated,
		author = Content#content.author,
		my = Content#content.my
	}.

content2document_n([], Acc)->
	lists:reverse(Acc);
content2document_n([H|R], Acc)->
	Document = content2document_1(H),
	content2document_n(R, [Document|Acc]).

content2document(Contents) when is_list(Contents)->
	content2document_n(Contents, []);
content2document(Content)->
	content2document_1(Content).

document2content_1(Type, Document)->
	#content{
		application = element(1, Document#document.id),
		id = element(2, Document#document.id),
		xn_type = Type,
		title = null,
		summary = null,
		published = Document#document.published,
		updated = Document#document.updated,
		author = Document#document.author,
		xn_private = false,
		link = null,
		content = null,
		xn_width = null,
		xn_height = null,
		my = Document#document.my
	}.

document2content_n([], _, Acc)->
	lists:reverse(Acc);
document2content_n([H|R], Type, Acc)->
	Content = document2content_1(Type, H),
	document2content_n(R, Type, [Content|Acc]).

document2content(Type, Documents) when is_list(Documents) ->
	document2content_n(Documents, Type, []);
document2content(Type, Document)->
	document2content_1(Type, Document).
		