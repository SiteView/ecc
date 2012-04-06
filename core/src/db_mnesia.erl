-module(db_mnesia).
-compile(export_all).

%-define(Node, node()).

%% IMPORTANT: The next line must be included
%%            if we want to call qlc:q(...)

-include_lib("stdlib/include/qlc.hrl").

%-record(?Table, {id, rights}).


%Open the database
open() ->
	mnesia:create_schema([node()]),
	mnesia:start().

%Close the database
close() ->
	mnesia:stop().

%Open Table
%TableList=[Table]
open_table(TableList) ->
	mnesia:wait_for_tables(TableList, 20000). 

%Create  localhost database
init()->
    close(),
	mnesia:create_schema([node()]),
	open().

%Remove  local database
drop()->
    close(),
	mnesia:delete_schema([node()]),
	open().
	
%Create a set of type table (that is, tuples of different keys)
%Record_fields=[Field]
create(Table, Record_fields)->
    mnesia:create_table(Table, [{attributes, Record_fields}, {disc_copies, [node()]}]). %{atomic,ok} | {aborted,{already_exists,tablename}}
	

%Bulk insert data, Key overwrite existing table records
%RowListÈç£º[{Table, Id, Rights}]
insert_data(RowList) ->	
    F = fun() -> 
			lists:foreach(fun mnesia:write/1, RowList)
		end,
    mnesia:transaction(F).


%RowÈç£º{Table, Id, Rights}
simpleinsert_data(Row) ->
	F = fun() -> 
			mnesia:write(Row) %like that :mnesia:write(element(1, Row), Row, write)			
		end,
	mnesia:transaction(F). %·µ»Ø{atomic,ok} | {aborted, Reason}
	

%Inserts a row (the last inserted)
insert_data(Table, Id, Row) ->
    case read_data(Table, Id) of
		{atomic, []} ->			
			F = fun() -> 
					mnesia:write(Row)
				end,
			mnesia:transaction(F); 
		{atomic, _} ->
			{error, existed};
		{aborted, Reason} ->
			{aborted, Reason}
	end.
	
	
%update data
%NewData=tuple()
update_data(Table, Id, NewData) ->
	case read_data(Table, Id) of
		{atomic, []} ->
			{atomic, ok};
		{atomic, _} ->			
			F = fun() -> 
					mnesia:write(NewData)
				end,
			mnesia:transaction(F);
		{aborted, Reason} ->
			{aborted, Reason}
	end.


%Id=atom()
%A row in the table to read the data, if Id does not exist, Val returned to [], if the Table does not exist, then return {aborted, {no_exists, Table}}
read_data(Table, Id) ->
	F = fun() ->
			mnesia:read({Table, Id}) %%{tablename£¬Key}
		end,
	mnesia:transaction(F). %%return {atomic, Val},Val:[{usergroup1, '1.1', ...}]

%Read all data in the table
read_all(Table) ->
	Q = qlc:q([X || X <- mnesia:table(Table)]),					%%SQL equivalent£ºselect * from table 
	F = fun() -> qlc:e(Q) end,	
	mnesia:transaction(F).  %%return success{atomic, Val}£¬if Table unexit,return{aborted,{no_exists,{usergroup12,disc_copies}}}


%Delete a row in the data table
delete_data(Table, Id) ->
	F = fun() ->
			mnesia:delete({Table, Id})   %%{tablename£¬Key}
		end,
	mnesia:transaction(F).
	
%Clear all data in the table
clear_data(Table)->
	mnesia:clear_table(Table).	

%(Permanent) drop table
delete_table(Table) ->
	mnesia:delete_table(Table). %return {aborted, Reason} | {atomic, ok}
