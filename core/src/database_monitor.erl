%% @copyright 2008-2009 Dragonflow
%% @author shaohua.wang, jian.huang, kaiyang.cheng
%% @version 1.0

%% @doc database_monitor
%%
%% This module is the monitor for database querying.
%%1. Select data from on table from the destinate database, including query time, columns and rows number of table and the first line of the table
%%2. Versions supported: Any database with a valid JDBC driver that supports SQL queries
%%	a. db2 8.x and 9.x
%%	b. informix 9.x
%%	c. Sql Server 2000, 2005
%%	d. Oracle 8i, 9i, 10g and 11g
%%	e. Sybase 11.x
%%	f. Mysql 4.x and 5.x
%% Platform: All

-module(database_monitor, [BASE]).
-extends(db_monitor_base).
-compile(export_all).

-include("monitor.hrl").
-include("monitor_template.hrl").
-include("db_monitor_base.hrl").


%% @spec new() -> Obj
%% @type Obj = term()
%% @doc create a new instance for database_monitor.
new() ->
	Base = db_monitor_base:new(),	
	{?MODULE,Base}.


%% @spec update() -> Result
%% @type Result = term()
%% @doc update is the run function called by schedule to monitor database.
update() ->	
	BASE:update(),
	ok.


%% @spec get_classifier(error) -> List
%% @type List = [Tuple]
%% @type Tule = {status, Logic, Value}
%% @type Logic = '!=' | '==' | '>' | '<' | 'contain
%% @type Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error) ->
	case THIS:get_property(error_classifier) of
		{ok, {error_classifier, Classifier}} ->
			Classifier;
		_ ->
			[{status, '==', "error"}]
	end;
get_classifier(warning) ->
	case THIS:get_property(warning_classifier) of
		{ok, {warning_classifier, Classifier}} ->
			Classifier;
		_ ->
			[{status, '==', "n/a"}]
	end;
get_classifier(good) -> 
	case THIS:get_property(good_classifier) of
		{ok, {good_classifier, Classifier}} ->
			Classifier;
		_ ->
			[{status, '==', "good"}]
	end.

%% @spec get_template_property() -> List
%% @type List = [term()]
%% @doc get_template_property is the function called by schedule to show user the GUI interface to input data.
get_template_property()->
	BASE:get_template_property().

	
verify(Params) ->
	BASE:verify(Params).

%% @spec getScalarValues(Prop, Params) -> List
%% @type Prop = atom()
%% @type Params = [term()]
%% @type List = [Opt]
%% @type Opt = {ShowContent, Value}
%% @type ShowContent = string()
%% @type Value = string()
%% @doc getScalarValues is the function called by schedule to show a dropdown list box and its value
getScalarValues(Prop,Params)->
	case Prop of		
		driver ->
			[
			 {"JDBC Driver for Oracle", "oracle.jdbc.driver.OracleDriver"},
			 {"JDBC Driver for MS SQL Server 2000", "com.microsoft.jdbc.sqlserver.SQLServerDriver"},
			 {"JDBC Driver for DB2-V8-V9", "com.ibm.db2.jcc.DB2Driver"},
			 {"JDBC Driver for MS SQL Server 2005", "com.microsoft.sqlserver.jdbc.SQLServerDriver"},
			 {"JDBC Driver for Informix", "com.informix.jdbc.IfxDriver"},
			 {"JDBC Driver for Sybase-11", "com.sybase.jdbc2.jdbc.SybDriver"},
			 {"JDBC Driver for MySQL", "com.mysql.jdbc.Driver"}
			];
		_ ->
			BASE:getScalarValues(Prop, Params)
	end.

