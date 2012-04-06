package com.dragonflow.siteview.dbutils;

public class MysqlMonitor extends AbstractDBMonitorImpl {

	@Override
	public String getDbType() {
		return DBMonitor.DBTYPE_MYSQL;
	}

	@Override
	public String getPort() {
		return "3306";
	}

}
