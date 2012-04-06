package com.dragonflow.siteview.dbutils;

public class InformixMonitor extends AbstractDBMonitorImpl {

	@Override
	public String getDbType() {
		return DBMonitor.DBTYPE_INFORMIX;
	}

	@Override
	public String getPort() {
		return "1526";
	}

}
