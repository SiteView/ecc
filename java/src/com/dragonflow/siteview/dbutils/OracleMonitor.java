package com.dragonflow.siteview.dbutils;

public class OracleMonitor extends AbstractDBMonitorImpl {

	@Override
	public String getDbType() {
		return DBMonitor.DBTYPE_ORACLE;
	}

	@Override
	public String getPort() {
		return "1521";
	}

}
