package com.dragonflow.siteview.dbutils;

import java.util.Map;

public interface DBMonitor {
	public static final String CONFIG_URL = "url";
	public static final String CONFIG_DRIVER = "driver";
	public static final String PARAM_USERNAME = "config_username";
	public static final String PARAM_PASSWORD = "config_password";
	
	public static final String DBTYPE_ORACLE = "oracle";
	public static final String DBTYPE_MYSQL = "mysql";
	public static final String DBTYPE_INFORMIX = "informix";
	public void setParameters(Map<String,String> param) throws Exception;
	public String getValue(String key) throws Exception;
}
