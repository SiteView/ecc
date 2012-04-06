package com.dragonflow.siteview.dbutils;

import java.util.HashMap;
import java.util.Map;

public class Test {

	public static void main(String[] args)throws Exception{
		try{
			Map<String,String> param = new HashMap<String,String>();
			param.put("config_hostname", "192.168.5.17");
			param.put("config_database", "sysmaster");
			param.put("config_informixserver", "test");
			param.put("config_dbspace", "rootdbs");
			param.put(DBMonitor.PARAM_USERNAME, "informix");
			param.put(DBMonitor.PARAM_PASSWORD, "informix");
			System.out.println(DBMonitorFactory.getDBMonitorValue(DBMonitor.DBTYPE_INFORMIX,param,"dbspace_total_free_percent"));

/*			param.put("config_hostname", "localhost");
			param.put("config_port", "3306");
			param.put("config_database", "mysql");
			param.put("config_username", "root");
			param.put("config_password", "888888");
			System.out.println(DBMonitorFactory.getDBMonitorValue(DBMonitor.DBTYPE_MYSQL,param,"Threads_connected"));
*/
		}catch(Exception e){
			e.printStackTrace();
		}
	}
}
