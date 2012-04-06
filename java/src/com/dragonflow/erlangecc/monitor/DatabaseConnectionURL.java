package com.dragonflow.erlangecc.monitor;

public class DatabaseConnectionURL {
	public static final String SQL_SERVER_2000_JDBC_DRIVER = com.microsoft.jdbc.sqlserver.SQLServerDriver.class
			.getName();
	public static final String ORACLE_JDBC_DRIVER = oracle.jdbc.driver.OracleDriver.class.getName();
	public static final String MY_SQL_DRIVER_5 = com.mysql.jdbc.Driver.class.getName();
	public static final String DB2_DRIVER = com.ibm.db2.jcc.DB2Driver.class.getName();
	public static final String INFORMIX_DRIVER = com.informix.jdbc.IfxDriver.class.getName();
	public static final String SYBASE_5_DRIVER = com.sybase.jdbc2.jdbc.SybDriver.class.getName();
	public static final String SQL_SERVER_2005_JDBC_DRIVER = com.microsoft.sqlserver.jdbc.SQLServerDriver.class
			.getName();

	public static String getConnectionURL(String driverType, String rawUrl) {
		if (rawUrl == null || driverType == null) {
			return "";
		}
		rawUrl = rawUrl.trim();
		String host = "";
		String port = "";
		String databaseName = "";
		int start = 0;
		int end = 0;
		if (driverType.equals(SQL_SERVER_2000_JDBC_DRIVER)) {
			String jdbcHeader = "jdbc:microsoft:sqlserver://";
			end = rawUrl.indexOf(':');
			host = rawUrl.substring(0, end);
			start = end + 1;
			end = rawUrl.indexOf('/');
			port = rawUrl.substring(start, end);
			start = end + 1;
			databaseName = rawUrl.substring(start);
			String dbUrl = host.trim() + ":" + port.trim() + ";DatabaseName=" + databaseName.trim()
					+ ";SelectMethod=cursor";
			// "jdbc:microsoft:sqlserver://localhost:1433;DatabaseName=ecc;SelectMethod=cursor";
			String jdbcUrl = jdbcHeader + dbUrl;
			return jdbcUrl;
		} else if (driverType.equals(ORACLE_JDBC_DRIVER)) {
			String jdbcHeader = "jdbc:oracle:thin:@";
			end = rawUrl.indexOf(':');
			host = rawUrl.substring(0, end);
			start = end + 1;
			end = rawUrl.indexOf('/');
			port = rawUrl.substring(start, end);
			start = end + 1;
			databaseName = rawUrl.substring(start);
			// jdbc:oracle:thin:@192.168.0.40:1521:ORACLE9I
			String dbUrl = host.trim() + ":" + port.trim() + ":" + databaseName.trim();
			String jdbcUrl = jdbcHeader + dbUrl;
			return jdbcUrl;

		} else if (driverType.equals(MY_SQL_DRIVER_5)) {
			String jdbcHeader = "jdbc:mysql://";
			end = rawUrl.indexOf(':');
			host = rawUrl.substring(0, end);
			start = end + 1;
			end = rawUrl.indexOf('/');
			port = rawUrl.substring(start, end);
			start = end + 1;
			databaseName = rawUrl.substring(start);
			// jdbc:mysql://localhost:3306/test
			String dbUrl = host.trim() + ":" + port.trim() + "/" + databaseName.trim();
			String jdbcUrl = jdbcHeader + dbUrl;
			return jdbcUrl;

		} else if (driverType.equals(DB2_DRIVER)) {
			String jdbcHeader = "jdbc:db2://";
			end = rawUrl.indexOf(':');
			host = rawUrl.substring(0, end);
			start = end + 1;
			end = rawUrl.indexOf('/');
			port = rawUrl.substring(start, end);
			start = end + 1;
			databaseName = rawUrl.substring(start);
			String dbUrl = host.trim() + ":" + port.trim() + "/" + databaseName.trim();
			String jdbcUrl = jdbcHeader + dbUrl;
			return jdbcUrl;
		}else if(driverType.equals(SQL_SERVER_2005_JDBC_DRIVER)){
			String jdbcHeader = "jdbc:sqlserver://";
			end = rawUrl.indexOf(':');
			host = rawUrl.substring(0, end);
			start = end + 1;
			end = rawUrl.indexOf('/');
			port = rawUrl.substring(start, end);
			start = end + 1;
			databaseName = rawUrl.substring(start);
			// url = jdbc:sqlserver://192.168.0.41:1433;databaseName=siteview
			String dbUrl = host.trim() + ":" + port.trim() + ";" + "databaseName=" + databaseName.trim();
			String jdbcUrl  = jdbcHeader + dbUrl;
			return jdbcUrl;
		}else if(driverType.equals(SYBASE_5_DRIVER)){
			String jdbcHeader = "jdbc:sybase:Tds:";
			end = rawUrl.indexOf(':');
			host = rawUrl.substring(0, end);
			start = end + 1;
			end = rawUrl.indexOf('/');
			port = rawUrl.substring(start, end);
			start = end + 1;
			databaseName = rawUrl.substring(start);
			String dbUrl = host.trim() + ":" + port.trim() + "/" + databaseName.trim();
			String jdbcUrl  = jdbcHeader + dbUrl;
			return jdbcUrl;
		}else if(driverType.equals(INFORMIX_DRIVER)){
			String jdbcHeader = "jdbc:informix-sqli://";
			String jdbcUrl  = jdbcHeader + rawUrl;
			return jdbcUrl;
		}
		return "";
	}

	public static void main(String[] args) {
		String rawUrl = "localhost:1521/oracle9i";
		String driverType = MY_SQL_DRIVER_5;
		String connUrl = getConnectionURL(driverType, rawUrl);
		System.out.println(connUrl);

	}

}
