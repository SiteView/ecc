package com.dragonflow.erlangecc.monitor;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.dbcp.BasicDataSource;

import com.dragonflow.siteview.CloseUtils;
import com.dragonflow.siteview.exception.SQLqueryException;
import com.dragonflow.siteview.infra.util.ServicePlatform;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public class DatabaseMonitor extends BaseMonitor {
	int dbMaxColumns = 10;
	int dbMaxRows = 100;
	int dbMaxSummary = 100;
	String dbUserName = "";
	String dbUrl = "";
	String userpass = "";
	String query_cmd = "";
	int connectionTimeout = 60;
	int queryTimeout = 60;
	String driverName = "";
	String queryFileCmd = "";
	String status = "";
	String dbMachineName = "localhost";
	public Map<String, OtpErlangObject> resultContents = new HashMap<String, OtpErlangObject>();
	int rows = 0;
	int columns = 0;
	long roundTripTime = -1L;
	String firstColumn = "";
	String content_match = "";

	BasicDataSource dbs = new BasicDataSource();
	String lastError = "";
	static final String EMPTY_ERLANG_LIST_STRING = (new OtpErlangList())
			.toString();

	public static final String GOOD = "good";
	public static final String STATUS = "status";
	public static final String ERROR = "error";
	public static final String CAPTION = "caption";
	public static final String JAVA_TIME_USED = "java_time_used";
	public static final String ROWS = "rows";
	public static final String SQL_ERROR = "sql_error";
	public static final String SQL_ERROR_REASON = "sql_error_reason";
	public static final String COLUMNS = "columns";

	// this function must run befor update()
	public void initArgs(Map<String, Object> map) {
		String s = null;

		try {
			s = (String) map.get("queryFileCmd");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.queryFileCmd = s;
			}
			s = (String) map.get("dbMaxColums");
			if (s != null && s.length() > 0) {
				this.dbMaxColumns = Integer.parseInt((String) map
						.get("dbMaxColums"));
			}
			s = (String) map.get("dbMaxRows");
			if (s != null && s.length() > 0) {
				this.dbMaxRows = Integer.parseInt(s);
			}
			s = (String) map.get("driverName");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.driverName = s.trim();
			}
			s = (String) map.get(ROWS);
			if (s != null && s.length() > 0) {
				this.rows = Integer.parseInt(s);
			}
			s = (String) map.get("queryTimeout");
			if (s != null && s.length() > 0) {
				this.queryTimeout = Integer.parseInt(s) * 1000;
			}
			s = (String) map.get("connectionTimeout");
			if (s != null && s.length() > 0) {
				this.connectionTimeout = Integer.parseInt(s) * 1000;
			}
			s = (String) map.get("dbMaxSummary");
			if (s != null && (s.length() > 0)) {
				this.dbMaxSummary = Integer.parseInt(s);
			}
			s = (String) map.get("dbUserName");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.dbUserName = s.trim();
			}
			s = (String) map.get("dbUrl");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.dbUrl = s.trim();
			}
			s = (String) map.get("userpass");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.userpass = s.trim();
			}
			s = (String) map.get("query_cmd");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.query_cmd = s.trim();
			}
			s = (String) map.get("content_match");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.content_match = s.trim();
			}
		} catch (Exception e) {
			lastError = e.getMessage();
			e.printStackTrace();
		}

	}

	private void init_config() {
		try {
			String jdbcUrl = DatabaseConnectionURL.getConnectionURL(
					this.driverName, this.dbUrl);
			System.out
					.println("------ get database connection url from rawUrl: "
							+ jdbcUrl);
			dbs.setUrl(jdbcUrl);
			dbs.setDriverClassName(this.driverName);
			dbs.setUsername(this.dbUserName);
			dbs.setPassword(this.userpass);
			dbs.setMaxWait(this.connectionTimeout);
			dbs.setMaxActive(2);
		} catch (Exception e) {
			lastError = e.getMessage();
			e.printStackTrace();
		}
	}

	public Map<String, Object> update() {
		this.initArgs(this.getMessage().getParams());
		Map<String, Object> map = new HashMap<String, Object>();
		this.init_config();
		Connection conn = null;
		ResultSet results = null;
		PreparedStatement statement = null;
		try {
			long begin = System.currentTimeMillis();
			conn = dbs.getConnection();

			if (this.query_cmd != null && this.query_cmd.length() > 0) {
				statement = conn.prepareStatement(this.query_cmd);

			} else if (this.queryFileCmd != null
					&& this.queryFileCmd.length() > 0) {
				statement = conn.prepareStatement(this.queryFileCmd);
			} else {
				throw new SQLqueryException("Query Command Error");
			}
			if (this.queryTimeout > 0) {
				statement.setQueryTimeout(this.queryTimeout);
			}
			statement.setFetchSize(this.dbMaxRows);
			results = statement.executeQuery();

			// 1. set columns of the queried result.
			ResultSetMetaData meta = results.getMetaData();
			this.columns = meta.getColumnCount();
			map.put(COLUMNS, "" + this.columns);

			// 2. set caption of the queried result.
			String label = "";
			for (int i = 1; i <= this.dbMaxColumns && i <= this.columns; i++) {
				label = label + meta.getColumnLabel(i) + "\t";
			}
			label += "\n";
			map.put(CAPTION, label);

			int count = 0;
			int rowth = 0;
			while (results.next() && count++ < this.dbMaxRows) {
				rowth++;
				String row = "";
				for (int i = 1; i <= this.dbMaxColumns && i <= this.columns; i++) {
					Object obj = results.getObject(i);
					String col = null;
					if (obj == null) {
						col = "";
					} else {
						col = obj.toString();
					}
					row = row + col + "\t";

				}
				row = row + "\n";

				// 3. set each record of the queried result.
				map.put("row_" + rowth + "th", row);
			}

			// 4. set rows of the queried result.
			map.put(ROWS, "" + rowth);

			// 5. set content match value
			if (this.content_match.length() > 0
					&& !this.content_match.equals("undefined")) {
//				String[] ss = this.content_match.split(",");
			String pattern = "";
//				for (String s : ss) {
//					pattern += s + "\t";
//				}
				pattern = content_match.trim();
				String first_row = map.get("row_1th").toString().trim();
				boolean match = false;
				if (first_row != null) {
					Boolean siregu=isRegularExpression(pattern);
					if (siregu)
					{
					 match = first_row.matches(pattern.substring(1,pattern.lastIndexOf("/") ));
					}else
					{
						match=first_row.contains(pattern);
					}
				}
				map.put("content_match", (new Boolean(match)).toString());
			}

			String row_1th = (String) map.get("row_1th");
			if (row_1th != null && row_1th.length() > 0) {
				map.put("column_1th", row_1th.split("\t")[0]);
			}

			// 6. set time used for querying the result.
			long time_used = System.currentTimeMillis() - begin;
			map.put(JAVA_TIME_USED, "" + time_used / 1000);

		} catch (SQLqueryException e) {
			lastError = e.getMessage();
			// lastError = "SQL Query Error";
			e.printStackTrace();

		} catch (SQLException e) {
			lastError = e.getMessage();
			// lastError = "SQL Query Error";
			e.printStackTrace();
		} finally {
			try {
				if (results != null)
					results.close();
			} catch (SQLException e) {
			}
			CloseUtils.closeStatement(statement);
			CloseUtils.closeConnection(conn);
		}

		return map;
	}

	public static boolean isRegularExpression(String s) {
		if (s.startsWith("/")) {
			if (s.endsWith("/")) {
				
					return true;
			}
		}
		return false;
	}

	@Override
	public int handleMessage() {
		String action = this.getMessage().getAction();
		Map<String, Object> result = null;
		if (action != null && action.equals("update")) {
			result = this.update();
			if (lastError.length() > 0) {
				result
						.put(ERROR, ServicePlatform
								.replaceUnicodeChar(lastError));
			}
			System.out.println("update result: " + result);
			this.sendResponse("ok", result);
		} else {
			result = new HashMap<String, Object>();
			result.put("error", "no such action. \n");
			this.sendResponse("error", result);
		}
		return 0;
	}

	public static void main(String[] args) {
		DatabaseMonitor mo = new DatabaseMonitor();
		mo.driverName = "com.informix.jdbc.IfxDriver";
		mo.dbUrl = "192.168.1.1:1234/dg:INFORMIXSERVER=myserver";
		mo.update();
	}

}
