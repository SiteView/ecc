package com.dragonflow.erlangecc.monitor;

import java.io.File;
import java.io.FileInputStream;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jgl.Array;

import org.apache.commons.dbcp.BasicDataSource;
import org.apache.commons.lang.math.NumberUtils;

import com.dragonflow.siteview.CloseUtils;
import com.dragonflow.siteview.infra.util.ServicePlatform;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.informix.util.stringUtil;

public class OracleRACMonitor extends BaseMonitor {
	OtpErlangList EMPTY_ERLANG_LIST_STRING = new OtpErlangList();
	static String template_path = ServicePlatform.getRoot()
			+ "/templates.applications/oracle_jdbc_monitor.txt";
	public static String CRLF = "\r\n";
	String lastError = "";
	Map<String, Object> Counters = new HashMap<String, Object>();
	String server1 = "192.168.6.201", server2 = "192.168.6.202";
	String port1 = "1521", port2 = "1521";
	String database = "SY.siteview";
	String UserName = "system";
	String Password = "system";
	Map<String, String> statsNameMap;
	String driver = "oracle.jdbc.driver.OracleDriver";
	int Connectiontimeout = 60;
	int Querytimeout = 60;
	private static final String SYSTEM_TABLE = "V$SYSSTAT";
	private static final String SESSION_TABLE = "V$SESSTAT";
	private static final String FREE_TABLE_TABLE = "% Free Table Space";
	static String instanceNameQuery = getSettingValue("INSTANCENAMEQUERY=");
	static String counterNameQuery = getSettingValue("COUNTERNAMEQUERY=");
	static String sysResults = getSettingValue("SYSRESULTS=");
	static String sesResults = getSettingValue("SESRESULTS=");
	static StringBuffer pre_error = new StringBuffer("");
	static String sidValueQuery = getSettingValue("SIDVALUEQUERY=");
	static String statCounterQuery = getSettingValue("STATCOUNTERQUERY=");
	static String freeTableSpaceQuery = getSettingValue("FREETABLESPACEQUERY=");
	static List<String> tableNameList = new ArrayList<String>();
	static List<String> tableQueryList = new ArrayList<String>();
	BasicDataSource dbs = new BasicDataSource();
	Connection conn = null;
	static {
		try {
			Class.forName(oracle.jdbc.driver.OracleDriver.class.getName())
					.newInstance();
		} catch (InstantiationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		int i = 0;
		do {
			String table_name = getSettingValue("TABLENAME" + i + "=");
			String table_query = getSettingValue("TABLEQUERY" + i + "=");
			if (table_name == null || table_name.trim().length() == 0
					|| table_query == null || table_query.trim().length() == 0) {
				break;
			}
			tableNameList.add(table_name);
			tableQueryList.add(table_query);
			i++;
		} while (true);
		System.out.println(tableNameList);
		System.out.println(tableQueryList);
	}

	public static String getSettingValue(String entry) {
		StringBuffer file_buf = new StringBuffer();
		try {
			File template = new File(template_path);
			file_buf = readTemplateFile(template);
			String[] query_cmds = file_buf.toString().split(CRLF);
			for (int i = 0; i < query_cmds.length; i++) {
				if (query_cmds[i].startsWith(entry)) {
					return query_cmds[i].substring(entry.length());
				}
			}
		} catch (Exception e) {
			pre_error.append(e.getMessage() + "; <br>");
			e.printStackTrace();
		}

		return "";
	}

	private static StringBuffer readTemplateFile(File file) {
		FileInputStream file_input = null;
		StringBuffer buf = new StringBuffer();

		try {
			byte bytes[] = new byte[32768];
			file_input = new FileInputStream(file);
			while ((file_input.read(bytes)) != -1) {
				buf.append(new String(bytes));
			}
			CloseUtils.closeInputStream(file_input);
		} catch (Exception e) {
			pre_error.append(e.getMessage() + "; <br>");
			e.printStackTrace();
		} finally {
			CloseUtils.closeInputStream(file_input);
		}
		return buf;
	}

	public void initArgs(Map<String, Object> map) {
		String s = null;

		try {
			s = (String) map.get("pHostname1");
			if (s != null && s.length() > 0) {
				this.server1 = s.trim();
			}
			s = (String) map.get("pHostname2");
			if (s != null && s.length() > 0) {
				this.server2 = s.trim();
			}
			s = (String) map.get("pPort1");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.port1 = s.trim();
			}
			s = (String) map.get("pPort2");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.port2 = s.trim();
			}
			s = (String) map.get("pDatabase");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.database = s.trim();
			}
			s = (String) map.get("pUser");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.UserName = s.trim();
			}
			s = (String) map.get("pPassword");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.Password = s.trim();
			}
			s = (String) map.get("pDriver");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.driver = s.trim();
			}
			s = (String) map.get("pConnection_timeout");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.Connectiontimeout = Integer.parseInt(s.trim());
			}
			s = (String) map.get("pQuery_timeout");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.Querytimeout = Integer.parseInt(s.trim());
			}

		} catch (Exception e) {
			lastError = e.getMessage();
			e.printStackTrace();
		}

	}

	public void initArgs1(Map<String, Object> map) {
		String s = null;

		try {
			s = (String) map.get("pHostname1");
			if (s != null && s.length() > 0) {
				this.server1 = s.trim();
			}
			s = (String) map.get("pHostname2");
			if (s != null && s.length() > 0) {
				this.server2 = s.trim();
			}
			s = (String) map.get("pPort1");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.port1 = s.trim();
			}
			s = (String) map.get("pPort2");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.port2 = s.trim();
			}
			s = (String) map.get("pDatabase");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.database = s.trim();
			}
			s = (String) map.get("pUser");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.UserName = s.trim();
			}
			s = (String) map.get("pPassword");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.Password = s.trim();
			}
			s = (String) map.get("pDriver");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.driver = s.trim();
			}
			s = (String) map.get("pConnection_timeout");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.Connectiontimeout = Integer.parseInt(s.trim());
			}
			s = (String) map.get("pQuery_timeout");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.Querytimeout = Integer.parseInt(s.trim());
			}

			Object o = map.get("counters");
			if (o != null && (o instanceof OtpErlangList)) {
				OtpErlangList counters = (OtpErlangList) o;
				if (counters.arity() > 0) {
					OtpErlangObject[] objs = counters.elements();
					for (OtpErlangObject e : objs) {
						if (e instanceof OtpErlangTuple) {
							OtpErlangTuple t = (OtpErlangTuple) e;
							String id = ((OtpErlangString) t.elementAt(0))
									.stringValue();
							String value = ((OtpErlangString) t.elementAt(1))
									.stringValue();
							this.Counters.put(id, value);
						}
					}
				}
			} else {
				this.Counters.put("MALFORMED COUNTER ID", null);
			}
			// this.Counters = ErlangUtils.erlangList2arrayList(counters);

		} catch (Exception e) {
			lastError = e.getMessage();
			e.printStackTrace();
		}

	}

	private Connection getconnection() {
		try {
			String conurl = "jdbc:oracle:thin:@(DESCRIPTION=(ADDRESS_LIST=(ADDRESS=(PROTOCOL=TCP)( HOST="
					+ this.server1
					+ ")(PORT="
					+ this.port1
					+ "))(ADDRESS=(PROTOCOL=TCP)(HOST="
					+ this.server2
					+ ")(PORT="
					+ this.port2
					+ ")))(CONNECT_DATA=(SERVICE_NAME=" + this.database + ")))";

			DriverManager.setLoginTimeout(Connectiontimeout);
			conn = DriverManager.getConnection(conurl, this.UserName,
					this.Password);

		} catch (Exception e) {
			this.lastError = e.getMessage();
			e.printStackTrace();
		}

		return conn;

	}

	public Map<String, Object> getBrowseData() {
		this.initArgs(this.getMessage().getParams());
		Map<String, Object> resp = new HashMap<String, Object>();
		conn = this.getconnection();
		if (conn == null) {
			return resp;
		}

		try {
			List<String> counterNameList = this.getResult(conn,
					counterNameQuery);
			List<String> instanceNameList = null;
			List<String> sidValueList = null;
			List<String> freeTableSpaceList = null;
			List<String> tableQueryArray[] = new ArrayList[tableNameList.size()];

			if (this.lastError.toString().length() <= 0) {
				instanceNameList = getResult(conn, instanceNameQuery);
				if (this.lastError.toString().length() <= 0) {
					sidValueList = getResult(conn, sidValueQuery);
				}
				if (this.lastError.toString().length() <= 0) {
					freeTableSpaceList = getResult(conn, freeTableSpaceQuery);
				}
			}
			int j = 0;
			for (int i = 0; i < tableQueryList.size(); i++) {
				String tableQueryEntry = (String) (tableQueryList.get(i));
				tableQueryArray[j++] = getResult(conn, tableQueryEntry);
			}

			buildXML(resp, counterNameList, instanceNameList, sidValueList,
					freeTableSpaceList, tableQueryArray);

		} catch (Exception e) {
			this.lastError = e.getMessage() + "; <br>";
			e.printStackTrace();
		}
		if (conn != null)
			CloseUtils.closeConnection(conn);

		return resp;
	}

	public Map<String, Object> update() {
		this.initArgs1(this.getMessage().getParams());
		conn = this.getconnection();
		HashMap<String, Object> hashmap = new HashMap<String, Object>();
		Map<String, Object> results = new HashMap<String, Object>();
		int counter_errors=0;
		Set<String> counter_set = this.Counters.keySet();
		int counter_size = this.Counters.size();
		String[] states = new String[counter_size];
		Map<String, Object> resp = new HashMap<String, Object>();
		Iterator<String> counter_it;
		try {

			if (conn == null) {
				throw new Exception(this.lastError);
			}
			List<String> freeTableSpace_array = null;
			counter_it = counter_set.iterator();
			int ses_count = 0;
			label: for (int i = 0; i < counter_size && counter_it.hasNext(); i++) {
				String counter = counter_it.next();
				String counter_key = counter;
				String counter_value = "";
				if (counter.indexOf(SYSTEM_TABLE) >= 0) {
					List<String> sysStat_array = null;
					if ((sysStat_array = (ArrayList<String>) hashmap
							.get(sysResults)) == null) {
						sysStat_array = this.getResult(conn, sysResults);
						hashmap.put(sysResults, sysStat_array);
					}
					counter = counter.substring(counter.indexOf(SYSTEM_TABLE)
							+ SYSTEM_TABLE.length());
					String counter_name = counter.substring(counter
							.indexOf(" ") + 1, counter.length());
					int index = 0;
					do {
						if (index >= sysStat_array.size()) {
							continue label;
						}
						String ss = (String) sysStat_array.get(index);
						if (ss.startsWith(counter_name + "\t")) {
							String columns[] = ss.split("\t");
							states[i] = columns[1];
						}
						index++;
					} while (true);
				}
				if (counter.indexOf(SESSION_TABLE) >= 0) {
					counter = counter.substring(counter.indexOf(SESSION_TABLE)
							+ SESSION_TABLE.length());
					counter = counter.substring(counter.indexOf(" ") + 1);
					String counter_name = counter.substring(
							counter.lastIndexOf("/") + 1).trim();
					String sid = counter.substring(0, counter.indexOf("/"));
					if (this.statsNameMap == null) {
						this.statsNameMap = getStatNameMap(conn);
					}
					counter_value = (String) this.statsNameMap
							.get(counter_name);
					String ses_sid_key = sesResults + " WHERE SID = " + sid;
					List<String> sesStat_array = null;
					if ((sesStat_array = (List) hashmap.get(ses_sid_key)) == null) {
						sesStat_array = getResult(conn, ses_sid_key);
						hashmap.put(ses_sid_key, sesStat_array);
					}
					int index = 0;
					// int x = 0;
					boolean flag = false;
					do {
						if (index >= sesStat_array.size()) {
							// System.out.println("counter : " + counter_name +
							// "repeated times: "+ x);
							continue label;
						}
						String row = sesStat_array.get(index);
						String columns[] = row.split("\t");
						flag = columns[1].equals(counter_value);
						if (flag) {
							states[i] = columns[2];
						}
						index++;
					} while (true);

				}
				if (counter.indexOf(FREE_TABLE_TABLE) >= 0) {
					if (freeTableSpace_array == null) {
						freeTableSpace_array = getResult(conn,
								freeTableSpaceQuery);
					}
					counter = counter.substring(counter
							.indexOf(FREE_TABLE_TABLE)
							+ FREE_TABLE_TABLE.length());
					String user = counter.substring(counter.indexOf(" ") + 1);
					int index = 0;
					do {
						if (index >= freeTableSpace_array.size()) {
							continue label;
						}
						String row = freeTableSpace_array.get(index);
						int hj = row.indexOf(user);
						if (row.indexOf(user) >= 0) {
							String columns[] = row.split("\t");
							states[i] = ""
									+ (100 - NumberUtils.toInt(columns[1]));
						}
						index++;

					} while (true);
				}
				for (int k = 0; k < tableNameList.size(); k++) {
					String s6 = (String) tableNameList.get(k);
					if (counter.indexOf(s6) < 0) {
						continue;
					}
					String s9 = (String) tableNameList.get(k);
					List<String> tableName_array;
					if ((tableName_array = (List<String>) hashmap.get(s9)) == null) {
						tableName_array = getResult(conn, s9);
						hashmap.put(s9, tableName_array);
					}
					String s13 = s6;
					counter = counter.substring(counter.indexOf(s13)
							+ s13.length());
					String s14 = counter.substring(counter.indexOf(" ") + 1);
					for (int index = 0; index < tableName_array.size(); index++) {
						String s16 = tableName_array.get(index);
						if (s16.toUpperCase().indexOf(s14.toUpperCase()) >= 0) {
							String as4[] = s16.split("\t");
							states[i] = as4[1];
						}
					}
				}
			}
		} catch (Exception e) {
			this.lastError=e.getMessage() ;
			e.printStackTrace();
		} finally {
			StringBuffer stateString;

			if (this.lastError.length() <= 0) {
			
				StringBuffer statestring = new StringBuffer();
				int counterSize = this.Counters.size();
				counter_it = counter_set.iterator();
				int j=0;
				for (int i = 0; i < counterSize; i++) {
					String counterName = (String) counter_it.next();
                    String value=states[i];
					if (states[i] != null)
					{
						String v=states[i];
						try {
							int V1 = Integer.parseInt(v);
							resp.put(counterName, V1);
						} catch (Exception ex) {
							resp.put(counterName, v);
						}
						resp.put(counterName, states[i]);
					}
					else {
						resp.put(counterName, "n/a");
						counter_errors++;
					}
					
					if (value==null ||value.equals(""))
						statestring.append(counterName).append(" = \"\"");
					else {
						statestring.append(counterName).append(" = ").append(value);
					}

					if (j != counterSize - 1)
						statestring.append(", ");
					++j;
				}
				resp.put("stateString",statestring.toString());
			} else {
				int counterSize = this.Counters.size();
				counter_it = counter_set.iterator();
				for (int i = 0; i < counterSize; i++) {
					String counterName = (String) counter_it.next();
					resp.put(counterName, "n/a");
				}
				counter_errors = counterSize;
				resp.put("stateString", "update fail");
			}
			resp.put("countersInError", counter_errors);
			// OtpErlangList counter_list =
			// this.getCounterResults(this.counters);
			// results.put(COUNTERS_VALUE, counter_list);
			CloseUtils.closeConnection(conn);
		}

		return resp;
	}
	Map<String, String> getStatNameMap(Connection conn) {
		HashMap<String, String> statsMap = new HashMap<String, String>();
		if(conn == null){
			return statsMap;
		}
		// StringBuffer buffer = new StringBuffer();
		List<String> array = getResult(conn, statCounterQuery);
		for (int i = 0; i < array.size(); i++) {
			String as[] = array.get(i).split("\t");
			statsMap.put(as[1], as[0]);
		}

		return statsMap;
	}

	private String buildXML(Map<String, Object> map, List counterSysResult,
			List instanceSysResult, List instanceSesResult,
			List tableSpaceQuery, List tableQueryArray[]) {
		String prefix, nameprefix;

		map.put("V$SYSSTAT ", "V$SYSSTAT");
		for (int j = 0; j < counterSysResult.size(); j++) {
			prefix = "V$SYSSTAT";
			String counter = (String) counterSysResult.get(j);
			if (ServicePlatform.containsValidXMLCharsOnly(counter))
				map.put(prefix + " " + counter, prefix + "/" + counter);
		}

		map.put("V$SESSTAT ", "V$SESSTAT");
		for (int i = 0; i < instanceSesResult.size(); i++) {
			prefix = "V$SESSTAT";
			nameprefix = "V$SESSTAT";
			String objectName = (String) instanceSesResult.get(i);
			objectName = objectName.replace('\t', '/');
			if (!objectName.endsWith("/"))
				objectName = (new StringBuilder()).append(objectName).append(
						"/").toString();
			if (!ServicePlatform.containsValidXMLCharsOnly(objectName))
				continue;

			nameprefix += " " + objectName;
			prefix += "/"
					+ objectName.substring(0, objectName.length() - 1)
							.replaceAll("/", "-");
			map.put(nameprefix, prefix);
			for (int j = 0; j < counterSysResult.size(); j++) {
				String counter = (String) counterSysResult.get(j);
				if (ServicePlatform.containsValidXMLCharsOnly(counter))
					map.put(nameprefix + " " + counter, prefix + "/" + counter);
			}
		}

		map.put("% Free Table Space ", "% Free Table Space");
		for (int j = 0; j < tableSpaceQuery.size(); j++) {
			prefix = "% Free Table Space";
			String counter = (String) tableSpaceQuery.get(j);
			counter = counter.toUpperCase();
			if (counter.startsWith("--") || counter.startsWith("TSNAME"))
				continue;
			String counterSplitTab = counter
					.substring(0, counter.indexOf("\t"));
			if (ServicePlatform.containsValidXMLCharsOnly(counterSplitTab))
				map.put(prefix + " " + counterSplitTab, prefix + "/"
						+ counterSplitTab);
		}

		if (tableQueryArray.length > 0) {
			for (int i = 0; i < tableQueryArray.length; i++) {
				List queryArray = tableQueryArray[i];
				if (queryArray == null)
					break;
				String tableNameEntry = (String) tableNameList.get(i);
				if (!ServicePlatform.containsValidXMLCharsOnly(tableNameEntry))
					continue;
				prefix = tableNameEntry;
				map.put(prefix + " ", prefix + " ");
				for (int j = 0; j < queryArray.size(); j++) {
					String counter = (String) queryArray.get(j);
					counter = counter.toUpperCase();
					if (counter.startsWith("--")
							|| counter.startsWith("TSNAME"))
						continue;
					if (counter.indexOf('\t') >= 0) {
						String counterSplitTab = counter.substring(0, counter
								.indexOf("\t"));
						if (ServicePlatform
								.containsValidXMLCharsOnly(counterSplitTab))
							map.put(prefix + " " + counterSplitTab + " ",
									prefix + "/" + counterSplitTab);
					}
				}
			}

		}
		return "";
	}

	List<String> getResult(Connection conn, String query_cmd) {
		PreparedStatement statement = null;
		ResultSet resultset = null;
		List<String> array = new ArrayList<String>();
		if (conn == null) {
			return array;
		}
		try {
			statement = conn.prepareStatement(query_cmd);
			if (this.Querytimeout > 0) {
				statement.setQueryTimeout(this.Querytimeout);
			}
			resultset = statement.executeQuery();
			if (resultset == null) {
				this.lastError = "no results from query" + "; ";
				throw new Exception("no results from query");
			}
			int cols = resultset.getMetaData().getColumnCount();
			while (resultset.next()) {
				try {
					String cur_row = "";
					for (int i = 1; i <= cols; i++) {
						cur_row = cur_row + resultset.getString(i);
						if (i < cols) {
							cur_row = cur_row + "\t";
						}
					}
					array.add(cur_row);
				} catch (Exception e) {
					this.lastError = e.getMessage() + "; <br>";
				}
			}
		} catch (SQLException e) {
			this.lastError = e.getMessage() + "; <br>";
			e.printStackTrace();
		} catch (Throwable e) {
			this.lastError = e.getMessage() + "; <br>";
			e.printStackTrace();
		} finally {
			try {
				if (resultset != null)
					resultset.close();
			} catch (SQLException e) {
			}
			if (statement != null)
				CloseUtils.closeStatement(statement);
		}

		return array;
	}

	@Override
	public int handleMessage() {
		// TODO Auto-generated method stub
		String action = this.getMessage().getAction();
		String error = null;

		Map<String, Object> resp = null;

		if (action != null && action.equals("update")) {
			resp = this.update();
			error = this.lastError.toString();
			ArrayList<String> R = new ArrayList<String>();
			if (lastError.length() > 0) {
				resp.put("error", ServicePlatform.replaceUnicodeChar(error));
				this.sendResponse("error", resp);
			} else {
				System.out.println("update result: " + resp);
				this.sendResponse2("ok", resp);
			}

		} else if (action != null && action.equals("getBrowseData")) {
			resp = this.getBrowseData();
			error = this.lastError.toString();
			if (error != null && error.length() > 0) {
				resp = new HashMap<String, Object>();
				resp.put("error", ServicePlatform.replaceUnicodeChar(error));
				this.sendResponse("error", resp);
			} else {
				System.out.println("getBrowseData data result: \n" + resp);
				this.sendResponse2("ok", resp);
			}
		}
		return 0;
	}

}
