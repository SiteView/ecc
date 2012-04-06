package com.dragonflow.erlangecc.monitor;

import java.io.File;
import java.io.FileInputStream;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.dbcp.BasicDataSource;
import org.apache.commons.lang.StringEscapeUtils;
import org.apache.commons.lang.SystemUtils;
import org.apache.commons.lang.math.NumberUtils;

import com.dragonflow.siteview.CloseUtils;
import com.dragonflow.siteview.ErlangUtils;
import com.dragonflow.siteview.Strings;
import com.dragonflow.siteview.infra.util.ServicePlatform;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;


public class OracleJDBCMonitor extends BaseMonitor {
	String connection_url = "";
	String user = "";
	String password = "";
	String driver = "oracle.jdbc.driver.OracleDriver";
	int connection_timeout = 60;
	int query_timeout = 60;
	int max_counter = 1;
	// String query_cmd = "";
	Map<String, Object> counters = new HashMap<String, Object>();

	StringBuffer lastError = new StringBuffer("");

	Map<String, String> statsNameMap;
	public Map<String, Object> contentValues = new HashMap<String, Object>();
	Map<String, String> queryValues = new HashMap<String, String>();
	Map<String, Object> browseData = new HashMap<String, Object>();

	BasicDataSource dbs = new BasicDataSource();
	NumberFormat numberFormat = NumberFormat.getNumberInstance();
	int counter_errors = 0;

	public static final String COUNTERS_IN_ERROR = "counters_in_error";
	public static final String EMPTY_ERLANG_LIST_STRING = (new OtpErlangList()).toString();
	public static final String COUNTERS_VALUE = "counters_value";
	public static final long ERROR_STATE = -999;
	public static final String ERROR = "error";
	public static final String STATE_STRING = "state_string";
	public static final String COUNTERS_NAME = "counters_name";
	public static final String INSTANCE_NAME = "instance_name";
	public static final String SID_VALUES = "sid_values";
	public static final String FREE_TABLE_SPACE = "free_table_space";

	static final String CRLF = "\r\n";
	static final String FS = SystemUtils.FILE_SEPARATOR;
	static String template_path = ServicePlatform.getRoot() +  "/templates.applications/oracle_jdbc_monitor.or";
	public static final String SYSTEM_TABLE = "V$SYSSTAT";
	public static final String SESSION_TABLE = "V$SESSTAT";
	public static final String FREE_TABLE_TABLE = "% Free Table Space";

	static List<String> tableNameList = new ArrayList<String>();
	static List<String> tableQueryList = new ArrayList<String>();
	static String sidValueQuery = OracleJDBCMonitor.getSettingValue("SIDVALUEQUERY=");
	static String sysResults = getSettingValue("SYSRESULTS=");
	static String sesResults = getSettingValue("SESRESULTS=");
	static String instanceNameQuery = getSettingValue("INSTANCENAMEQUERY=");
	static String counterNameQuery = getSettingValue("COUNTERNAMEQUERY=");
	static String statCounterQuery = getSettingValue("STATCOUNTERQUERY=");
	static String freeTableSpaceQuery = getSettingValue("FREETABLESPACEQUERY=");
	static StringBuffer pre_error = new StringBuffer("");

	static {
		int i = 0;
		do {
			String table_name = getSettingValue("TABLENAME" + i + "=");
			String table_query = getSettingValue("TABLEQUERY" + i + "=");
			if (table_name == null || table_name.trim().length() == 0 || table_query == null
					|| table_query.trim().length() == 0) {
				break;
			}
			tableNameList.add(table_name);
			tableQueryList.add(table_query);
			i++;
		} while (true);
		System.out.println(tableNameList);
		System.out.println(tableQueryList);
	}

	public OracleJDBCMonitor() {
		init();
	}

	private void init() {
		numberFormat.setMaximumFractionDigits(2);
		numberFormat.setMinimumFractionDigits(1);
		if (pre_error.length() > 0) {
			this.lastError.append(pre_error);
			System.out.println("Class Loader Error: " + pre_error.toString());
		}
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

	void initArgs(Map<String, Object> map) {
		String s = null;
		try {
			s = (String) map.get("connection_url");
			if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.connection_url = s.trim();
			}
			s = (String) map.get("user");
			if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.user = s.trim();
			}
			s = (String) map.get("password");
			if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.password = s.trim();
			}
			s = (String) map.get("driver");
			if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.driver = s.trim();
			}
			s = (String) map.get("connection_timeout");
			if (s != null && s.length() > 0) {
				this.connection_timeout = Integer.parseInt(s.trim())*1000;
			}
			s = (String) map.get("query_timeout");
			if (s != null && s.length() > 0) {
				this.query_timeout = Integer.parseInt(s.trim())*1000;
			}
			s = (String) map.get("max_counter");
			if (s != null && s.length() > 0) {
				this.max_counter = Integer.parseInt(s.trim());
			}
			Object o = map.get("counters");
			if (o != null && (o instanceof OtpErlangList)) {
				OtpErlangList counters = (OtpErlangList) o;
				if (counters.arity() > 0) {
					OtpErlangObject[] objs = counters.elements();
					for (OtpErlangObject e : objs) {
						if (e instanceof OtpErlangTuple) {
							OtpErlangTuple t = (OtpErlangTuple) e;
							String id = ((OtpErlangString) t.elementAt(0)).stringValue();
							String value = ((OtpErlangString) t.elementAt(1)).stringValue();
							this.counters.put(id, value);
						}
					}
				}
			} else {
				this.counters.put("MALFORMED COUNTER ID", null);
			}
		} catch (Exception e) {
			this.lastError.append(e.getMessage() + "; <br>");
			e.printStackTrace();
		}

	}

	public Map<String, Object> update() {
		this.initArgs(this.getMessage().getParams());
		init_config();
		HashMap<String, Object> hashmap = new HashMap<String, Object>();
		Map<String, Object> results = new HashMap<String, Object>();

		Set<String> counter_set = this.counters.keySet();
		int counter_size = this.counters.size();
		String[] states = new String[counter_size];

		Connection conn = null;
		Iterator<String> counter_it;
		try {
			conn = this.getConn();
			if(conn == null){
				throw new Exception("Access Datatbase Error. ");
			}
			HashMap<String, Object> queryMap=queryValues(conn);
			List<String> freeTableSpace_array = null;
			counter_it = counter_set.iterator();
			int ses_count = 0;
			label: for (int i = 0; i < counter_size && counter_it.hasNext(); i++) {
				String counter = counter_it.next();
				String counter_key = counter;
				String counter_value = "";
				String lablename=this.counters.get(counter).toString();
				if (lablename.indexOf("oracle Running")>=0)
				{
					if(queryMap.containsKey(counter))
					{
						states[i]=queryMap.get(counter).toString();
					}else
					{
						states[i]="n/a";
					}
					continue label;
				}				
				if (counter.indexOf(SYSTEM_TABLE) >= 0) {
					List<String> sysStat_array = null;
					if ((sysStat_array = (ArrayList<String>) hashmap.get(sysResults)) == null) {
						sysStat_array = this.getResult(conn, sysResults);
						hashmap.put(sysResults, sysStat_array);
					}
					counter = counter.substring(counter.indexOf(SYSTEM_TABLE) + SYSTEM_TABLE.length());
					String counter_name = counter.substring(counter.indexOf(" ") + 1, counter.length());
					int index = 0;
					do {
						if (index >= sysStat_array.size()) {
							continue label;
						}
						String ss = (String) sysStat_array.get(index);
						if (ss.startsWith(counter_name+"\t")) {
							String columns[] = ss.split("\t");
							states[i] = columns[1];
						}
						index++;
					} while (true);
				}
				if (counter.indexOf(SESSION_TABLE) >= 0) {
					counter = counter.substring(counter.indexOf(SESSION_TABLE) + SESSION_TABLE.length());
					counter = counter.substring(counter.indexOf(" ") + 1);
					String counter_name = counter.substring(counter.lastIndexOf("/") + 1).trim();
					String sid = counter.substring(0, counter.indexOf("/"));
					if (this.statsNameMap == null) {
						this.statsNameMap = getStatNameMap(conn);
					}
					counter_value = (String) this.statsNameMap.get(counter_name);
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
							// System.out.println("counter : " + counter_name + "repeated times: "+ x);
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
						freeTableSpace_array = getResult(conn, freeTableSpaceQuery);
					}
					counter = counter.substring(counter.indexOf(FREE_TABLE_TABLE) + FREE_TABLE_TABLE.length());
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
							states[i] = "" + (100 - NumberUtils.toInt(columns[1]));
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
					counter = counter.substring(counter.indexOf(s13) + s13.length());
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
			this.lastError.append(e.getMessage() + "; <br>");
			e.printStackTrace();
		}finally{
			StringBuffer stateString;
			
			if(this.lastError.length()<=0)
			{
				this.counter_errors = 0;
				int counterSize = this.counters.size();
				counter_it = counter_set.iterator();
				for(int i =0; i< counterSize; i++)
				{
					String counterName = (String)counter_it.next();
					if(states[i]!=null)
						this.counters.put(counterName, states[i]);
					else
					{
						this.counters.put(counterName, "n/a");
						this.counter_errors++;
					}
				}
			}
			else
			{
				int counterSize = this.counters.size();
				counter_it = counter_set.iterator();
				for(int i =0; i< counterSize; i++)
				{
					String counterName = (String)counter_it.next();
					this.counters.put(counterName, "n/a");
				}
				this.counter_errors = counterSize;
			}
			this.counters.put(COUNTERS_IN_ERROR, this.counter_errors);
//			OtpErlangList counter_list = this.getCounterResults(this.counters);
//			results.put(COUNTERS_VALUE, counter_list);
			CloseUtils.closeConnection(conn);
		}
	
		return this.counters;
	}

	private HashMap<String, Object> queryValues(Connection conn)
	{
		HashMap<String, Object> queryValues = new HashMap<String, Object>();
		String strsql="";
		PreparedStatement statement = null;
		ResultSet results = null;
		try
		{
		//1
		long response_time=0;//响应时间
		strsql="select user from dual";	
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		long begin = System.currentTimeMillis();
		results = statement.executeQuery();
		response_time= System.currentTimeMillis()-begin;
		queryValues.put("ResponseTime", response_time);
		//2 阀值90
		double Buffer_cache_hit_rate   =0.0;
		//strsql="SELECT round((P1.value + P2.value - P3.value) / (P1.value + P2.value),2) Buffer_cache_hit_rate  FROM   v$sysstat P1, v$sysstat P2, v$sysstat P3 WHERE  P1.name = 'db block gets' AND    P2.name = 'consistent gets'  AND P3.name = 'physical reads'";	
		strsql="SELECT round(1-((P1.value-P2.value-P3.value)/P4.value),2) Buffer_cache_hit_rate from v$sysstat P1,v$sysstat P2, v$sysstat P3, v$sysstat P4 where P1.name = 'physical reads' and P2.name = 'physical reads direct' and P3.name = 'physical reads direct (lob)' and P4.name = 'session logical reads'";
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			Buffer_cache_hit_rate = results.getDouble("Buffer_cache_hit_rate");
		}
		queryValues.put("BufferCacheHitRatio", Buffer_cache_hit_rate);
		
		//3
		double Buffer_Hit_Ratio_Index   =0.0;
		strsql="SELECT round(((lr.VALUE - ts.VALUE - sr.total_waits) / (lr.VALUE - ts.VALUE)),2) Buffer_Hit_Ratio_Index from v$sysstat lr, v$sysstat ts, v$system_event sr WHERE lr.NAME = 'session logical reads' AND ts.NAME = 'table scan blocks gotten' AND sr.event = 'db file sequential read'";
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			Buffer_Hit_Ratio_Index = results.getDouble("Buffer_Hit_Ratio_Index");
		}
		queryValues.put("BufferHitRatioIndex", Buffer_Hit_Ratio_Index);
		
		//4
		double Buffer_Hit_Ratio_Scan   =0.0;
		strsql="SELECT round(((ts.VALUE - (pr.VALUE - sr.total_waits)) / (ts.VALUE)),2) Buffer_Hit_Ratio_Scan from v$sysstat pr, v$sysstat ts, v$system_event sr WHERE pr.NAME = 'physical reads' AND ts.NAME = 'table scan blocks gotten' AND sr.event = 'db file sequential read'";
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			Buffer_Hit_Ratio_Scan = results.getDouble("Buffer_Hit_Ratio_Scan");
		}
		queryValues.put("BufferHitRatioScan", Buffer_Hit_Ratio_Scan);
		
		//5阀值98
		double Dictionary_Cache_Hit_Rate   =0.0;
		strsql="SELECT round(SUM(gets - getmisses) / SUM(gets),2) Dictionary_Cache_Hit_Rate from v$rowcache ";
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			Dictionary_Cache_Hit_Rate = results.getDouble("Dictionary_Cache_Hit_Rate");
		}
		queryValues.put("DictionaryCacheHitRate", Dictionary_Cache_Hit_Rate);
		
		//6阀值85
		double Library_Cache_Get_Hit_Ratio   =0.0;
		strsql="SELECT round(SUM(gethits)/ SUM(gets),2) Library_Cache_Get_Hit_Ratio FROM v$librarycache";
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			Library_Cache_Get_Hit_Ratio = results.getDouble("Library_Cache_Get_Hit_Ratio");
		}
		queryValues.put("LibraryCacheGetHitRatio", Library_Cache_Get_Hit_Ratio);
		
		//7阀值90
		double Library_Cache_Pin_Hit_Ratio   =0.0;
		strsql="SELECT round(SUM(pinhits)/ SUM(pins),2) Library_Cache_Pin_Hit_Ratio FROM v$librarycache";
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			Library_Cache_Get_Hit_Ratio = results.getDouble("Library_Cache_Pin_Hit_Ratio");
		}
		queryValues.put("LibraryCachePinHitRatio", Library_Cache_Get_Hit_Ratio);
		
		//8
		String Physical_reads ="n/a";
		strsql="select value Physical_reads from v$sysstat where name='physical reads'";	
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			Physical_reads = results.getString("Physical_reads");
		}
		//9
		String Physical_writes ="n/a";
		strsql="select value Physical_writes from v$sysstat where name='physical writes'";	
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			Physical_writes = results.getString("Physical_writes");
		}
		Thread.sleep(1000); 
		//8
		String Physical_reads0 ="n/a";
		strsql="select value Physical_reads0 from v$sysstat where name='physical reads'";	
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			Physical_reads0 = results.getString("Physical_reads0");
		}
		double Physical_reads_rate= Math.round(Math.abs(Double.parseDouble(Physical_reads0)-Double.parseDouble(Physical_reads))*10)/10.0;
		queryValues.put("PhysicalReadsPersec", Physical_reads_rate);
		//9
		String Physical_writes0 ="n/a";
		strsql="select value Physical_writes0 from v$sysstat where name='physical writes'";	
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			Physical_writes0= results.getString("Physical_writes0");
		}
		double Physical_writes_rate= Math.round(Math.abs(Double.parseDouble(Physical_writes0)-Double.parseDouble(Physical_writes))*10)/10.0;
		queryValues.put("PhysicalWritesPersec", Physical_writes_rate);
		
		}catch(Exception ex)
		{
			ex.printStackTrace();
		}
		return queryValues;
	}
	private String getStateString(Map<String, Object> counter_values) {
		StringBuffer buf = new StringBuffer("");
		if (counter_values == null || counter_values.size() <= 0) {
			return "";
		}
		Set<String> set = counter_values.keySet();
		Iterator<String> it = set.iterator();
		while (it.hasNext()) {
			String key = it.next();
			String value = (String) counter_values.get(key) + Strings.HTTP_LF;
			buf.append(key + " = " + value);

		}
		return buf.toString();
	}
/*
	private OtpErlangList getCounterResults(Map<String, Object> counter_values) {
		OtpErlangList erList = null;
		if (counter_values == null || counter_values.size() <= 0) {
			return new OtpErlangList();
		}
		try {
			List<OtpErlangTuple> list = new ArrayList<OtpErlangTuple>();
			Set<String> set = counter_values.keySet();
			Iterator<String> it = set.iterator();
			while (it.hasNext()) {
				String counter_key = it.next();
				String counter_value = (String) counter_values.get(counter_key);
				double d = Double.parseDouble(counter_value);
				long value = Math.round(d);
				OtpErlangTuple tuple = ErlangUtils.createTuple(new OtpErlangAtom(counter_key), new OtpErlangLong(value));
				list.add(tuple);
			}
			OtpErlangTuple[] tuples = new OtpErlangTuple[list.size()];
			list.toArray(tuples);
			erList = new OtpErlangList(tuples);
		} catch (Exception e) {
			this.lastError.append(e.getMessage() + "; <br>");
			e.printStackTrace();
		}

		return erList;
	}
*/
	private void init_config() {
		try {
			String jdbcUrl = DatabaseConnectionURL.getConnectionURL(this.driver, this.connection_url);
System.out.println("------get database connection url: " + jdbcUrl);
			dbs.setUrl(jdbcUrl);
			dbs.setDriverClassName(this.driver);
			dbs.setUsername(this.user);
			dbs.setPassword(this.password);
			dbs.setMaxWait(this.connection_timeout);
			dbs.setMaxActive(3);
		} catch (Exception e) {
			this.lastError.append(e.getMessage() + "; <br>");
			e.printStackTrace();
		}

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
		}finally{
			CloseUtils.closeInputStream(file_input);			
		}
		return buf;
	}

	List<String> getResult(Connection conn, String query_cmd) {
		PreparedStatement statement = null;
		ResultSet resultset = null;
		List<String> array = new ArrayList<String>();
		if(conn == null){
			return array;
		}		
		try {
			statement = conn.prepareStatement(query_cmd);
			if (this.query_timeout > 0) {
				statement.setQueryTimeout(this.query_timeout);
			}
			resultset = statement.executeQuery();
			if (resultset == null) {
				this.lastError.append("no results from query" + "; ");
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
					this.lastError.append(e.getMessage() + "; <br>");
				}
			}
		} catch (SQLException e) {
			this.lastError.append(e.getMessage() + "; <br>");
			e.printStackTrace();
		} catch (Throwable e) {
			this.lastError.append(e.getMessage() + "; <br>");
			e.printStackTrace();
		} finally {
			try
			{
				if(resultset!=null)
					resultset.close();
			}
			catch(SQLException e)
			{
			}
			if(statement!=null)
				CloseUtils.closeStatement(statement);
		}

		return array;
	}

    private String buildXML(Map<String, Object> map, List counterSysResult, List instanceSysResult, List instanceSesResult, List tableSpaceQuery, List tableQueryArray[])
    {
    	String prefix, nameprefix;
    	
    	map.put("V$SYSSTAT ", "V$SYSSTAT");    	
        for(int j = 0; j < counterSysResult.size(); j++)
        {
        	prefix = "V$SYSSTAT";
            String counter = (String)counterSysResult.get(j);
            if(ServicePlatform.containsValidXMLCharsOnly(counter))
                map.put(prefix+ " "+ counter, prefix + "/" + counter);
        }

        map.put("V$SESSTAT ", "V$SESSTAT");
        for(int i = 0; i < instanceSesResult.size(); i++)
        {
            prefix = "V$SESSTAT";
            nameprefix = "V$SESSTAT";
            String objectName = (String)instanceSesResult.get(i);
            objectName = objectName.replace('\t', '/');
            if(!objectName.endsWith("/"))
                objectName = (new StringBuilder()).append(objectName).append("/").toString();
            if(!ServicePlatform.containsValidXMLCharsOnly(objectName))
                continue;
            
            nameprefix += " " + objectName;
            prefix += "/" +  objectName.substring(0, objectName.length()-1).replaceAll("/", "-");
            map.put(nameprefix, prefix);
            for(int j = 0; j < counterSysResult.size(); j++)
            {
                String counter = (String)counterSysResult.get(j);
                if(ServicePlatform.containsValidXMLCharsOnly(counter))
                    map.put(nameprefix + " " + counter, prefix + "/" + counter);
            }
        }

        map.put("% Free Table Space ", "% Free Table Space");
        for(int j = 0; j < tableSpaceQuery.size(); j++)
        {
            prefix = "% Free Table Space";
            String counter = (String)tableSpaceQuery.get(j);
            counter = counter.toUpperCase();
            if(counter.startsWith("--") || counter.startsWith("TSNAME"))
                continue;
            String counterSplitTab = counter.substring(0, counter.indexOf("\t"));
            if(ServicePlatform.containsValidXMLCharsOnly(counterSplitTab))
                map.put(prefix + " " + counterSplitTab, prefix+"/"+counterSplitTab);
        }

        if(tableQueryArray.length > 0)
        {
            for(int i = 0; i < tableQueryArray.length; i++)
            {
                List queryArray = tableQueryArray[i];
                if(queryArray == null)
                    break;
                String tableNameEntry = (String)tableNameList.get(i);
                if(!ServicePlatform.containsValidXMLCharsOnly(tableNameEntry))
                    continue;
                prefix = tableNameEntry;
                map.put(prefix+" ", prefix+" ");
                for(int j = 0; j < queryArray.size(); j++)
                {
                    String counter = (String)queryArray.get(j);
                    counter = counter.toUpperCase();
                    if(counter.startsWith("--") || counter.startsWith("TSNAME"))
                        continue;
                    if(counter.indexOf('\t') >= 0)
                    {
                        String counterSplitTab = counter.substring(0, counter.indexOf("\t"));
                        if(ServicePlatform.containsValidXMLCharsOnly(counterSplitTab))
                            map.put(prefix + " " + counterSplitTab+" ", prefix+ "/" + counterSplitTab);
                    } 
                }
            }

        }
        if (map.size()>0)
        {
        	map.put("oracleaction", "oracle Running");
        	map.put("ResponseTime", "oracle Running/Response Time");
        	map.put("IOReadsWrites", "oracle Running/IO operating frequency");
        	map.put("PhysicalReadsPersec", "oracle Running/IO operating frequency/Physical Reads Persec");
        	map.put("PhysicalWritesPersec", "oracle Running/IO operating frequency/Physical Writes Persec");
        	map.put("ActionHitRatio", "oracle Running/Action Hit Ratio");
        	map.put("BufferCacheHitRatio", "oracle Running/Action Hit Ratio/buffer cache hit ratio");
        	map.put("BufferHitRatioIndex", "oracle Running/Action Hit Ratio/buffer hit ratio (index)");
        	map.put("BufferHitRatioScan", "oracle Running/Action Hit Ratio/buffer hit ratio (scan)");
        	map.put("DictionaryCacheHitRate", "oracle Running/Action Hit Ratio/dictionary cache hit rate");
        	map.put("LibraryCacheGetHitRatio", "oracle Running/Action Hit Ratio/library cache get hit ratio");
        	map.put("LibraryCachePinHitRatio", "oracle Running/Action Hit Ratio/library cache pin hit ratio");
        }
        return "";
    }
    
	Map<String, Object> getBrowseData() {
		this.initArgs(this.getMessage().getParams());
		this.init_config();
		
		Map<String, Object> resp = new HashMap<String, Object>();
		Connection conn = this.getConn();
		if(conn == null){
			return resp;
		}

		try {
			List<String> counterNameList = this.getResult(conn, counterNameQuery);
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
		    for(int i = 0; i < tableQueryList.size(); i++)
		    {
		    	String tableQueryEntry = (String)(tableQueryList.get(i));
		    	tableQueryArray[j++] = getResult(conn, tableQueryEntry);
		    }
		    
		    buildXML(resp, counterNameList, instanceNameList, sidValueList, freeTableSpaceList, tableQueryArray);
		    
		} catch (Exception e) {
			this.lastError.append(e.getMessage() + "; <br>");
			e.printStackTrace();
		}
		if(conn!=null)
			CloseUtils.closeConnection(conn);
			
		return resp;
	}

	private Connection getConn() {
		Connection conn = null;
		try {
			conn = this.dbs.getConnection();
		} catch (SQLException e) {
			this.lastError.append(e.getMessage() + "; <br>");
			e.printStackTrace();
		}
		return conn;
	}
	
	@Override
	public void run() {
		this.handleMessage();
	}

	@Override
	public int handleMessage() {
		// String action = "update";
		String action = this.getMessage().getAction();
		String error = null;

		Map<String, Object> resp = null;

		if (action != null && action.equals("update")) {
			resp = this.update();
			error = this.lastError.toString();
			if (error != null && error.length() > 0) {
				resp = new HashMap<String, Object>();
				resp.put(ERROR, ServicePlatform.replaceUnicodeChar(error));
				this.sendResponse(ERROR, resp);
			} else {
System.out.println("update data result: \n" + resp);
				this.sendResponse2("ok", resp);
			}
		} else if (action != null && action.equals("getBrowseData")) {
			resp = this.getBrowseData();
			error = this.lastError.toString();
			if (error != null && error.length() > 0) {
				resp = new HashMap<String, Object>();
				resp.put(ERROR, ServicePlatform.replaceUnicodeChar(error));
				this.sendResponse(ERROR, resp);
			} else {
System.out.println("update data result: \n" + resp);
				this.sendResponse2("ok", resp);
			}
		}		
		return 0;
	}

	public Map<String, Object> removeSomeMap(Map<String, Object> map)
	{
		Map<String, Object> tmp = new HashMap<String, Object>();
		Iterator<Map.Entry<String, Object>> iter = map.entrySet().iterator();
		int i=0;
		while(iter.hasNext())
		{
		    Map.Entry<String, Object> entry = (Map.Entry<String, Object>)iter.next();     
			String key = entry.getKey();
			String value = (String)entry.getValue();
			tmp.put(key, value);
			if(i>20)
				break;
			i++;
		}
		
		return tmp;
	}

	public static void main(String[] args) {
		OracleJDBCMonitor dr = new OracleJDBCMonitor();
		dr.connection_url = "192.168.0.61:1521/oracle10g";
		dr.user = "system";
		dr.password = "systest";
/*		dr.counters.put("V$SESSTAT 8/SYS/ORACLE9I/jrew.exe/ table scans (long tables)", "V$SESSTAT/8/SYS/ORACLE9I/jrew.exe//table scans (long tables)");
		
		dr.update();*/
		System.out.println(dr.counters);
		Map<String, Object> map = dr.getBrowseData();
       Set keys = map.keySet();
        Iterator it = keys.iterator();
        do
        {
            if(!it.hasNext())
                break;
            String key = (String)it.next();
            String description = (String)map.get(key);
            if(key.indexOf("V$SESSTAT")>=0)
            	System.out.println(key+" = "+description);
        }while(true);
		System.out.println(keys.size());
	}
}
