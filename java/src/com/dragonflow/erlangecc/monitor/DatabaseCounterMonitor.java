package com.dragonflow.erlangecc.monitor;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.commons.dbcp.BasicDataSource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.dragonflow.siteview.CloseUtils;
import com.dragonflow.siteview.ErlangUtils;
import com.dragonflow.siteview.infra.util.ServicePlatform;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class DatabaseCounterMonitor extends BaseMonitor {

	int max_counter = 1;
	String driver = "";
	String connection_url = "";
	String user = "";
	String password = "";
	String query_cmd = "";
	String divisor_query_cmd = "";
	int connection_timeout = 1;
	int query_timeout = 1;
	boolean no_cummulative;
	boolean no_divisor;
	int counters_size;
	Connection conn = null;	
	PreparedStatement statement = null;
	
	StringBuffer lastError = new StringBuffer("");
	public Map<String, Object> last_measurements = null;
	public Map<String, Object> contentValues = new HashMap<String, Object>();
	public Map<String, Object> counters = new HashMap<String, Object>();
	Map<String, String> queryValues = new HashMap<String, String>();
	BasicDataSource dbs = new BasicDataSource();
	NumberFormat numberFormat = NumberFormat.getNumberInstance();

	public static final String SITEVIEW_DIVISOR_QUERY_RESULT = "SITEVIEW_DIVISOR_QUERY_RESULT".toLowerCase();
	public static final String SITEVIEW_RUN_TIME_MILLIS_KEY = "SITEVIEW_RUN_TIME_MILLIS_KEY".toLowerCase();
	public static final String PROPERTY_NAME_COUNTERS_IN_ERROR = "counters_in_error";
	public static final String COUNTER_VALUE = "counter_value";
	public static final String ERROR_STATE = "-999.0";
	public static final String ERROR = "error";
	public static final String GOOD = "ok";
	public static final String COUNTER_VALUES = "counter_values";
	public static final String CURRENT_MEASUREMENTS = "current_measurements";
	public static final String STATE_STRING = "state_string";
	public static final String BROWSE_DATA = "browse_data";

	public static final String EMPTY_ERLANG_LIST_STRING = (new OtpErlangList()).toString();

	public DatabaseCounterMonitor() {
		numberFormat.setMaximumFractionDigits(2);
		numberFormat.setMinimumFractionDigits(1);
	}

	private void initArgs(Map<String, Object> map) {
		String s = null;
		try {
			s = (String) map.get("max_counter");
			if (s != null && s.length() > 0) {
				this.max_counter = Integer.parseInt(s.trim());
			}
			Object obj = map.get("last_measurements");
			if (obj != null && (obj instanceof OtpErlangList)) {
				saveLastMesurements((OtpErlangList)obj);
			}
			// OtpErlangList counters = (OtpErlangList) map.get("counters");
			// if (counters != null && counters.arity() > 0) {
			// OtpErlangObject[] objs = counters.elements();
			// for (OtpErlangObject e : objs) {
			// if (e instanceof OtpErlangTuple) {
			// OtpErlangTuple t = (OtpErlangTuple) e;
			// String id = t.elementAt(1).toString();
			// this.counters.put(id, t.elementAt(0));
			// }
			// }
			// } else {
			// this.counters.put("MALFORMED COUNTER ID", null);
			// }
			Object o = map.get("counters");
			if (o != null && (o instanceof OtpErlangList)) {
				OtpErlangList counters = (OtpErlangList) o;
				if (counters.arity() > 0) {
					OtpErlangObject[] objs = counters.elements();
					for (OtpErlangObject e : objs) {
						if (e instanceof OtpErlangTuple) {
							OtpErlangTuple t = (OtpErlangTuple) e;
							String id = ((OtpErlangString) t.elementAt(1)).stringValue();
							String value = ((OtpErlangString) t.elementAt(0)).stringValue();
							this.counters.put(value, id);
						}
					}
				}
			} else {
				this.counters.put("MALFORMED COUNTER ID", null);
			}
			s = (String) map.get("driver");
			if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.driver = s.trim();
			}
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
			s = (String) map.get("query_cmd");
			if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.query_cmd = s.trim();
			}
			s = (String) map.get("divisor_query_cmd");
			if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.divisor_query_cmd = s.trim();
			}
			s = (String) map.get("connection_timeout");
			if (s != null && s.length() > 0) {
				this.connection_timeout = Integer.parseInt(s.trim());
			}
			s = (String) map.get("query_timeout");
			if (s != null && s.length() > 0) {
				this.query_timeout = Integer.parseInt(s.trim());
			}
			s = (String) map.get("no_cummulative");
			if (s != null && s.length() > 0) {
				this.no_cummulative = Boolean.parseBoolean(s);
			}
			s = (String) map.get("no_divisor");
			if (s != null && s.length() > 0) {
				this.no_divisor = Boolean.parseBoolean(s);
			}
			s = (String) map.get("counters_size");
			if (s != null && s.length() > 0) {
				this.counters_size = Integer.parseInt(s);
			}
		} catch (Exception e) {
			this.lastError.append(e.getMessage());
			e.printStackTrace();
		}
	}

	private void saveLastMesurements(OtpErlangList list) {
		if(last_measurements==null)
		{
			last_measurements = new HashMap<String, Object>();
		}
		else
		{
			last_measurements.clear();
		}
		try {
			OtpErlangObject[] objs = list.elements();
			for (OtpErlangObject each : objs) {
				if (each instanceof OtpErlangTuple) {
					OtpErlangTuple tuple = (OtpErlangTuple) each;
					String tuple_key = tuple.elementAt(0).toString();
					tuple_key = tuple_key.replaceAll("'", "");
					String obj = tuple.elementAt(1).toString();
					obj = obj.replaceAll("\"","");
					last_measurements.put(tuple_key, obj);
				}
			}
		} catch (Exception e) {
			this.lastError.append(e.getMessage());
			e.printStackTrace();
		}
	}

	private void init_config() {
		try {
			String jdbcUrl = DatabaseConnectionURL.getConnectionURL(this.driver, this.connection_url);
System.out.println("------ get database connection url from rawUrl: " + jdbcUrl);
			dbs.setUrl(jdbcUrl);
			dbs.setDriverClassName(this.driver);
			dbs.setUsername(this.user);
			dbs.setPassword(this.password);
			dbs.setMaxWait(this.connection_timeout);
			dbs.setMaxActive(3);
		} catch (Exception e) {
			this.lastError.append(e.getMessage());
			e.printStackTrace();
		}
	}

	private Connection getConn() {
		if(conn!=null)
			return conn;
		
		try {
			conn = this.dbs.getConnection();
		} catch (SQLException e) {
			this.lastError.append(e.getMessage());
			e.printStackTrace();
		}
		return conn;
	}

	private Double getDivisor()
    throws SQLException, ClassNotFoundException, InstantiationException, IllegalAccessException
    {
		ResultSet resultset = null;
		Double double1;
		try
		{
			resultset = runQuery(this.divisor_query_cmd);
			resultset.next();
			double1 = new Double(resultset.getDouble(1));
		}
		finally
		{
			if(resultset != null)
			{
				try
				{
				 resultset.close();
				}
				catch(SQLException sqlexception)
				{
				}
			}
         }
		return double1;
    }
	
	private ResultSet runQuery(String query)
    throws InstantiationException, IllegalAccessException, ClassNotFoundException, SQLException
    {
		Connection conn = this.getConn();
		if(conn==null)
			return null;
		
		ResultSet resultset = null;
		try {			
			statement = conn.prepareStatement(query);
			if(this.query_timeout > 0)
				statement.setQueryTimeout(this.query_timeout);
			resultset = statement.executeQuery();	
		}
		catch (SQLException e) {
			this.lastError.append(e.getMessage());
			return null;
		}		
		return resultset;
    }
	
	public TreeMap getResult(StringBuffer stringbuffer, String query)
	{
		boolean flag = false;
		ResultSet resultset = null;
		TreeMap treemap = new TreeMap();
	     
		try 
		{
			resultset = runQuery(query);
			if(this.divisor_query_cmd.length() > 0)
			{
				treemap.put("SITEVIEW_DIVISOR_QUERY_RESULT", getDivisor());
			}
			treemap.put("SITEVIEW_RUN_TIME_MILLIS_KEY", System.currentTimeMillis() + "");
			if(resultset == null)
			{
				this.lastError.append("no results from query");
				throw new Exception("no results from query");
			}
			
			ResultSetMetaData resultsetmetadata = resultset.getMetaData();
			int i = resultsetmetadata.getColumnCount();
			String[] as = new String[i - 1];
			for(int j = 0; j < as.length; j++)
			{
				as[j] = resultsetmetadata.getColumnLabel(j + 2);
			}

			while(resultset.next())
			{
				try 
				{
					String s1 = resultset.getString(1);
					HashMap hashmap = new HashMap();
					for(int k = 0; k < as.length; k++)
					{
						String s2 = as[k];
						hashmap.put(s2, resultset.getString(k + 2));
					}

					treemap.put(s1, hashmap);
				}
				catch (Exception exception) {
					this.lastError.append(exception.getMessage());
				}
			}
	     }
	     catch (SQLException e) 
	     {
	    	 flag = true;
	    	 this.lastError.append("Error from database driver: " + e.getMessage());
	     }
	     catch (ClassNotFoundException e) {
	    	 flag = true;
	    	 this.lastError.append("Driver not found: " + e.getMessage());
	     }
	     catch (Exception ex) 
	     {
	    	 flag = true;
	    	 this.lastError.append(ex.getMessage());
	     }
	     finally 
	     {
	    	 if(resultset != null)
	    	 {
	    		 try
	    		 {
	    			 resultset.close();
	    		 }
	    		 catch(Exception exception2)
	    		 {
	    		 }
	    	}
			CloseUtils.closeStatement(statement);
			CloseUtils.closeConnection(conn);
			conn = null;
			statement = null;
	     }
	     return treemap;
	}
	
	public Map<String, Object> getResult(String query)
	{
		boolean flag = false;
		ResultSet resultset = null;
		Map<String, Object> treemap = new HashMap<String,Object>();
	     
		try 
		{
			resultset = runQuery(query);
			if(this.divisor_query_cmd.length() > 0)
			{
				treemap.put("SITEVIEW_DIVISOR_QUERY_RESULT", getDivisor());
			}
			treemap.put("SITEVIEW_RUN_TIME_MILLIS_KEY", System.currentTimeMillis() + "");
			if(resultset == null)
			{
				this.lastError.append("no results from query");
				throw new Exception("no results from query");
			}
			
			ResultSetMetaData resultsetmetadata = resultset.getMetaData();
			int i = resultsetmetadata.getColumnCount();
			String[] as = new String[i - 1];
			for(int j = 0; j < as.length; j++)
			{
				as[j] = resultsetmetadata.getColumnLabel(j + 2);
			}

			while(resultset.next())
			{
				try 
				{
					String s1 = resultset.getString(1);
					treemap.put(s1, s1);
					for(int k = 0; k < as.length; k++)
					{
						String s2 = as[k];
						treemap.put(s1+ "|||" + s2, resultset.getString(k + 2));
					}
				}
				catch (Exception exception) {
					this.lastError.append(exception.getMessage());
				}
			}
	     }
	     catch (SQLException e) 
	     {
	    	 flag = true;
	    	 this.lastError.append("Error from database driver: " + e.getMessage());
	     }
	     catch (ClassNotFoundException e) {
	    	 flag = true;
	    	 this.lastError.append("Driver not found: " + e.getMessage());
	     }
	     catch (Throwable e) 
	     {
	    	 flag = true;
	    	 this.lastError.append(e.getMessage());
	     }
	     finally 
	     {
	    	 if(resultset != null)
	    	 {
	    		 try
	    		 {
	    			 resultset.close();
	    		 }
	    		 catch(Exception exception2)
	    		 {
	    		 }
	    	}
			CloseUtils.closeStatement(statement);
			CloseUtils.closeConnection(conn);
			conn = null;
			statement = null;
	     }
	     return treemap;
	}
		
	private Set getObjectKeys(TreeMap treemap)
	{
	    Set set = treemap.keySet();
	    set.remove("SITEVIEW_RUN_TIME_MILLIS_KEY");
	    set.remove("SITEVIEW_DIVISOR_QUERY_RESULT");
	    return set;
	}
	
	private Map<String,Object> buildXml(TreeMap treemap)
	{
		Map<String, Object>map = new HashMap<String,Object>();
		Set set = getObjectKeys(treemap);
		
	    for(Iterator iterator = set.iterator(); iterator.hasNext();)
	    {
	        String groupPrefix = (String)iterator.next();
	        map.put(groupPrefix, groupPrefix);
	        HashMap hashmap = (HashMap)treemap.get(groupPrefix);
	        Set set1 = hashmap.keySet();
	        Iterator iterator1 = set1.iterator();
	        while(iterator1.hasNext()) 
	        {
	             String s1 = (String)iterator1.next();
	             map.put(groupPrefix + "|||" + s1, groupPrefix + "/" + s1);
	        }
	    }	    
	    return map;
	 }
	
	public Map<String, Object> getBrowseData()
	{
		StringBuffer stringbuffer = new StringBuffer();
		
		this.initArgs(this.getMessage().getParams());
		this.init_config();
		
	    try 
	    {
	    	TreeMap treemap = getResult(stringbuffer, query_cmd);
	    	return buildXml(treemap);
	    }
	    catch (RuntimeException exception) {
	    	this.lastError.append(exception.getMessage());
	    	return null;
	    }
	}

	private long getResultTimeStamp(Map<String, Object> treemap)
	 {
	     if(treemap != null)
	     {
	         String s = (String)treemap.get("SITEVIEW_RUN_TIME_MILLIS_KEY");
	         if(s != null)
	         {
	             return Long.parseLong(s);
	         }
	     }
	     return 0L;
	 }

	private String getCounterValue(Map<String, Object> treemap, String s)
	{
	     if(treemap != null)
	     {
	         return (String)treemap.get(s);
	     }
	     return null;
	 }

	private Map<String, Object> setCurrentMeasures(Map<String, Object> curMap){
		Map<String, Object> saveMap = new HashMap<String, Object>();
		if(curMap == null || curMap.size() <= 0){
			return saveMap;
		}
		Set<String> set = curMap.keySet();
		Iterator<String> it = set.iterator();
		while(it.hasNext()){
			String key = it.next();
			Object value = curMap.get(key);
			saveMap.put(key, value);
		}		
		return saveMap;		
	}	
	
	private OtpErlangList getCurrentMeasures(Map<String, Object> map) {
		OtpErlangList list = new OtpErlangList();
		if (map == null || map.size() <= 0) {
			return list;
		}		
		try {
			Set<String> set = map.keySet();
			Iterator<String> it = set.iterator();
			List<OtpErlangTuple> array = new ArrayList<OtpErlangTuple>();			
			for (int i = 0; it.hasNext(); i++) {
				String atom = it.next();
				Object value = map.get(atom);
				if (atom == null || atom.length() <= 0 || (value instanceof Map)) {
					continue;
				}
				OtpErlangTuple tuple = ErlangUtils.createTuple(new OtpErlangAtom(atom), new OtpErlangString(value.toString()));
				array.add(tuple);
			}
			OtpErlangTuple[] tuples = new OtpErlangTuple[array.size()];
			array.toArray(tuples);
			list = new OtpErlangList(tuples);
		} catch (Exception e) {
			this.lastError.append(e.getMessage() + "; <br>");
			e.printStackTrace();
		}
		return list;
	}
	
	private OtpErlangList getErlangListFromMap(Map<String, String> map) {
		OtpErlangList list = new OtpErlangList();
		if (map == null || map.size() <= 0) {
			return list;
		}
		try {
			Set<String> set = map.keySet();
			Iterator<String> it = set.iterator();
			OtpErlangTuple[] tuples = new OtpErlangTuple[map.size()];
			for(int i=0; it.hasNext(); i++){
				String atom = it.next();
				String value = map.get(atom);
				tuples[i] = ErlangUtils.createTuple(new OtpErlangAtom(atom), new OtpErlangString(value));
			}
			list = new  OtpErlangList(tuples);
		} catch (Exception e) {
			this.lastError.append(e.getMessage() + "; <br>");
			e.printStackTrace();
		}		
		return list;
	}
	
	Map<String, Object> update() 
	{
		StringBuffer stringbuffer;
		this.initArgs(this.getMessage().getParams());
		this.init_config();
		Map<String, Object> map = new HashMap<String, Object>();

		Set<String> set = this.counters.keySet();
		Map<String, Object> currentMap = null;
		Map<String, Object> previousMap = this.last_measurements;

		try {
			if (!this.no_cummulative || previousMap == null) 
			{
				previousMap = this.getResult(this.query_cmd);
				Thread.sleep(2000);
			}
					
			currentMap = this.getResult(this.query_cmd);
			double divisor = 0.0D;
			if (this.divisor_query_cmd == null || this.divisor_query_cmd.length() == 0) {
				divisor = (double) (getResultTimeStamp(currentMap) - getResultTimeStamp(previousMap)) / 1000F;
			} 
			else {
				String one = currentMap.get("SITEVIEW_DIVISOR_QUERY_RESULT") + "";
				String two = previousMap.get("SITEVIEW_DIVISOR_QUERY_RESULT") + "";
				double double1 = Double.parseDouble(one);
				double double2 = previousMap != null ? Double.parseDouble(two) : 0.0D;
				if (previousMap != null && double1 != 0 && double2 != 0) {
					divisor = double1 - double2;
				} 
				else {
					divisor = Double.MAX_VALUE;
				}
			}
					
			String[] states = new String[this.counters_size + 1];
			Iterator<String> it_id = set.iterator();
			for (int i = 0; (i < states.length) && it_id.hasNext(); i++) 
			{
				String state = ERROR_STATE;
				String counter_id_name = it_id.next();

				String counterValue_1 = getCounterValue(currentMap, counter_id_name);
				String counterValue_2 = getCounterValue(previousMap, counter_id_name);
						
				if (!this.no_divisor && !this.no_cummulative && counterValue_1 != null) 
				{
					state = "" + counterValue_1;
				} 
				else if (counterValue_1 != null && counterValue_2 != null) 
				{
					try {
						double d1 = Double.parseDouble(counterValue_1);
						double d2 = Double.parseDouble(counterValue_2);
						double d = d1;
						if (this.no_cummulative) {
							d = d1 - d2;
						}
						
						if (this.no_divisor) {
							if (divisor != 0.0D) {
								d = d1 / divisor;
							} 
							else {
								d = 0.0D;
							}
						}
						state = this.numberFormat.format(d);
					} 
					catch (NumberFormatException e) {
						state = ERROR_STATE;
						this.lastError.append("Unable to parse values ");
						e.printStackTrace();
					}

				} 
				else if (counterValue_1 == null) 
				{
					state = ERROR_STATE;
					this.lastError.append("<br>Unable to retrieve counter: " + counter_id_name + "; " );
				}
				states[i] = state;
			}

			last_measurements = this.setCurrentMeasures(currentMap);

			String state_string = "";
			Iterator<String> it_name = set.iterator();
			if (this.lastError.length() <= 0) {
				int errors = 0;
				List<String> list = new ArrayList<String>();
				Map<String, String> counterValues = new HashMap<String, String>();
				for (int i = 0; (i < states.length) && it_name.hasNext(); i++) {
					String counterName = it_name.next();
					if (counterName.equals("MALFORMED COUNTER ID")) {
						continue;
					}
					if (states[i] != null && !states[i].equals(ERROR_STATE)) {
						counterValues.put(counterName, states[i]);
					} else {
						counterValues.put(counterName, ERROR_STATE);
						errors++;
					}
					String counterId = (String)this.counters.get(counterName);
					list.add(counterId + " = " + states[i]);
				}
				
				OtpErlangList erlist = this.getErlangListFromMap(counterValues);
				map.put(COUNTER_VALUES, erlist);
				it_name = null;
				map.put(PROPERTY_NAME_COUNTERS_IN_ERROR, (errors));

				StringBuffer buffer = new StringBuffer();
				Iterator<String> it = list.iterator();
				while (it.hasNext()) {
					buffer.append(it.next());
					if (it.hasNext()) {
						buffer.append("<br>");
					}
				}
				state_string = buffer.toString();
				last_measurements.putAll(contentValues);
			} else {
				it_name = set.iterator();
				Map<String, String> counterValues = new HashMap<String, String>();
				for (int i = 0; i < states.length && it_name.hasNext(); i++) {
					String counterName = it_name.next();
					counterValues.put(counterName, ERROR_STATE);
				}
				OtpErlangList list = this.getErlangListFromMap(counterValues);
				map.put(COUNTER_VALUES, list);
				map.put(PROPERTY_NAME_COUNTERS_IN_ERROR, this.counters_size);
				state_string = this.lastError.toString();
				last_measurements.putAll(contentValues);
			}
			OtpErlangList curList = this.getCurrentMeasures(last_measurements);

			map.put(CURRENT_MEASUREMENTS, curList);
			map.put(STATE_STRING, state_string);
		}catch(Exception e){
			lastError.append(e.getMessage() + "");
			e.printStackTrace();
		}finally{
		}
		return map;
	}
	
	@Override
	public int handleMessage() {
		String action = this.getMessage().getAction();
		Map<String, Object> result = null;
		
		if (action != null && action.equals("update")) {
			result = this.update();
			if (lastError.toString().length() > 0) {
				if(this.lastError.toString().length()>250)
					result.put(ERROR, ServicePlatform.replaceUnicodeChar(this.lastError.toString().substring(0,250)));
				else
					result.put(ERROR, ServicePlatform.replaceUnicodeChar(lastError.toString()));
			}
			this.sendResponse(GOOD, result);
		} else if (action != null && action.equals("getBrowseData")) {
			result = this.getBrowseData();
			if(lastError.toString().length() > 0)
			{
				if(this.lastError.toString().length()>250)
					this.sendResponse(ServicePlatform.replaceUnicodeChar(this.lastError.toString().substring(0,250)), counters);
				else
					this.sendResponse(ServicePlatform.replaceUnicodeChar(this.lastError.toString()), counters);
			}			
			else
			{
				this.sendResponse2("ok", result);			
			}
		} else {
			result = new HashMap<String, Object>();
			result.put(ERROR, "no such action. \n");
			this.sendResponse(ERROR, result);
		}

		return 0;
	}
	
	public static void main(String[] args) {
		DatabaseCounterMonitor mo = new DatabaseCounterMonitor();
		mo.driver = "oracle.jdbc.driver.OracleDriver";
		mo.connection_url = "192.168.0.40:1521/CY";
		mo.query_cmd = "select * from cyn";
		mo.divisor_query_cmd = "";
		mo.user = "system";
		mo.password = "manager";
		mo.query_timeout  = 60000;
		
		System.out.println(mo.getBrowseData());		
	}

}
