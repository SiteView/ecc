package com.dragonflow.erlangecc.monitor;

import java.io.File;
import java.io.Serializable;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.apache.commons.dbcp.BasicDataSource;


import com.dragonflow.siteview.CloseUtils;
import com.dragonflow.siteview.ErlangUtils;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ibm.db2.jcc.DB2DataSource;
import com.ibm.db2.jcc.DB2Driver;
import com.dragonflow.siteview.infra.db.util.*;
import com.dragonflow.siteview.infra.util.ServicePlatform;
//import com.dragonflow.siteview.infra.db.util.DB2MonitorConfig;

public class DB2Monitor extends BaseMonitor {
	String server = "192.168.0.20";
	String user = "db2admin";
	String password = "db2admin";
	String instance = "cy";
	String partition = "-1";
	int port = 50000;
	private int conn_timeout = 60*1000;
	private int query_timeout = 60*1000;
	

	StringBuffer lastError = new StringBuffer("");
	Map<String, Object> counters = new HashMap<String, Object>();
	Map<String, Object> counters_value = new HashMap<String, Object>();

	Connection conn = null;	
	PreparedStatement statement = null;
	BasicDataSource dbs = new BasicDataSource();
	DB2DataSource db2s = new DB2DataSource();
	NumberFormat numberFormat = NumberFormat.getNumberInstance();
	int counter_errors = 0;

	public static final String EMPTY_ERLANG_LIST_STRING = (new OtpErlangList()).toString();
	public static final String DB2_Driver = com.ibm.db2.jcc.DB2Driver.class.getName();
	
	public static final String ERROR = "error";
	public static final String COUNTER_ERROR_STATE = "n/a";
	public static final String COUNTERS_IN_ERROR = "counters_in_error";
	public static final String COUNTERS_VALUE = "counters_value";
	public static final String STATE_STRING = "state_string";
//	public static final String SNAPSHOT_DATABASE_COUNTERS = "snapshot_database_counters";
//	public static final String SNAPSHOT_DBM_COUNTERS = "snapshot_dbm_counters";
	public static final String  DB2COUNTERS = "db2counters";
	
	
	private void init_config() {
		try {
			// dbs.setValidationQuery(TEST_SQL);
			
			String conn_url = this.server + ":" + this.port + "/" + this.instance; 
			String jdbcUrl = DatabaseConnectionURL.getConnectionURL(DB2_Driver, conn_url);
System.out.println("------get database connection url: " + jdbcUrl);

			dbs.setUrl(jdbcUrl);
			dbs.setDriverClassName(DB2_Driver);
			dbs.setUsername(this.user);
			dbs.setPassword(this.password);
			dbs.setMaxWait(this.conn_timeout * 1000); 	// millisecond.
			dbs.setMaxActive(3);
			

		} catch (Exception e) {
			this.lastError.append(e.getMessage() + "; <br>");
			e.printStackTrace();
		}

	}

	void initArgs(Map<String, Object> map) {
		String s = null;

		try {
			s = (String) map.get("server");
			if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.server = s.trim();
			}
			s = (String) map.get("user");
			if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.user = s.trim();
			}
			s = (String) map.get("password");
			if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.password = s.trim();
			}
			s = (String) map.get("partition");
			if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.partition = s.trim();
			}
			s = (String) map.get("port");
			if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.port = Integer.parseInt(s.trim());
			}			
			s = (String) map.get("instance");
			if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.instance = s.trim();
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
			lastError.append(e.getMessage() + "; <br>");
			e.printStackTrace();
		}

	}
	
	private Set getAllQueries()
    	throws Exception
    {
        String  path =  ServicePlatform.getRoot();
        
        System.out.println(path);
		Set queries = DB2MonitorConfig.getQueries(this.instance, this.partition, path+"/templates.applications/DB2_Queries.xml");
        System.out.println(queries);
		return queries;
    }
	
	public Map getCounterValues(Set queries, boolean breakOnError)
	{
	    TreeMap allResults;
	    allResults = new TreeMap();
	    conn = this.getConn();
		if(conn==null)
		{
			return null;
		}
	try{
		for(Iterator iterator = queries.iterator(); iterator.hasNext();)
	    {
			ResultSet resultset = null;
			try {			
	            DB2Query db2Query = (DB2Query)iterator.next();
	            TreeMap resultsMap = new TreeMap();
	            allResults.put(db2Query.name, resultsMap);
	            String query = db2Query.sql;
	            String tableIndex = db2Query.tableIndex;
	            try
	            {
					statement = conn.prepareStatement(query);
					if(this.query_timeout > 0)
						statement.setQueryTimeout(this.query_timeout);
					resultset = statement.executeQuery();	
	            }
	            catch(SQLException e)
	            {
	            	if(breakOnError)
	            		throw e;
	            }
	            catch(Exception ex)
	            {
	            	if(breakOnError)
	            		throw ex;	            	
	            }
	            resultsMap.put("SITEVIEW_RUN_TIME_MILLIS_KEY", (new StringBuilder()).append(System.currentTimeMillis()).append("").toString());
	            if(resultset == null)
	                resultsMap.put("no counters available check snapshot switches", "no counters available");
	            else
	                DatabaseUtils.populateMap(resultset, resultsMap, this.lastError, tableIndex);
	        }
			catch(SQLException e)
			{
				if(resultset != null)
				{
					try
	            	{
						resultset.close();
	            	}
	            	// Misplaced declaration of an exception variable
	            	catch(SQLException ex)
	            	{
	            		this.lastError.append(ex.getMessage());
	        			CloseUtils.closeStatement(statement);
	            		continue;
	            	}
				}
        		this.lastError.append(e.getMessage());
    			CloseUtils.closeStatement(statement);
        		continue;
	        }
            catch(Exception ex)
            {
        		this.lastError.append(ex.getMessage());
        		break;
        	}
	        if(resultset!= null)
	        try
	        {
	        	resultset.close();
	        }
	        // Misplaced declaration of an exception variable
	        catch(SQLException e)
	        {
				CloseUtils.closeStatement(statement);
	        }
	    }
	}
	catch(Exception ex)
	{
		System.out.println("error in query db2:"+ex);
	}
	finally{
		CloseUtils.closeStatement(statement);
		CloseUtils.closeConnection(conn);
	}
        return allResults;
	}
	
    private List getCounterIDs()
    {
        List counterIDs = new ArrayList();
        Set key = this.counters.keySet();
        for(Iterator it = key.iterator(); it.hasNext(); )
        {
            String rawCounterID = (String)it.next();
            String counterID = DatabaseUtils.getCounterIDFromRawID(rawCounterID);
            counterIDs.add(counterID);
        }

        return counterIDs;
    }
	
    private Set getQueriesToRun()
    {
        Set allQueries = null;
        Set queriresToRun = new HashSet();
        try
        {
            allQueries = getAllQueries();
        }
        catch(Exception e)
        {
            return queriresToRun;
        }

        Collection counterIDs = getCounterIDs();
        Iterator iterator = counterIDs.iterator();
        do
        {
            if(!iterator.hasNext())
                break;
            String counterID = (String)iterator.next();
            String queryName = DatabaseUtils.getTreeKey(counterID);
            DB2Query db2Query = DB2Query.findQuery(allQueries, queryName);
            if(db2Query != null)
                queriresToRun.add(db2Query);
        } while(true);
        return queriresToRun;
    }	
    
    protected Map<String, Object> update()
    {
		this.initArgs(this.getMessage().getParams());
		this.init_config();

		int nCountersInError = 0;
    	int counterNumber = 1;
    	Set queriresToRun = getQueriesToRun();
    	Map thisRunCounterMap = null;
    	Map lastRunCounterMap = null;
    	Map<String, Object> result = new HashMap<String, Object>();
    	String status  = "";
		try{
		thisRunCounterMap = getCounterValues(queriresToRun, true);
    	TreeSet counterStatuses = new TreeSet();
    	List<OtpErlangTuple>  tuple_list = new ArrayList<OtpErlangTuple>();
    	Collection counterIDS = getCounterIDs();
    	for(Iterator iterator = counterIDS.iterator(); iterator.hasNext();)
    	{
    		String counterID = (String)iterator.next();
    		String counterValue = DatabaseUtils.getCounterValue(thisRunCounterMap, counterID);
    		if("n/a".equals(counterValue))
    			nCountersInError++;
    		status += this.counters.get(counterID) + "=" + counterValue + "<br>";
    		OtpErlangTuple tuple = ErlangUtils.createTuple(new OtpErlangAtom(counterID), new OtpErlangString(counterValue));
    		tuple_list.add(tuple);
    	}

		OtpErlangTuple[] tuples = new OtpErlangTuple[tuple_list.size()];
		tuple_list.toArray(tuples);
		OtpErlangList erList = new OtpErlangList(tuples);
		result.put(COUNTERS_IN_ERROR, nCountersInError);
		result.put(COUNTERS_VALUE, erList);
		result.put(STATE_STRING, status);
		}
		catch(Exception e)
		{
			System.out.println("error in update");
		}
		finally{
			CloseUtils.closeConnection(conn);
		}
		return result;
    }
	
	public HashMap<String, Object> getBrowseData()
	{
		this.initArgs(this.getMessage().getParams());
		this.init_config();
		Map result = null;
        try
        {
        	result = getCounterValues(getAllQueries(), false);
        }
        catch(Exception e)
        {
        	this.lastError.append(e.getMessage());
        	return null;
        }
        try
        {
        	HashMap<String, Object> xml = DatabaseUtils.buildXml(result, this.lastError);
	        return xml;	
        }
        catch(Exception exception)
        {
        	this.lastError.append(exception.getMessage());
        	return null;
        }
    }

	@Override
	public int handleMessage() {
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
				this.sendResponse("ok", resp);
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
	
	private Connection getConn() {
		Connection conn = null;
		try {
			conn = this.dbs.getConnection();
		} catch (SQLException e) {
			this.lastError.append(e.getMessage() + "; <br>");
			e.printStackTrace();
		}
		try {
			this.dbs.close();
		} catch (SQLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return conn;
	}

	public static void main(String[] args) {
		System.out.println("get in");
	}

}
