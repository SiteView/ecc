package com.dragonflow.erlangecc.monitor;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

import jgl.Array;

import org.apache.commons.dbcp.BasicDataSource;

import com.dragonflow.siteview.CloseUtils;
import com.dragonflow.siteview.exception.SQLqueryException;
import com.dragonflow.siteview.infra.util.ServicePlatform;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public class jdbcLogger extends BaseMonitor {
    java.sql.Connection connection;
    int reconnectSeconds;
    long reconnectTime;
    int maxStringLength;
    int autoCommitCounter;
    int autoCommit;
    java.sql.PreparedStatement logStatementCache;
    jgl.Array logCache;
    java.lang.String insertStatement;
    java.lang.String server;
    int vars;
    java.lang.String driver;
    java.lang.String method = "";
    boolean useNulls;
    java.lang.String propertyFilter[];
    java.lang.String propertySkip[];
    boolean usingBackupSignalFile;
    boolean usingBackupDatabase;
    boolean debug;
    java.sql.PreparedStatement linkStatementCache;
    jgl.Array linkCache;
    java.lang.String linkServer;
    int linkVars;
    private Map<String,Object> map;
    StringBuffer lastError = new StringBuffer("");
    StringBuffer stateString = new StringBuffer("");
    BasicDataSource dbs = new BasicDataSource();
    int connectionTimeout = 5000;
    
    
	@Override
	public int handleMessage() {
		String action = this.getMessage().getAction();
		System.out.println("action: " + action);
		Map<String, Object> result = null;
		result = new HashMap<String, Object>();
		map = this.getMessage().getParams();
		System.out.println("map: " + map);
		logCache = new Array();
		java.lang.String url = (String)map.get("logJdbcURL");
		if (action != null && action.equals("setupConnection")) {
			try
			{
				this.setupConnection(url);
			}
			catch(java.lang.Exception exception)
			{
				System.out.println("exception: " + exception.getMessage());
				result.put("error","connect error");
				this.sendResponse("error", result);
			}
			if (lastError.length() > 0) {
				result.put("error",this.lastError.toString());
				this.sendResponse("error", result);
			}else
			{
				result.put("ok",this.stateString.toString());
				this.sendResponse("ok", result);
			}
		}else{
			if (action != null && action.equals("log")){
				System.out.println("do log");
				jgl.Array logContent = creatArgs();
				try
				{
					log(logContent);
				}
				catch (java.lang.Exception exception)
				{
					lastError.append("exception: " + exception.getMessage());
				}
				if(lastError.length()>0)
				{
					result.put("error",lastError.toString());
					this.sendResponse("error", result);
				}else
				{
					result.put("ok",stateString.toString());
					this.sendResponse("ok", result);
				}
					
				
			}
		}
		return 0;
	}
	
	public jgl.Array creatArgs()
	{
		jgl.Array array;
        array = new Array();
//		datex VARCHAR(255), serverName VARCHAR(255), class VARCHAR(255), sample VARCHAR(255), category VARCHAR(255), groupName VARCHAR(255), monitorName VARCHAR(255), status VARCHAR(255), monitorID VARCHAR(255), value1 VARCHAR(255), value2 VARCHAR(255), value3 VARCHAR(255), value4 VARCHAR(255), value5 VARCHAR(255), value6 VARCHAR(255), value7 VARCHAR(255), value8 VARCHAR(255), value9 VARCHAR(255), value10 VARCHAR(255))
		String date = getValue("date",map);
		array.add(date);
		String serverName = getValue("serverName",map);
		array.add(serverName);
		String cla = getValue("class",map);
		array.add(cla);
		String sample = getValue("sample",map);
		array.add(sample);
		String category = getValue("category",map);
		array.add(category);
		String groupName = getValue("groupName",map);
		array.add(groupName);
		String monitorName = getValue("monitorName",map);
		array.add(monitorName);
		String status = getValue("status",map);
		array.add(status);
		String monitorID = getValue("monitorID",map);
		array.add(monitorID);
		String value1 = getValue("value1",map);
		array.add(value1);
		String value2 = getValue("value2",map);
		array.add(value2);
		String value3 = getValue("value3",map);
		array.add(value3);
		String value4 = getValue("value4",map);
		array.add(value4);
		String value5 = getValue("value5",map);
		array.add(value5);
		String value6 = getValue("value6",map);
		array.add(value6);
		String value7 = getValue("value7",map);
		array.add(value7);
		String value8 = getValue("value8",map);
		array.add(value8);
		String value9 = getValue("value9",map);
		array.add(value9);
		String value10 = getValue("value10",map);
		array.add(value10);
		return array;
	}
	
	public String getValue(String s,Map<String, Object> map)
	{
		String value;
		if(map.get(s)==null)
		{
			value = "";
		}else
		{
			value = (String)map.get(s);
		}
		return value;
	}
	
	public Integer toint(String s)
	{
		Integer value = 0;
	try
	{
		value = Integer.parseInt(s);
	}
	catch(java.lang.Exception exception)
	{
		value = 0;
	}
	return value;
	}
	
	
	synchronized void setupConnection(java.lang.String s)
    throws java.lang.Exception
    {
	    if(connection == null)
	    {
	        java.lang.String s1 = getValue("logJdbcURLBackup",map);
	        driver = getValue("logJdbcDriver",map);
	        if(driver.length()<=0)
	        {
	        	lastError.append("driver is empty");
	        }
	        java.lang.String s2 = getValue("logJdbcUser",map);
	        java.lang.String s3 = getValue("logJdbcPassword",map);
	        java.lang.String s4 = getValue("logJdbcPropertyFilter",map);
	        if(s4.length() > 0)
	        {
	            propertyFilter = s4.split(",");
	        }
	        java.lang.String s5 = getValue("logJdbcPropertySkip",map);
	        if(s5.length() > 0)
	        {
	            propertySkip = s5.split(",");
	        }
	        int i = toint(getValue("logJdbcMaxString",map));
	        if(i > 0)
	        {
	            maxStringLength = i;
	        }
	        autoCommit = toint(getValue("logJdbcAutoCommit",map));
	        if(autoCommit == 0)
	        {
	            autoCommit = 20;
	        }
	        reconnectSeconds = toint(getValue("logJdbcReconnect",map));
	        if(reconnectSeconds == 0)
	        {
	            reconnectSeconds = 600;
	        }
	        System.out.println("jdbc log, reconnect after " + reconnectSeconds + " seconds");
	        System.out.println("jdbc log, commit after " + autoCommit + " records");
	        if(method.length() > 0)
	        {
	        	System.out.println("jdbc log, method, " + method);
	        }
	        java.lang.Class.forName(driver).newInstance();
	        try
	        {
	        	System.out.println("get in connect");
				String jdbcUrl = DatabaseConnectionURL.getConnectionURL(this.driver, s);
				System.out.print("raw url is:" + jdbcUrl);
				dbs.setUrl(jdbcUrl);
				dbs.setDriverClassName(this.driver);
				dbs.setUsername(s2);
				dbs.setPassword(s3);
				dbs.setMaxWait(this.connectionTimeout);
				dbs.setMaxActive(2);
				connection = dbs.getConnection();
				stateString.append("connect to:" + s);
				System.out.println("connect to:" + s);
	        }
	        catch(java.lang.Exception exception)
	        {
	        	System.out.print("connect to backup");
	            if(s1.length() != 0)
	            {
	            	String jdbcUrl = DatabaseConnectionURL.getConnectionURL(this.driver, s1);
    				dbs.setUrl(jdbcUrl);
    				dbs.setDriverClassName(this.driver);
    				dbs.setUsername(s2);
    				dbs.setPassword(s3);
    				dbs.setMaxWait(this.connectionTimeout);
    				dbs.setMaxActive(2);
    				connection = dbs.getConnection();
    				stateString.append("connect to backup:" + s1);
    				System.out.println("connect to:" + s1);
	            } else
	            {
	            	stateString.append("connect error ");
	            	lastError.append("connect error " + exception);
	            	System.out.println("lastError: " + lastError);
	                throw exception;
	            }
	        }
	        System.out.println("jdbc log, logged in");
	    }else{
	    	System.out.println("connect exist");
	    	stateString.append("connect already exist");
	    }
    }
	
	java.lang.String createTable(java.sql.Connection connection1, java.lang.String s)
    {
        java.lang.String s1 = "";
        java.sql.Statement statement = null;
        try
        {
            statement = connection1.createStatement();
            if(s.startsWith("call"))
            {
                s = "{" + s + "}";
            }
//            set monitor state into jdbc database
            statement.executeUpdate(s);
            stateString.append(" table created");
            System.out.println("jdbc log, log table created");
        }
        catch(java.lang.Exception exception)
        {
        	stateString.append(" jdbc log, log table not created, "+exception);
        	System.out.println("jdbc log, log table not created, " + exception);
            s1 = exception.getMessage();
        }
        finally
        {
            try
            {
                if(statement != null)
                {
                    statement.close();
                    connection1.commit();
                }
            }
            catch(java.lang.Exception exception2) { }
        }
        return s1;
    }
	
	int countVars(java.lang.String s)
    {
        int i = 0;
        for(int j = 0; j < s.length(); j++)
        {
            if(s.charAt(j) == '?')
            {
                i++;
            }
        }

        return i;
    }
	
	public static java.lang.String padWithZeros(int i, int j)
    {
        java.lang.String s = "" + i;
        int k = s.length();
        for(int l = 0; l < j - k; l++)
        {
            s = "0" + s;
        }

        return s;
    }
	
	java.sql.PreparedStatement getStatement()
    throws java.lang.Exception
    {
	    if(logStatementCache == null)
	    {
	        if(reconnectTime != 0L && java.lang.System.currentTimeMillis() < reconnectTime)
	        {
	            return null;
	        }
	        java.lang.String s = getValue("logJdbcURL",map);
	        if(s.length() == 0)
	        {
	            return null;
	        }
	        setupConnection(s);
	        System.out.println("after setup connection\n");
	        java.lang.String s1 = getValue("logJdbcInsert",map);
	        java.lang.String s2 = getValue("logJdbcCreate",map);
	        method = getValue("logJdbcMethod",map);
	        java.lang.String s3 = createTable(connection, s2);
	        System.out.println("after create table\n");
//	        first create a table
	        if(s3.indexOf("invalid column name") != -1)
	        {
	        	System.out.println("jdbc log, fixup date column, " + s3);
	            s2 = s2.replace("date VAR", "datex VAR");
	            map.put("logJdbcCreate", s2);
	            s3 = createTable(connection, s2);
	        }
//	        second insert the data to this table
	        vars = countVars(s1);
	        System.out.println("jdbc log, prepare insert, " + vars + ", " + s1);
	        if(s1.startsWith("call"))
	        {
	            logStatementCache = connection.prepareCall("{" + s1 + "}");
	        } else
	        {
	            logStatementCache = connection.prepareStatement(s1);
	        }
	        System.out.println("jdbc log, connected");
	    }
	    return logStatementCache;
	}
	
	void closeConnection()
    {
        System.out.println("jdbc log, closing database connection");
        if(logStatementCache != null)
        {
            try
            {
                logStatementCache.close();
                logStatementCache = null;
            }
            catch(java.lang.Exception exception)
            {
            	System.out.println("jdbc logger error: closing statement, " + exception);
            }
        }
        if(linkStatementCache != null)
        {
            try
            {
                linkStatementCache.close();
                linkStatementCache = null;
            }
            catch(java.lang.Exception exception1)
            {
            	System.out.println("jdbc logger error: closing link statement, " + exception1);
            }
        }

        if(connection != null)
        {
            try
            {
                connection.close();
                connection = null;
            }
            catch(java.lang.Exception exception2)
            {
            	System.out.println("jdbc logger error: closing, " + exception2);
            }
        }
        System.out.println("jdbc log, closed");
    }
	
	public void clearParameters(java.sql.PreparedStatement preparedstatement)
    {
		System.out.println("get clear parameters");
        try
        {
            preparedstatement.clearParameters();
            if(driver.indexOf("sun.jdbc.odbc") != -1)
            {
                ((sun.jdbc.odbc.JdbcOdbcPreparedStatement)preparedstatement).FreeParams();
            }
        }
        catch(java.sql.SQLException sqlexception)
        {
            System.out.println("could not free parameters" + sqlexception.toString());
        }
    }
	
	void flush(java.sql.PreparedStatement preparedstatement, jgl.Array array)
    throws java.sql.SQLException
    {
	    int i = array.size();
	    java.sql.SQLException sqlexception = null;
	    while(array.size() > 0) 
	    {
	        jgl.Array array1 = (jgl.Array)array.popFront();
	        System.out.println("array1:"+array1);
	        int j = array1.size();
	        try
	        {
	            for(int k = 0; k < j; k++)
	            {
	                preparedstatement.setObject(k + 1, array1.at(k), 12);
	            }
	
	            preparedstatement.executeUpdate();
	            continue;
	        }
	        catch(java.sql.SQLException sqlexception1)
	        {
	            sqlexception = sqlexception1;
	            java.lang.String s = "";
	            for(int l = 0; l < j; l++)
	            {
	                if(l != 0)
	                {
	                    s = s + ",";
	                }
	                s = s + array1.at(l);
	            }
	
	            lastError.append("jdbc logger error: exec, " + sqlexception1 + ", record=" + s);
	            clearParameters(preparedstatement);
	        }
	    }
	    connection.commit();
        stateString.append(" jdbc logger commit complete, " + i + " records");
	    if(sqlexception != null)
	    {	
	    	lastError.append("jdbc logger error:"+sqlexception);
	        throw sqlexception;
	    } else
	    {
	        return;
	    }
	    }
		
		void execute(java.sql.PreparedStatement preparedstatement, jgl.Array array, jgl.Array array1)
	    throws java.sql.SQLException
	    {
			System.out.println("get in execute");
		    array.add(array1);
	        flush(preparedstatement, array);
    }
	
	public synchronized void log(jgl.Array array)
    {
        try {
        java.sql.PreparedStatement preparedstatement;
        preparedstatement = getStatement();
        System.out.println("after getStatement");
        if(preparedstatement == null)
        {
            return;
        }
            execute(preparedstatement, logCache, array);
        }
        catch(java.lang.Exception exception)
        {
            System.out.println("jdbc logger error: " + exception);
            reconnectTime = (long)(reconnectSeconds * 1000) + java.lang.System.currentTimeMillis();
            closeConnection();
        }
    }
	
	public static void main(String[] args) {
		System.out.println("runing jdbc logger");		
	}

}