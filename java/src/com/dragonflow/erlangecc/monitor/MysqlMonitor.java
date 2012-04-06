package com.dragonflow.erlangecc.monitor;

import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.apache.commons.dbcp.BasicDataSource;

import com.dragonflow.siteview.informix.DbInformix;
import com.dragonflow.siteview.infra.util.ServicePlatform;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;


public class MysqlMonitor extends BaseMonitor {
	String database;//
	String Host;
	String Port = "3306";
	String UserName;
	String Password;
	String DriverName = "com.mysql.jdbc.Driver";
	int connection_timeout = 60;
	int query_timeout = 60;
	OtpErlangList EMPTY_ERLANG_LIST_STRING = new OtpErlangList();
	String lastError = "";
	Map<String, Object> Counters = new HashMap<String, Object>();
	BasicDataSource dbs = new BasicDataSource();
	public void initArgs(Map<String, Object> map) {
		String s = null;

		try {
			s = (String) map.get("database");
			if (s != null && s.length() > 0) {
				this.database = s.trim();
			}
			s = (String) map.get("host");
			if (s != null && s.length() > 0) {
				this.Host = s.trim();
			}
			s = (String) map.get("port");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.Port = s.trim();
			}
			s = (String) map.get("username");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.UserName = s.trim();
			}
			s = (String) map.get("password");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.Password = s.trim();
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
			}

		} catch (Exception e) {
			lastError = e.getMessage();
			e.printStackTrace();
		}

	}
	private Connection getconn() {
		Connection conn=null;
		try {
			String jdbcUrl ="jdbc:mysql://"+this.Host+":"+this.Port+"/"+this.database; 
			dbs.setUrl(jdbcUrl);
			dbs.setDriverClassName(this.DriverName);
			dbs.setUsername(this.UserName);
			dbs.setPassword(this.Password);
			dbs.setMaxWait(this.connection_timeout);
			dbs.setMaxActive(3);
			conn=dbs.getConnection();
		} catch (Exception e) {
			
			e.printStackTrace();
		}
		return conn;

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
		strsql="show variables like 'max_connections'";	
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			queryValues.put(results.getString(1), results.getObject(2));
		}
		//2 
		strsql="show global status like 'Max_used_connections'";	
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			queryValues.put(results.getString(1), results.getObject(2));
		}
		
		//3
		strsql="show global status like 'Threads_connected'";	
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			queryValues.put(results.getString(1), results.getObject(2));
		}
		
		//4
		strsql="show global status like 'Open_tables'";	
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			queryValues.put(results.getString(1), results.getObject(2));
		}
		
		//5
		strsql="show global status like '%slave%'";	
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			queryValues.put(results.getString(1), results.getObject(2));
		}
		
		//6
		strsql="show global status like 'Threads%'";	
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			queryValues.put(results.getString(1), results.getObject(2));
		}
		
		//7
		strsql="show global status like  'Qcache%'";	
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			queryValues.put(results.getString(1), results.getObject(2));
		}
		
		//8
		strsql="show global status like  'Sort%'";	
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			queryValues.put(results.getString(1), results.getObject(2));
		}
		//9
		strsql="show global status like  'Table_locks%'";	
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			queryValues.put(results.getString(1), results.getObject(2));
		}
		//10
		int Key_read_requests=0;
		int Key_reads=0;
		strsql="show global status like  'key_read%'";	
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		int i=0;
		while (results.next()) {
			if(i==0) Key_read_requests=results.getInt(2);
			if(i==1) Key_reads=results.getInt(2);
			i++;
		}
		if (Key_read_requests==0)
		{
			queryValues.put("key_buffer_hit_rate", 1);
		}else
		{
			 BigDecimal bd=new BigDecimal((1-Key_reads/Key_read_requests));
		     bd=bd.setScale(2, BigDecimal.ROUND_HALF_UP);
		     queryValues.put("key_buffer_hit_rate", bd.doubleValue());
		}
		//11
		int Innodb_buffer_pool_read_requests=0;
		int Innodb_buffer_pool_reads=0;
		strsql="show global status like  'Innodb_buffer_pool_read_requests'";	
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			 Innodb_buffer_pool_read_requests=results.getInt(2);
		}
		strsql="show global status like  'Innodb_buffer_pool_reads'";	
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		while (results.next()) {
			Innodb_buffer_pool_reads=results.getInt(2);
		}
		if (Innodb_buffer_pool_read_requests==0)
		{
			queryValues.put("innodb_buffer_hit_rate", 1);
		}else
		{
			 BigDecimal bd=new BigDecimal((1-Innodb_buffer_pool_reads/Innodb_buffer_pool_read_requests));
		     bd=bd.setScale(2, BigDecimal.ROUND_HALF_UP);
		     queryValues.put("innodb_buffer_hit_rate", bd.doubleValue());
		}
		//12
		int Qcache_hits=0;
		int Qcache_inserts=0;
		strsql="show global status like  'Qcache_hits'";	
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			Qcache_hits=results.getInt(2);
		}
		strsql="show global status like  'Qcache_inserts'";	
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			Qcache_inserts=results.getInt(2);
		}
		if (Qcache_hits==0&&Qcache_inserts==0)
		{
			queryValues.put("query_cache_hit_rate", 1);
		}else
		{
			 BigDecimal bd=new BigDecimal((Qcache_hits / (Qcache_hits + Qcache_inserts )));
		     bd=bd.setScale(2, BigDecimal.ROUND_HALF_UP);
		     queryValues.put("query_cache_hit_rate", bd.doubleValue());
		}
		//13
		int Threads_created=0;
		int Connections=0;
		strsql="show global status like  'Threads_created'";	
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			Threads_created=results.getInt(2);
		}
		strsql="show global status like  'Connections'";	
		statement = conn.prepareStatement(strsql);
		statement.setQueryTimeout(this.query_timeout);
		results = statement.executeQuery();
		while (results.next()) {
			Connections=results.getInt(2);
		}
		if (Connections==0)
		{
			queryValues.put("thread_cache_hit_rate", 1);
		}else
		{
			 BigDecimal bd=new BigDecimal(1 - Threads_created / Connections);
		     bd=bd.setScale(2, BigDecimal.ROUND_HALF_UP);
		     queryValues.put("thread_cache_hit_rate", bd.doubleValue());
		}
		}catch(Exception ex)
		{
			ex.printStackTrace();
		}
		return queryValues;
	}
	public Map<String, Object> update() {

		Map<String, Object> result = new HashMap<String, Object>();
		this.initArgs(this.getMessage().getParams());
		Connection conn=this.getconn();
		
		HashMap<String, Object> queryMap = this.queryValues(conn);
		
		int len = this.Counters.size();
		if (queryMap.containsKey("error")) {
			result.put("stateString", "update fail");
			result.put("countersInError", len);
		} else {
			result.put("pStatus","ok");
			int errorCount = 0;
			StringBuffer statestring = new StringBuffer();
			int j=0;
			for (String s : this.Counters.keySet()) {
				String s1 = this.Counters.get(s).toString();// .replace("-",
				// "/");
                String value="";
				if (queryMap.containsKey(s)) {
					String v = queryMap.get(s).toString();
					value=v;
					result.put(s1, v);
					result.put(s, v);
					
				} else {
					++errorCount;
					value="n/a";
					result.put(s, "n/a");
					result.put(s1, "n/a");
				}
				if (value.equals(""))
					statestring.append(s1).append(" = \"\"");
				else {
					statestring.append(s1).append(" = ").append(value);
				}

				if (j != len - 1)
					statestring.append(", ");
				++j;
			}
			result.put("stateString", statestring.toString());
			result.put("countersInError", errorCount);

		}
		return result;
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
				R.add(error);
				this.sendResponse3("error", R);
			} else {
				System.out.println("update result: " + resp);
				this.sendResponse2("ok", resp);
			}

		}
		return 0;
	}

	

	public static void main(String[] args) {
		try {
//			BasicDataSource dbs = new BasicDataSource();
//			dbs.setUrl("jdbc:mysql://192.168.6.197:3306/test");
//			dbs.setDriverClassName("com.mysql.jdbc.Driver");
//			dbs.setUsername("root");
//			dbs.setPassword("system");
//			dbs.setMaxWait(6000);
//			dbs.setMaxActive(3);
//			Connection conn = null;
//			conn =  dbs.getConnection();
//			String strsql = "show global status like 'handler_read%'";
//			PreparedStatement statement = null;
//			ResultSet results = null;
//			statement = conn.prepareStatement(strsql);
//			statement.setQueryTimeout(60000);
//			results = statement.executeQuery();
//			while (results.next()) {
//				System.out.println(results.getObject(1));	
//				System.out.println(results.getObject(2));	
//			
//			}
			System.out.println();
		} catch (Exception e) {
			e.printStackTrace();
		}
	
		
	}
}
