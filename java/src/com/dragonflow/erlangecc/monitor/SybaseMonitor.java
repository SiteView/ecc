package com.dragonflow.erlangecc.monitor;


import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.CallableStatement;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.dragonflow.erlangecc.common.ErrorCode;
import com.dragonflow.erlangecc.util.SvMessage;
import com.dragonflow.siteview.ErlangUtils;
import com.dragonflow.siteview.infra.util.ServicePlatform;
import com.ericsson.otp.erlang.OtpErlangList;

public class SybaseMonitor extends BaseMonitor{
	private static final int SYBASE_PORT=5000;
	private String lastError;
	
	public SybaseMonitor() {
		super();
	}
	
	public SybaseMonitor(SvMessage message) {
		super(message);

	}


	
	public int getCounters(Map<String,Object> mapCounters,String host,String usr,String pwd){
		try {
			Class.forName("com.sybase.jdbc2.jdbc.SybDriver");
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
			setLastError(e.getLocalizedMessage());
			return ErrorCode.NOT_FOUND_CLASS;
		}
		
		List lstNames = new ArrayList();
		
		String url = "jdbc:sybase:Tds:" + host + ":" + SYBASE_PORT;
		
		Connection conn = null;
		String prefix;
		
		try {
			conn = DriverManager.getConnection(url, usr,pwd);
			Statement stmt=conn.createStatement();
			String sql = "select sysdatabases.name  as name from sysdatabases ,sysusages where sysdatabases .dbid = sysusages.dbid";
			ResultSet rs;
			try
			{
				rs = stmt.executeQuery(sql);
				prefix = "Table Space";
				mapCounters.put(prefix, prefix);
				while(rs.next()){
					String strName = rs.getString("name");
					String total_prefix = prefix + "/"+ strName;
					mapCounters.put(total_prefix,total_prefix);
					total_prefix += "/";
					lstNames.add(strName);
					
					String Counter = "TableSpace$tablespace_usedPercent$" + strName;
					mapCounters.put(total_prefix + "percent used(%)", Counter);
					Counter = "TableSpace$tablespace_used$" + strName;
					mapCounters.put(total_prefix + "used(MB)", Counter);
					Counter = "TableSpace$tablespace_freePercent$" + strName;
					mapCounters.put(total_prefix + "percent free(%)", Counter);
					Counter = "TableSpace$tablespace_free$" + strName;
					mapCounters.put(total_prefix + "free(MB)", Counter);
					Counter = "TableSpace$tablespace_total$" + strName;
					mapCounters.put(total_prefix + "total(MB)", Counter);
				}
			}catch(SQLException e){
				e.printStackTrace();
				setLastError(e.getMessage());
			}
			
			
			mapCounters.put("Perform", "Perform");
			sql = "select lastrun,cpu_busy,io_busy,idle,pack_received,pack_sent,pack_errors,total_read,total_write,total_errors,connections from spt_monitor";
			try{
				rs = stmt.executeQuery(sql);
				mapCounters.put("Perform/cpu busy", "Perform$cpu_busy");
				mapCounters.put("Perform/io busy", "Perform$io_busy");
				mapCounters.put("Perform/pack received", "Perform$pack_received");
				mapCounters.put("Perform/pack sent", "Perform$pack_sent");
				mapCounters.put("Perform/pack errors", "Perform$pack_errors");
				mapCounters.put("Perform/total read", "Perform$total_read");
				mapCounters.put("Perform/total write", "Perform$total_write");
				mapCounters.put("Perform/connect", "Perform$connect");
			}catch(SQLException e){
				e.printStackTrace();
				setLastError(e.getMessage());
			}
			sql = "sp_transactions";
			
			try{
				stmt.executeQuery(sql);
				mapCounters.put("Perform/transactions", "Perform$transactions");
			}catch(SQLException e){
				e.printStackTrace();
				setLastError(e.getLocalizedMessage());
			}
			sql = "sp_who";
			try{
				stmt.executeQuery(sql);
				mapCounters.put("Perform/connections", "Perform$connections");
			}catch(SQLException e){
				e.printStackTrace();
				setLastError(e.getMessage());
			}
			
			prefix = "Memory";
			mapCounters.put(prefix,prefix);
			
			sql = "sp_configure 'logical memory'";
			try{
				rs=stmt.executeQuery(sql);
				if(rs.next()){
					mapCounters.put(prefix+"/memory used", prefix+"$memory_used");
				}
			}catch(SQLException e){
				e.printStackTrace();
				setLastError(e.getMessage());
			}
			
			prefix = "Log Space";
			mapCounters.put(prefix, prefix);
			
			//日志空间
			sql = "select s.name, (select max(convert(int,t.status & 1)) " +
					"from YOUR_DATABASE.dbo.systhresholds t where s.segment=t.segment) status, " +
					"sum(u.size)*2 size,sum(u.size - curunreservedpgs(u.dbid, u.lstart, u.unreservedpgs))*2.000/1024 used," +
					" @@thresh_hysteresis hysteresis from YOUR_DATABASE.dbo.syssegments s, master.dbo.sysusages u" +
					" where 1 = 1 and ((u.segmap / ((s.segment & 1) + 1)) / power(2, (s.segment & 30))) & 1 = 1 " +
					" and u.dbid = db_id('YOUR_DATABASE') and s.name like '%' group by s.name order by 1";

			if (lstNames.size()>0){
				sql = sql.replace("YOUR_DATABASE", (String)lstNames.get(0));
				try{
					rs = stmt.executeQuery(sql);
					for(int i=0; i< lstNames.size();i++){
						String total_prefix = prefix + "/" + lstNames.get(i);
						mapCounters.put(total_prefix, total_prefix);
						mapCounters.put(total_prefix+ "/" + "log free(MB)", prefix+"$"+"log_free$" + lstNames.get(i));
						mapCounters.put(total_prefix+ "/" + "log free precent(%)", prefix+"$"+"log_freePrecent$" + lstNames.get(i));
					}
				}catch(SQLException e){
					e.printStackTrace();
					setLastError(e.getMessage());
				}
			}
			
			prefix = "Blocked";
			mapCounters.put(prefix+ " Process", prefix);
			//死锁
			sql = "select spid,hostname,blocked, time_blocked from sysprocesses where  blocked>0 order by time_blocked DESC";
			try{
				stmt.executeQuery(sql);
				mapCounters.put(prefix+" Process/"+"blocked longest process", prefix+"$"+"blocked_process");
				mapCounters.put(prefix+" Process/"+"blocked longest time", prefix+"$"+"blocked_time");
			}catch(SQLException e){
				e.printStackTrace();
				setLastError(e.getMessage());
			}
			
			sql = "select count(*) total from sysprocesses where  blocked>0";
			try{
				stmt.executeQuery(sql);
				mapCounters.put(prefix+" Process/"+"blocked processes number", prefix+"$"+"blocked_sum");
			}catch(SQLException e){
				e.printStackTrace();
				setLastError(e.getMessage());
			}
			stmt.close();
			conn.close();

		} catch (SQLException e) {
			try {
				if (conn!=null)
					conn.close();
			} catch (SQLException e1) {
				e1.printStackTrace();
			}
			e.printStackTrace();
			setLastError(e.getMessage());
		} 

		if(mapCounters.size()>0)
			return ErrorCode.OK;
		return ErrorCode.CONNECT_DATABASE_ERROR;
	}
	
	


	public int Delta(int src, int dst)
	{
		if(src>dst)
			return src-dst;
		else
			return 0;
	}

	public int getValues(Map<String,Object> mapValues,String host,String usr,String pwd,Map<String,Object> counters){
		try {
			Class.forName("com.sybase.jdbc2.jdbc.SybDriver");
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
			setLastError(e.getMessage());
			return ErrorCode.NOT_FOUND_CLASS;
		}
		String url = "jdbc:sybase:Tds:" + host + ":" + SYBASE_PORT;
		List lstNames = new ArrayList();
		Connection conn = null;
		String prefix;
		
		try {
			try{
				conn = DriverManager.getConnection(url, usr,pwd);
			}catch (SQLException e) {
				setLastError(e.getMessage());
				e.printStackTrace();
				return ErrorCode.CONNECT_DATABASE_ERROR;
			} 
			
			prefix = "TableSpace";
			Statement stmt=conn.createStatement();
			String sql = "select name from sysdatabases";
			ResultSet rs = null;
			
			try
			{
				rs = stmt.executeQuery(sql);
				while(rs.next()){
					String strName = rs.getString("name");
					lstNames.add(strName);
				}
			}catch(SQLException e){
				e.printStackTrace();
			}
	
			for(int i=0;i<lstNames.size();i++)
			{
				try 
				{
					String strDatabase = (String)lstNames.get(i);
					stmt.execute("use " + strDatabase);
					CallableStatement cs = conn.prepareCall("{call sp_spaceused}");
					boolean flag = cs.execute();

					int updateCount = -1;
					rs = null;
					String strTotal=null,strUsed=null,strFree=null;
					do
					{
						updateCount= cs.getUpdateCount();
						if(flag = false && updateCount!=-1)
						{
							cs.getMoreResults();
							continue;
						}
						rs = cs.getResultSet();
						if(rs!=null)
						{
							ResultSetMetaData resultsetmetadata = rs.getMetaData();
							for(int k=1; k<=resultsetmetadata.getColumnCount(); k++)
							{
								if(resultsetmetadata.getColumnLabel(k).equals("database_size"))
								{
									if(rs.next())
									{
										strTotal = rs.getString("database_size");
									}									
								}
								if(resultsetmetadata.getColumnLabel(k).equals("reserved"))
								{
									if(rs.next())
									{
										strUsed = rs.getString("reserved");
									}									
								}
								if(resultsetmetadata.getColumnLabel(k).equals("unused"))
								{
  									strFree = rs.getString("unused");
								}
							}
						}
						cs.getMoreResults();
						continue;
					}while(updateCount!=-1 || rs != null);
		
					if(strTotal==null || strUsed==null || strFree==null)
					{
						cs.close();
						break;
					}
						
					int nIndex;
					double fTotal, fFree;
					if((nIndex=strTotal.indexOf(" MB"))>=0)
					{
						fTotal = Double.parseDouble(strTotal.substring(0, nIndex));
					}
					else if((nIndex=strTotal.indexOf(" GB"))>=0)
					{
						fTotal = Double.parseDouble(strTotal.substring(0, nIndex))*1024.0;
					}
					else if((nIndex=strTotal.indexOf(" KB"))>=0)
					{
						fTotal = Double.parseDouble(strTotal.substring(0, nIndex))/1024.0;
					}
					else
					{
						fTotal = -1;
					}
							
					if((nIndex=strUsed.indexOf(" MB"))>=0)
					{
						fFree = fTotal - Double.parseDouble(strUsed.substring(0, nIndex));
					}
					else if((nIndex=strUsed.indexOf(" GB"))>=0)
					{
						fFree = fTotal - Double.parseDouble(strUsed.substring(0, nIndex))*1024.0;
					}
					else if((nIndex=strUsed.indexOf(" KB"))>=0)
					{
						fFree = fTotal-Double.parseDouble(strUsed.substring(0, nIndex))/1024.0;
					}
					else
					{
						fFree = -1;
					}
						
					if((nIndex=strFree.indexOf(" MB"))>=0)
					{
						fFree = fFree + Double.parseDouble(strFree.substring(0, nIndex));
					}
					else if((nIndex=strUsed.indexOf(" GB"))>=0)
					{
						fFree = fFree + Double.parseDouble(strFree.substring(0, nIndex))*1024.0;
					}
					else if((nIndex=strFree.indexOf(" KB"))>=0)
					{
						fFree = fFree + Double.parseDouble(strFree.substring(0, nIndex))/1024.0;
					}
					else
					{
						fFree = -1;
					}

					String Counter = prefix+"$"+"tablespace_usedPercent$" + strDatabase;
					if (fTotal>0)
						mapValues.put(Counter,(float)(fTotal-fFree)/fTotal * 100);
					else
						mapValues.put(Counter,0);
				
					Counter = prefix+"$"+"tablespace_used$" + strDatabase;
					mapValues.put(Counter,fTotal-fFree);
						
					Counter = prefix+"$"+"tablespace_freePercent$" + strDatabase;
					if (fTotal > 0)
						mapValues.put(Counter,(float)fFree/fTotal*100);
					else
						mapValues.put(Counter,0);
						
					Counter = prefix+"$"+"tablespace_free$" + strDatabase;
					mapValues.put(Counter,fFree);
						
					Counter = prefix+"$"+"tablespace_total$" + strDatabase;
					mapValues.put(Counter,fTotal);
			
					cs.close();
				}catch(SQLException e){
					e.printStackTrace();
				}
			}
			
			stmt.execute("use master");
			stmt.executeQuery("sp_monitor");
			sql = "select lastrun,cpu_busy,io_busy,idle,pack_received,pack_sent,pack_errors,total_read,total_write,total_errors,connections from spt_monitor";
			try{
				rs = stmt.executeQuery(sql);
				if (rs.next()){
					if(counters.get("Perform$cpu_busy_total")!=null && !counters.get("Perform$cpu_busy_total").equals("n/a"))
						mapValues.put("Perform$cpu_busy",Delta(rs.getInt("cpu_busy"), Integer.parseInt((String)counters.get("Perform$cpu_busy_total"))));
					else
						mapValues.put("Perform$cpu_busy", 0);
					mapValues.put("Perform$cpu_busy_total",rs.getInt("cpu_busy"));

					if(counters.get("Perform$io_busy_total")!=null && !counters.get("Perform$io_busy_total").equals("n/a"))
						mapValues.put("Perform$io_busy",Delta(rs.getInt("io_busy"), Integer.parseInt((String)counters.get("Perform$io_busy_total"))));
					else
						mapValues.put("Perform$io_busy", 0);
					mapValues.put("Perform$io_busy_total",rs.getInt("io_busy"));

					if(counters.get("Perform$pack_received_total")!=null && !counters.get("Perform$pack_received_total").equals("n/a"))
						mapValues.put("Perform$pack_received",Delta(rs.getInt("pack_received"), Integer.parseInt((String)counters.get("Perform$pack_received_total"))));
					else
						mapValues.put("Perform$pack_received", 0);
					mapValues.put("Perform$pack_received_total",rs.getInt("pack_received"));

					if(counters.get("Perform$pack_sent_total")!=null && !counters.get("Perform$pack_sent_total").equals("n/a"))
						mapValues.put("Perform$pack_sent",Delta(rs.getInt("pack_sent"), Integer.parseInt((String)counters.get("Perform$pack_sent_total"))));
					else
						mapValues.put("Perform$pack_sent", 0);
					mapValues.put("Perform$pack_sent_total",rs.getInt("pack_sent"));

					if(counters.get("Perform$pack_errors_total")!=null && !counters.get("Perform$pack_errors_total").equals("n/a"))
						mapValues.put("Perform$pack_errors",Delta(rs.getInt("pack_errors"), Integer.parseInt((String)counters.get("Perform$pack_errors_total"))));
					else
						mapValues.put("Perform$pack_errors", 0);
					mapValues.put("Perform$pack_errors_total",rs.getInt("pack_errors"));

					if(counters.get("Perform$total_read_total")!=null && !counters.get("Perform$total_read_total").equals("n/a"))
						mapValues.put("Perform$total_read",Delta(rs.getInt("total_read"), Integer.parseInt((String)counters.get("Perform$total_read_total"))));
					else
						mapValues.put("Perform$total_read", 0);
					mapValues.put("Perform$total_read_total",rs.getInt("total_read"));

					if(counters.get("Perform$total_write_total")!=null && !counters.get("Perform$total_write_total").equals("n/a"))
						mapValues.put("Perform$total_write",Delta(rs.getInt("total_write"), Integer.parseInt((String)counters.get("Perform$total_write_total"))));
					else
						mapValues.put("Perform$total_write", 0);
					mapValues.put("Perform$total_write_total",rs.getInt("total_write"));

					if(counters.get("Perform$connect_total")!=null && !counters.get("Perform$connect_total").equals("n/a"))
						mapValues.put("Perform$connect",Delta(rs.getInt("connections"), Integer.parseInt((String)counters.get("Perform$connect_total"))));
					else
						mapValues.put("Perform$connect", 0);
					mapValues.put("Perform$connect_total", rs.getInt("connections"));
				}
			}catch(SQLException e){
				e.printStackTrace();
			}

			sql = "sp_transactions";			
			try{
				rs = stmt.executeQuery(sql);
				int iCount = 0;
				while(rs.next()){
					++iCount;
				}
				mapValues.put("Perform$transactions", iCount);
			}catch(SQLException e){
				e.printStackTrace();
			}
			
			sql = "sp_who";
			try{
				rs = stmt.executeQuery(sql);
				int iCount = 0;
				while(rs.next()){
					++iCount;
				}
				mapValues.put("Perform$connections", iCount);
			}catch(SQLException e){
				e.printStackTrace();
				setLastError(e.getMessage());
			}
			
			try{
				sql = "sp_configure 'physical memory'";
				rs=stmt.executeQuery(sql);
				long used = 0,max = 0;
				if(rs.next()){
					used = rs.getLong("Memory Used");
				}
				
				sql = "sp_configure 'max memory'";
				rs=stmt.executeQuery(sql);

				if(rs.next()){
					max = rs.getLong("Memory Used");
				}
				
				if(max>0)
					mapValues.put("Memory$memory_used", (float)used/max*100);
				else
					mapValues.put("Memory$memory_used", 0);
					
			}catch(SQLException e){
				e.printStackTrace();
			}
			
			//日志空间
			sql = "select s.name, (select max(convert(int,t.status & 1)) " +
			"from YOUR_DATABASE.dbo.systhresholds t where s.segment=t.segment) status, " +
			"sum(u.size)*2 size,sum(u.size - curunreservedpgs(u.dbid, u.lstart, u.unreservedpgs))*2.000/1024 used," +
			" @@thresh_hysteresis hysteresis from YOUR_DATABASE.dbo.syssegments s, master.dbo.sysusages u" +
			" where 1 = 1 and ((u.segmap / ((s.segment & 1) + 1)) / power(2, (s.segment & 30))) & 1 = 1 " +
			" and u.dbid = db_id('YOUR_DATABASE') and s.name like '%' group by s.name order by 1";
			
			prefix = "Log Space";
			
			if (lstNames.size()>0){
				for(int i=0;i<lstNames.size();i++){
					String strDatabase = (String)lstNames.get(i);
					String tmpsql = sql.replace("YOUR_DATABASE", strDatabase);
					try{
						rs = stmt.executeQuery(tmpsql);
						if (rs.next()){
							
							float used = rs.getFloat("used");
							float size = rs.getFloat("size");
							mapValues.put(prefix+"$"+"log_free$" + strDatabase,size/1024-used);
							mapValues.put(prefix+"$"+"log_freePrecent$" + strDatabase,(float)(size/1024-used)/(size/1024)*100);
						}
						else
						{
							mapValues.put(prefix+"$"+"log_free$" + strDatabase,0);
							mapValues.put(prefix+"$"+"log_freePrecent$" + strDatabase,0);						
						}
					}catch(SQLException e){
						e.printStackTrace();
					
					}
				}
			}
			
			prefix="Blocked";
			
			//死锁
			sql = "select spid,hostname,blocked, time_blocked from sysprocesses where  blocked>0 order by time_blocked DESC";
			try{
				rs = stmt.executeQuery(sql);
				if (rs.next()){
					mapValues.put(prefix+"$"+"blocked_process",rs.getLong("spid"));
					mapValues.put(prefix+"$"+"blocked_time(ms)",rs.getLong("time_blocked"));
				}else{
					mapValues.put(prefix+"$"+"blocked_process",0);
					mapValues.put(prefix+"$"+"blocked_time",0);
				}
					
			}catch(SQLException e){
				e.printStackTrace();
			}
			
			sql = "select count(*) total from sysprocesses where  blocked>0";
			try{
				rs = stmt.executeQuery(sql);
				if(rs.next()){
					mapValues.put(prefix+"$"+"blocked_sum",rs.getLong("total"));
				}
				else
					mapValues.put(prefix+"$"+"blocked_sum",rs.getLong("total"));
					
			}catch(SQLException e){
				e.printStackTrace();
			}
			
			stmt.close();
			conn.close();
		} catch (SQLException e) {
			try {
				if (conn!=null)
					conn.close();
			} catch (SQLException e1) {
				e1.printStackTrace();
			}
			e.printStackTrace();
		} 
		
		return ErrorCode.OK;
	}

	public static void main(String[] args) {

		Map<String,Object> counters = new HashMap<String,Object>();
		SybaseMonitor sy = new SybaseMonitor();
		
/*		sy.getValues(counters,"192.168.0.65","sa","888888");
		
		Iterator<Map.Entry<String, Object>> it = counters.entrySet().iterator();
		
		while(it.hasNext()){
			Map.Entry<String, Object> ent = it.next();
			System.out.println("key="+ent.getKey() + ";value="+ent.getValue());
		}
		/*
		try {
			Class.forName("com.sybase.jdbc2.jdbc.SybDriver");
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		}
		String url ="jdbc:sybase:Tds:192.168.2.231:5000"; 
		Connection conn;
		try {
			conn = DriverManager.getConnection(url, "sa","");
			Statement stmt=conn.createStatement();
			String sql = "sp_monitor";
			ResultSet rs = stmt.executeQuery(sql);
			while(rs.next()){
				System.out.println(rs.getString(4));
			}
		} catch (SQLException e) {
			
			e.printStackTrace();
		} 
		*/

	}


	@Override
	public int handleMessage() {
		SvMessage msg = this.getMessage();
		if (msg.getAction().equals("getCounters")){
			Map<String,Object> params = msg.getParams();
			Map<String,Object> counters = new HashMap<String,Object>();
			String host = (String)params.get("host");
			String usr = (String)params.get("usr");
			String pwd = (String)params.get("pwd");
			if (0==getCounters(counters, host, usr, pwd)){
				this.sendResponse2("ok", counters);
			}else{
				if(this.getLastError().length()>250)
					this.sendResponse(ServicePlatform.replaceUnicodeChar(this.getLastError().substring(0,250)), counters);
				else
					this.sendResponse(ServicePlatform.replaceUnicodeChar(this.getLastError()), counters);
			}
		}else if (msg.getAction().equals("getValues")){
			Map<String,Object> params = msg.getParams();
			Map<String,Object> values = new HashMap<String,Object>();
			String host = (String)params.get("host");
			String usr = (String)params.get("usr");
			String pwd = (String)params.get("pwd");
			OtpErlangList counterslist = (OtpErlangList)params.get("counters");
			Map<String,Object> counter = ErlangUtils.erlangListToMap(counterslist);
			Map<String,Object> counters =  new HashMap<String,Object>();
			
			Iterator<Map.Entry<String, Object>> iter = counter.entrySet().iterator();     
	        while(iter.hasNext()) {     
			    Map.Entry<String, Object> entry = (Map.Entry<String, Object>)iter.next();     
			    String propId = entry.getKey(); 
			    int Len = propId.length();
			    if(Len>2)
			    	propId = propId.substring(1, Len-1);
	            counters.put(propId, entry.getValue());
	        }
			System.out.println(counters);
			
			if (0==this.getValues(values,host,usr,pwd,counters)){
				this.sendResponse2("ok", values);
			}else{
				if(this.getLastError().length()>250)
					this.sendResponse(ServicePlatform.replaceUnicodeChar(this.getLastError().substring(0,250)), values);
				else
					this.sendResponse(ServicePlatform.replaceUnicodeChar(this.getLastError()), values);
			}
		}
		return ErrorCode.OK;
	}

	public String getLastError() {
		return lastError;
	}
	
	private void setLastError(String Error) {
		lastError = Error;
	}
	
}
