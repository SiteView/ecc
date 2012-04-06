package com.dragonflow.erlangecc.monitor;

import java.io.File;
import java.io.FileInputStream;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.dbcp.BasicDataSource;
import org.apache.commons.lang.StringEscapeUtils;
import org.apache.commons.lang.SystemUtils;
import org.apache.commons.lang.math.NumberUtils;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import COM.datachannel.xml.om.Document;

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

public class OracleQuestMonitor extends BaseMonitor {
	String connection_url = "";
	String user = "";
	String password = "";
	String driver = "oracle.jdbc.driver.OracleDriver";
	int connection_timeout = 60 * 1000;
	int query_timeout = 60 * 1000;
	// String query_cmd = "";
	Map<String, Object> counters = new HashMap<String, Object>();

	StringBuffer lastError = new StringBuffer("");
	Map<String, Object> browseData = new HashMap<String, Object>();

	BasicDataSource dbs = new BasicDataSource();

	public static final String EMPTY_ERLANG_LIST_STRING = (new OtpErlangList())
			.toString();

	void initArgs(Map<String, Object> map) {
		String s = null;
		try {
			s = (String) map.get("connection_url");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.connection_url = s.trim();
			}
			s = (String) map.get("user");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.user = s.trim();
			}
			s = (String) map.get("password");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.password = s.trim();
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
//select from v$parameter 都需要验证正确性
	public Map<String, Object> getqueryValues() {
		Map<String, Object> queryValues = new HashMap<String, Object>();
		this.initArgs(this.getMessage().getParams());
		init_config();
		Connection conn = null;
		PreparedStatement statement = null;
		ResultSet results = null;
		String strsql = "";
		try {
			conn = this.getConn();
			long tempv=0;
			// ================sessions============
			long response_time=0;//响应时间
			strsql="select user from dual";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			long begin = System.currentTimeMillis();
			results = statement.executeQuery();
			response_time= System.currentTimeMillis()-begin;
			queryValues.put("response_time", response_time);
			
			
			String total_users="n/a";
			String active_users="n/a";
			String percent_active="n/a";
			strsql="select count(username) total_users,count(distinct username) active_users , round(count(distinct username)/count(username),2) percent_active from v$session";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				total_users = results.getString("total_users");
				active_users = results.getString("active_users");
				percent_active = results.getString("percent_active");

			}
			queryValues.put("total_users", total_users);
			queryValues.put("active_users", active_users);
			queryValues.put("percent_active", percent_active);
			
			//================Server Processes===========
			String pga_total ="n/a";
			//strsql="select  round(value/1024/1024,0) pga_total from  V$PGASTAT where name='aggregate PGA target parameter'";	
			strsql="select  value pga_total from  V$PGASTAT where name='aggregate PGA target parameter'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				pga_total = results.getString("pga_total");
			}
			queryValues.put("pga_total",getV(pga_total));
			
			String pga_used  ="n/a";
			//strsql="select round(value/1024/1024,1) pga_used  from  V$PGASTAT where name='total PGA inuse'";	
			strsql="select value pga_used  from  V$PGASTAT where name='total PGA inuse'";
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				pga_used = results.getString("pga_used");
			}
			queryValues.put("pga_used", getV(pga_used));
			
			String Dedicated_servers  ="n/a";
			strsql="select count(*) Dedicated_servers  from v$session where server='DEDICATED' and type='USER' ";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Dedicated_servers = results.getString("Dedicated_servers");
			}
			queryValues.put("Dedicated_servers", Dedicated_servers );
			
			
			String shared_servers  ="n/a";
			strsql="select count(distinct name) shared_servers from  v$shared_server";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				shared_servers = results.getString("shared_servers");
			}
			queryValues.put("shared_servers", shared_servers );
			
			String Dispatcher_processes  ="n/a";
			strsql="select count(distinct name) Dispatcher_processes from v$dispatcher";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Dispatcher_processes = results.getString("Dispatcher_processes");
			}
			queryValues.put("Dispatcher_processes", Dispatcher_processes );
			
			String Parallel_query  ="n/a";
			strsql="SELECT value Parallel_query FROM V$PQ_SYSSTAT WHERE statistic = 'Servers Busy'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Parallel_query = results.getString("Parallel_query");
			}
			if(Parallel_query.equals("n/a"))
			{
				Parallel_query="0";
			}
			queryValues.put("Parallel_query", Parallel_query );
			
			String job_queues   ="n/a";
			strsql="select value job_queues from v$parameter where name='job_queue_processes'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				job_queues = results.getString("job_queues");
			}
			queryValues.put("job_queues", job_queues );
			
			//=======================SGA===================
			String total_sga   ="n/a";//ok
			//strsql="select round(sum(bytes)/1024/1024,0) total_sga from v$sgastat";	
			strsql="select sum(bytes) total_sga from v$sgastat";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				total_sga = results.getString("total_sga");
			}
			queryValues.put("total_sga", getV(total_sga) );
			
			String Buffer_cache   ="n/a";
			//strsql="select round(value/1024,2) Buffer_cache from v$parameter where name='db_cache_size'";	
			strsql="select value Buffer_cache from v$parameter where name='db_cache_size'";
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Buffer_cache = results.getString("Buffer_cache");
			}
			queryValues.put("Buffer_cache", getV(Buffer_cache) );
			
			String Recycle_pool   ="n/a";
			//strsql="select round(value/1024,2) Recycle_pool from v$parameter where name= 'db_recycle_cache_size'";
			strsql="select value Recycle_pool from v$parameter where name= 'db_recycle_cache_size'";
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Recycle_pool = results.getString("Recycle_pool");
			}
			queryValues.put("Recycle_pool", getV(Recycle_pool) );
			
			String Keep_pool   ="n/a";
			//strsql="select round(value/1024,2) Keep_pool from v$parameter where name= 'DB_KEEP_CACHE_SIZE'";	
			strsql="select value Keep_pool from v$parameter where name= 'db_keep_cache_size'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Keep_pool = results.getString("Keep_pool");
			}
			queryValues.put("Keep_pool", getV(Keep_pool) );
			
			//SELECT 1 – ((physical.value – direct.value – lobs.value) / logical.value) “Buffer Cache Hit Ratio” FROM V$SYSSTAT physical, V$SYSSTAT direct, V$SYSSTAT lobs, V$SYSSTAT logical WHERE physical.name = ‘physical reads’ AND direct.name = ‘physical reads direct’ AND lobs.name = ‘physical reads direct (lob)’ AND logical.name = ’session logical reads’;
			String Buffer_cache_hit_rate   ="n/a";
			//strsql="SELECT round((P1.value + P2.value - P3.value) / (P1.value + P2.value),2) Buffer_cache_hit_rate  FROM   v$sysstat P1, v$sysstat P2, v$sysstat P3 WHERE  P1.name = 'db block gets' AND    P2.name = 'consistent gets'  AND P3.name = 'physical reads'";	
			strsql="SELECT round(1-((P1.value-P2.value-P3.value)/P4.value),2) Buffer_cache_hit_rate from v$sysstat P1,v$sysstat P2, v$sysstat P3, v$sysstat P4 where P1.name = 'physical reads' and P2.name = 'physical reads direct' and P3.name = 'physical reads direct (lob)' and P4.name = 'session logical reads'";
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Buffer_cache_hit_rate = results.getString("Buffer_cache_hit_rate");
			}
			queryValues.put("Buffer_cache_hit_rate", Buffer_cache_hit_rate );
			
			String redo_buffer   ="n/a";
			//strsql="select round(value/1024/1024,2) redo_buffer from v$sga where name='Redo Buffers'";	
			strsql="select value redo_buffer from v$sga where name='Redo Buffers'";
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				redo_buffer = results.getString("redo_buffer");
			}
			queryValues.put("redo_buffer", getV(redo_buffer) );
			
			String Shared_pool    ="n/a";
			//strsql="select round(value/1024,2) Shared_pool from v$parameter where name='shared_pool_size'";	
			strsql="select value Shared_pool from v$parameter where name='shared_pool_size'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Shared_pool = results.getString("Shared_pool");
			}
			queryValues.put("Shared_pool", getV(Shared_pool) );
			
			String shared_pool_percent    ="n/a";
			strsql="select 1-Round((s.bytes/p.value),2) shared_pool_percent from   v$sgastat s,v$parameter p where s.name='free memory' and p.name='shared_pool_size' and s.pool='shared pool'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			try
			{
			results = statement.executeQuery();
			while (results.next()) {
				shared_pool_percent = results.getString("shared_pool_percent");
			}
			}catch(Exception ex1)
			{
				shared_pool_percent="1";
			}
			queryValues.put("shared_pool_percent", shared_pool_percent );
			
			String large_pool    ="n/a";
			//strsql="select round(value/1024,2) large_pool from v$parameter where name='large_pool_size'";	
			strsql="select value large_pool from v$parameter where name='large_pool_size'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				large_pool = results.getString("large_pool");
			}
			queryValues.put("large_pool", getV(large_pool) );
			
			String java_pool    ="n/a";
			//strsql="select round(value/1024,2) java_pool from v$parameter where name='java_pool_size'";	
			strsql="select value java_pool from v$parameter where name='java_pool_size'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				java_pool = results.getString("java_pool");
			}
			queryValues.put("java_pool", getV(java_pool) );
			
			long sga_targe_value=0; 
			strsql="select value sga_targe_value from v$parameter where name='sga_target'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				sga_targe_value = results.getLong("sga_targe_value");
			}
			if(sga_targe_value>0)
			queryValues.put("Auto_mode", "On");
			else queryValues.put("Auto_mode", "Off");
			
			//======================Background Processes========================
			String dbwr_num    ="n/a";
			strsql="select value dbwr_num  from v$parameter where name='db_writer_processes'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				dbwr_num = results.getString("dbwr_num");
			}
			queryValues.put("dbwr_num", dbwr_num );
			
			String rvwr_num    ="n/a";
			strsql="select count(*) rvwr_num from v$bgprocess where name like 'RVWR%' and paddr<>'00'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				rvwr_num = results.getString("rvwr_num");
			}
			queryValues.put("rvwr_num", rvwr_num );
			
			String lgwr_num    ="n/a";
			strsql="select count(*) lgwr_num from v$bgprocess where name like 'LGWR%' and paddr<>'00'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				lgwr_num = results.getString("lgwr_num");
			}
			queryValues.put("lgwr_num", lgwr_num );
			
			String arch_num    ="n/a";
			//strsql="select count(*) arch_num  from v$bgprocess where name like 'ARCH%' and paddr<>'00'";	
			strsql="select value arch_num  from v$parameter where name ='log_archive_max_processes'";
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				arch_num = results.getString("arch_num");
			}
			queryValues.put("arch_num", arch_num );
			
			
			//=======================DISK STORAGE=======================
			
			String Online_files="n/a";
			strsql="select count(*) Online_files  from v$datafile where status='ONLINE' or status='SYSTEM'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Online_files = results.getString("Online_files");
			}
			queryValues.put("Online_files", Online_files );
			
			String Online_tablespaces="n/a";
			//与数据文件对应v$datafile 不含temp（v$tempfile）
			strsql="select count(*) Online_tablespaces from v$tablespace where name<>'TEMP' ";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Online_tablespaces = results.getString("Online_tablespaces");
			}
			queryValues.put("Online_tablespaces", Online_tablespaces );
			
			String pages_used="n/a";
			String pages_percent_used="n/a";
			//strsql="select round((b.pages_all-a.pages_free)/1024/1024/1024,2) pages_used,round((1-a.pages_free/b.pages_all),2) pages_percent_used from (select sum(bytes) pages_free from user_free_space) a, (select sum(bytes) pages_all from v$datafile) b";	
			strsql="select (b.pages_all-a.pages_free) pages_used,round((1-a.pages_free/b.pages_all),2) pages_percent_used from (select sum(bytes) pages_free from user_free_space) a, (select sum(bytes) pages_all from v$datafile) b";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				pages_used = results.getString("pages_used");
				pages_percent_used = results.getString("pages_percent_used");
			}
			queryValues.put("pages_used", getV(pages_used)  );
			queryValues.put("pages_percent_used", pages_percent_used );
			
			String flash_pages_all="n/a";
			String flash_used="n/a";
			String flash_percent_used="n/a";
			//strsql="select round(sum(space_limit)/1024/1024/1024,2) flash_pages_all, round(sum(space_used)/1024,2) flash_used from v$recovery_file_dest";	
			strsql="select sum(space_limit) flash_pages_all, sum(space_used) flash_used from v$recovery_file_dest";
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				flash_pages_all = results.getString("flash_pages_all");
				flash_used = results.getString("flash_used");
			}
			if(flash_pages_all.equals("0"))
			{
				flash_percent_used="1";	
			}else
			{
				flash_percent_used=""+(Float.parseFloat(flash_used)/Float.parseFloat(flash_pages_all));
			}
			queryValues.put("flash_pages_all", getV(flash_pages_all) );
			queryValues.put("flash_used", getV(flash_used));
			queryValues.put("flash_percent_used", flash_percent_used);
			
			String group_count="n/a";
			String log_average_size="n/a";
			//strsql="select count(group#) group_count,round(avg(bytes)/1024/1024,1) log_average_size from v$log";	
			strsql="select count(group#) group_count,avg(bytes) log_average_size from v$log";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				group_count = results.getString("group_count");
				log_average_size = results.getString("log_average_size");
			}
			queryValues.put("group_count", group_count );
			queryValues.put("log_average_size", getV(log_average_size) );
			
			String log_percent_active="n/a";
			strsql="select round((all_size-inactive_size)/all_size,2) log_percent_active from (select sum(bytes) inactive_size from v$log where status='INACTIVE') a,(select sum(bytes) all_size from v$log) b";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				log_percent_active = results.getString("log_percent_active");
			}
			queryValues.put("log_percent_active", log_percent_active );
			
			String Archive_count ="n/a";
			//	strsql="select count(*) Archive_count from v$archive_dest where binding='MANDATORY'";	
			strsql="select count(destination) Archive_count from v$archive_dest";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Archive_count = results.getString("Archive_count");
			}
			queryValues.put("Archive_count", Archive_count );
			
			String Archive_disks ="n/a";
			strsql="select count(distinct name) Archive_disks from V$ARCHIVED_LOG";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Archive_disks = results.getString("Archive_disks");
			}
			queryValues.put("Archive_disks", Archive_disks );
			
			String total_archive ="n/a";
			//strsql="select round(sum(QUOTA_SIZE)/1024/1024/1024,2) total_archive from v$archive_dest";	
			strsql="select sum(QUOTA_SIZE) total_archive from v$archive_dest";
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				total_archive = results.getString("total_archive");
			}
			queryValues.put("total_archive",getV(total_archive));
			
			String used_archive ="0";
			//strsql="select round(sum(QUOTA_USED)/1024/1024/1024,2) used_archive from v$archive_dest";	
			strsql="select sum(QUOTA_USED) used_archive from v$archive_dest";
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				used_archive = results.getString("used_archive");
			}
			String free_archive=Long.parseLong(total_archive)-Long.parseLong(used_archive)+"";
			queryValues.put("free_archive", getV(free_archive));
			String percent_archive="n/a";
			if (Float.parseFloat(total_archive)==0)
			{
				percent_archive="0";
			}else
			{
				percent_archive=Float.parseFloat(free_archive)/Float.parseFloat(total_archive)+"";
			}
			queryValues.put("percent_archive", percent_archive);
			
			String max_time ="n/a";
			String Archiving_rate="n/a";
			strsql="select to_char(max(first_time),'yyyy-MM-dd HH24:mi:ss') max_time from V$ARCHIVED_LOG";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				max_time = results.getString("max_time");
			}
			if(max_time==null)
			{
				max_time="n/a";
			}
			if(!max_time.equals("n/a"))
			{
				//mb_per_sec
				strsql="SELECT SUM(blocks*block_size)/1024/1024/60/5 Archiving_rate FROM V$ARCHIVED_LOG WHERE first_time BETWEEN to_date(?, 'yyyy-mm-dd hh24:mi:ss') and to_date(?, 'yyyy-mm-dd hh24:mi:ss') ";	
				statement = conn.prepareStatement(strsql);
				Date d=new Date(max_time);
				long d1= d.getTime()-1000*5*60;
				Date d2=new Date(d1);
				SimpleDateFormat df=new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
				statement.setString(1,df.format(d2));
				statement.setString(2,max_time);
				statement.setQueryTimeout(this.query_timeout);
				results = statement.executeQuery();
				while (results.next()) {
					Archiving_rate = results.getString("Archiving_rate");
				}
				
			}
			if(Archiving_rate.equals("n/a"))
			{
				Archiving_rate="0";
			}
			queryValues.put("Archiving_rate", Archiving_rate);
			
			//=================执行两次的差值/5 seconds=====================
			//第一次
			String send_bytes ="n/a";
			strsql="select value/1024 send_bytes from v$sysstat where name='bytes sent via SQL*Net to client'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				send_bytes = results.getString("send_bytes");
			}
			
			//2
			String received_bytes ="n/a";
			strsql="select value/1024 received_bytes from v$sysstat where name='bytes received via SQL*Net from client'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				received_bytes = results.getString("received_bytes");
			}
			//3
			String Logical_reads ="n/a";
			strsql="select value Logical_reads from v$sysstat where name='session logical reads'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Logical_reads = results.getString("Logical_reads");
			}
			//4
			String Block_changes ="n/a";
			strsql="select value Block_changes from v$sysstat where name='db block changes'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Block_changes = results.getString("Block_changes");
			}
			//5
			String Redo_buffer_entries ="n/a";
			strsql="select value Redo_buffer_entries from v$sysstat where name='redo entries'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Redo_buffer_entries = results.getString("Redo_buffer_entries");
			}
			//6
			String Parse_requests ="n/a";
			strsql="select value Parse_requests from v$sysstat where name='parse count (total)'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Parse_requests = results.getString("Parse_requests");
			}
			//7
			String SQL_execution ="n/a";
			strsql="select value SQL_execution from v$sysstat where name='execute count'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				SQL_execution = results.getString("SQL_execution");
			}
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
			//10
			String Flashback_IO ="n/a";
			strsql="select value Flashback_IO from v$sysstat where name='flashback log writes'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Flashback_IO = results.getString("Flashback_IO");
			}
			//11
			String Redo_writes ="n/a";
			strsql="select value Redo_writes from v$sysstat where name='redo writes'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Redo_writes = results.getString("Redo_writes");
			}
			//sleep 时间
			Thread.sleep(5000);  
			//第二次
			String send_bytes0 ="n/a";
			strsql="select value/1024 send_bytes0 from v$sysstat where name='bytes sent via SQL*Net to client'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				send_bytes0 = results.getString("send_bytes0");
			}
			double send_rate= Math.round(Math.abs(Double.parseDouble(send_bytes0)-Double.parseDouble(send_bytes))*10)/50.0;
			queryValues.put("send_rate", send_rate+"");
			//2
			String received_bytes0 ="n/a";
			strsql="select value/1024 received_bytes0 from v$sysstat where name='bytes received via SQL*Net from client'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				received_bytes0 = results.getString("received_bytes0");
			}
			double received_rate= Math.round(Math.abs(Double.parseDouble(received_bytes0)-Double.parseDouble(received_bytes))*10)/50.0;
			queryValues.put("received_rate", received_rate+"");
			//3
			String Logical_reads0 ="n/a";
			strsql="select value Logical_reads0 from v$sysstat where name='session logical reads'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Logical_reads0 = results.getString("Logical_reads0");
			}
			double Logical_reads_rate= Math.round(Math.abs(Double.parseDouble(Logical_reads0)-Double.parseDouble(Logical_reads))*10)/50.0;
			queryValues.put("Logical_reads_rate", Logical_reads_rate+"");
			//4
			String Block_changes0 ="n/a";
			strsql="select value Block_changes0 from v$sysstat where name='db block changes'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Block_changes0 = results.getString("Block_changes0");
			}
			double Block_changes_rate= Math.round(Math.abs(Double.parseDouble(Block_changes0)-Double.parseDouble(Block_changes))*10)/50.0;
			queryValues.put("Block_changes_rate", Block_changes_rate);
			//5
			String Redo_buffer_entries0 ="n/a";
			strsql="select value Redo_buffer_entries0 from v$sysstat where name='redo entries'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Redo_buffer_entries0 = results.getString("Redo_buffer_entries0");
			}
			double Redo_buffer_entries_rate= Math.round(Math.abs(Double.parseDouble(Redo_buffer_entries0)-Double.parseDouble(Redo_buffer_entries))*10)/50.0;
			queryValues.put("Redo_buffer_entries_rate", Redo_buffer_entries_rate);
			//6
			String Parse_requests0 ="n/a";
			strsql="select value Parse_requests0 from v$sysstat where name='parse count (total)'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Parse_requests0 = results.getString("Parse_requests0");
			}
			double Parse_requests_rate= Math.round(Math.abs(Double.parseDouble(Parse_requests0)-Double.parseDouble(Parse_requests))*10)/50.0;
			queryValues.put("Parse_requests_rate", Parse_requests_rate);
			
			//7
			String SQL_execution0 ="n/a";
			strsql="select value SQL_execution0 from v$sysstat where name='execute count'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				SQL_execution0 = results.getString("SQL_execution0");
			}
			double SQL_execution_rate= Math.round(Math.abs(Double.parseDouble(SQL_execution0)-Double.parseDouble(SQL_execution))*10)/50.0;
			queryValues.put("SQL_execution_rate", SQL_execution_rate);
			//8
			String Physical_reads0 ="n/a";
			strsql="select value Physical_reads0 from v$sysstat where name='physical reads'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Physical_reads0 = results.getString("Physical_reads0");
			}
			double Physical_reads_rate= Math.round(Math.abs(Double.parseDouble(Physical_reads0)-Double.parseDouble(Physical_reads))*10)/50.0;
			queryValues.put("Physical_reads_rate", Physical_reads_rate);
			//9
			String Physical_writes0 ="n/a";
			strsql="select value Physical_writes0 from v$sysstat where name='physical writes'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Physical_writes0= results.getString("Physical_writes0");
			}
			double Physical_writes_rate= Math.round(Math.abs(Double.parseDouble(Physical_writes0)-Double.parseDouble(Physical_writes))*10)/50.0;
			queryValues.put("Physical_writes_rate", Physical_writes_rate);
			//10
			String Flashback_IO0 ="n/a";
			strsql="select value Flashback_IO0 from v$sysstat where name='flashback log writes'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Flashback_IO0 = results.getString("Flashback_IO0");
			}
			double Flashback_IO_rate= Math.round(Math.abs(Double.parseDouble(Flashback_IO0)-Double.parseDouble(Flashback_IO))*10)/50.0;
			queryValues.put("Flashback_IO_rate", Flashback_IO_rate);
			//11
			String Redo_writes0 ="n/a";
			strsql="select value Redo_writes0 from v$sysstat where name='redo writes'";	
			statement = conn.prepareStatement(strsql);
			statement.setQueryTimeout(this.query_timeout);
			results = statement.executeQuery();
			while (results.next()) {
				Redo_writes0 = results.getString("Redo_writes0");
			}
			double Redo_writes_rate= Math.round(Math.abs(Double.parseDouble(Redo_writes0)-Double.parseDouble(Redo_writes))*10)/50.0;
			queryValues.put("Redo_writes_rate", Redo_writes_rate);
		} catch (Exception ex) {
			this.lastError.append(ex.getMessage());

		} finally {
			try {
				if (results != null)
					results.close();
			} catch (SQLException e) {
			}
			CloseUtils.closeStatement(statement);
			CloseUtils.closeConnection(conn);
		}
		return queryValues;
	}

	public String getV(String Vt)
	{
		if(Vt==null||Vt.equals("n/a")||Vt.equals(""))
		{
			return "n/a";
		}
		long V1=Long.parseLong(Vt);
		String value="";
		double tempv=0.0000000000;
		tempv=V1/1024.0;
		int len=String.valueOf((int)tempv).length();
		if (len>3)
		{
			tempv=V1/1024/1024.0;
			len=String.valueOf((int)tempv).length();
			if(len>3)
			{
				tempv=V1/1024/1024/1024.0;
				len=String.valueOf((int)tempv).length();
				if(len>3)
				{
					value=(int)tempv+" GB";
				}else
				{
					if(len==3)
					{
					 value=(int)tempv+" GB";
					}else if(len==2)
					{
						  BigDecimal bd=new BigDecimal(tempv); 
					      bd=bd.setScale(1, BigDecimal.ROUND_HALF_UP); 
					      value=bd.doubleValue()+" GB";
					}else if(len==1)
					{
						 BigDecimal bd=new BigDecimal(tempv); 
					     bd=bd.setScale(2, BigDecimal.ROUND_HALF_UP); 
					     value=bd.doubleValue()+" GB";
					}
				}
				
			}else
			{
				if(len==3)
				{
				 value=(int)tempv+" MB";
				}else if(len==2)
				{
					  BigDecimal bd=new BigDecimal(tempv); 
				      bd=bd.setScale(1, BigDecimal.ROUND_HALF_UP); 
				      value=bd.doubleValue()+" MB";
				}else if(len==1)
				{
					 BigDecimal bd=new BigDecimal(tempv); 
				     bd=bd.setScale(2, BigDecimal.ROUND_HALF_UP); 
				     value=bd.doubleValue()+" MB";
				}
			}
			
		}else
		{
			if(len==3)
			{
			 value=(int)tempv+" KB";
			}else if(len==2)
			{
				  BigDecimal bd=new BigDecimal(tempv); 
			      bd=bd.setScale(1, BigDecimal.ROUND_HALF_UP); 
			      value=bd.doubleValue()+" KB";
			}else if(len==1)
			{
				 BigDecimal bd=new BigDecimal(tempv); 
			     bd=bd.setScale(2, BigDecimal.ROUND_HALF_UP); 
			     value=bd.doubleValue()+" KB";
			}
		}
		return value;
	}
	
	public Map<String, Object> update() {
		this.initArgs(this.getMessage().getParams());
		init_config();
		Map<String, Object> hashmap = this.getqueryValues();
		Map<String, Object> results = new HashMap<String, Object>();
		Set<String> counter_set = this.counters.keySet();
		int len=counter_set.size();
		if(this.lastError.toString().isEmpty())
		{
			results.put("pStatus", "ok");
			
		}else
		{
			results.put("pStatus", "error");	
		}
		StringBuffer statestring = new StringBuffer();
		int j=0;
		for(String skey:counter_set)
		{
			if(!results.containsKey(skey))
			{
				results.put(skey, "n/a");
				Object v="n/a";
				if(hashmap.containsKey(skey))
				{
					 v=hashmap.get(skey);
					results.put(skey, v);
					
				}else
				{
					results.put(skey, v);
				}
				Object s1=this.counters.get(skey);
				if (v.toString().equals(""))
					statestring.append(s1).append(" = \"\"");
				else {
					statestring.append(s1).append(" = ").append(v.toString());
				}
				if (j != len - 1)
					statestring.append(", ");
				++j;
			}
		}
		results.put("stateString", statestring.toString()); 
		return results;
	}

	private void init_config() {
		try {
			String jdbcUrl = DatabaseConnectionURL.getConnectionURL(
					this.driver, this.connection_url);
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


	Map<String, Object> getBrowseData() {
		this.initArgs(this.getMessage().getParams());
		this.init_config();

		Map<String, Object> resp = new HashMap<String, Object>();
		Connection conn = this.getConn();
		if (conn == null) {
			this.lastError.append("connect to oracle fail!");
			return resp;
		}
		StringBuffer xml = new StringBuffer();
		xml.append("<browse_data>");
		xml.append("<object name=\"").append("性能指标数据项").append("\"");
		xml.append(" id=\"").append("0").append("\"");
		xml.append(">");

		xml.append("<object name=\"").append("Oracle连接情况").append("\"");
		xml.append(" id=\"").append("1").append("\"");
		xml.append(">");
		// counter

		xml.append("<counter name=\"").append("SQL*Net response").append("\"");
		xml.append(" id=\"").append("response_time").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("SQL*Net users").append("\"");
		xml.append(" id=\"").append("total_users").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Active users").append("\"");
		xml.append(" id=\"").append("active_users").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Percent active Users").append("\"");
		xml.append(" id=\"").append("percent_active").append("\"");
		xml.append(">");
		xml.append("</counter>");
		//
		xml.append("</object>");

		xml.append("<object name=\"").append("Server Processes").append("\"");
		xml.append(" id=\"").append("2").append("\"");
		xml.append(">");
		// counter

		xml.append("<counter name=\"").append("PGA aggregate target").append("\"");
		xml.append(" id=\"").append("pga_total").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Total PGA currently in use").append("\"");
		xml.append(" id=\"").append("pga_used").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Dedicated servers").append("\"");
		xml.append(" id=\"").append("Dedicated_servers").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Shared servers").append("\"");
		xml.append(" id=\"").append("shared_servers").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Dispatchers").append("\"");
		xml.append(" id=\"").append("Dispatcher_processes").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Parallel query").append("\"");
		xml.append(" id=\"").append("Parallel_query").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Job queue").append("\"");
		xml.append(" id=\"").append("job_queues").append("\"");
		xml.append(">");
		xml.append("</counter>");
		//
		xml.append("</object>");

		xml.append("<object name=\"").append("System Global Area").append("\"");
		xml.append(" id=\"").append("3").append("\"");
		xml.append(">");
		// counter
		xml.append("<counter name=\"").append("The total size of SGA").append("\"");
		xml.append(" id=\"").append("total_sga").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Buffer cache").append("\"");
		xml.append(" id=\"").append("Buffer_cache").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Recycle pool").append("\"");
		xml.append(" id=\"").append("Recycle_pool").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Keep pool").append("\"");
		xml.append(" id=\"").append("Keep_pool").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Buffer Cache Hit Ratio").append("\"");
		xml.append(" id=\"").append("Buffer_cache_hit_rate").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Redo buffer").append("\"");
		xml.append(" id=\"").append("redo_buffer").append("\"");
		xml.append(">");
		xml.append("</counter>");
		
		xml.append("<counter name=\"").append("Shared pool").append("\"");
		xml.append(" id=\"").append("Shared_pool").append("\"");
		xml.append(">");
		xml.append("</counter>");
		
		xml.append("<counter name=\"").append("The percentage of the used Shared Pool").append("\"");
		xml.append(" id=\"").append("shared_pool_percent").append("\"");
		xml.append(">");
		xml.append("</counter>");
		
		xml.append("<counter name=\"").append("Java pool").append("\"");
		xml.append(" id=\"").append("java_pool").append("\"");
		xml.append(">");
		xml.append("</counter>");
		
		xml.append("<counter name=\"").append("Large pool").append("\"");
		xml.append(" id=\"").append("large_pool").append("\"");
		xml.append(">");
		xml.append("</counter>");
		
		xml.append("<counter name=\"").append("memory management mode").append("\"");
		xml.append(" id=\"").append("Auto_mode").append("\"");
		xml.append(">");
		xml.append("</counter>");

		//
		xml.append("</object>");

		xml.append("<object name=\"").append("Background Processes").append("\"");
		xml.append(" id=\"").append("4").append("\"");
		xml.append(">");
		// counter

		xml.append("<counter name=\"").append("Database writer").append("\"");
		xml.append(" id=\"").append("dbwr_num").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Recovery Writer process").append("\"");
		xml.append(" id=\"").append("rvwr_num").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Log writer").append("\"");
		xml.append(" id=\"").append("lgwr_num").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Archiver").append("\"");
		xml.append(" id=\"").append("arch_num").append("\"");
		xml.append(">");
		xml.append("</counter>");
		//
		xml.append("</object>");

		xml.append("<object name=\"").append("DISK STORAGE").append("\"");
		xml.append(" id=\"").append("5").append("\"");
		xml.append(">");
		// counter

		xml.append("<counter name=\"").append("Online data files").append("\"");
		xml.append(" id=\"").append("Online_files").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Online tablespaces").append("\"");
		xml.append(" id=\"").append("Online_tablespaces").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Used space").append("\"");
		xml.append(" id=\"").append("pages_used").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("The percentage of used space").append("\"");
		xml.append(" id=\"").append("pages_percent_used").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("The size of the flashback recovery area").append("\"");
		xml.append(" id=\"").append("flash_pages_all").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("flashback recovery area in use").append("\"");
		xml.append(" id=\"").append("flash_used").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("The percent of flashback recovery area in use").append("\"");
		xml.append(" id=\"").append("flash_percent_used").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Redo log groups").append("\"");
		xml.append(" id=\"").append("group_count").append("\"");
		xml.append(">");
		xml.append("</counter>");
		
		xml.append("<counter name=\"").append("Average size of Redo logs").append("\"");
		xml.append(" id=\"").append("log_average_size").append("\"");
		xml.append(">");
		xml.append("</counter>");
		
		xml.append("<counter name=\"").append("The percentage of the not archived log(s)").append("\"");
		xml.append(" id=\"").append("log_percent_active").append("\"");
		xml.append(">");
		xml.append("</counter>");
		
		xml.append("<counter name=\"").append("Archive count").append("\"");
		xml.append(" id=\"").append("Archive_count").append("\"");
		xml.append(">");
		xml.append("</counter>");
		
		xml.append("<counter name=\"").append("Archive disks").append("\"");
		xml.append(" id=\"").append("Archive_disks").append("\"");
		xml.append(">");
		xml.append("</counter>");
		
		xml.append("<counter name=\"").append("Total archive space").append("\"");
		xml.append(" id=\"").append("total_archive").append("\"");
		xml.append(">");
		xml.append("</counter>");
		
		xml.append("<counter name=\"").append("Minimum archive space").append("\"");
		xml.append(" id=\"").append("free_archive").append("\"");
		xml.append(">");
		xml.append("</counter>");
		
		xml.append("<counter name=\"").append("The percentage of available disk space for the archive log").append("\"");
		xml.append(" id=\"").append("percent_archive").append("\"");
		xml.append(">");
		xml.append("</counter>");
		//
		xml.append("</object>");
		
		
		xml.append("<object name=\"").append("Database running Rate").append("\"");
		xml.append(" id=\"").append("6").append("\"");
		xml.append(">");
		// counter

		xml.append("<counter name=\"").append("SQL*Net send(KBPs)").append("\"");
		xml.append(" id=\"").append("send_rate").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("SQL*Net received(KBPs)").append("\"");
		xml.append(" id=\"").append("received_rate").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Logical readsPs(blksPs)").append("\"");
		xml.append(" id=\"").append("Logical_reads_rate").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Block changesPs(changesPs)").append("\"");
		xml.append(" id=\"").append("Block_changes_rate").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Redo buffer entriesPs(redo entsPs)").append("\"");
		xml.append(" id=\"").append("Redo_buffer_entries_rate").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Parse requestsPs(Parse reqsPs)").append("\"");
		xml.append(" id=\"").append("Parse_requests_rate").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("SQL execution rate(SQL execPs)").append("\"");
		xml.append(" id=\"").append("SQL_execution_rate").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("Physical readsPs(blksPs)").append("\"");
		xml.append(" id=\"").append("Physical_reads_rate").append("\"");
		xml.append(">");
		xml.append("</counter>");
		
		xml.append("<counter name=\"").append("Physical writesPs(blksPs)").append("\"");
		xml.append(" id=\"").append("Physical_writes_rate").append("\"");
		xml.append(">");
		xml.append("</counter>");
		
		xml.append("<counter name=\"").append("Flashback IPO(IOPs)").append("\"");
		xml.append(" id=\"").append("Flashback_IO_rate").append("\"");
		xml.append(">");
		xml.append("</counter>");
		
		xml.append("<counter name=\"").append("Redo writesPs(blksPs)").append("\"");
		xml.append(" id=\"").append("Redo_writes_rate").append("\"");
		xml.append(">");
		xml.append("</counter>");
		
		xml.append("<counter name=\"").append("Archiving rate").append("\"");
		xml.append(" id=\"").append("Archiving_rate").append("\"");
		xml.append(">");
		xml.append("</counter>");
		//
		xml.append("</object>");
		
		xml.append("</object>");
		xml.append("</browse_data>");
		Document document = new Document();
		document.loadXML(xml.toString());
		Element root = (Element) document.getFirstChild();
		buildTreemap(resp, root, "");
		return resp;
	}
	public void buildTreemap(Map<String, Object> res_map, Element node,
			String parentName) {
		if (node == null || !node.hasChildNodes()) {
			return;
		}
		NodeList tnodelist = node.getChildNodes();
		try {
			for (int j = 0; j < tnodelist.getLength(); j++) {
				Element tempNode = (Element) tnodelist.item(j);
				String type = tempNode.getTagName().toLowerCase();
				String id = "";
				id = tempNode.getAttribute("id");
				String name = tempNode.getAttribute("name").replace("/", "-");
				if (parentName == "") {
					if (!id.isEmpty()) {
						if (!res_map.containsKey(id)) {
							if (type.equals("counter"))
								res_map.put(id, name);
							// res_map.put(name1, name);
							else
								res_map.put(id, name);
						}

					}
				} else {

					if (!id.isEmpty()) {
						if (!res_map.containsKey(id)) {
							if (type.equals("counter"))
								// res_map.put(parentName1+name1,parentName+name);
								res_map.put(id, parentName + name);
							else
								res_map.put(id, parentName + name);
						}
					}
				}
				buildTreemap(res_map, tempNode, parentName + name + "/");

			}
		} catch (Exception r) {
			System.out.println(r);
		}

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
			ArrayList<String> R = new ArrayList<String>();
			if (lastError.length() > 0) {
				R.add(error);
				this.sendResponse3("error", R);
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
