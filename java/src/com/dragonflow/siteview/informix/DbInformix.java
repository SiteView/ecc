package com.dragonflow.siteview.informix;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.HashMap;

import org.apache.commons.dbcp.BasicDataSource;

import com.dragonflow.siteview.websphere.util.JMXObject;

public class DbInformix {

	String dbUser = "";
	String dbUrl = "";
	String dbpass = "";
	String driverName = "";
	int connectionTimeout = 60;
	int queryTimeout = 60;
	BasicDataSource datasource = new BasicDataSource();
	Connection con = null;
	StringBuffer error = new StringBuffer();
	HashMap<String, String> countersMap = null;

	public DbInformix() {
	}

	public DbInformix(String Url, String User, String PassWord, String driverName) {
		this.dbUrl = Url;
		this.dbUser = User;
		this.dbpass = PassWord;
		this.driverName = driverName;
		countersMap = new HashMap<String, String>();
	}

	// 连接
	public Connection getConnection() {
		datasource.setUrl(this.dbUrl);
		datasource.setDriverClassName(this.driverName);
		datasource.setUsername(this.dbUser);
		datasource.setPassword(this.dbpass);
		datasource.setMaxWait(this.connectionTimeout);
		datasource.setMaxActive(2);

		try {
			return datasource.getConnection();
		} catch (SQLException e) {
			this.error.append(e.getMessage());
			// TODO Auto-generated catch block
//			System.out.println(e.getMessage());
		}
		return null;

	}

	// 读写性能监测
	// reads 读操作数 pagesread 读页数 writes写操作数 pageswritten 写页数
	public void getChunkIO() {
		con = getConnection();
		String strsql = "select * from syschkio";
		PreparedStatement statement = null;
		ResultSet results = null;
		long reads = 0;// 读次数
		long pagesread = 0;// 读页数
		long writes = 0;// 写次数
		long pageswritten = 0;// 写页数

		long Totalreads = 0;// 读次数
		long Totalpagesread = 0;// 读页数
		long Totalwrites = 0;// 写次数
		long Totalpageswritten = 0;// 写页数
		if (con == null) {
			error.append("database connection fail!");
		} else {
			try {
				statement = con.prepareStatement(strsql);
				statement.setQueryTimeout(this.queryTimeout);
				results = statement.executeQuery();
				// 1. set columns of the queried result.
				while (results.next()) {
					Totalreads += Integer.parseInt(results.getString("reads"));
					Totalpagesread += Integer.parseInt(results
							.getString("pagesread"));
					Totalwrites += Integer
							.parseInt(results.getString("writes"));
					Totalpageswritten += Integer.parseInt(results
							.getString("pageswritten"));

				}
				reads = Totalreads;// 读次数
				pagesread = Totalpagesread;// 读页数
				writes = Totalwrites;// 写次数
				pageswritten = Totalpageswritten;// 写页数

				//
				this.countersMap.put("reads", reads + "");
				this.countersMap.put("pagesread", pagesread + "");
				this.countersMap.put("writes", writes + "");
				this.countersMap.put("pageswritten", pageswritten + "");

				results.close();
				con.close();

			} catch (SQLException e) {
				// TODO Auto-generated catch block
				error.append(e.getMessage());
				e.printStackTrace();
			}
		}

	}

	// 常规会话统计信息
	public void getSession() {
		con = getConnection();
		String strsql = "select * from syssessions";
		PreparedStatement statement = null;
		ResultSet results = null;
		// long sid;//对话号
		// String username = "";//用户名
		// long uid;//用户unix号
		// long pid;//用户进程号
		// String hostname;//主机名
		// String tty;//tty端口
		// long connected = 0;//用户连接时间
		// String feprogram;//程序名
		// long pooladdr = 0;//专有对话池指针
		//
		// long is_wlatch = 0;//标志1=YES，0=NO，等待锁存
		// long is_wlock = 0;//标志1=YES，0=NO，等待锁
		// long is_wbuff = 0;//标志1=YES，0=NO，等待缓冲区
		// long is_wckpt = 0;//标志1=YES，0=NO，等待校验点
		// long is_wlogbuf = 0;//标志1=YES，0=NO，等待日志缓冲区
		// long is_wtrans = 0;//标志1=YES，0=NO，等待事务
		// long is_monitor = 0;//标志1=YES，0=NO，监视进程
		// long is_incrit = 0;//标志1=YES，0=NO，在关键段中

		long state = 0;// 标志

		// 数据统计
		int nPerNormal = 0;// 通畅率
		int nPerWait = 0;// 等待率

		int nconnect = 0;// 连接数
		int nwlatch = 0;// 等待锁存数
		int nwlock = 0;// 等待锁数
		int nwbuff = 0;// 等待缓冲区数
		int nwckpt = 0;// 等待校验点数
		int nwlogbuf = 0;// 等待日志缓冲区数
		int nwtrans = 0;// 等待事务数
		int nmonitor = 0;// 监视进程数
		int nincrit = 0;// 在关键段中数
		if (con == null) {
			error.append("database connection fail!");
		} else {
			try {
				statement = con.prepareStatement(strsql);
				statement.setQueryTimeout(this.queryTimeout);
				results = statement.executeQuery();
				// 1. set columns of the queried result.
				while (results.next()) {
					// 统计数
					// 等待锁存数
					if (results.getString("is_wlatch").equals("1"))
						nwlatch++;
					// 等待锁数
					if (results.getString("is_wlock").equals("1"))
						nwlock++;
					// 等待缓冲区
					if (results.getString("is_wbuff").equals("1"))
						nwbuff++;
					// 等待校验点
					if (results.getString("is_wckpt").equals("1"))
						nwckpt++;
					// 等待日志缓冲区
					if (results.getString("is_wlogbuf").equals("1"))
						nwlogbuf++;
					// 等待事务
					if (results.getString("is_wtrans").equals("1"))
						nwtrans++;
					// 监视进程
					if (results.getString("is_monitor").equals("1"))
						nmonitor++;
					// 在关键段中
					if (results.getString("is_incrit").equals("1"))
						nincrit++;

					// 连接数
					nconnect++;

				}
				// 计算通畅率和等待率
				if (nconnect > 0) {
					// 等待率
					nPerWait = (nwlatch + nwlock + nwbuff + nwckpt + nwlogbuf + nwtrans);
					nPerWait = (nPerWait * 100) / nconnect;

					// 通畅率
					nPerNormal = 100 - nPerWait;
				} else {
					nPerWait = 0;
					nPerNormal = 100;
				}

				//
				this.countersMap.put("nwlatch", nwlatch + "");
				this.countersMap.put("nwlock", nwlock + "");
				this.countersMap.put("nwbuff", nwbuff + "");
				this.countersMap.put("nwckpt", nwckpt + "");
				this.countersMap.put("nwlogbuf", nwlogbuf + "");
				this.countersMap.put("nwtrans", nwtrans + "");
				this.countersMap.put("nmonitor", nmonitor + "");
				this.countersMap.put("nincrit", nincrit + "");
				this.countersMap.put("nconnect", nconnect + "");
				this.countersMap.put("nPerWait", nPerWait + "");
				this.countersMap.put("nPerNormal", nPerNormal + "");

				results.close();
				con.close();

			} catch (SQLException e) {
				// TODO Auto-generated catch block
				error.append(e.getMessage());
				e.printStackTrace();
			}
		}

	}

	// 锁情况(监控锁)

	public void getLock() {
		con = getConnection();
		String strsql = "select * from syslocks";
		PreparedStatement statement = null;
		ResultSet results = null;
		// 总锁数
		int nLock = 0;
		// 死锁数
		int nDeadLock = 0;
		// 死锁率
		int nPerDeadLock = 0;

		int B_Lock = 0;// ——————>byte lock(字节锁)
		int IS_Lock = 0;// ——————>intent shared lock(意图共享锁)
		int S_Lock = 0;// ——————>shared lock(共享锁)
		int XS_Lock = 0;// ——————>repeatable read shared key(可重复读共享锁)

		int U_Lock = 0;// ——————>update lock(更新锁)

		int IX_Lock = 0;// ——————>intent exclusive lock(意图独占锁)
		int SIX_Lock = 0;// ——————>shared intent exclusive(共享意图独占锁)
		int X_Lock = 0;// ——————>exclusive lock(独占锁)
		int XR_Lock = 0;// ——————>repreatable read exclusive(可重复读独占锁)

		String dbsname = "";// 数据库
		String tabname = "";// 表名
		long rowidlk = 0;// 索引关键字锁的行号
		long keynum = 0;// 索引关键字锁的关键字号
		String type = "";// 锁类型
		long owner = 0;// 锁拥有者对话ID
		String waiter = "";// 第一个等待者对话ID
		if (con == null) {
			error.append("database connection fail!");
		} else {
			try {
				statement = con.prepareStatement(strsql);
				statement.setQueryTimeout(this.queryTimeout);
				results = statement.executeQuery();
				// 1. set columns of the queried result.
				while (results.next()) {
					nLock++;

					// 死锁数
					waiter = results.getString("waiter");
					if (waiter != "")
						nDeadLock++;

					// 共享锁
					type = results.getString("type");
					type.replace(" ", "");
					if (type == "B")
						B_Lock++;
					else if (type == "IS")
						IS_Lock++;
					else if (type == "S")
						S_Lock++;
					else if (type == "XS")
						XS_Lock++;

					// 更新锁
					else if (type == "U")
						U_Lock++;

					// 独占锁
					else if (type == "X")
						X_Lock++;
					else if (type == "IX")
						IX_Lock++;
					else if (type == "SIX")
						SIX_Lock++;
					else if (type == "XR")
						XR_Lock++;

				}

				// 各种共享锁和独占锁求和
				B_Lock = B_Lock + IS_Lock + S_Lock + XS_Lock;
				X_Lock = X_Lock + IX_Lock + SIX_Lock + XR_Lock;

				// 死锁率
				if (nLock > 0)
					nPerDeadLock = (100 * nDeadLock) / nLock;
				//
				this.countersMap.put("nPerDeadLock", nPerDeadLock + "");
				this.countersMap.put("nLock", nLock + "");
				this.countersMap.put("B_Lock", B_Lock + "");
				this.countersMap.put("U_Lock", U_Lock + "");
				this.countersMap.put("X_Lock", X_Lock + "");
				this.countersMap.put("nDeadLock", nDeadLock + "");

				results.close();
				con.close();

			} catch (SQLException e) {
				// TODO Auto-generated catch block
				error.append(e.getMessage());
				e.printStackTrace();
			}
		}

	}

	// 长事务等待

	public void GetWait() {
		con = getConnection();
		String strsql = "select * from sysseswts";
		PreparedStatement statement = null;
		ResultSet results = null;
		long sid = 0;// 对话ID
		String reason = "";// 对话ID
		long numwaits = 0;// 等待原因说明
		long cumtime = 0;// 这个原因的等待累计时间
		long maxtime = 0;// 这个原因的等待最长时间
		if (con == null) {
			error.append("database connection fail!");
		} else {
			try {
				statement = con.prepareStatement(strsql);
				statement.setQueryTimeout(this.queryTimeout);
				results = statement.executeQuery();
				// 1. set columns of the queried result.
				while (results.next()) {
					sid = Integer.parseInt(results.getString("sid"));
					reason = results.getString("reason");
					numwaits = Integer.parseInt(results.getString("numwaits"));
					cumtime = Integer.parseInt(results.getString("cumtime"));
					maxtime = Integer.parseInt(results.getString("maxtime"));

				}

				//
				this.countersMap.put("sid", sid + "");
				this.countersMap.put("reason", reason);
				this.countersMap.put("numwaits", numwaits + "");
				this.countersMap.put("cumtime", cumtime + "");
				this.countersMap.put("maxtime", maxtime + "");
				results.close();
				con.close();

			} catch (SQLException e) {
				// TODO Auto-generated catch block
				error.append(e.getMessage());
				e.printStackTrace();
			}
		}

	}

	// 数据库空间检测

	public void GetSpace() {

		con = getConnection();
		String strsql = "select count(number), sum(used) from syslogs";
		// syschunks chunks总数 chunks总大小 chunks剩余总大
		// strSql.Format("select count(chknum),sum(chksize),sum(nfree) from syschunks;");
		// sysextents 数据库表总数 页总数
		// strSql.Format("select count(tabname),sum(size) from sysextents;");
		PreparedStatement statement = null;
		ResultSet results = null;
		long nPerDbspace = 0;
		long nLogFileSum = 0;
		long nLogFileSize = 0;
		long nChunks = 0;
		long nChunkTotalSize = 0;
		long nChunkFreeSize = 0;
		long nTabSum = 0;
		long nTabPageSum = 0;

		if (con == null) {
			error.append("database connection fail!");
		} else {
			try {
				statement = con.prepareStatement(strsql);
				statement.setQueryTimeout(this.queryTimeout);
				results = statement.executeQuery();
				// 1. set columns of the queried result.
				while (results.next()) {
					nLogFileSum = Integer.parseInt(results.getString(1));
					nLogFileSize = Integer.parseInt(results.getString(2));

				}
				strsql = "select count(chknum),sum(chksize),sum(nfree) from syschunks";

				statement = con.prepareStatement(strsql);
				statement.setQueryTimeout(this.queryTimeout);
				results = statement.executeQuery();
				while (results.next()) {
					nChunks = Integer.parseInt(results.getString(1));
					nChunkTotalSize = Integer.parseInt(results.getString(2));
					nChunkFreeSize = Integer.parseInt(results.getString(3));

				}
				strsql = "select count(tabname),sum(size) from sysextents";

				statement = con.prepareStatement(strsql);
				statement.setQueryTimeout(this.queryTimeout);
				results = statement.executeQuery();
				while (results.next()) {
					nTabSum = Integer.parseInt(results.getString(1));
					nTabPageSum = Integer.parseInt(results.getString(2));

				}
				if (nChunkTotalSize > 0) {
					nPerDbspace = (nChunkTotalSize - nChunkFreeSize) * 100;
					nPerDbspace = nPerDbspace / nChunkTotalSize;
				}

				this.countersMap.put("nPerDbspace", nPerDbspace + "");
				this.countersMap.put("nLogFileSum", nLogFileSum + "");
				this.countersMap.put("nLogFileSize", nLogFileSize + "");
				this.countersMap.put("nChunks", nChunks + "");
				this.countersMap.put("nChunkTotalSize", nChunkTotalSize + "");
				this.countersMap.put("nChunkFreeSize", nChunkFreeSize + "");
				this.countersMap.put("nTabSum", nTabSum + "");
				this.countersMap.put("nTabPageSum", nTabPageSum + "");
				results.close();
				con.close();

			} catch (SQLException e) {
				// TODO Auto-generated catch block
				error.append(e.getMessage());
				e.printStackTrace();
			}
		}

	}
	
	//日志详细信息
	public void GetSysLog() {

		con = getConnection();
		String strsql = "select number,uniqid,is_used, is_current,is_backed_up,is_new,is_archived,is_temp from syslogs";

		PreparedStatement statement = null;
		ResultSet results = null;
		int number = 0;//日志文件号
		int uniqid = 0;//日志文件唯一ID
		int is_used = 0;//已用状态
		int is_current = 0;//当前使用
		int is_backed_up = 0;//备份状态
		int is_new = 0;//新建状态
		int is_archived = 0;//存档状态
		int is_temp = 0;//临时状态

		if (con == null) {
			error.append("database connection fail!");
		} else {
			try {
				statement = con.prepareStatement(strsql);
				statement.setQueryTimeout(this.queryTimeout);
				results = statement.executeQuery();
				String n="";
				// 1. set columns of the queried result.
				while (results.next()) {
					number = Integer.parseInt(results.getString(1));
					uniqid = Integer.parseInt(results.getString(2));
					is_used=Integer.parseInt(results.getString(3));
					is_current=Integer.parseInt(results.getString(4));
					is_backed_up=Integer.parseInt(results.getString(5));
					is_new=Integer.parseInt(results.getString(6));
					is_archived=Integer.parseInt(results.getString(7));
					is_temp=Integer.parseInt(results.getString(8));
				    n=results.getString(1);
				    boolean tmp=false;
					this.countersMap.put("number", number + "");
					this.countersMap.put("uniqid", uniqid + "");
					tmp=is_used==1?true:false;
					this.countersMap.put("is_used of "+n,  tmp+ "");
					tmp=is_current==1?true:false;
					this.countersMap.put("is_current of "+n, tmp + "");
					tmp=is_backed_up==1?true:false;
					this.countersMap.put("is_backed_up of "+n, tmp + "");
					tmp=is_new==1?true:false;
					this.countersMap.put("is_new of "+n, tmp + "");
					tmp=is_archived==1?true:false;
					this.countersMap.put("is_archived of "+n, tmp + "");
					tmp=is_temp==1?true:false;
					this.countersMap.put("is_temp of "+n, tmp + "");
				}
				results.close();
				con.close();

			} catch (SQLException e) {
				// TODO Auto-generated catch block
				error.append(e.getMessage());
				e.printStackTrace();
			}
		}

	}
	
	//表空间详细信息
	public void GetDBSpace() {

		con = getConnection();
		//select name[1,8] dbspace,sum(chksize) Pages_size,sum(chksize) - sum(nfree) Pages_used,sum(nfree) Pages_free, round ((sum(nfree)) / (sum(chksize)) * 100, 2) percent_free from sysdbspaces d, syschunks c where d.dbsnum = c.dbsnum group by 1 order by 1;
		String strsql = "select d.name dbspace,sum(c.chksize) Pages_size,sum(c.chksize) - sum(c.nfree) Pages_used,sum(c.nfree) Pages_free, round ((sum(c.nfree)) / (sum(c.chksize)) * 100, 2) percent_free from sysdbspaces d, syschunks c where d.dbsnum = c.dbsnum group by name order by name;";
		//String strsql = "select name[1,8] dbspace,sum(chksize) Pages_size,sum(chksize) - sum(nfree) Pages_used,sum(nfree) Pages_free, round ((sum(nfree)) / (sum(chksize)) * 100, 2) percent_free from sysdbspaces d, syschunks c where d.dbsnum = c.dbsnum group by 1 order by 1;";
		PreparedStatement statement = null;
		ResultSet results = null;
		String dbspace="";//表空间名称
		String Pages_size = "";//实际大小
		String Pages_used=""; //已用大小
		String Pages_free = "";//剩余大小
		String percent_free = "";//剩余百分率
		

		if (con == null) {
			error.append("database connection fail!");
		} else {
			try {
				statement = con.prepareStatement(strsql);
				statement.setQueryTimeout(this.queryTimeout);
				results = statement.executeQuery();
				String n="";
				// 1. set columns of the queried result.
				while (results.next()) {
					dbspace = results.getString(1).trim();
					Pages_size = results.getString(2);
					Pages_used=results.getString(3);
					Pages_free=results.getString(4);
					percent_free=results.getString(5);
				   
					this.countersMap.put("dbspace",dbspace);
					this.countersMap.put("Pages_size of "+dbspace,Pages_size);
					this.countersMap.put("Pages_used of "+dbspace,Pages_used);
					this.countersMap.put("Pages_free of "+dbspace,Pages_free);
					this.countersMap.put("percent_free of "+dbspace,percent_free);
					
				}
				results.close();
				con.close();

			} catch (SQLException e) {
				// TODO Auto-generated catch block
				try {
					con.close();
				} catch (SQLException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
				error.append(e.getMessage());
				e.printStackTrace();
			}
		}

	}
 
	public HashMap<String, String> getUpdate() {
		this.countersMap.clear();
		
		
			this.getChunkIO();
			if (this.error.length()==0)
			{
			this.getLock();
			this.getSession();
			this.GetSpace();
			this.GetWait();
			this.GetSysLog();
			this.GetDBSpace();
			}
		
		if (this.error.length()>0)
		{
//			System.out.println(this.error.toString());
			this.countersMap.put("error", error.toString());
		}
		return this.countersMap;
	}

	// reads 读操作数 pagesread 读页数 writes写操作数 pageswritten 写页数
	//
	public StringBuffer xmlBrowseData(String serverName) {
		StringBuffer xml = new StringBuffer();
		xml.append("<browse_data>");
		xml.append("<object name=\"").append(serverName).append("\"");
		xml.append(" id=\"").append("0").append("\"");
		xml.append(">");

		xml.append("<object name=\"").append("Informix读写性能").append("\"");
		xml.append(" id=\"").append("1").append("\"");
		xml.append(">");
		// counter

		xml.append("<counter name=\"").append("读操作数").append("\"");
		xml.append(" id=\"").append("reads").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("读页数").append("\"");
		xml.append(" id=\"").append("pagesread").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("写操作数").append("\"");
		xml.append(" id=\"").append("writes").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("写页数").append("\"");
		xml.append(" id=\"").append("pageswritten").append("\"");
		xml.append(">");
		xml.append("</counter>");
		//
		xml.append("</object>");

		xml.append("<object name=\"").append("Informix连接情况").append("\"");
		xml.append(" id=\"").append("2").append("\"");
		xml.append(">");
		// counter

		xml.append("<counter name=\"").append("通畅率").append("\"");
		xml.append(" id=\"").append("nPerNormal").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("等待率").append("\"");
		xml.append(" id=\"").append("nPerWait").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("连接数").append("\"");
		xml.append(" id=\"").append("nconnect").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("等待锁存数").append("\"");
		xml.append(" id=\"").append("nwlatch").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("等待锁数").append("\"");
		xml.append(" id=\"").append("nwlock").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("等待缓冲区数").append("\"");
		xml.append(" id=\"").append("nwbuff").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("等待校验点数").append("\"");
		xml.append(" id=\"").append("nwckpt").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("等待日志缓冲区数").append("\"");
		xml.append(" id=\"").append("nwlogbuf").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("等待事务数").append("\"");
		xml.append(" id=\"").append("nwtrans").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("监视进程数").append("\"");
		xml.append(" id=\"").append("nmonitor").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("在关键段中数").append("\"");
		xml.append(" id=\"").append("nincrit").append("\"");
		xml.append(">");
		xml.append("</counter>");

		//
		xml.append("</object>");

		xml.append("<object name=\"").append("Informix锁情况").append("\"");
		xml.append(" id=\"").append("3").append("\"");
		xml.append(">");
		// counter
		xml.append("<counter name=\"").append("总锁数").append("\"");
		xml.append(" id=\"").append("nLock").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("死锁数").append("\"");
		xml.append(" id=\"").append("nDeadLock").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("死锁率").append("\"");
		xml.append(" id=\"").append("nPerDeadLock").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("byte lock(字节锁)").append("\"");
		xml.append(" id=\"").append("B_Lock").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("update lock(更新锁)").append("\"");
		xml.append(" id=\"").append("U_Lock").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("exclusive lock(独占锁)").append(
				"\"");
		xml.append(" id=\"").append("X_Lock").append("\"");
		xml.append(">");
		xml.append("</counter>");

		//
		xml.append("</object>");

		xml.append("<object name=\"").append("Informix长事务等待").append("\"");
		xml.append(" id=\"").append("4").append("\"");
		xml.append(">");
		// counter

		xml.append("<counter name=\"").append("会话ID").append("\"");
		xml.append(" id=\"").append("sid").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("原因").append("\"");
		xml.append(" id=\"").append("reason").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("等待事务数").append("\"");
		xml.append(" id=\"").append("numwaits").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("原因的等待累计时间").append("\"");
		xml.append(" id=\"").append("cumtime").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("原因的等待最长时间").append("\"");
		xml.append(" id=\"").append("maxtime").append("\"");
		xml.append(">");
		xml.append("</counter>");
		//
		xml.append("</object>");

		xml.append("<object name=\"").append("Informix数据库空间").append("\"");
		xml.append(" id=\"").append("5").append("\"");
		xml.append(">");
		// counter

		xml.append("<counter name=\"").append("逻辑日志总占用空间").append("\"");
		xml.append(" id=\"").append("nPerDbspace").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("文件数量").append("\"");
		xml.append(" id=\"").append("nLogFileSum").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("文件大小").append("\"");
		xml.append(" id=\"").append("nLogFileSize").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("chunks总数").append("\"");
		xml.append(" id=\"").append("nChunks").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("chunks总大小").append("\"");
		xml.append(" id=\"").append("nChunkTotalSize").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("数据库表总数").append("\"");
		xml.append(" id=\"").append("nTabSum").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("页总数").append("\"");
		xml.append(" id=\"").append("nTabPageSum").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("chunks剩余总大小").append("\"");
		xml.append(" id=\"").append("nChunkFreeSize").append("\"");
		xml.append(">");
		xml.append("</counter>");
		//
		xml.append("</object>");
		
		//============日志详细信息=========
		xml.append("<object name=\"").append("Informix日志详细信息").append("\"");
		xml.append(" id=\"").append("6").append("\"");
		xml.append(">");
		// counter
		con = getConnection();
		String strsql = "select number,uniqid,is_used, is_current,is_backed_up,is_new,is_archived,is_temp from syslogs";
		PreparedStatement statement = null;
		ResultSet results = null;
		int number = 0;//日志文件号
		int uniqid = 0;//日志文件唯一ID
		int is_used = 0;//已用状态
		int is_current = 0;//当前使用
		int is_backed_up = 0;//备份状态
		int is_new = 0;//新建状态
		int is_archived = 0;//存档状态
		int is_temp = 0;//临时状态

		if (con == null) {
			error.append("database connection fail!");
		} else {
			try {
				statement = con.prepareStatement(strsql);
				statement.setQueryTimeout(this.queryTimeout);
				results = statement.executeQuery();
				String n="";
				// 1. set columns of the queried result.
				while (results.next()) {
					number = Integer.parseInt(results.getString(1));
					uniqid = Integer.parseInt(results.getString(2));
					is_used=Integer.parseInt(results.getString(3));
					is_current=Integer.parseInt(results.getString(4));
					is_backed_up=Integer.parseInt(results.getString(5));
					is_new=Integer.parseInt(results.getString(6));
					is_archived=Integer.parseInt(results.getString(7));
					is_temp=Integer.parseInt(results.getString(8));
				    n=results.getString(1);
				    xml.append("<object name=\"").append(number+"").append("\"");
					xml.append(" id=\"").append(number+""+uniqid).append("\"");
					xml.append(">");
					xml.append("<counter name=\"").append("已用状态").append("\"");
					xml.append(" id=\"").append("is_used of "+n).append("\"");
					xml.append(">");
					xml.append("</counter>");
					xml.append("<counter name=\"").append("当前使用").append("\"");
					xml.append(" id=\"").append("is_current of "+n).append("\"");
					xml.append(">");
					xml.append("</counter>");
					xml.append("<counter name=\"").append("备份状态").append("\"");
					xml.append(" id=\"").append("is_backed_up of "+n).append("\"");
					xml.append(">");
					xml.append("</counter>");
					xml.append("<counter name=\"").append("新建状态").append("\"");
					xml.append(" id=\"").append("is_new of "+n).append("\"");
					xml.append(">");
					xml.append("</counter>");
					xml.append("<counter name=\"").append("存档状态").append("\"");
					xml.append(" id=\"").append("is_archived of "+n).append("\"");
					xml.append(">");
					xml.append("</counter>");
					xml.append("<counter name=\"").append("临时状态").append("\"");
					xml.append(" id=\"").append("is_temp of "+n).append("\"");
					xml.append(">");
					xml.append("</counter>");
					xml.append("</object>");
				}
				results.close();
				con.close();

			} catch (SQLException e) {
				// TODO Auto-generated catch block
				error.append(e.getMessage());
				e.printStackTrace();
			}
		}


		//
		xml.append("</object>");
		//============日志详细信息=========

		//============表空间信息================
		xml.append("<object name=\"").append("Informix表空间信息").append("\"");
		xml.append(" id=\"").append("7").append("\"");
		xml.append(">");
		// counter
		con = getConnection();
		//select name[1,8] dbspace,sum(chksize) Pages_size,sum(chksize) - sum(nfree) Pages_used,sum(nfree) Pages_free, round ((sum(nfree)) / (sum(chksize)) * 100, 2) percent_free from sysdbspaces d, syschunks c where d.dbsnum = c.dbsnum group by 1 order by 1;
		 strsql = "select d.name dbspace,sum(c.chksize) Pages_size,sum(c.chksize) - sum(c.nfree) Pages_used,sum(c.nfree) Pages_free, round ((sum(c.nfree)) / (sum(c.chksize)) * 100, 2) percent_free from sysdbspaces d, syschunks c where d.dbsnum = c.dbsnum group by name order by name;";

		 statement = null;
		 results = null;
		String dbspace="";//表空间名称
		String Pages_size = "";//实际大小
		String Pages_used=""; //已用大小
		String Pages_free = "";//剩余大小
		String percent_free = "";//剩余百分率
		

		if (con == null) {
			error.append("database connection fail!");
		} else {
			try {
				statement = con.prepareStatement(strsql);
				statement.setQueryTimeout(this.queryTimeout);
				results = statement.executeQuery();
				String n="";
				// 1. set columns of the queried result.
				while (results.next()) {
					dbspace = results.getString(1).trim();
					Pages_size = results.getString(2);
					Pages_used=results.getString(3);
					Pages_free=results.getString(4);
					percent_free=results.getString(5);
				   
					xml.append("<object name=\"").append(dbspace).append("\"");
					xml.append(" id=\"").append(dbspace).append("\"");
					xml.append(">");
					xml.append("<counter name=\"").append("实际大小").append("\"");
					xml.append(" id=\"").append("Pages_size of "+dbspace).append("\"");
					xml.append(">");
					xml.append("</counter>");
					xml.append("<counter name=\"").append("已用大小").append("\"");
					xml.append(" id=\"").append("Pages_used of "+dbspace).append("\"");
					xml.append(">");
					xml.append("</counter>");
					xml.append("<counter name=\"").append("剩余大小").append("\"");
					xml.append(" id=\"").append("Pages_free of "+dbspace).append("\"");
					xml.append(">");
					xml.append("</counter>");
					xml.append("<counter name=\"").append("剩余百分率").append("\"");
					xml.append(" id=\"").append("percent_free of "+dbspace).append("\"");
					xml.append(">");
					xml.append("</counter>");
					xml.append("</object>");
					
				}
				results.close();
				con.close();

			} catch (SQLException e) {
				// TODO Auto-generated catch block
				try {
					con.close();
				} catch (SQLException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
				error.append(e.getMessage());
				e.printStackTrace();
			}
		}



		//
		xml.append("</object>");
		//============表空间信息=========
		xml.append("</object>");
		xml.append("</browse_data>");
		return xml;

	}

}
