package com.dragonflow.siteview.dbutils;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.Map;

import javax.sql.DataSource;

import org.apache.commons.dbcp.BasicDataSource;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;


public abstract class AbstractDBMonitorImpl implements DBMonitor {
	private static final Log log = LogFactory.getLog(AbstractDBMonitorImpl.class);
	
	abstract public String getDbType() ;
	abstract public String getPort() ;
	
	private Map<String, String> param = null;

	public void setParameters(Map<String, String> param) throws Exception {
		this.param = param;
	}
	public Map<String, String> getParameters() throws Exception {
		return this.param;
	}
	public String getValue(String key) throws Exception
	{
		Connection connection = null;
		try {
			connection = getConnection();

			String sql = Configure.getPerformanceSQL(getDbType(), key);
			sql = convertString(sql);
			PreparedStatement statement = connection
					.prepareStatement(sql);
			ResultSet resultSet = statement.executeQuery();

			String retValue = null;

			while (resultSet.next()) {
				retValue = resultSet.getString(Configure.getPerformanceName(getDbType(), key));
				break;
			}
			resultSet.close();
			statement.close();
			return retValue;
		} catch (Exception e) {
			log.error(e.getMessage());
			throw new RuntimeException("���ܣ�" + key ,e);
		} finally {
			if (connection != null && connection.isClosed() == false)
				connection.close();
		}
	}
	
	private DataSource datasource = null;
	public Connection getConnection() throws Exception {
		if (datasource == null){
			String className = Configure.getConfigureValue(getDbType(), CONFIG_DRIVER);
			String url = Configure.getConfigureValue(getDbType(), CONFIG_URL);
			String username = getParameters().get(PARAM_USERNAME);
			String password = getParameters().get(PARAM_PASSWORD);
			url = convertString(url);
			datasource = getDataSource(className,url,username,password);    
		}
		return datasource.getConnection();    
	}
	
	private String convertString(String source) throws Exception{
		String retString = source;
		for(String key : param.keySet()){
			if (key == null) continue;
			String value = param.get(key);
			if (value == null) continue;
			retString = retString.replace(key, value);
		}
		retString = retString.replace("config_port", this.getPort());
		return retString;
	}
	
	private DataSource getDataSource(String className,String connectURI,String username,String password) {
		BasicDataSource ds = new BasicDataSource();
		ds.setDriverClassName(className);
		ds.setUsername(username);
		ds.setPassword(password);
		ds.setUrl(connectURI);
		return ds;
	}

}
