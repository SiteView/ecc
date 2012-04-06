package com.dragonflow.siteview.dbutils;

import java.util.Properties;

public class Configure {
	
	public static String getExpression(String dbtype,String performance) throws Exception
	{
		return getConfigureValue(dbtype , "Expression." + performance);
	}
	public static String getPerformanceSQL(String dbtype,String performance) throws Exception
	{
		return getConfigureValue(dbtype , "SQL." + performance);
	}
	public static String getPerformanceName(String dbtype,String performance) throws Exception
	{
		return getConfigureValue(dbtype , "NAME." + performance);
	}

	public static String getConfigureValue(String dbtype,String key) throws Exception
	{
		if (dbtype == null || "".equals(dbtype)) throw new Exception("数据库类型没有设置");
		if (key == null || "".equals(key)) throw new Exception("数据库的参数名称没有设置");
		return getConfigureValue(dbtype + "." + key);
	}
	public static String getConfigureValue(String key) throws Exception
	{
		return (String)getConfigureProperties().get(key);
	}
	public static Properties getProperties(String filename) throws Exception{
		Properties prop = new Properties();
		prop.load(Configure.class.getResourceAsStream(filename));
		return prop;
	}
	public static Properties getConfigureProperties() throws Exception{
		return getProperties("/config.properties");
	}
	
}
