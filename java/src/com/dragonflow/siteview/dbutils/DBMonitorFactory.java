package com.dragonflow.siteview.dbutils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.wltea.expression.ExpressionEvaluator;
import org.wltea.expression.ExpressionExecutor;
import org.wltea.expression.ExpressionToken;
import org.wltea.expression.ExpressionToken.ETokenType;
import org.wltea.expression.datameta.Variable;


public class DBMonitorFactory {
	private static Map<String,DBMonitor> map = new ConcurrentHashMap<String,DBMonitor>();
	public static DBMonitor getDBMonitor(String dbtype ,Map<String,String> param) throws Exception{
		int hashCode = param.hashCode();
		String key = dbtype + "-" + hashCode;
		DBMonitor monitor = map.get(key);
		if (monitor == null){
			if (DBMonitor.DBTYPE_ORACLE.equals(dbtype)){
				monitor = new OracleMonitor();
			}else if (DBMonitor.DBTYPE_INFORMIX.equals(dbtype)){
				monitor = new InformixMonitor();
			}else if (DBMonitor.DBTYPE_MYSQL.equals(dbtype)){
				monitor = new MysqlMonitor();
			}
			if (monitor !=null){
				monitor.setParameters(param);
				map.put(key, monitor);
			}
		}
		return monitor;
	}

	public static String getDBMonitorValue(String dbtype ,Map<String,String> param,String performance) throws Exception{

		String expression = Configure.getExpression(dbtype, performance);
		if (expression != null){
			//表达式存在的时候，计算表达式
			ExpressionExecutor ee = new ExpressionExecutor();
			List<ExpressionToken> l = ee.analyze(expression);
			List<Variable> var = new ArrayList<Variable>();
			for (ExpressionToken token : l){
				if (ETokenType.ETOKEN_TYPE_VARIABLE.equals(token.getTokenType())){
					String varName = token.getVariable().getVariableName();
					if (varName == null) continue;
					String retval = getDBMonitorValue(dbtype ,param,varName);
					if (retval == null) continue;
					var.add(Variable.createVariable(varName, Double.parseDouble(retval)));
				}
			}
			return "" + ExpressionEvaluator.evaluate(expression,var);
		}
		//普通取值
		DBMonitor monitor = getDBMonitor(dbtype ,param);
		return monitor.getValue(performance);
	}
	
}
