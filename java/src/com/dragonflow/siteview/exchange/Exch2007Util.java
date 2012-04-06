package com.dragonflow.siteview.exchange;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Exch2007Util {
	
//	 private static final Log logger;

	   public static String[] getCounterTypeAndNameByID(String id)
	   {
	     String counterID = id.substring(id.indexOf(32, 0) + 1);
	     return getCounterTypeAndName(counterID);
	   }
	 
	   public static String[] getCounterTypeAndName(String counterName)
	   {
	     String[] parts = counterName.split("/");
	     if (parts.length < 2) {
//	       logger.error("Counter doesn't contain two parts: Type and Name: " + counterName);
	       return null;
	     }
	     return parts;
	   }
	 
	   public static List<AbstractExchMngtShellData> getCmdletDataObjectResults(int timeout, ExchMngtShellCmdlet cmdlet, MultiValueHashMap<String, String> countersNames, Map<String, String> propsMapper, Map<String, Map<String, String>> results, Map<String, String> errResults)
	    // throws SiteScopeException
	   {
	     return ExchMngtShellProcess.create(timeout).runCommand(cmdlet, countersNames, propsMapper, results, errResults);
	   }
	 
	   public static List<AbstractExchMngtShellData> getCmdletDataObjectResults(int timeout, ExchMngtShellCmdlet cmdlet, Map<String, String> propsMapper)
	     //throws SiteScopeException
	   {
	     return getCmdletDataObjectResults(timeout, cmdlet, ExchSrvRolesData.getCountersNamesMapper(), propsMapper, new HashMap(), new HashMap());
	   }
	 
	   public static String getCounterName(String[] counter)
	   {
	     if (counter.length == 2)
	       return counter[1];
	     return buildCounterName(counter);
	   }
	 
	   private static String buildCounterName(String[] counter)
	   {
	     assert (counter.length > 2) : "counter length must be greater than 2";
	     StringBuilder name = new StringBuilder();
	     for (int i = 1; i < counter.length; ++i) {
	       name.append(counter[i]);
	       if (i != counter.length - 1) {
	         name.append("/");
	       }
	     }
	     return name.toString();
	   }
	 
	   static
	   {
//	     logger = LogFactory.getEasyLog(Exch2007Util.class);
	   }

}
