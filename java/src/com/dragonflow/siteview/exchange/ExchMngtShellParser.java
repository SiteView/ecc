package com.dragonflow.siteview.exchange;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Iterator;;

public class ExchMngtShellParser {
	 

	  private static Map<String, String> cmdletsMapper = new HashMap();

	  private static void initCmdletMapper()
	   {
	     cmdletsMapper.put("Get-ExchangeServer", "--- GET-EXCHANGESERVER_OUTPUT ---");
	     cmdletsMapper.put("MAPI Connectivity", "--- TEST-MAPICONNECTIVITY_OUTPUT ---");
	     cmdletsMapper.put("Mail Flow", "--- TEST-MAILFLOW_OUTPUT ---");
	     cmdletsMapper.put("Exchange Search", "--- TEST-EXCHANGESEARCH_OUTPUT ---");
	     cmdletsMapper.put("OWA Connectivity", "--- TEST-OWACONNECTIVITY_OUTPUT ---");
	     cmdletsMapper.put("Web Services Connectivity", "--- TEST-WEBSERVICESCONNECTIVITY_OUTPUT ---");
	   }
	 
	   public static List<AbstractExchMngtShellData> parseSingleCommand(ExchMngtShellCmdlet cmdlet, MultiValueHashMap<String, String> countersNamesMapper, List<String> outputResult, Map<String, Map<String, String>> results, Map<String, String> errResults)
	   throws Exception{
	     List dataObjResultList = new LinkedList();
	 
	     Map outputPerCmdletMapper = getOutputMapper(outputResult);
	 
	     if (!(outputPerCmdletMapper.isEmpty()))
	     {
	       Set countersTypesSet = countersNamesMapper.keySet();
	       Iterator ite=countersTypesSet.iterator();

	       while(ite.hasNext()){
	    	   String cType=(String)ite.next();
	         Collection countersNames = (Collection)countersNamesMapper.get(cType);
	         if (countersNames == null) {
	           //logger.error("countersNamesMapper does not contain mapping for this key: " + cType);
//	           errResults.put(cType, SiteScopeResource.getFormattedString(30202L, new String[] { cType }));
	         }
	 
	         String cTitle = (String)cmdletsMapper.get(cType);
	         if (cTitle == null) {
//	           logger.error("cmdletsMapper does not contain mapping for this key: " + cType);
//	           errResults.put(cType, SiteScopeResource.getFormattedString(30202L, new String[] { cType }));
	         }

	         List cmdletOutput = (List)outputPerCmdletMapper.get(cTitle);
	         if (cmdletOutput == null) {
	          // logger.error("outputPerCmdletMapper does not contain mapping for this key: " + cType);
//	           errResults.put(cType, SiteScopeResource.getFormattedString(30202L, new String[] { cType }));
	         }
	 
	         if (hasShellError(cmdletOutput)) {
	           StringBuilder errMsg = new StringBuilder().append("Error occurred while running cmdlet: ").append(cType).append(". The Error:\n");
	           invertListToString(cmdletOutput, errMsg);
	           //logger.error(errMsg.toString());
//	           errResults.put(cType, SiteScopeResource.getFormattedString(30202L, new String[] { cType }));
	         }
	 
	         AbstractExchMngtShellData data = ExchDataFactory.createDataObject(cmdlet, cmdletOutput, new LinkedList(countersNames));
	         dataObjResultList.add(data);
	         Map countersValuesMapper = data.getCountersValuesMapper();
	         results.put(cType, countersValuesMapper);
	       }
	       return dataObjResultList;
	     }
	 
	     StringBuilder outputBuilder = new StringBuilder();
	     if (hasShellError(outputResult))
	       outputBuilder.append("Error occurred while running the script: ").append(cmdlet.getScriptPath()).append("\nThe Error as returned from Exchange Management Shell is:\n");
	     else {
	       outputBuilder.append("Error occurred while parsing the cmdlet output:\n");
	     }
	     invertListToString(outputResult, outputBuilder);
	     throw new Exception(cmdlet.getScriptPath());
	     //logger.error(outputBuilder.toString());

	    //throw new SiteScopeOperationalException(30203L, new String[] { cmdlet.getScriptPath() });
	   }
	 
	   private static void invertListToString(List<String> strList, StringBuilder strBuilder)
	   {
	    for (String outputLine : strList)
	       strBuilder.append(outputLine).append('\n');
	   }
	 
	   private static boolean hasShellError(List<String> cmdletOutput)
	   {
	     for (String line : cmdletOutput) {
	       if ((line.matches("\\+.*<<<<.*")) || (line.matches(".*<<<<.*")) || (line.matches("At\\s+.*:\\d+\\s+.*:\\d+"))) {
//	         if (logger.isDebugEnabled()) {
//	           logger.debug("Error was found in line: " + line);
//	         }
	         return true;
	       }
	    }
	     return false;
	   }
	 
	   private static Map<String, List<String>> getOutputMapper(List<String> result)
	   {
	     Map outputMapper = new HashMap();
	 
	     for (int i = 0; i < result.size(); ++i) {
	       String title = (String)result.get(i);
	       if (title.matches("---\\s+.*\\s+---")) {
	         List cmdletOutput = new LinkedList();

	         for (int j = i + 1; j < result.size(); ++j) {
	           String line = (String)result.get(j);
	 
	           if (line.matches("---\\s+.*\\s+---")) {
	             i = j - 1;
	             break;
	           }
	 
	           if (j == result.size() - 1) {
	             i = j - 1;
	           }
	           cmdletOutput.add(line);
	         }
	 
	         addOutputToMapper(cmdletOutput, outputMapper, title);
	       }
	     }
	 
	     return outputMapper;
	   }
	 
	   private static void addOutputToMapper(List<String> cmdletOutput, Map<String, List<String>> outputMapper, String title) {
	     if (!(cmdletOutput.isEmpty())) {
	       outputMapper.put(title, cmdletOutput);
//	       if (logger.isDebugEnabled()) {
	         StringBuilder outputBuilder = new StringBuilder().append("The Content found for the title [ ").append(title).append(" ] is:\n");
	         invertListToString(cmdletOutput, outputBuilder);
	        // logger.debug(outputBuilder.toString());
//	       }
	     }
//	     } else if (logger.isDebugEnabled()) {
//	       //logger.debug("No content found for title: " + title);
//	     }
	   }
	 
	   public static String getValue(String key, List<String> values)
	   {
	     String regex = "\\s+:\\s+";
	     for (String line : values) {
	       String[] tokens = line.split(regex);
	       if (tokens.length == 2) {
	         String prefix = tokens[0].trim();
	         if (prefix.equalsIgnoreCase(key)) {
//	           if (logger.isDebugEnabled()) {
//	             logger.debug("Getting value for key " + key + ": value = " + tokens[1].trim());
//	           }
	           return tokens[1].trim();
	         }
	       }
	    }
	 
//	     logger.error("No value was found for key: " + key);
	     return null;
	   }
	 
	   public static String getValue(String counterParent, String counterName, List<String> output)
	   {
	     String regex = "\\s+:\\s+";
	 
	     for (int i = 0; i < output.size(); ++i) {
	       String[] tokens = getTokensByDelimiter((String)output.get(i), regex);
	       if (tokens.length == 2) {
	         String suffix = tokens[1].trim();
	         if (!(suffix.equalsIgnoreCase(counterParent)))
	           continue;
	         for (int j = i + 1; j < output.size(); ++j) {
	          tokens = getTokensByDelimiter((String)output.get(j), regex);
	           if (tokens.length == 2) {
	             String prefix = tokens[0].trim();
	             if (prefix.equalsIgnoreCase(counterName)) {
//	               if (logger.isDebugEnabled()) {
//	                 logger.debug("Getting value for key " + counterParent + "/" + counterName + ": value = " + tokens[1].trim());
//	               }
	               return tokens[1].trim();
	             }
	 
	             if (prefix.equalsIgnoreCase("ClientAccessServer")) {
	               break;
	             }
	           }
	         }
	 
	         break;
	       }
	 
	     }
	 
//	     logger.error("No value was found for key: " + counterParent + "/" + counterName);
	     return null;
	   }
	 
	  private static String[] getTokensByDelimiter(String expr, String regexDel) {
	     return expr.split(regexDel);
	   }
	 
	   static
	   {
	     initCmdletMapper();
	   }

}
