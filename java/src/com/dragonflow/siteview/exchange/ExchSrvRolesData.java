package com.dragonflow.siteview.exchange;

import java.util.LinkedList;
import java.util.List;


public class ExchSrvRolesData extends AbstractExchMngtShellData {
	   //private static final Log logger;
	   private static List<String> countersLabels;
	 
	   public static MultiValueHashMap<String, String> getCountersNamesMapper()
	   {
	     MultiValueHashMap mapper = new MultiValueHashMap();
	    for (String label : countersLabels) {
	       mapper.putValue("Get-ExchangeServer", label);
	     }
	     return mapper;
	  }
	 
	   public void loadData(List<String> pValues, List<String> countersLabels)
	   {
	     super.loadData(pValues, countersLabels);
	   }
	 
	   protected void loadValue(String role, List<String> pValues) {
	     String value = ExchMngtShellParser.getValue(role, pValues);
	 
	     if ((value == null) || ((value.compareToIgnoreCase("False") != 0) && (value.compareToIgnoreCase("True") != 0)))
	     {
	       this.countersValuesMapper.put(role, "False");
	       //logger.error("Invalid input: " + value + ", should be " + "True" + " or " + "False");
	     } else {
	       this.countersValuesMapper.put(role, value);
	     }
	   }
	 
	   private boolean isServerRole(String role)
	   {
	     assert (this.countersValuesMapper.get(role) != null) : "Server Role can not be null. Must be True or False";
	 
	     return (((String)this.countersValuesMapper.get(role)).compareToIgnoreCase("True") == 0);
	   }
	 
	   public boolean isRunnableCmd(List<String> roles)
	   {
	     boolean retVal = true;
	     for (String role : roles) {
	       retVal &= isServerRole(role);
	     }
	     return retVal;
	   }
	 
	   public static List<String> getRoles() {
	     return countersLabels;
	   }
	 
	   static
	   {
	     //logger = LogFactory.getEasyLog(ExchSrvRolesData.class);
	 
	     countersLabels = new LinkedList();
	     countersLabels.add("IsClientAccessServer");
	     countersLabels.add("IsEdgeServer");
	     countersLabels.add("IsHubTransportServer");
	     countersLabels.add("IsMailboxServer");
	     countersLabels.add("IsUnifiedMessagingServer");
	   }

}
