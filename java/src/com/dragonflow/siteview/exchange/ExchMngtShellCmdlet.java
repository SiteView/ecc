package com.dragonflow.siteview.exchange;

import java.text.MessageFormat;
import java.util.Map;

public class ExchMngtShellCmdlet {
	
//	private static final Log logger = LogFactory.getEasyLog(ExchMngtShellCmdlet.class);
	   protected static final String GENERAL_COMMAND = "PowerShellSPACE-PSConsoleFileSPACE\"{0}\"SPACE-CommandSPACE\". ''{1}'' {2}\"";
	   protected static MessageFormat CMDLET_FORM = new MessageFormat(getGeneralCmdlet());
	   private static MessageFormat PARAMS_FORM = new MessageFormat("{0} {1} {2} {3} {4} {5} {6} {7} {8} {9} {10} {11} {12}");
	 
	   private static String getGeneralCmdlet()
	   {
	     return "PowerShellSPACE-PSConsoleFileSPACE\"{0}\"SPACE-CommandSPACE\". ''{1}'' {2}\"";
	   }
	 
	   protected String buildCmdlet(Map<String, String> propsMapper) {
	     String pscFilePath = (String)propsMapper.get("$PS_CONSOLE_FILE$");
	     String[] args = { pscFilePath, getScriptPath(), getCmdletParamsAsString(propsMapper) };
	     String cmdlet = CMDLET_FORM.format(args);
	 
//	     if (logger.isDebugEnabled()) {
//	       logger.debug("Building Cmdlet: " + cmdlet);
//	     }
	 
	     return cmdlet;
	   }
	 
	   public String[] buildCmdletAsArray(Map<String, String> propsMapper)
	   {
	     String cmdlet = buildCmdlet(propsMapper);
	     return cmdlet.split("SPACE");
	   }
	 
	   public AbstractExchMngtShellData getMyDataObject() {
	     return new ExchMngShellData();
	   }
	 
	   protected String getCmdletParamsAsString(Map<String, String> propsMapper)
	   {
	     String server = ((String)propsMapper.get("$SERVER$")).toLowerCase();
	     String account = ((String)propsMapper.get("$ACCOUNT$")).toLowerCase();
	     String dc = (String)propsMapper.get("$DOMAIN_CONTROLLER$");
	 
	     String isHubTransportServer = getValueAsParam((String)propsMapper.get("IsHubTransportServer"));
	     String isClientAccessServer = getValueAsParam((String)propsMapper.get("IsClientAccessServer"));
	     String isEdgeServer = getValueAsParam((String)propsMapper.get("IsEdgeServer"));
	     String isMailboxServer = getValueAsParam((String)propsMapper.get("IsMailboxServer"));
	     String isUnifiedMessagingServer = getValueAsParam((String)propsMapper.get("IsUnifiedMessagingServer"));

	     String isTestMapiConnectivity = getValueAsParam((String)propsMapper.get("MAPI Connectivity"));
	     String isTestMailFlow = getValueAsParam((String)propsMapper.get("Mail Flow"));
	     String isTestExchangeSearch = getValueAsParam((String)propsMapper.get("Exchange Search"));
	     String isTestOwaConnectivity = getValueAsParam((String)propsMapper.get("OWA Connectivity"));
	     String isTestWebServicesConnectivity = getValueAsParam((String)propsMapper.get("Web Services Connectivity"));
	 
	     String[] args = { server, account, dc, isHubTransportServer, isClientAccessServer, isEdgeServer, isMailboxServer, isUnifiedMessagingServer, isTestMapiConnectivity, isTestMailFlow, isTestExchangeSearch, isTestOwaConnectivity, isTestWebServicesConnectivity };
	 
	     return PARAMS_FORM.format(args);
	   }
	 
	   private String getValueAsParam(String value)
	   {
	     if ((value != null) && (value.compareToIgnoreCase("True") == 0)) {
	       return "$true";
	     }
	 
	     return "$false";
	   }
	 
	   public String getScriptPath() {
	     return ExchConstants.TEST_CMDLETS_SCRIPT_PATH;
	   }

}
