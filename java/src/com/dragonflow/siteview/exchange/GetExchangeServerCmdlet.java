package com.dragonflow.siteview.exchange;

import java.text.MessageFormat;
import java.util.Map;

public class GetExchangeServerCmdlet extends ExchMngtShellCmdlet
 {
   private static MessageFormat PARAMS_FORM = new MessageFormat("{0}");
	 
   protected String getCmdletParamsAsString(Map<String, String> propsMapper) {
	     String server = ((String)propsMapper.get("$SERVER$")).toLowerCase();
	     String[] args = { server };
	     return PARAMS_FORM.format(args);
	   }
	 
	   public AbstractExchMngtShellData getMyDataObject()
	   {
	     return new ExchSrvRolesData();
	   }
	
	   public String getScriptPath()
	   {
	    return ExchConstants.GET_EXCHANGE_SCRIPT_PATH;
	   }
}
