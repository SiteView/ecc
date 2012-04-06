package com.dragonflow.siteview.exchange;

import java.util.List;

public class ExchDataFactory {
	 public static AbstractExchMngtShellData createDataObject(ExchMngtShellCmdlet command, List<String> pData, List<String> countersNames)
	    {
	      AbstractExchMngtShellData data = getDataDigester(command);
	    data.loadData(pData, countersNames);
	     return data;
	    }
	 
	    private static AbstractExchMngtShellData getDataDigester(ExchMngtShellCmdlet cmdlet)
	    {
	      return cmdlet.getMyDataObject();
	   }
}
