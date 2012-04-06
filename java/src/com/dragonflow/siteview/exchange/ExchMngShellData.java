package com.dragonflow.siteview.exchange;

import java.util.List;

public class ExchMngShellData extends AbstractExchMngtShellData
{
	   protected void loadValue(String key, List<String> pValues)
	   {
	     String value = null;
	     if (key.indexOf("/") == -1) {
	       value = ExchMngtShellParser.getValue(key, pValues);
	     } else {
	       String[] counter = Exch2007Util.getCounterTypeAndName(key);
	       if (counter != null) {
	         value = ExchMngtShellParser.getValue(counter[0], counter[1], pValues);
	       }
	     }
	 
	     if (value == null) {
	       return;
	     }
	 
	    this.countersValuesMapper.put(key, value);
	   }

	
}