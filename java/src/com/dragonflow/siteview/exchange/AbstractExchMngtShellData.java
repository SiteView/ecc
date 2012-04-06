package com.dragonflow.siteview.exchange;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public abstract class AbstractExchMngtShellData {
	 protected Map<String, String> countersValuesMapper;
	 
	   public AbstractExchMngtShellData()
	   {
	     this.countersValuesMapper = new HashMap();
	   }
	 
	   public void loadData(List<String> pValues, List<String> countersLabels)
	   {
	     this.countersValuesMapper = new HashMap();
	     for (String counterLabel : countersLabels)
	       loadValue(counterLabel, pValues);
	   }
	 
	   public Map<String, String> getCountersValuesMapper()
	   {
	     return this.countersValuesMapper;
	   }
	 
	   protected abstract void loadValue(String paramString, List<String> paramList);

}
