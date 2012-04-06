package com.dragonflow.erlangecc.monitor;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class ChildTuopuData 
{
//	private final static Logger logger = Logger.getLogger(ChildTuopuData.class);
	
    private Document domTree = null;
    
    private String strStatu = "good";
    private String strRoot = "";
    private String strApp = "";
    
	Map<String, Map<String, Object>> mapMonitors = null;
	Map<String, Map<String, Object>> mapGroups = null;
	Map<String, Map<String, Object>> mapMachines = null;	
	
    public String getStatu() {
		return strStatu;
	}


	public void setStatu(String strStatu) {
		this.strStatu = strStatu;
	}

	Map<String, Map<String, String>> mapIn;
    
    public ChildTuopuData(String strRootIn,  String strAppIn, String strName, Map<String, Map<String, Object>> mapGroupsIn, 
	    	Map<String, Map<String, Object>> mapMachinesIn, Map<String, Map<String, Object>> mapMonitorsIn)
    {
		try
		{
			strRoot = strRootIn;
			strApp = strAppIn;
			
			String strPath = "";
			strPath += strApp;
			strPath += strName;			
			strPath += ".files\\data.xml";
			
			mapMonitors = mapMonitorsIn;
			mapGroups = mapGroupsIn;
			mapMachines = mapMachinesIn;
			
	    	//1、初始化并读取data.xml数据
			this.domTree = MakeTuopuData.ReadXml(strPath);
	        String xpathString = ".//Page";  
	        
			NodeList pPageList = MakeTuopuData.selectNodes(this.domTree, xpathString);
			mapIn =  new HashMap();
			
			//2、取得所有用户定义的shape属性
			for (int i=0; i<pPageList.getLength(); i++) 
			{
				Node pPage = pPageList.item(i);
				
				xpathString = ".//Shape";
				NodeList pShapeList = MakeTuopuData.selectNodes(pPage, xpathString);
				for (int j=0; j<pShapeList.getLength(); j++)
				{
					Node pShape = pShapeList.item(j);
					
					if(MakeTuopuData.IsHyperlinkNodesExisted(pShape))
					{
						Map<String, String> value = new LinkedHashMap(); 
						
						value.put("Des", MakeTuopuData.RetrievePropertyValue(pShape, "SV_Des"));
						value.put("IP", MakeTuopuData.RetrievePropertyValue(pShape, "SV_IP"));
						value.put("Link", MakeTuopuData.RetrievePropertyValue(pShape, "SV_Link"));
//						value.put("SV_App", RetrievePropertyValue(pShape, "SV_App"));
						value.put("Group", MakeTuopuData.RetrievePropertyValue(pShape, "SV_Group"));
						value.put("Entity", MakeTuopuData.RetrievePropertyValue(pShape, "SV_Entity"));
						value.put("Monitor", MakeTuopuData.RetrievePropertyValue(pShape, "SV_Monitor"));
						
						mapIn.put(pShape.getAttributes().getNamedItem("ID").getNodeValue(), value);
					}
				}
			}
			
			//调试信息
			if(strName.equals("227")){
//				logger.info("ChildTuopuData mapIn:-----------------------------------------------------------");
				for(String key1 : mapIn.keySet())
		        {
//					logger.info(key1);
					
					for(String key2 : mapIn.get(key1).keySet())
		            {
		                if (!mapIn.get(key1).get(key2).equals(""))
		                {
//		                	logger.info("     " + key2 + " : " + mapIn.get(key1).get(key2).toString());
		                }
		            }
		        }
			}
			
			//3、从服务器得子拓扑图状态。
			GetChildTuopuStatuFromServer(mapIn);
	        
			//调试信息
//			logger.info("GetChildTuopuStatuFromServer Statu : " +  strStatu);			

		}
		catch (Exception ex) 
		{
			
		}	
    }
	
    
	//取子拓扑图状态, 一旦错误等就返回， 不需要获取出所有数据
	public void GetChildTuopuStatuFromServer(Map<String, Map<String, String>> mapIn)
	{
		try
		{

			String strTmpStatu = "good";
			//匹配条件参数 + 监测数据 --> TuopuData 
	        for (String key : mapIn.keySet())
	        {
	        	Map<String, String> tmpValue = new HashMap();
	
	            for(String key1 : mapIn.get(key).keySet())
	            {
	                //
	                if(key1.equals("Group") && !mapIn.get(key).get("Group").equals(""))
	                {
	                    tmpValue.put("Group", mapIn.get(key).get("Group"));
	                    for(String key2 : mapGroups.keySet())
	                    {
//	                    	if (mapGroups.get(key2).get("needtype").equals("group") && mapGroups.get(key2).get("sv_name").indexOf(mapIn.get(key).get("Group")) != -1)
	                    	if (mapGroups.get(key2).get("name").toString().indexOf(mapIn.get(key).get("Group")) != -1)
	                        {
	                            //构成监测器列表出来
	                    		strTmpStatu = GetMonitorStatuFromTree(key2, tmpValue.keySet().size(), false);	                    		
	                    		
	                    		if(strTmpStatu.equals("error"))
	                    		{
	                    			strStatu = "error";
	                    			return;
	                    		}
	                    		
	                    		if(strTmpStatu.equals("warning"))
	                    		{
	                    			strStatu = "warning";
	                    			
	                    		}
	                        }
	                    }
	                }
	                else if (key1.equals("Entity") && !mapIn.get(key).get("Entity").equals(""))
	                {
	                    tmpValue.put("Entity", mapIn.get(key).get("Entity"));
	                    for(String key2 : mapMachines.keySet())
	                    {
	                    	
//	                    	if (mapMachines.get(key2).get("needtype").equals("entity") && mapName.get(key2).get("sv_name").indexOf(mapIn.get(key).get("Entity")) != -1)
	                    	if (mapMachines.get(key2).get("hostName").toString().indexOf(mapIn.get(key).get("Entity")) != -1)	                    		
	                        {
	                            //构成监测器列表出来
	                    		strTmpStatu = GetMonitorStatuFromTree(key2, tmpValue.keySet().size(), true);
	                    		
	                    		if(strTmpStatu.equals("error"))
	                    		{
	                    			strStatu = "error";
	                    			return;
	                    		}
	                    		
	                    		if(strTmpStatu.equals("warning"))
	                    		{
	                    			strStatu = "warning";
	                    		}
	                        }
	                    }
	                }
	                else if (key1.equals("Monitor") && !mapIn.get(key).get("Monitor").equals(""))
	                {
	                    tmpValue.put("Monitor", mapIn.get(key).get("Monitor"));
	                    for(String key2 : mapMonitors.keySet())
	                    {
//	                        if (mapMonitors.get(key2).get("needtype").equals("monitor") && mapName.get(key2).get("sv_name").indexOf(mapIn.get(key).get("Monitor")) != -1)
		                    if (mapMonitors.get(key2).get("name").toString().indexOf(mapIn.get(key).get("Monitor")) != -1)
	                        {
	                            //构成监测器列表出来
	                        	strTmpStatu = GetMonitorStatuFromTree(key2, tmpValue.keySet().size(), true);
	                    		
	                    		if(strTmpStatu.equals("error"))
	                    		{
	                    			strStatu = "error";
	                    			return;
	                    		}
	                    		
	                    		if(strTmpStatu.equals("warning"))
	                    		{
	                    			strStatu = "warning";
	                    		}
	                        }
	                    }
	                }                    
	                else if (key1.equals("IP") && !mapIn.get(key).get("IP").equals(""))
	                {
	                    tmpValue.put("IP", mapIn.get(key).get("IP"));
	                    for(String key2 : mapMachines.keySet())
	                    {
	                    	if(!mapMachines.get(key2).containsKey("hostAddress"))
	                    		continue;
	                    	
//	                        if (mapMachineName.get(key2).get("needtype").equals("entity") && mapMachineName.get(key2).get("_MachineName").equals(mapIn.get(key).get("IP")))
		                    if (mapMachines.get(key2).get("hostAddress").toString().equals(mapIn.get(key).get("IP")))
	                        {
	                            //构成监测器列表出来
	                        	strTmpStatu = GetMonitorStatuFromTree(key2, tmpValue.keySet().size(), true);
	                    		
	                    		if(strTmpStatu.equals("error"))
	                    		{
	                    			strStatu = "error";
	                    			return;
	                    		}
	                    		
	                    		if(strTmpStatu.equals("warning"))
	                    		{
	                    			strStatu = "warning";
	                    		}

	                        }
	                    }
	                }
	                else if (key1.equals("Des") && !mapIn.get(key).get("Des").equals(""))
	                {
	                    //直接加入返回列表项 且在第一位 暂时不管
	
	                }
	                else if (key1.equals("App") && !mapIn.get(key).get("App").equals(""))
	                {
	                    //暂时不管
	                }
	                else if (key1.equals("Link") && !mapIn.get(key).get("Link").equals(""))
	                {
						//递归获取关联的其他子图的状态
	                	ChildTuopuData childTuopu = new ChildTuopuData(strRoot, strApp, mapIn.get(key).get("Link"), mapGroups, mapMachines, mapMonitors);
	                	strTmpStatu = childTuopu.getStatu();
	                	
                		if(strTmpStatu.equals("error"))
                		{
                			strStatu = "error";
                			return;
                		}
                		
                		if(strTmpStatu.equals("warning"))
                		{
                			strStatu = "warning";
                		}

	                }
	                else
	                {
	                	
	                }
	            }
	        }
		}
		catch (Exception ex)
		{
//			logger.info(ex.toString());			
		}
	}
	
	//获取监测器列表数据
    //strSvid + .  -->Group Entity IP
    //strSvid == --> monitor	
	public String GetMonitorStatuFromTree(String strSvid, int index, boolean bIsMonitor)
	{
		String strTmp = "good";
        int k = index;

        if (bIsMonitor)
        {
            if (mapMonitors.containsKey(strSvid))
            {
                //Monitor                        
            	if(mapMonitors.get(strSvid).get("category").toString().equals("error") || mapMonitors.get(strSvid).get("category").toString().equals("bad"))
            	{
            		return "error";
            	}
            	
            	if(mapMonitors.get(strSvid).get("category").toString().equals("warning"))
            	{
            		strTmp = "warning";
            	}
            }        	

        }
        else
        {
            strSvid += ".";
            for(String strId : mapMonitors.keySet())
            {
                if (strId.indexOf(strSvid) == 0)
                {                   
                    //Group Entity IP
                    if(mapMonitors.get(strId).get("category").toString().equals("error") || "bad".equals(mapMonitors.get(strId).get("category").toString()))
                    {
                    	return "error";
                    }
                    
                	if(mapMonitors.get(strId).get("category").toString().equals("warning"))
                	{
                		strTmp = "warning";
                	}
                    
                    k++;
                }
                else
                {
                	
                }
            }            
        }
        
		return strTmp;
	}
    
}
