package com.dragonflow.erlangecc.monitor;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import javax.xml.XMLConstants;
import javax.xml.namespace.NamespaceContext;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.sun.org.apache.xml.internal.serialize.OutputFormat;
import com.sun.org.apache.xml.internal.serialize.XMLSerializer;

	public class MakeTuopuData 
	{
	    private Document domTree = null;
	    
	    private String message;
	    private String strSessionId = "test";
	    private String strRoot = "";
	    private String strApp = "";
	    private String strRootList = "";
	    
	    Map<String, Map<String, String>> mapIn = null;
	    
		Map<String, Map<String, Object>> mapMonitors = null;
		Map<String, Map<String, Object>> mapGroups = null;
		Map<String, Map<String, Object>> mapMachines = null;	
	    
	    public MakeTuopuData(String strRootIn, String strAppIn, String strName, Map<String, Map<String, Object>> mapGroupsIn, 
	    	Map<String, Map<String, Object>> mapMachinesIn, Map<String, Map<String, Object>> mapMonitorsIn)
	    {
			try
			{
				strRoot = strRootIn;
				strApp = strAppIn;
//				strSessionId = desktop.getSession().getAttribute("usersessionid").toString();
//				System.out.println("MakeTuopuData Root:" + strRoot);
//				System.out.println("MakeTuopuData Name:" + strName);

				mapMonitors = mapMonitorsIn;
				mapGroups = mapGroupsIn;
				mapMachines = mapMachinesIn;	
				
////////////////////////////////////////////
				
				//原ShowTuopu的逻辑:
				
				//拷贝frameset.js　main_2.htm　widgets.htm　文件到tuopu 文件目录
				String strSrcPath1 = strRoot + 
				"bak\\main_2.htm";
				String strSrcPath2 = strRoot + 
				"bak\\frameset.js";
				
				String strDestPath1 = strApp + strName + ".files\\main_2.htm";
				String strDestPath2 = strApp + strName + ".files\\frameset.js";				
	
				delFile(strDestPath1);
				delFile(strDestPath2);
				
				copyFile(strSrcPath1, strDestPath1);
				copyFile(strSrcPath2, strDestPath2);
				
				//读取../tuoplist/下的strPageid.htm里的var g_FileList  
				//用来替换../tuoplist/strPageid.files/main_2.htm里的var g_FileList的值。
				String strSrcPath3 = strApp + 
				"" + strName + ".htm";
				
				String strContent = readTxt(strSrcPath3, "UTF-8");
				
				int nStartPos = strContent.indexOf("new FileEntry");
				int nEndPos = strContent.substring(nStartPos - 1).indexOf(");");
	
				String strGfileList = strContent.substring(nStartPos - 1).substring(0, nEndPos);
	
				//更新main_2.html中的mulu.files为指定拓扑的正确文件路径（递规替换）
				String strContent1 = readTxt(strSrcPath1, "UTF-8");
				String strReplaceId = "../";
				strReplaceId += strName;
				strReplaceId += ".files/";
				
				String strContentTmp = strContent1.replaceAll("../tuoplist/mulu.files/", strReplaceId);
				
				//用strPageid.htm里的var g_FileList来替换
				//../tuoplist/strPageid.files/main_2.htm里的var g_FileList的值。
				String strContentMain = strContentTmp.replaceAll("ReplaceFileEntry", strGfileList);
				strContentTmp = strContentMain;
				
				//如果存在是否可以不做此操作了？
				createFile(strDestPath1, strContentTmp);
				
				//解决菜单显示位置问题， 在Tomact下visio.css区分大小写， 原来的Visio.css读不出来了
				//需动态改名称。
				String strCssPath = strApp + "";
				strCssPath += strName;
				strCssPath += ".files\\visio.css";
				
	//			File cssfile = new File(strCssPath);			
	//			if(!cssfile.exists())
				{
					String strCssOldath = strApp + "";
					strCssOldath += strName;
					strCssOldath += ".files\\Visio.css";
					
					File cssfile = new File(strCssOldath);
					cssfile.renameTo(new File(strCssPath));
				}
				
	////////////////////////////////////////////
				
				//原Maketuopudata的逻辑:
//				domTree = new Document();
				String strPath = strApp + "";
				strPath += strName;			
				strPath += ".files\\data.xml";
				
		    	//1、初始化并读取data.xml数据
				this.domTree = ReadXml(strPath);
		        String xpathString = ".//Page";  
		        
				NodeList pPageList = selectNodes(domTree, xpathString);
				mapIn =  new HashMap();
				
				//2、取得所有用户定义的shape属性
				for (int i=0; i<pPageList.getLength(); i++) 
				{
					Node pPage = pPageList.item(i);
					
					xpathString = ".//Shape";
					NodeList pShapeList = selectNodes(pPage, xpathString);
					for (int j=0; j<pShapeList.getLength(); j++)
					{
						Node pShape = pShapeList.item(j);
						if(IsHyperlinkNodesExisted(pShape))
						{
							Map<String, String> value = new LinkedHashMap(); 
							value.put("Des", RetrievePropertyValue(pShape, "SV_Des"));
							value.put("IP", RetrievePropertyValue(pShape, "SV_IP"));
							value.put("Link", RetrievePropertyValue(pShape, "SV_Link"));
//							value.put("SV_App", RetrievePropertyValue(pShape, "SV_App"));
							value.put("Group", RetrievePropertyValue(pShape, "SV_Group"));
							value.put("Entity", RetrievePropertyValue(pShape, "SV_Entity"));
							value.put("Monitor", RetrievePropertyValue(pShape, "SV_Monitor"));
							
							mapIn.put(pShape.getAttributes().getNamedItem("ID").getNodeValue(), value);
						}
					}
				}
				
				//调试信息
//				System.out.println("mapIn");
				for(String key1 : mapIn.keySet())
		        {	
//					System.out.println(key1);					
					for(String key2 : mapIn.get(key1).keySet())
		            {
		                if (!mapIn.get(key1).get(key2).equals(""))
		                {
//		                	System.out.println("     " + key2 + " : " + mapIn.get(key1).get(key2).toString());
		                }
		            }
		        }

//				System.out.println("mapGroups");
//				for(String key1 : mapGroups.keySet())
//		        {	
//					System.out.println(key1);					
//					for(String key2 : mapGroups.get(key1).keySet())
//		            {
//		                if (!mapGroups.get(key1).get(key2).equals(""))
//		                {
//		                	System.out.println("     " + key2 + " : " + mapGroups.get(key1).get(key2).toString());
//		                }
//		            }
//		        }
				
//				System.out.println("mapMachines");
//				for(String key1 : mapMachines.keySet())
//		        {	
//					System.out.println(key1);					
//					for(String key2 : mapMachines.get(key1).keySet())
//		            {
//		                if (!mapMachines.get(key1).get(key2).equals(""))
//		                {
//		                	System.out.println("     " + key2 + " : " + mapMachines.get(key1).get(key2).toString());
//		                }
//		            }
//		        }

//				System.out.println("mapMonitors");
//				for(String key1 : mapMonitors.keySet())
//		        {	
//					System.out.println(key1);					
//					for(String key2 : mapMonitors.get(key1).keySet())
//		            {
//		                if (!mapMonitors.get(key1).get(key2).equals(""))
//		                {
//		                	System.out.println("     " + key2 + " : " + mapMonitors.get(key1).get(key2).toString());
//		                }
//		            }
//		        }
				
				//3、从服务器得所有与shape属性相关的数据
				Map<String, Map<String, String>> result = GetTuopuDataFromServer(mapIn);
		        
				//没有排序的混乱的信息
				Map<String,Map<String, String>> resultCopy = new HashMap<String,Map<String, String>>();
				for(String key1 : result.keySet())
		        {	
					Map<String, String> sonLinkedHashMap = new LinkedHashMap<String, String>();
	
					List<String> bad_keys = new ArrayList<String>();
					List<String> disable_keys = new ArrayList<String>();
					List<String> error_keys = new ArrayList<String>();
					List<String> warning_keys = new ArrayList<String>();
					List<String> ok_keys = new ArrayList<String>();
					List<String> other_keys = new ArrayList<String>();
					List<String> all_keys = new ArrayList<String>();
	
					for(String key2 : result.get(key1).keySet())
		            {
	//				     monitor46Svid : 1.50.3.131
	//				     monitor46Des : Service：ClipBook 是否已启动=False, 运行状态=Stopped, 服务状态=OK, 对应的进程名称=NA, 运行实例个数(个)=0, 
	//				     monitor46State : error
	
						String value = result.get(key1).get(key2);
						if("".equals(value)){
							continue;
						}
						if(key2.startsWith("monitor")){
							if("bad".equals(value)){
								String temp = key2.replace("State", "");
								bad_keys.add(temp+"Svid");
								bad_keys.add(temp+"Des");
								bad_keys.add(temp+"State");
							}else if("disable".equals(value)){
								String temp = key2.replace("State", "");
								disable_keys.add(temp+"Svid");
								disable_keys.add(temp+"Des");
								disable_keys.add(temp+"State");
							}else if("error".equals(value)){
								String temp = key2.replace("State", "");
								error_keys.add(temp+"Svid");
								error_keys.add(temp+"Des");
								error_keys.add(temp+"State");
							}else if("warning".equals(value)){
								String temp = key2.replace("State", "");
								warning_keys.add(temp+"Svid");
								warning_keys.add(temp+"Des");
								warning_keys.add(temp+"State");
							}else if("good".equals(value)){
								String temp = key2.replace("State", "");
								ok_keys.add(temp+"Svid");
								ok_keys.add(temp+"Des");
								ok_keys.add(temp+"State");
							}
						}else
						{
							other_keys.add(key2);
						}
		            }
					//利用有序的 key 写入值
					if(bad_keys.size()>1){
						all_keys.addAll(bad_keys);
					}
					if(error_keys.size()>1){
						all_keys.addAll(error_keys);
					}
					if(disable_keys.size()>1){
						all_keys.addAll(disable_keys);
					}
					if(warning_keys.size()>1){
						all_keys.addAll(warning_keys);
					}
					if(ok_keys.size()>1){
						all_keys.addAll(ok_keys);
					}
					if(other_keys.size()>1){
						all_keys.addAll(other_keys);
					}
					
					for(String key2 : all_keys){
						sonLinkedHashMap.put(key2, result.get(key1).get(key2).toString());
					}				
					resultCopy.put(key1, sonLinkedHashMap);
		        }
				
				
//				//调试信息
//				System.out.println("GetTuopuDataFromServer result:");			
//				StringBuffer strDebugContent = new StringBuffer("");
//				for(String key1 : result.keySet())
//		        {	
//					System.out.println(key1);
//	
//					strDebugContent.append(key1);
//					strDebugContent.append("\r\n");
//	
//					for(String key2 : result.get(key1).keySet())
//		            {
//		                if (!result.get(key1).get(key2).equals(""))
//		                {
////		                	logger.info("     " + key2 + " : " + result.get(key1).get(key2).toString());
//		                	
//		                	strDebugContent.append("     " + key2 + " : " + result.get(key1).get(key2).toString());
//		    				strDebugContent.append("\r\n");
//		    				System.out.println(strDebugContent);
//		                }
//		            }
//					
//	//				delFile("e:\\tuopudatadebug.txt");
//	//				createFile("e:\\tuopudatadebug.txt", strDebugContent.toString());
//		        }
				
//				//调试信息
//				for(String key1 : resultCopy.keySet())
//				{	
////					logger.info(key1);
//				
//					strDebugContent.append(key1);
//					strDebugContent.append("\r\n");
//				
//					for(String key2 : resultCopy.get(key1).keySet())
//				    {
//				        if (!resultCopy.get(key1).get(key2).equals(""))
//				        {
////				        	logger.info("     " + key2 + " : " + resultCopy.get(key1).get(key2).toString());
//				        	
//				        	strDebugContent.append("     " + key2 + " : " + resultCopy.get(key1).get(key2).toString());
//							strDebugContent.append("\r\n");	                	
//				        }
//				    }
//					
//				//	delFile("e:\\tuopudatadebug.txt");
//				//	createFile("e:\\tuopudatadebug.txt", strDebugContent.toString());
//				}
	
	
			
				//根据后台数据构造新的菜单节点（根据监测器状态等）
				for(String key1 : result.keySet())
		        {
					//删除原来的所有菜单节点（根据监测器状态等）
	//				Node pShapeNode = this.domTree.getElementById(key1);
					String strSelect = String.format("//Shape[@ID=\"%s\"]", key1);
			    	Node pShapeNode  = selectSingleNode(this.domTree, strSelect);
			    	
					if(pShapeNode == null)
						continue;
	
					//删除原来的数据
					DeleteHyperlinkNodes(pShapeNode);
					
					
					Node pMenuNode  = selectSingleNode(pShapeNode, "Scratch/B/SolutionXML/HLURL:Hyperlinks");
					
	//				if(result.get(key1).containsKey("Link"))
	//				{	
						InsertHyperlinkNode(this.domTree, pMenuNode, result.get(key1), strSessionId);					
	//				}
	//				else
	//				{
	//					 InsertHyperlinkNode(this.domTree, pMenuNode, result.get(key1), strSessionId);				
	//				}
		        }
	
	/*			
				//根据后台数据构造新的菜单节点（根据监测器状态等）
				for(String key1 : result.keySet())
		        {
					//删除原来的所有菜单节点（根据监测器状态等）
	//				Node pShapeNode = this.domTree.getElementById(key1);
					String strSelect = String.format("//Shape[@ID=\"%s\"]", key1);
			    	Node pShapeNode  = selectSingleNode(this.domTree, strSelect);
			    	
					if(pShapeNode == null)
						continue;
	
					//删除原来的数据
					DeleteHyperlinkNodes(pShapeNode);
					
					
					Node pMenuNode  = selectSingleNode(pShapeNode, "Scratch/B/SolutionXML/HLURL:Hyperlinks");
					
	//				if(result.get(key1).containsKey("Link"))
	//				{	
						InsertHyperlinkNode(this.domTree, pMenuNode, result.get(key1), strSessionId);					
	//				}
	//				else
	//				{
	//					 InsertHyperlinkNode(this.domTree, pMenuNode, result.get(key1), strSessionId);				
	//				}
		        }
	*/			
				//保存data.xml文件
				FileOutputStream fos = new FileOutputStream(strPath);
				try{
				OutputFormat formatter = new OutputFormat();
				formatter.setPreserveSpace(true);
				XMLSerializer serializer =new XMLSerializer(fos, formatter);
				serializer.serialize(this.domTree);			
			}finally{
				fos.close();
			}
			
			//读取*.files\vml_*.tpl并根据数据状态重新创建vml_*.html以改变节点颜色等。
			String strTplFile = strPath;
			String strNew = "", strOld = "", strTmpContent = "";
			
//			//设置边框宽度及颜色
//			IniFile iniGen = new IniFile("tuopuset.ini");
//			try
//			{
//				iniGen.load();
//			} catch (Exception e1)
//			{
//			}
			String c1,c2,c3;
			String w1,w2,w3;
//			if (iniGen.getSectionList().isEmpty())
//			{
				c1="red";
				c2="yellow";
				c3="green";
				w1="3";
				w2="3";
				w3="0";
//			} else
//			{
//				String tempc = "";
//				tempc = iniGen.getValue("tuopusetid", "c1");
//				c1=tempc;
//				tempc = iniGen.getValue("tuopusetid", "c2");
//				c2=tempc;
//				tempc = iniGen.getValue("tuopusetid", "c3");
//				c3=tempc;
//				tempc = iniGen.getValue("tuopusetid", "w1");
//				w1=tempc;
//				tempc = iniGen.getValue("tuopusetid", "w2");
//				w2=tempc;
//				tempc = iniGen.getValue("tuopusetid", "w3");
//				w3=tempc;
//			}
			
			for (int i=0; i<pPageList.getLength(); i++) 
			{			
//				if(m_iVersion02==0)
				{
					//按visio 2003 执行
					strNew = String.format("vml_%d.tpl", (i+1));
					strTplFile = strPath.replace("data.xml", strNew);
				}
//				else
//				{
//					//按visio 2002 执行
//					strNew.Format("_vml_%d.tpl", (j+1));
//					strHtmFile.Replace(".xml", strNew);
//				}
				
				//打开tpl文件， 并读取内容
				strContent = readTxt(strTplFile, "UTF-8");
								
				//根据后台数据改变边框颜色（ShapeId + IP及Group等）
				for(String key1 : result.keySet())
		        {	
//					Node pShapeNode = this.domTree.getElementById(key1);
//					mapIn.get(key1).keySet();
					
					//根据拓扑图单元状态替换文件内容
					if(result.get(key1).containsKey("StatState"))
					{						
						if(result.get(key1).get("StatState").equals("error") || result.get(key1).get("StatState").equals("bad"))
						{
//							System.out.println("result StatState:" + result.get(key1).get("StatState").toString());
							strOld = String.format("fillcolor=\"%s\\+color\"", GetFillColorStr(mapIn.get(key1)));
							strNew = String.format("filled=\"f\"  stroked=\"t\" strokecolor=\"%s\" fillcolor=\"%s\" strokeweight=\"%spt\"", c1, c1, w1);
							
//							System.out.println("result strOld:" + strOld);
//							System.out.println("result strNew:" + strNew);
//							int index = strContent.indexOf(strOld);
//							if(!w1.equals("0"))
							{
							strTmpContent = strContent.replaceAll(strOld, strNew);
							strContent = strTmpContent;
							}
						}
						else if(result.get(key1).get("StatState").equals("warning"))
						{
							strOld = String.format("fillcolor=\"%s\\+color\"", GetFillColorStr(mapIn.get(key1)));
							strNew = String.format("filled=\"f\"  stroked=\"t\" strokecolor=\"%s\" fillcolor=\"%s\" strokeweight=\"%spt\"", c2, c2, w2);
							
//							int index = strContent.indexOf(strOld);
//							if(!w2.equals("0"))
							{
							strTmpContent = strContent.replaceAll(strOld, strNew);							
							strContent = strTmpContent;		
							}
						}
						else
						{
							strOld = String.format("fillcolor=\"%s\\+color\"", GetFillColorStr(mapIn.get(key1)));
							strNew = String.format("filled=\"f\"  stroked=\"t\" strokecolor=\"%s\" fillcolor=\"%s\" strokeweight=\"%spt\"", c3, c3, w3);
							
//							int index = strContent.indexOf(strOld);
//							if(!w3.equals("0"))
							{
							strTmpContent = strContent.replaceAll(strOld, strNew);							
							strContent = strTmpContent;
							}
						}
					}
				}				
				
				strTmpContent = strContent.replaceAll("href=\"#\"", "href=\"javascript:void(null)\"");
				strContent = strTmpContent;
				
				String strCurUrl = "";
				
//				if(nIsMainTain == 1)
				{
					//if(nIsMainTainLeader == 1)
//						strCurUrl.Format("parent.location.replace(\"../../../fcgi-bin/showmaintain.exe?pageid=%s&version=0&usrleader=%d&maintain=%d\")", strPageId, nIsMainTainLeader, nIsMainTain);
				}
//				else
//					strCurUrl = String.format("parent.location.replace(\"../../../main/tuoplist/showtuopu.zul?name=%s&version=0\")", strName);
				    strCurUrl = String.format("top.location.replace(\"../../../../web/showTuopu?name=%s\")", strName);
					
//				strTmpContent = strContent.replaceAll("parent.location.reload()", strCurUrl);//mf:tuopo自动刷新
				strTmpContent = this.myReplace(strContent, "parent.location.reload()", strCurUrl);//mf:tuopo自动刷新
				strContent = strTmpContent;
				
				int nflagpos =  strContent.indexOf("*#siteview7endflag#*");	
				if(nflagpos == -1)
				{
					strTmpContent = strContent;
				}
				else
				{
					strTmpContent = strContent.substring(0, nflagpos);
				}
				
				strContent = strTmpContent;
				
				String strHtmFile = strTplFile.replace(".tpl", ".htm");
//				System.out.println("content:" + strContent);
//				System.out.println("strHtmFile:" + strHtmFile);			
				createFile(strHtmFile, strContent, "UTF-8");
			}
			
	//			CStringList lstShape, lstColor, lstWeight;
	
				//GetStatusFromPingIni(cTDL,strXMLFile);
				
				//4、根据shape的相关数据构造菜单及超连接所需的颜色等
	//			FormatXML(pDoc, pPageNode, cTDL, lstShape, lstColor, lstWeight);
	
	//			CString strNew = _T("");
	//			CString strHtmFile = strXMLFile;
	//			if(m_iVersion02==0)
	//			{
	//				//按visio 2003 执行
	//				strNew.Format("vml_%d.tpl", (j+1));
	//				strHtmFile.Replace("data.xml", strNew);
	//			}
	//			else
	//			{
	//				//按visio 2002 执行
	//				strNew.Format("_vml_%d.tpl", (j+1));
	//				strHtmFile.Replace(".xml", strNew);
	//			}
				
				//5、更改节点颜色属性等
	//			OperaHtm(strHtmFile, lstShape, lstColor, lstWeight);
	//		}
		}
		catch (Exception ex) 
		{
			
		}	
    }
    


    //////////////////////针对XML的特殊封装的操作函数////////////////////////
    
    //读取指定Xml文件
	public static Document ReadXml(String xmlFilename) throws IOException 
	{
		Document docIn = null;  
		try 
	      {
	        // Convert filename into a DOM tree
	        DocumentBuilderFactory domFactory =
	          DocumentBuilderFactory.newInstance();
	        domFactory.setNamespaceAware(true);
	        DocumentBuilder builder = domFactory.newDocumentBuilder();
	        docIn  = builder.parse(new File(xmlFilename));
	      }
	      catch (SAXException e) 
	      {
	        throw new IOException("Error in document parsing: " + e.getMessage());
	      }
	      catch (ParserConfigurationException e) 
	      {
	        throw new IOException("Error in configuring parser: " + e.getMessage());
	      }
	      
	      return docIn;
	}
	
	//查找节点，并返回第一个符合条件节点
	public static Node selectSingleNode(Object source, String express) 
	{
	     Node result = null;
	     XPathFactory xpathFactory = XPathFactory.newInstance();	   
	     XPath xpath = xpathFactory.newXPath();
	
//	     xpath.setNamespaceContext(new NamespaceContextProvider("v",
//		 "http://schemas.microsoft.com/visio/2003/SVGExtensions/"));
	     
	     xpath.setNamespaceContext(new NamespaceContextProvider("HLURL",
		 "urn:schemas-microsoft-com:office:visio:dghlinkext"));	     
	     try {
	      result = (Node) xpath.evaluate(express, source, XPathConstants.NODE);
	     } catch (XPathExpressionException e) {
	      e.printStackTrace();
	     }
	
	     return result;
	}
	
	//查找节点，返回符合条件的节点集。
	public static NodeList selectNodes(Object source, String express) 
	{
	     NodeList result = null;
	     XPathFactory xpathFactory = XPathFactory.newInstance();
	     XPath xpath = xpathFactory.newXPath();
	     
//	     xpath.setNamespaceContext(new NamespaceContextProvider("v", 
//	    		 "http://schemas.microsoft.com/visio/2003/SVGExtensions/"));
	
	     xpath.setNamespaceContext(new NamespaceContextProvider("HLURL",
		 "urn:schemas-microsoft-com:office:visio:dghlinkext"));
	
	     try {
	      result = (NodeList) xpath.evaluate(express, source, XPathConstants.NODESET);
	     } catch (XPathExpressionException e) {
	      e.printStackTrace();
	     }
	
	     return result;
	}
	
	//获取用户自定义属性值(SV_IP等)
	public static String RetrievePropertyValue(Node pShape, String strPropName)
	{
	//	logger.info(pShape.getAttributes().getNamedItem("ID").getNodeValue());
	//	logger.info(strPropName);
		
		String strSelect = "";
		String strValue = "";
	
		try
		{
			strSelect = String.format("Prop[@NameU=\"%s\"]", strPropName);
	    	
	    	Node pNode  = selectSingleNode(pShape, strSelect);
	    	if(pNode != null)
	    	{
	    		Node pChildNode = selectSingleNode(pNode, "Value");
	    		if(pChildNode != null)
	    		{
					strValue = pChildNode.getTextContent();
					//logger.info(strValue);
	    		}	    		
	    	}
		}
		catch (Exception ex) 
		{
			return "";    		
		}
		
		return strValue;
	}
	
	//HLURL节点是否存在 ?
	public static boolean IsHyperlinkNodesExisted(Node pShape)
	{
	//	logger.info(pShape.getAttributes().getNamedItem("ID").getNodeValue());
		
//		String strXpath = "Scratch/B/SolutionXML/HLURL:Hyperlinks/HLURL:Hyperlink";
		String strXpath = "Scratch/B/SolutionXML/HLURL:Hyperlinks";
		
		try
		{
			Node pNode  = selectSingleNode(pShape, strXpath);
			if(pNode != null)
			{
	    		return true;
			}
		}
		catch (Exception ex) 
		{
			return false;    		
		}
		
		return false;
	}
	
	//插入Xml节点
	public static void  InsertNode(Document docIn, Node parentNode, String strNodeName, String strNodeText, String szNodeNamespaceURI , String szAttr1Name, String szAttr1Value)
	{
		try
		{
			Element pElement = null;
			if(szNodeNamespaceURI.equals(""))    		
				pElement = docIn.createElement(strNodeName);
			else
				pElement = docIn.createElementNS(szNodeNamespaceURI, strNodeName);
			
//			pElement.setNodeValue(strNodeText);
			pElement.setTextContent(strNodeText);
			
			if(!szAttr1Name.equals(""))
				pElement.setAttribute(szAttr1Name, szAttr1Value);
			
//			parentNode.insertBefore(pElement, arg1)(pElement);		
			parentNode.appendChild(pElement);
		}
		catch (Exception ex) 
		{
			return ;    		
		}		
	}
	
	//
	private static String GetStateFormStr(String strState)
	{
		String strType = "";
		
		if(strState.equals("good"))
		{
			strType = "1";
		}
		else if(strState.equals("warning"))
		{
			strType = "2";
		}
		else if(strState.equals("error"))
		{
			strType = "3";
		}
		else if(strState.equals("disable"))
		{
			strType = "4";
		}
		else if(strState.equals("bad"))
		{
			strType = "5";
		}
		else
		{
			strType = strState;
		}

		return strType;
	}
	
	private static String GetFillColorStr(Map<String, String> map)
	{
		String strType = "";
		
		for(String key : map.keySet())
		{
			if(!map.get(key).equals(""))
			{
				strType += String.format("SV_%s:%s", key, map.get(key));
				break;
			}
		}

		return strType;
	}	
	
	//插入菜单项
	public static void InsertMenuItem(Document docIn, Node parentNode, String strID, String strDes, String strAddress, String strState)
	{
		Element pElement = docIn.createElementNS("urn:schemas-microsoft-com:office:visio:dghlinkext", "HLURL:Hyperlink");
		pElement.setAttribute("ID", strID);
		parentNode.appendChild(pElement);
		
		//描述
		InsertNode(docIn, pElement,  "HLURL:Description", strDes, "urn:schemas-microsoft-com:office:visio:dghlinkext", "", "");
		
		//小报告
		InsertNode(docIn, pElement,  "HLURL:Address", strAddress, "urn:schemas-microsoft-com:office:visio:dghlinkext", "", "");
		InsertNode(docIn, pElement,  "HLURL:SubAddress", "", "urn:schemas-microsoft-com:office:visio:dghlinkext", "", "");
		InsertNode(docIn, pElement,  "HLURL:AbsoluteURL", strAddress, "urn:schemas-microsoft-com:office:visio:dghlinkext", "", "");
		
		//<HLURL:ExtraInfo></HLURL:ExtraInfo>
		InsertNode(docIn, pElement,  "HLURL:ExtraInfo","", "urn:schemas-microsoft-com:office:visio:dghlinkext", "", "");
		InsertNode(docIn, pElement,  "HLURL:Default", "", "urn:schemas-microsoft-com:office:visio:dghlinkext", "", "");
		
		//状态
		InsertNode(docIn, pElement,  "HLURL:Status", GetStateFormStr(strState), "urn:schemas-microsoft-com:office:visio:dghlinkext", "", "");
		
		if(!strAddress.equals("#"))
			InsertNode(docIn, pElement,  "HLURL:NewWindow", "3", "urn:schemas-microsoft-com:office:visio:dghlinkext", "", "");
		else
			InsertNode(docIn, pElement,  "HLURL:NewWindow", "0", "urn:schemas-microsoft-com:office:visio:dghlinkext", "", "");		
	}
	
	//插入HyperlinkNode节点
	public static void InsertHyperlinkNode(Document docIn, Node parentNode, Map<String, String> menuInfo, String strId)
	{		
		try
		{
			String strIndex = "", strID = "";
			String strStateKey = "", strStateValue = ""; 
			String strDesKey = "", strDesValue = ""; 
			String strSvidKey = "", strSvidValue = "";
			String strAddress = "";
			
			boolean bAddMenu = false;
			
			//用户描述项
			if(menuInfo.containsKey("Des"))
			{
				strID = "0";
				strDesValue = menuInfo.get("Des");
				strStateValue = "-100";
				strAddress = "#";
				
				InsertMenuItem(docIn, parentNode, strID, strDesValue, strAddress, strStateValue);
			}
			
			//用户定义标识菜单项
			if(menuInfo.containsKey("IP"))
			{
				strID = "0";
				strDesValue = "SV_IP:" + menuInfo.get("IP");
				strStateValue = "-100";
				strAddress = "#";

				InsertMenuItem(docIn, parentNode, strID, strDesValue, strAddress, strStateValue);
			}
			else if(menuInfo.containsKey("Entity"))
			{
				strID = "0";
				strDesValue = "SV_Entity:" + menuInfo.get("Entity");
				strStateValue = "-100";
				strAddress = "#";

				InsertMenuItem(docIn, parentNode, strID, strDesValue, strAddress, strStateValue);
			}
			else if(menuInfo.containsKey("Group"))
			{
				strID = "0";
				strDesValue = "SV_Group:" + menuInfo.get("Group");
				strStateValue = "-100";
				strAddress = "#";

				InsertMenuItem(docIn, parentNode, strID, strDesValue, strAddress, strStateValue);
			}			
			else if(menuInfo.containsKey("Monitor"))
			{
				strID = "0";
				strDesValue = "SV_Monitor:" + menuInfo.get("Monitor");
				strStateValue = "-100";
				strAddress = "#";

				InsertMenuItem(docIn, parentNode, strID, strDesValue, strAddress, strStateValue);	
			}
			else if(menuInfo.containsKey("Link"))
			{
				strID = "0";
				strDesValue = "Go To Page:" + menuInfo.get("Link");
//				strDesValue = "SV_Link:" + menuInfo.get(key);
				strStateValue = menuInfo.get("StatState");
				strAddress = "../../../../web/showTuopu?name=" + menuInfo.get("Link");
				InsertMenuItem(docIn, parentNode, strID, strDesValue, strAddress, strStateValue);
				
			}
			else
			{
				
			}
			
			//监测器列表
			int i = 0;
			bAddMenu = false;
			for (String key : menuInfo.keySet())
			{
				if(key.indexOf("monitor") != -1 && key.indexOf("Svid") != -1)
				{
					strIndex = key.substring(0, key.length() - 4);
					strStateKey = strIndex + "State"; 
					strDesKey = strIndex + "Des";
					strSvidKey = key;
					
					if(!menuInfo.containsKey(strStateKey) || !menuInfo.containsKey(strDesKey) || !menuInfo.containsKey(strSvidKey))
					{
						bAddMenu = false;
						continue;
					}
					else
					{
						bAddMenu = true;	
						
						strDesValue = menuInfo.get(strDesKey);
						strSvidValue = menuInfo.get(strSvidKey);
						strStateValue = menuInfo.get(strStateKey);
						strAddress = "../../../../web/adhocReport?monitors=" + menuInfo.get(strSvidKey)+ "&sid=" + strId + "&queryID=0&parms=1";
						
						if(strDesValue.equals("") || strStateValue.equals(""))
						{
							bAddMenu = false;
							continue;
						}
						
						i++;
						strID = String.valueOf(i);
					}
				}
				
				if(bAddMenu)
				{	
//						if(i>=5)
//							 return;
					InsertMenuItem(docIn, parentNode, strID, strDesValue, strAddress, strStateValue);
				}
				
				bAddMenu = false;
			}
		}
		catch (Exception ex) 
		{
			return ;
		}		
		
		//varType 暂时不加
//		Element pElement = this.domTree.createElementNS("HLURL:Hyperlink", "urn:schemas-microsoft-com:office:visio:dghlinkext");    	
//		pElement.setAttribute("ID", menuInfo.get("ID"));
//		parentNode.appendChild(pElement);
//		
//	//	for(String key : menuInfo.keySet())
//		{	
//			InsertNode(pElement,  "HLURL:Description", menuInfo.get("DESC").toString(), "urn:schemas-microsoft-com:office:visio:dghlinkext", "", "");
//
//			InsertNode(pElement,  "HLURL:Address", menuInfo.get("ADDR").toString(), "urn:schemas-microsoft-com:office:visio:dghlinkext", "", "");
//			InsertNode(pElement,  "HLURL:SubAddress", "", "urn:schemas-microsoft-com:office:visio:dghlinkext", "", "");
//			//<HLURL:ExtraInfo></HLURL:ExtraInfo>
//			InsertNode(pElement,  "HLURL:ExtraInfo","", "urn:schemas-microsoft-com:office:visio:dghlinkext", "", "");
//			InsertNode(pElement,  "HLURL:Default", "", "urn:schemas-microsoft-com:office:visio:dghlinkext", "", "");
//			InsertNode(pElement,  "HLURL:AbsoluteURL", menuInfo.get("ADDR").toString(), "urn:schemas-microsoft-com:office:visio:dghlinkext", "", "");
//			
//			InsertNode(pElement,  "HLURL:Status", menuInfo.get("STAT").toString(), "urn:schemas-microsoft-com:office:visio:dghlinkext", "", "");
//			
//		}
//		
//		if(menuInfo.get("ADDR").equals("#"))
//		{
//			InsertNode(pElement, "HLURL:NewWindow", "0", "urn:schemas-microsoft-com:office:visio:dghlinkext", "", "");
//		}
//		else
//		{
//			InsertNode(pElement, "HLURL:NewWindow", "3", "urn:schemas-microsoft-com:office:visio:dghlinkext", "", "");
//		}
	}
	
	//删除HyperlinkNode节点
	public void  DeleteHyperlinkNodes(Node pShape)
	{
//		Node pParent = pShape.getParentNode();
		
		String strXpath = "Scratch/B/SolutionXML/HLURL:Hyperlinks/HLURL:Hyperlink";    	
		
		try
		{
			NodeList pNodeList = selectNodes(pShape, strXpath);
			
			for (int i=0; i<pNodeList.getLength(); i++) 
			{
				Node pNode = pNodeList.item(i);
				Node pParent = pNode.getParentNode(); 
				pParent.removeChild(pNode);
			}
	
		}
		catch (Exception ex) 
		{
			return ;    		
		}
	}
	
	//////////////////////针对XML的特殊封装的操作函数////////////////////////


	//////////////////////取拓扑图数据函数///////////////////////////////////
	
//	//取拓扑图数据
	public Map<String, Map<String, String>> GetTuopuDataFromServer (Map<String, Map<String, String>> mapIn)
	{	
		Map<String, Map<String, String>> resultData = new HashMap();
		
		try
		{
	        //匹配条件参数 + 监测数据 --> TuopuData          
			
	        String strState = "good";
	        for (String key : mapIn.keySet())
	        {
	        	Map<String ,String> map = mapIn.get(key);
	        	
	        	Map<String, String> tmpValue = new HashMap<String, String>();
	
	            int j = 0;                
	            for(String key1 : map.keySet())
	            {
	                //
	                if(key1.equals("Group") && !map.get("Group").equals(""))
	                {
	                    j = 0;
	                    strState = "good";
	                    tmpValue.put("StatState", strState); // 应该是统计出来的
	                    tmpValue.put("Group", map.get("Group"));
	                    for(String key2 : mapGroups.keySet())
	                    {
//	                    	if(mapName.get(key2).isEmpty())
//	                    		continue;

	                    	if (mapGroups.get(key2).get("name").toString().indexOf(map.get("Group")) != -1)
	                        {
	                            //构成监测器列表出来
	                        	Map<String, String> tmpValue1 = MakeMonitorInfoFromTree(key2, tmpValue.keySet().size(), false);
	
	                            tmpValue.put("Svid" + String.valueOf(j), key2);
	                            tmpValue.put("Svid" + String.valueOf(j) + "Name", mapGroups.get(key2).get("name").toString());
	
	                            for(String key3 : tmpValue1.keySet())
	                            {
	                                tmpValue.put(key3, tmpValue1.get(key3));
	                            }
	
	                            j++;
	                        }
	                    }
	                }
	                else if (key1.equals("Entity") && !map.get("Entity").equals(""))
	                {
	                    j = 0;
	                    strState = "good";
	                    tmpValue.put("StatState", strState);
	                    tmpValue.put("Entity", map.get("Entity"));
	                    for(String key2 : mapMachines.keySet())
	                    {
//	                    	if(mapName.get(key2).isEmpty())
//	                    		continue;
	                    	
	                    	if (mapMachines.get(key2).get("hostName").toString().indexOf(map.get("Entity")) != -1)
	                        {
	                            //构成监测器列表出来
	                        	Map<String, String> tmpValue1 = MakeMonitorInfoFromTree(key2, tmpValue.keySet().size(), true);
	                            tmpValue.put("Svid" + String.valueOf(j), key2);
	                            tmpValue.put("Svid" + String.valueOf(j) + "Name", mapMachines.get(key2).get("name").toString());
	
	                            for(String key3 : tmpValue1.keySet())
	                            {
	                                tmpValue.put(key3, tmpValue1.get(key3));
	                            }
	                            
	                            j++;
	                        }
	                    }
	                }
	                else if (key1.equals("Monitor") && !map.get("Monitor").equals(""))
	                {
	                    j = 0;
	                    strState = "good";
	                    tmpValue.put("StatState", strState);
	                    tmpValue.put("Monitor", map.get("Monitor"));
	                    for(String key2 : mapMonitors.keySet())
	                    {
//	                    	if(mapName.get(key2).isEmpty())
//	                    		continue;
	                    	
	                        if (mapMonitors.get(key2).get("name").toString().indexOf(map.get("Monitor")) != -1)
	                        {
	                            //构成监测器列表出来
	                        	Map<String, String> tmpValue1 = MakeMonitorInfoFromTree(key2, tmpValue.keySet().size(), true);
	                            tmpValue.put("Svid" + String.valueOf(j), key2);
	                            tmpValue.put("Svid" + String.valueOf(j) + "Name", mapMonitors.get(key2).get("name").toString());
	
	                            for(String key3 : tmpValue1.keySet())
	                            {
	                                tmpValue.put(key3, tmpValue1.get(key3));
	                            }
	                            
	                            j++;
	                        }
	                    }
	                }                    
	                else if (key1.equals("IP") && !map.get("IP").equals(""))
	                {
	                    j = 0;
	                    strState = "good";
	                    tmpValue.put("StatState", strState);
	                    tmpValue.put("IP", map.get("IP"));
	                    for(String key2 : mapMachines.keySet())
	                    {
//	                    	logger.info(key2);
	                    	
	                    	if(!mapMachines.get(key2).containsKey("hostAddress"))
	                    		continue;
	                    	
	                    	//if (mapMachineName[key2]["needtype"] == "entity" && mapMachineName[key2]["_MachineName"] == mapIn[key]["IP"])
	                        if (mapMachines.get(key2).get("hostAddress").toString().equals(map.get("IP")))
	                        {
	                            //构成监测器列表出来
	                        	Map<String, String> tmpValue1 = MakeMonitorInfoFromTree(key2, tmpValue.keySet().size(), true);
	                            tmpValue.put("Svid" + String.valueOf(j), key2);
	                            tmpValue.put("Svid" + String.valueOf(j) + "Name", mapMachines.get(key2).get("name").toString());
	
	                            for(String key3 : tmpValue1.keySet())
	                            {
	                                tmpValue.put(key3, tmpValue1.get(key3));
	                            }
	                            
	                            j++;
	                        }
	                    }
	                }
	                else if (key1.equals("Des") && !map.get("Des").equals(""))
	                {
	                    //直接加入返回列表项 且在第一位 暂时不管
	                    strState = "good";
	                    tmpValue.put("StatState", strState); // 应该是统计出来的
	                    tmpValue.put("Des", map.get("Des"));	                	
	
	                }
	                else if (key1.equals("App") && !map.get("App").equals(""))
	                {
	                    //暂时不管
	                    strState = "good";
	                    tmpValue.put("StatState", strState); // 应该是统计出来的
	                    tmpValue.put("App", map.get("App"));
	                }
	                else if (key1.equals("Link") && !map.get("Link").equals(""))
	                {
						//获取链接的子拓扑图状态
						ChildTuopuData childTuopu = new ChildTuopuData(strRoot, strApp, map.get("Link"), mapGroups, mapMachines, mapMonitors);//map.get("Link") = ip地址
						strState = childTuopu.getStatu();
	                    tmpValue.put("StatState", strState); // 应该是统计出来的
	                    tmpValue.put("Link", map.get("Link"));
	                }
	                else
	                { 
	                    
	                }
	            }
	
	            resultData.put(key, tmpValue);
	        }
	
	        String strStatState = "good";
	
	        //统计 每一个图 的总状态
	        for(String key1 : resultData.keySet())
	        {
	        	Map<String,String> map = resultData.get(key1);
	            strStatState = map.get("StatState");
	            for(String key2 : map.keySet())
	            {
	                if ("error".equals(strStatState))
                    {
                        //已经是错误 跳出循环 统计下一个图
                        break;
                    }
	                //if ("StatState".equals(key2)) continue;

	                
	                String value2 = map.get(key2);
	                if (value2 == null) continue;

//                    logger.info("key2 value == " + key2);
	                
                    if (! (key2.indexOf("State") > 0)) continue;
                    
//                    logger.info("State value == " + value2);

                    if ("disable".equals(value2))
                    {
                        continue;
                    }else if ("bad".equals(value2))
                    {
                        strStatState = "error";
                    }
                    else if ("good".equals(value2))
                    {                                
                        continue;//strStatState = value2; //ok warning error
                    }
                    else if ("error".equals(value2))
                    {
                        strStatState = "error"; 
                    }
                    else if ("warning".equals(value2))
                    {
                        strStatState = "warning"; 
                    }                    
                    else if ("warning".equals(strStatState) && "good".equals(value2))
                    {
                        continue;
                    }
                    else
                    {
                        continue;
                    }
	            }
	            
	            map.put("StatState", strStatState);
	        }
		}
		catch (Exception ex)
		{
//			logger.info(ex.toString());
		}
		
        return resultData;		
	}
	
	//获取监测器列表数据
    //strSvid + .  -->Group Entity IP
    //strSvid == --> monitor	
	public Map<String, String> MakeMonitorInfoFromTree(String strSvid, int index, boolean bIsMonitor)
	{
		Map<String, String> value = new HashMap();
        int k = index;

        if (bIsMonitor)
        {
            if (mapMonitors.containsKey(strSvid))
            {
                //Monitor                        
                value.put("monitor" + String.valueOf(k) + "State", mapMonitors.get(strSvid).get("category").toString());
                value.put("monitor" + String.valueOf(k) + "Des", mapMonitors.get(strSvid).get("name").toString() + " " + mapMonitors.get(strSvid).get("state_string").toString());
                value.put("monitor" + String.valueOf(k) + "Svid", strSvid);
            }
        }
        else
        {        	
            strSvid += ".";
            for(String strId : mapMonitors.keySet())
            {            	
                if (strId.indexOf(strSvid) == 0)
                {
                    value.put("monitor" + String.valueOf(k) + "State", mapMonitors.get(strId).get("category").toString());
                    value.put("monitor" + String.valueOf(k) + "Des", mapMonitors.get(strId).get("name").toString() + " " + mapMonitors.get(strId).get("state_string").toString());
                    value.put("monitor" + String.valueOf(k) + "Svid", strId);
                    k++;
                }
                else
                {
                	
                }
            }
//            for (int i = 0; i < tree.size(); i++)
//            {
//                String strId = "";
//
//                //获节点svid
//                strId = tree.get(i).get("sv_id");
//
//                if (tree.get(i).get("type").equals("monitor"))
//                {
//                    if (strId.indexOf(strSvid) == 0)
//                    {
//                        //Group Entity IP
//                        value.put("monitor" + String.valueOf(k) + "State", tree.get(i).get("status"));
//                        value.put("monitor" + String.valueOf(k) + "Des", tree.get(i).get("sv_name") + " " + tree.get(i).get("dstr"));
//                        value.put("monitor" + String.valueOf(k) + "Svid", tree.get(i).get("sv_id"));
//                        k++;
//                    }
//                    else
//                    {
//                        //
//                    }
//                }
//            }
        }
        
		return value;
	}
//	
//	//QueryNameInfo
//	public static Map<String, Map<String, String>> QueryNameInfo()
//	{
//		QueryInfo q = new QueryInfo();
//		
//		q.needkey= "sv_name";
//		q.setNeedType_all();
//		
//		try
//		{
//			return q.load();
//		}
//		catch(Exception e)
//		{
//			e.printStackTrace();
//		}
//		
//		return  null;
//	}
//	
//	//QueryMachineNameInfo
//	public static Map<String, Map<String, String>> QueryMachineNameInfo()
//	{
//		QueryInfo q= new QueryInfo();
//		
//		q.needkey= "sv_name,_MachineName";
//		q.setNeedType_all();
////		q.setNeedType_entity();
//		
//		try
//		{
//			return q.load();
//		}
//		catch(Exception e)
//		{
//			e.printStackTrace();
//		}
//		
//		return  null;
//	}
	
	//////////////////////取拓扑图数据函数///////////////////////////////////
	
	
	//////////////////////文件操作函数///////////////////////////////////////
	
	//替换文件内容， 解决java的replaceall用正则表达式带来的问题
	public static String myReplace(String strSrc, String strOld, String strNew)
	{		
		StringBuffer strSrcBuf = new StringBuffer("");
		String strTmpBuf = "";
		strSrcBuf.append(strSrc);
		
		if(strSrcBuf.indexOf(strOld) != -1)
		{
			strTmpBuf = strSrcBuf.replace(strSrcBuf.indexOf(strOld), strSrcBuf.indexOf(strOld) + strOld.length(), strNew).toString();			
		}
		
		return strTmpBuf;
	}
	
	//替换文件内容， 解决java的replaceall用正则表达式带来的问题
	public static String myReplaceAll(String source, String toReplace, String replacement)
	{
			int idx = source.lastIndexOf( toReplace );
			if (idx != -1) 
			{
				StringBuffer ret = new StringBuffer(source);
				ret.replace(idx, idx+toReplace.length(), replacement);
				while((idx=source.lastIndexOf(toReplace, idx-1)) != -1) 
				{
					ret.replace( idx, idx+toReplace.length(), replacement );
				}
				source = ret.toString();
			}

			return source;
	}
	
	/**  
	 * 新建目录  
	 * @param folderPath 目录  
	 * @return 返回目录创建后的路径  
	 */  
	public static String createFolder(String folderPath) {   
	    String txt = folderPath;   
	    try {   
	        java.io.File myFilePath = new java.io.File(txt);   
	        txt = folderPath;   
	        if (!myFilePath.exists()) {   
	            myFilePath.mkdir();   
	        }   
	    }   
	    catch (Exception e) {   
//	        message = "创建目录操作出错";   
	    }   
	    return txt;   
	}   
	   
	/**  
	 * 多级目录创建  
	 * @param folderPath 准备要在本级目录下创建新目录的目录路径 例如 c:myf  
	 * @param paths 无限级目录参数，各级目录以单数线区分 例如 a|b|c  
	 * @return 返回创建文件后的路径 例如 c:myfac  
	 */  
	public String createFolders(String folderPath, String paths){   
	    String txts = folderPath;   
	    try{   
	        String txt;   
	        txts = folderPath;   
	        StringTokenizer st = new StringTokenizer(paths,"|");   
	        for(int i=0; st.hasMoreTokens(); i++){   
	                txt = st.nextToken().trim();   
	                if(txts.lastIndexOf("/")!=-1){    
	                    txts = createFolder(txts+txt);   
	                }else{   
	                    txts = createFolder(txts+txt+"/");       
	                }   
	        }   
	   }catch(Exception e){   
	       message = "创建目录操作出错！";   
	       }   
	        return txts;   
	    }   
	  
	 /**  
	 * 有编码方式的文件创建  
	 * @param filePathAndName 文本文件完整绝对路径及文件名  
	 * @param fileContent 文本文件内容  
	 * @param encoding 编码方式 例如 GBK 或者 UTF-8  
	 * @return  
	 */  
	public void createFile(String filePathAndName, String fileContent, String encoding) {
		
			    PrintWriter myFile=null;
			    try {   
			        String filePath = filePathAndName;   
			        filePath = filePath.toString();   
			        File myFilePath = new File(filePath);   
			        if (!myFilePath.exists()) {   
			            myFilePath.createNewFile();   
			        }   
			        myFile = new PrintWriter(myFilePath,encoding);   
			        String strContent = fileContent;   
			        myFile.println(strContent);   
			        
			    }   
			    catch (Exception e) {   
			        message = "创建文件操作出错";   
			        }
			    finally
			    {
			    	try{myFile.close();}catch(Exception e){}
			    }       
	    }    
	  
   /**  
	 * 删除文件夹  
	 * @param folderPath 文件夹完整绝对路径  
	 * @return  
	 */  
	public static void delFolder(String folderPath) {   
	    try {   
	        delAllFile(folderPath); //删除完里面所有内容   
	        String filePath = folderPath;   
	        filePath = filePath.toString();   
	        java.io.File myFilePath = new java.io.File(filePath);   
	        myFilePath.delete(); //删除空文件夹   
	    }   
	    catch (Exception e) {   
//	        message = ("删除文件夹操作出错");   
	    }   
	}   
	   
	   
	/**  
	 * 删除指定文件夹下所有文件  
	 * @param path 文件夹完整绝对路径  
	 * @return  
	 * @return  
	 */  
	public static boolean delAllFile(String path) {   
	 boolean bea = false;   
	    File file = new File(path);   
	    if (!file.exists()) {   
	        return bea;   
	    }   
	    if (!file.isDirectory()) {   
	        return bea;   
	    }   
	    String[] tempList = file.list();   
	    File temp = null;   
	    for (int i = 0; i < tempList.length; i++) {   
	        if (path.endsWith(File.separator)) {   
	            temp = new File(path + tempList[i]);   
	        }else{   
	            temp = new File(path + File.separator + tempList[i]);   
	        }   
	        if (temp.isFile()) {   
	            temp.delete();   
	        }   
	        if (temp.isDirectory()) {   
	            delAllFile(path+"/"+ tempList[i]);//先删除文件夹里面的文件   
	            delFolder(path+"/"+ tempList[i]);//再删除空文件夹   
	                bea = true;   
	            }   
	        }   
	        return bea;   
	    }   
	  
	  
	/**  
	 * 复制整个文件夹的内容  
	 * @param oldPath 准备拷贝的目录  
	 * @param newPath 指定绝对路径的新目录  
	 * @return  
	 */  
	public static void copyFolder(String oldPath, String newPath) {   
	    try {   
	        new File(newPath).mkdirs(); //如果文件夹不存在 则建立新文件夹   
	        File a=new File(oldPath);   
	        String[] file=a.list();   
	        File temp=null;   
	        for (int i = 0; i < file.length; i++) {   
	            if(oldPath.endsWith(File.separator)){   
	                temp=new File(oldPath+file[i]);   
	            }else{   
	                temp=new File(oldPath+File.separator+file[i]);   
	            }   
	            if(temp.isFile()){   
	                FileInputStream input = null;   
	                FileOutputStream output = null;   
	                
	                try
	                {
 	                input = new FileInputStream(temp);   
	                output = new FileOutputStream(newPath + "/" + (temp.getName()).toString());   
	                byte[] b = new byte[1024 * 5];   
	                int len;   
	                while ((len = input.read(b)) != -1) {   
	                    output.write(b, 0, len);   
	                }   
	                output.flush();   
	              }finally{
	                try{output.close(); }catch(Exception e){}
	                try{input.close();  }catch(Exception e){}
	              }
	            }   
	            if(temp.isDirectory()){//如果是子文件夹   
	                copyFolder(oldPath+"/"+file[i],newPath+"/"+file[i]);   
	            }   
	        }   
	    }catch (Exception e) {   
//	        message = "复制整个文件夹内容操作出错";   
	        }   
	    }   
	  
	  
	 /**  
	 * 移动文件  
	 * @param oldPath  
	 * @param newPath  
	 * @return  
	 */  
		public void moveFile(String oldPath, String newPath) {   
		    copyFile(oldPath, newPath);   
		    delFile(oldPath);   
		}   
	       
	  
	 /**  
	 * 移动目录  
	 * @param oldPath  
	 * @param newPath  
	 * @return  
	 */  
	public void moveFolder(String oldPath, String newPath) {   
	    copyFolder(oldPath, newPath);   
	    delFolder(oldPath);   
	}   
	public String getMessage(){   
	    return this.message;   
	}  
	    
    /**
     * 复制单个文件
     * @param oldPathFile 准备复制的文件源
     * @param newPathFile 拷贝到新绝对路径带文件名
     * @return
     */
    public static void copyFile(String oldPathFile, String newPathFile) 
    {
    	InputStream inStream=null;
    	FileOutputStream fs =null;
        try {
            int bytesum = 0;
            int byteread = 0;
            File oldfile = new File(oldPathFile);
            if (oldfile.exists()) { //文件存在时
                inStream = new FileInputStream(oldPathFile); //读入原文件
                fs = new FileOutputStream(newPathFile);
                
			                byte[] buffer = new byte[1444];
			                while((byteread = inStream.read(buffer)) != -1){
			                    bytesum += byteread; //字节数 文件大小
			//                    logger.info(bytesum);
			                    fs.write(buffer, 0, byteread);
			                }

            }
        }
        catch (Exception e) 
        {
//            message = ("复制单个文件操作出错");
        }
        finally
        {
        	try{inStream.close();}catch(Exception e){};
        	try{fs.close();}catch(Exception e){};
        }
    }
    
    /**
     * 删除文件
     * @param filePathAndName 文本文件完整绝对路径及文件名
     * @return Boolean 成功删除返回true遭遇异常返回false
     */
    public static boolean delFile(String filePathAndName) 
    {
    	boolean bea = false;
        try 
        {
            String filePath = filePathAndName;
            File myDelFile = new File(filePath);
            if(myDelFile.exists()){
             myDelFile.delete();
             bea = true;
            }
            else
            {
             bea = false;
//             message = (filePathAndName+"删除文件操作出错");
            }
        }
        catch (Exception e) 
        {
//            message = e.toString();
        }
        
        return bea;
    }   
    
    /**
     * 新建文件
     * @param filePathAndName 文本文件完整绝对路径及文件名
     * @param fileContent 文本文件内容
     * @return
     */
    public void createFile(String filePathAndName, String fileContent) 
    {
     
        try {
            String filePath = filePathAndName;
            filePath = filePath.toString();
            File myFilePath = new File(filePath);
            if (!myFilePath.exists()) {
                myFilePath.createNewFile();
            }
//            FileWriter resultFile = new FileWriter(myFilePath);
//            PrintWriter myFile = new PrintWriter(resultFile);

						FileOutputStream fos=null;
						OutputStreamWriter osw=null;
            PrintWriter myFile = null;
            try
            {
						fos=new FileOutputStream(myFilePath);
						osw=new OutputStreamWriter(fos, "UTF-8");
            myFile = new PrintWriter(osw);

            String strContent = fileContent;            
            myFile.println(strContent);
          }finally
            {
	            try{myFile.close();}catch(Exception r){}
	            try{osw.close();}catch(Exception r){}
	            try{fos.close();}catch(Exception r){}
	            
	          }
//            resultFile.close();
        }
        catch (Exception e) 
        {
//            message = "创建文件操作出错";
        }
    }    
    /**
     * 读取文本文件内容
     * @param filePathAndName 带有完整绝对路径的文件名
     * @param encoding 文本文件打开的编码方式
     * @return 返回文本文件的内容
     */
    public String readTxt(String filePathAndName,String encoding) throws IOException
    {
	     encoding = encoding.trim();
	     StringBuffer str = new StringBuffer("");	     
	     String st = "";
	     FileInputStream fs=null;
	     InputStreamReader isr=null;
	     try
	     {
	      fs = new FileInputStream(filePathAndName);
	      if(encoding.equals("")){
	       isr = new InputStreamReader(fs);
	      }else{
	       isr = new InputStreamReader(fs,encoding);
	      }
	      BufferedReader br =null;
	      try
	      {
	       br= new BufferedReader(isr);	  
	       String data = "";
	       while((data = br.readLine())!=null)
	       {
	         str.append(data+"\r\n");	         
	       }
	      }
	      catch(Exception e)
	      {
	       str.append(e.toString());	       
	      }finally{
	    	  try{br.close();}catch(Exception e){}
	      }
	      st = str.toString();
	     }
	     catch(IOException es)
	     {
	      st = "";
	     }finally
	     {
	     	try{isr.close();}catch(Exception e){}
	     	try{fs.close();}catch(Exception e){}
	    }
	     
	     return st;     
    } 
    
	//////////////////////文件操作函数///////////////////////////////////////    
}


//解决xml带名称空间读写需要的Provider
class NamespaceContextProvider implements NamespaceContext 
{
    String boundPrefix, boundURI;

    NamespaceContextProvider(String prefix, String URI) {
        boundPrefix = prefix;
        boundURI = URI;
    }

    public String getNamespaceURI(String prefix) {
        if(prefix.equals(boundPrefix)) {
            return boundURI;
        } else if(prefix.equals(XMLConstants.XML_NS_PREFIX)) {
            return XMLConstants.XML_NS_URI;
        } else if(prefix.equals(XMLConstants.XMLNS_ATTRIBUTE)) {
            return XMLConstants.XMLNS_ATTRIBUTE_NS_URI;
        } else {
            return XMLConstants.DEFAULT_NS_PREFIX;
        }
    }

    public String getPrefix(String namespaceURI) {
        if(namespaceURI.equals(boundURI)) {
            return boundPrefix;
        } else if(namespaceURI.equals(XMLConstants.XML_NS_URI)) {
            return XMLConstants.XML_NS_PREFIX;
        } else if(namespaceURI.equals(XMLConstants.XMLNS_ATTRIBUTE_NS_URI)) {
            return XMLConstants.XMLNS_ATTRIBUTE;
        } else {
            return null;
        }
    }
    
    public Iterator getPrefixes(String namespaceURI) {
        // not implemented for the example
        return null;
    }    
}