package com.dragonflow.erlangecc.monitor;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;

import java.lang.management.MemoryUsage;
import java.math.BigDecimal;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Formatter;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
 
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanInfo;
import javax.management.MBeanServerConnection;
import javax.management.ObjectInstance;
import javax.management.ObjectName;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import COM.datachannel.xml.om.Document;

import com.dragonflow.siteview.informix.DbInformix;
import com.dragonflow.siteview.infra.util.ServicePlatform;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class TomcatMonitor extends BaseMonitor {

	String server = "192.168.6.197";
	String port = "8999";
	String userName = "";
	String password = "";
	StringBuffer lastError = new StringBuffer();
	OtpErlangList EMPTY_ERLANG_LIST_STRING = new OtpErlangList();
	Map<String, Object> Counters = new HashMap<String, Object>();

	public void initArgs(Map<String, Object> map) {
		String s = null;
		try {
			s = (String) map.get("server");
			if (s != null && s.length() > 0) {
				this.server = s.trim();
			}
			s = (String) map.get("port");
			if (s != null && s.length() > 0) {
				this.port = s.trim();
			}
			s = (String) map.get("userName");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.userName = s.trim();
			}
			s = (String) map.get("password");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.password = s.trim();
			}
			Object o = map.get("counters");
			if (o != null && (o instanceof OtpErlangList)) {
				OtpErlangList counters = (OtpErlangList) o;
				if (counters.arity() > 0) {
					OtpErlangObject[] objs = counters.elements();
					for (OtpErlangObject e : objs) {
						if (e instanceof OtpErlangTuple) {
							OtpErlangTuple t = (OtpErlangTuple) e;
							String id = ((OtpErlangString) t.elementAt(0))
									.stringValue();
							String value = ((OtpErlangString) t.elementAt(1))
									.stringValue();
							this.Counters.put(id, value);
						}
					}
				}
			} else {
				this.Counters.put("MALFORMED COUNTER ID", null);
			}

		} catch (Exception e) {
			lastError.append(e.getMessage());
			e.printStackTrace();
		}

	}
	public HashMap<String, String> GetCounters()
	{
		HashMap<String, String> countersMap=new HashMap<String, String>();
		try {  
			 
	           String jmxURL = "service:jmx:rmi:///jndi/rmi://"+this.server+":"+this.port+"/jmxrmi";//tomcat jmx url  
	           JMXServiceURL serviceURL = new JMXServiceURL(jmxURL);  
	             
	           Map map = new HashMap();  
	           String[] credentials = new String[] { this.userName , this.password };  
	           map.put("jmx.remote.credentials", credentials);   
	           JMXConnector connector = JMXConnectorFactory.connect(serviceURL, map);  
	           MBeanServerConnection  mbsc = connector.getMBeanServerConnection();  
	           //-------------------- Session ---------------   
	           ObjectName managerObjName = new ObjectName("Catalina:type=Manager,*");  
	           Set<ObjectName> s=mbsc.queryNames(managerObjName, null);  
	           int sessionCount=0;
	           int activesessions=0;
	           String maxActiveSessions="";
	           for (ObjectName obj:s){  
	               ObjectName objname=new ObjectName(obj.getCanonicalName());  
	               sessionCount=sessionCount+Integer.parseInt(mbsc.getAttribute( objname, "sessionCounter").toString());
	               activesessions=activesessions+Integer.parseInt(mbsc.getAttribute( objname, "activeSessions").toString());
	               maxActiveSessions= mbsc.getAttribute( objname, "maxActiveSessions").toString();
	           }  
	           countersMap.put("sessionCounter",sessionCount+"");
	           countersMap.put("activeSessions", activesessions+"");
	           countersMap.put("maxActiveSessions", maxActiveSessions+"");
	           //Process user time  requestProcessingTime ,worker=http*,name=HttpRequest*
//	           ObjectName ProcessObjName = new ObjectName("Catalina:type=RequestProcessor,*");  
//	           Set<ObjectName> ss=mbsc.queryNames(ProcessObjName, null);  
//	           int ProcessUserTime=0;
//	           int count=0;
//	           for (ObjectName obj:ss){  
//	               ObjectName objname=new ObjectName(obj.getCanonicalName());  
//	               ProcessUserTime=ProcessUserTime+Integer.parseInt(mbsc.getAttribute( objname, "requestProcessingTime").toString());
//	               count++;
//	           }  
//	           countersMap.put("ProcessUserTime", ProcessUserTime/count+"");
	           //------------------------ JVM -------------------------  
	           //堆使用率  
	           ObjectName heapObjName = new ObjectName("java.lang:type=Memory");  
	           MemoryUsage heapMemoryUsage =  MemoryUsage.from((CompositeDataSupport)mbsc.getAttribute(heapObjName, "HeapMemoryUsage"));  
	           long initMemory= heapMemoryUsage.getInit();
	           long maxMemory = heapMemoryUsage.getMax();//堆最大  
	           long commitMemory = heapMemoryUsage.getCommitted();//堆当前分配  
	           long usedMemory = heapMemoryUsage.getUsed();  
	           long freeMemory=commitMemory-usedMemory;
	           BigDecimal bd=new BigDecimal(initMemory/1024/1024.0); 
			   bd=bd.setScale(2, BigDecimal.ROUND_FLOOR); 
	           countersMap.put("initMemory",   bd.doubleValue()+"");
	           bd=new BigDecimal(maxMemory/1024/1024.0); 
			   bd=bd.setScale(2, BigDecimal.ROUND_FLOOR); 
	           countersMap.put("maxMemory",   bd.doubleValue()+"");
	           bd=new BigDecimal(commitMemory/1024/1024.0); 
			   bd=bd.setScale(2, BigDecimal.ROUND_FLOOR); 
	           countersMap.put("totalMemory",   bd.doubleValue()+"");
	           bd=new BigDecimal(freeMemory/1024/1024.0); 
			   bd=bd.setScale(2, BigDecimal.ROUND_FLOOR); 
	           countersMap.put("freeMemory",  bd.doubleValue()+"");
	             
	           //----------------- Thread Pool ----------------  name=http*
	           ObjectName threadpoolObjName = new ObjectName("Catalina:type=ThreadPool,*");  
	           Set<ObjectName> s1=mbsc.queryNames(threadpoolObjName, null);  
	           String maxThreads="";
	           String currentThreads="";
	           String currentThreadsBusy="";
	           for (ObjectName obj:s1){  
	               ObjectName objname=new ObjectName(obj.getCanonicalName());  
	               maxThreads=mbsc.getAttribute( objname, "maxThreads").toString();
	               currentThreads=mbsc.getAttribute( objname, "currentThreadCount").toString();
	               currentThreadsBusy=mbsc.getAttribute( objname, "currentThreadsBusy").toString(); 
	               if(obj.getCanonicalName().indexOf("http")!=-1)
	               {
	            	   countersMap.put("httpMaxThreads", maxThreads);
	    	           countersMap.put("httpCurrentThreadCount", currentThreads);
	    	           countersMap.put("httpCurrentThreadsBusy", currentThreadsBusy); 
	               }
	               if(obj.getCanonicalName().indexOf("ajp")!=-1)
	               {
	            	   countersMap.put("jkMaxThreads", maxThreads);
	    	           countersMap.put("jkCurrentThreadCount", currentThreads);
	    	           countersMap.put("jkCurrentThreadsBusy", currentThreadsBusy);
	               }
	               if(obj.getCanonicalName().indexOf("jk")!=-1)
	               {
	            	   countersMap.put("jkMaxThreads", maxThreads);
	    	           countersMap.put("jkCurrentThreadCount", currentThreads);
	    	           countersMap.put("jkCurrentThreadsBusy", currentThreadsBusy);
	               }
	           }  
	           
	           //----------------- GlobalRequestProcessor ---------------- 
	           ObjectName RequestProcessorObjName = new ObjectName("Catalina:type=GlobalRequestProcessor,*");  
	           Set<ObjectName> s2=mbsc.queryNames(RequestProcessorObjName, null);  
	           String requestCount="";
	           String errorCount="";
	           String processingTime="";
	           String bytesReceived="";
	           String bytesSent="";
	           long temp=0;
	           for (ObjectName obj:s2){  
	               ObjectName objname=new ObjectName(obj.getCanonicalName());  
	               requestCount=mbsc.getAttribute( objname, "requestCount").toString();
	               errorCount=mbsc.getAttribute( objname, "errorCount").toString();
	               temp=Long.parseLong(mbsc.getAttribute( objname, "bytesReceived").toString());
	               bd=new BigDecimal(temp/1024/1024.0); 
				   bd=bd.setScale(2, BigDecimal.ROUND_FLOOR); 
	               bytesReceived= bd.doubleValue()+""; 
	               
	               temp=Long.parseLong(mbsc.getAttribute( objname, "bytesSent").toString());
	               bd=new BigDecimal(temp/1024/1024.0); 
				   bd=bd.setScale(2, BigDecimal.ROUND_FLOOR); 
				   bytesSent= bd.doubleValue()+""; 
				   
				   temp=Long.parseLong(mbsc.getAttribute( objname, "processingTime").toString());
	               bd=new BigDecimal(temp/1000.0); 
				   bd=bd.setScale(3, BigDecimal.ROUND_FLOOR); 
				   processingTime= bd.doubleValue()+""; 
				   if(obj.getCanonicalName().indexOf("http")!=-1)
	               {
					   countersMap.put("httpRequestCount", requestCount);
			           countersMap.put("httpErrorCount", errorCount);
			           countersMap.put("httpBytesReceived", bytesReceived);
			           countersMap.put("httpBytesSent", bytesSent);
			           countersMap.put("httpProcessingTime", processingTime);
	               }
	               if(obj.getCanonicalName().indexOf("ajp")!=-1)
	               {
	            	   countersMap.put("jkRequestCount", requestCount);
	    	           countersMap.put("jkErrorCount", errorCount);
	    	           countersMap.put("jkBytesReceived", bytesReceived);
	    	           countersMap.put("jkBytesSent", bytesSent); 
	    	           countersMap.put("jkProcessingTime", processingTime);
	               }
	               if(obj.getCanonicalName().indexOf("jk")!=-1)
	               {
	            	   countersMap.put("jkRequestCount", requestCount);
	    	           countersMap.put("jkErrorCount", errorCount);
	    	           countersMap.put("jkBytesReceived", bytesReceived);
	    	           countersMap.put("jkBytesSent", bytesSent); 
	    	           countersMap.put("jkProcessingTime", processingTime);
	               }
	           }  
	         
	           System.out.println(countersMap);
	        } catch (Exception e) {  
	            e.printStackTrace();  
	        }  

		return countersMap;
		
	}

	public Map<String, Object> getBrowseData() {
		Map<String, Object> res_map = new HashMap<String, Object>();
		Document document = new Document();

		StringBuffer xml = new StringBuffer();
		xml.append("<browse_data>");
		xml.append("<object name=\"").append("browseData").append("\"");
		xml.append(" id=\"").append("0").append("\"");
		xml.append(">");

		xml.append("<object name=\"").append("Sessions").append("\"");
		xml.append(" id=\"").append("1").append("\"");
		xml.append(">");
		// counter

		xml.append("<counter name=\"").append("最大活动会话数").append("\"");
		xml.append(" id=\"").append("maxActiveSessions").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("活动会话数").append("\"");
		xml.append(" id=\"").append("activeSessions").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("当前会话数").append("\"");
		xml.append(" id=\"").append("sessionCounter").append("\"");
		xml.append(">");
		xml.append("</counter>");

		//
		xml.append("</object>");

		xml.append("<object name=\"").append("JVM性能").append("\"");
		xml.append(" id=\"").append("2").append("\"");
		xml.append(">");
		// counter

		xml.append("<counter name=\"").append("最大内存(MB)").append("\"");
		xml.append(" id=\"").append("maxMemory").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("总计内存(MB)").append("\"");
		xml.append(" id=\"").append("totalMemory").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("剩余内存(MB)").append("\"");
		xml.append(" id=\"").append("freeMemory").append("\"");
		xml.append(">");
		xml.append("</counter>");

		//
		xml.append("</object>");

		xml.append("<object name=\"").append("HTTP性能").append("\"");
		xml.append(" id=\"").append("3").append("\"");
		xml.append(">");
		// counter
		xml.append("<counter name=\"").append("http处理时间(s)").append("\"");
		xml.append(" id=\"").append("httpProcessingTime").append("\"");
		xml.append(">");
		xml.append("</counter>");
		
		xml.append("<counter name=\"").append("当前线程数").append("\"");
		xml.append(" id=\"").append("httpCurrentThreadCount").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("当前忙碌线程").append("\"");
		xml.append(" id=\"").append("httpCurrentThreadsBusy").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("最大线程").append("\"");
		xml.append(" id=\"").append("httpMaxThreads").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("请求统计数").append("\"");
		xml.append(" id=\"").append("httpRequestCount").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("错误统计数").append("\"");
		xml.append(" id=\"").append("httpErrorCount").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("接受数据(MB)").append(
				"\"");
		xml.append(" id=\"").append("httpBytesReceived").append("\"");
		xml.append(">");
		xml.append("</counter>");
		
		xml.append("<counter name=\"").append("发送数据(MB)").append(
		"\"");
        xml.append(" id=\"").append("httpBytesSent").append("\"");
        xml.append(">");
        xml.append("</counter>");

		//
		xml.append("</object>");

		xml.append("<object name=\"").append("JK性能").append("\"");
		xml.append(" id=\"").append("4").append("\"");
		xml.append(">");
		// counter
		xml.append("<counter name=\"").append("JK处理时间(s)").append("\"");
		xml.append(" id=\"").append("jkProcessingTime").append("\"");
		xml.append(">");
		xml.append("</counter>");
		
		xml.append("<counter name=\"").append("当前线程数").append("\"");
		xml.append(" id=\"").append("jkCurrentThreadCount").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("当前忙碌线程").append("\"");
		xml.append(" id=\"").append("jkCurrentThreadsBusy").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("最大线程").append("\"");
		xml.append(" id=\"").append("jkMaxThreads").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("请求统计数").append("\"");
		xml.append(" id=\"").append("jkRequestCount").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("错误统计数").append("\"");
		xml.append(" id=\"").append("jkErrorCount").append("\"");
		xml.append(">");
		xml.append("</counter>");

		xml.append("<counter name=\"").append("接受数据(MB)").append(
				"\"");
		xml.append(" id=\"").append("jkBytesReceived").append("\"");
		xml.append(">");
		xml.append("</counter>");
		
		xml.append("<counter name=\"").append("发送数据(MB)").append(
		"\"");
        xml.append(" id=\"").append("jkBytesSent").append("\"");
        xml.append(">");
        xml.append("</counter>");

		//
		xml.append("</object>");

	
		xml.append("</object>");
		xml.append("</browse_data>");
		document.loadXML(xml.toString());
		Element root = (Element) document.getFirstChild();
		buildTreemap(res_map, root, "");
		return res_map;
	
	}
	public void buildTreemap(Map<String, Object> res_map, Element node,
			String parentName) {
		if (node == null || !node.hasChildNodes()) {
			return;
		}
		NodeList tnodelist = node.getChildNodes();
		try {
			for (int j = 0; j < tnodelist.getLength(); j++) {
				Element tempNode = (Element) tnodelist.item(j);
				String type = tempNode.getTagName().toLowerCase();
				String id = "";
				id = tempNode.getAttribute("id");
				String name = tempNode.getAttribute("name").replace("/", "-");
				if (parentName == "") {
					if (!id.isEmpty()) {
						if (!res_map.containsKey(id)) {
							if (type.equals("counter"))
								res_map.put(id, name);
							// res_map.put(name1, name);
							else
								res_map.put(id, name);
						}

					}
				} else {

					if (!id.isEmpty()) {
						if (!res_map.containsKey(id)) {
							if (type.equals("counter"))
								// res_map.put(parentName1+name1,parentName+name);
								res_map.put(id, parentName + name);
							else
								res_map.put(id, parentName + name);
						}
					}
				}

				buildTreemap(res_map, tempNode, parentName + name + "/");

			}
		} catch (Exception r) {
			System.out.println(r);
		}

	}
	public Map<String, Object> update() {

		Map<String, Object> result = new HashMap<String, Object>();
		this.initArgs(this.getMessage().getParams());
		HashMap<String, String> cmap = GetCounters();
		
		int len = this.Counters.size();
		if (cmap.containsKey("error")) {
			this.lastError.append(cmap.get("error"));
			result.put("pStatus","error");
			result.put("stateString", "update fail");
			result.put("countersInError", len);
		} else {
			result.put("pStatus","ok");
			int errorCount = 0;
			StringBuffer statestring = new StringBuffer();
			int j=0;
			for (String s : this.Counters.keySet()) {
				String s1 = this.Counters.get(s).toString();// .replace("-",
				// "/");
                String value="";
				if (cmap.containsKey(s)) {
					String v = cmap.get(s);
					value=v;
					try {
						int V1 = Integer.parseInt(v);
						result.put(s1, V1);
						result.put(s, V1);
					} catch (Exception ex) {
						try
						{
						Float Vf=Float.parseFloat(v);
						result.put(s1, Vf);
						result.put(s, Vf);
						}catch(Exception ex1)
						{
					
						 result.put(s1, v);
						 result.put(s, v);
						 
						}
					}

				} else {
					++errorCount;
					value="n/a";
					result.put(s1, "n/a");
				}
				if (value.equals(""))
					statestring.append(s1).append(" = \"\"");
				else {
					statestring.append(s1).append(" = ").append(value);
				}

				if (j != len - 1)
					statestring.append(", ");
				++j;
			}
			result.put("stateString", statestring.toString());
			result.put("countersInError", errorCount);

		}
		return result;
	}


	@Override
	public int handleMessage() {
		// TODO Auto-generated method stub
		String action = this.getMessage().getAction();
		String error = null;

		Map<String, Object> resp = null;

		if (action != null && action.equals("update")) {
			resp = this.update();
			error = this.lastError.toString();
			ArrayList<String> R = new ArrayList<String>();
			if (lastError.length() > 0) {
				R.add(error);
				this.sendResponse3("error", R);
			} else {
				System.out.println("update result: " + resp);
				this.sendResponse2("ok", resp);
			}

		} else if (action != null && action.equals("getBrowseData")) {
			resp = this.getBrowseData();
			error = this.lastError.toString();
			if (error != null && error.length() > 0) {
				resp = new HashMap<String, Object>();
				resp.put("error", ServicePlatform.replaceUnicodeChar(error));
				this.sendResponse("error", resp);
			} else {
				System.out.println("getBrowseData data result: \n" + resp);
				this.sendResponse2("ok", resp);
			}
		}
		return 0;
	}
//	public static void main(String[] args) {
//		TomcatMonitor dd=new TomcatMonitor();
//		dd.GetCounters();
//	}
}
