/*
 * 
 * Created on 2005-3-7 1:20:16
 *
 * TuxedoMonitor.java
 *
 * History:
 *
 */
package com.dragonflow.erlangecc.monitor;

/**
 * Comment for <code>TuxedoMonitor</code>
 * 
 * @author 
 * @version 0.0
 *
 *
 */

import java.sql.Connection;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import COM.datachannel.xml.om.Document;

import com.dragonflow.siteview.CloseUtils;
import com.dragonflow.siteview.ErlangUtils;
import com.dragonflow.siteview.Strings;
import com.dragonflow.siteview.infra.util.ServicePlatform;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import jgl.Array;
//import jgl.HashMap;

public class TuxedoMonitor extends DispatcherMonitor
{
	String strServer = "";
	String strPort = "";	
	String strTuxClient = "";
	String strTuxData = "";
	String strUsername = "";
	String strPassword = "";
	String strId = "0";
	
	int max_counter = 0;
	public static final String COUNTERS_IN_ERROR = "counters_in_error";
	public static final String EMPTY_ERLANG_LIST_STRING = (new OtpErlangList()).toString();
	public static final String COUNTERS_VALUE = "counters_value";
	public static final long ERROR_STATE = -999;
	public static final String ERROR = "error";
	public static final String STATE_STRING = "state_string";
	public static final String COUNTERS_NAME = "counters_name";
	public static final String INSTANCE_NAME = "instance_name";
	public static final String SID_VALUES = "sid_values";
	public static final String FREE_TABLE_SPACE = "free_table_space";	
	
    static DispatcherConnection dispConnection = new DispatcherConnection();

	Map<String, Object> counters = new HashMap<String, Object>();

	StringBuffer lastError = new StringBuffer("");

    public TuxedoMonitor()
    {
    }

    public boolean isDispatcher()
    {
        return true;
    }

    public Array getConnectionProperties()
    {
        Array array = new Array();
        array.add(strServer);
	    array.add(strPort);
	    array.add(strTuxClient);
	    array.add(strTuxData);
	    array.add(strUsername);
	    array.add(strPassword);
        return array;
    }

    public Array getConnectionKeys()
    {
        Array array = new Array();        
        array.add("server");
        array.add("port");
	    array.add("tuxclient");
	    array.add("tuxdata");
	    array.add("usr");
	    array.add("pwd");
        return array;
    }
    
    public boolean isServerBased()
    {
        return true;
    }

    public String getFullID()
    {
    	return strId;
    }

    public boolean manageBrowsableSelectionsByID()
    {
        return false;
    }

    public boolean areBrowseIDsEqual(String s, String s1)
    {
        if(s == null || s1 == null)
        {
            return false;
        } else
        {
            return s.equals(s1);
        }
    }
    
    public Map<String, Object> update() 
    {
    	Map<String, Object> res_map = new HashMap<String, Object>();
		this.initArgs(this.getMessage().getParams());    	
    	super.update(this.counters);
    	return this.counters;
    }
    
    
    public String getCounterParamStr()
    {
    	if(this.counters.isEmpty())
    		return "";
    	
		Set<String> counter_set = this.counters.keySet();
		int counter_size = this.counters.size();
		String[] states = new String[counter_size];
		Iterator<String> counter_it;
		counter_it = counter_set.iterator();
		int ses_count = 0;
		String strCounter = "";
		for (int i = 0; i < counter_size && counter_it.hasNext(); i++) 
		{
			String counter = counter_it.next();
			String counter_key = counter;
			if(this.counters.get(counter_key) != null)
			{
				String counter_value = this.counters.get(counter_key).toString();
	            if (counter_value.length() > 0) 
	            {
	            	strCounter = strCounter + counter_key.length() + " " + counter_key;
	            }
			}
		}
		
    	return strCounter.length() + " " + strCounter;
    }
    
    private void buildNodeList(Map<String, Object> res_map, Element node, String strParentName)
    {
        if(node == null || !node.hasChildNodes())
        {
            return;
        }
        String strTemp = "";
        NodeList nodelist = node.getChildNodes();
        int i = nodelist.getLength();
        for(int j = 0; j < i; j++)
        {        	
        	Element node1 = (Element)nodelist.item(j);
            if(strParentName.equals(""))
            {
            	if(node1.getAttribute("id") == null)
            	{
            		strTemp = node1.getAttribute("name").replace("/", "-");
	            	res_map.put(strTemp.length() + " " + strTemp, (Object)(node1.getAttribute("name").replace("/", "-")));	            	
            	}
            	else
            	{
            		strTemp = node1.getAttribute("id");
            		res_map.put(strTemp.length() + " " + strTemp, (Object)(node1.getAttribute("name").replace("/", "-")) + "-Counter");
            	}            	
            	buildNodeList(res_map, node1, node1.getAttribute("name").replace("/", "-") + "/");
            }
            else
            {
            	if(node1.getAttribute("id") == null)
            	{
//            		strTemp = strParentName + node1.getAttribute("name").replace("/", "-");
            		strTemp = node1.getAttribute("name").replace("/", "-");
            		res_map.put(strTemp.length() + " " + strTemp, (Object)(strParentName + node1.getAttribute("name").replace("/", "-")));	            	
            	}
            	else
            	{
//            		node1.getNodeName().equals("counter")            		
            		strTemp = node1.getAttribute("id");
            		res_map.put(strTemp.length() + " " + strTemp, (Object)(strParentName + node1.getAttribute("name").replace("/", "-") + "-Counter"));
            	}
            	
            	buildNodeList(res_map, node1, strParentName + node1.getAttribute("name").replace("/", "-") + "/");
            }
        }
    }
    
    Map<String, Object> getBrowseData() 
    {
    	Map<String, Object> res_map = new HashMap<String, Object>();

		this.initArgs(this.getMessage().getParams());
		StringBuffer stringbuffer = new StringBuffer(); 
		String s1 = super.getBrowseData(stringbuffer);
		
		Document document = new Document();
		document.loadXML(s1);
		Element root = (Element) document.getFirstChild();
		buildNodeList(res_map, root, "");
//    	res_map.put("ddee", (Object)"ddee");
//    	res_map.put("ddeecccssxcxc", (Object)"ddee/cccc");
//    	res_map.put("ddeeccccssxcxc", (Object)"ddee/cccc/cxy");
//    	res_map.put("ddeeccccssxcxc1", (Object)"ddee/cccc/cxy1");
//    	res_map.put("ddeecccssxxxxxcxcxvc", (Object)"ddee/xxxxxxxxx");
    	return res_map;
	}
    
	void initArgs(Map<String, Object> map) 
	{		
		String s = null;
		try 
		{
			s = (String) map.get("server");
			if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.strServer = s.trim();
			}
			s = (String) map.get("port");
			if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.strPort = s.trim();
			}
			s = (String) map.get("tuxclient");
			if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.strTuxClient = s.trim();
			}
			s = (String) map.get("tuxdata");
			if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.strTuxData = s.trim();
			}			
			s = (String) map.get("username");
			if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.strUsername = s.trim();
			}
			s = (String) map.get("password");
			if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.strPassword = s.trim();
			}
			s = (String) map.get("id");
			if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.strId = s.trim();
			}			
			s = (String) map.get("max_counter");
			if (s != null && s.length() > 0) {
				this.max_counter = Integer.parseInt(s.trim());
			}			
			Object o = map.get("counters");
			if (o != null && (o instanceof OtpErlangList)) {
				OtpErlangList counters = (OtpErlangList) o;
				if (counters.arity() > 0) {
					OtpErlangObject[] objs = counters.elements();
					for (OtpErlangObject e : objs) {
						if (e instanceof OtpErlangTuple) {
							OtpErlangTuple t = (OtpErlangTuple) e;
							String id = ((OtpErlangString) t.elementAt(0)).stringValue();
							String value = ((OtpErlangString) t.elementAt(1)).stringValue();
							this.counters.put(id, value);
						}
					}
				}
			} else {
				this.counters.put("MALFORMED COUNTER ID", null);
			}
		} catch (Exception e) {
			this.lastError.append(e.getMessage() + "; <br>");
			e.printStackTrace();
		}
	}

	@Override
	public void run() 
	{
		this.handleMessage();
	}

	@Override
	public int handleMessage() 
	{
		String action = this.getMessage().getAction();
		String error = null;

		Map<String, Object> resp = null;

		if (action != null && action.equals("update")) 
		{
			resp = this.update();
			error = this.lastError.toString();
			if (error != null && error.length() > 0) 
			{
				resp = new HashMap<String, Object>();
				resp.put(ERROR, ServicePlatform.replaceUnicodeChar(error));
				this.sendResponse(ERROR, resp);
			}
			else 
			{
				System.out.println("update data result: \n" + resp);
				this.sendResponse2("ok", resp);
			}
		}
		else if (action != null && action.equals("getBrowseData")) 
		{
			resp = this.getBrowseData();
			error = this.lastError.toString();
			if (error != null && error.length() > 0) 
			{
				resp = new HashMap<String, Object>();
				resp.put(ERROR, ServicePlatform.replaceUnicodeChar(error));
				this.sendResponse(ERROR, resp);
			}
			else 
			{
				System.out.println("getBrowseData data result: \n" + resp);
				this.sendResponse2("ok", resp);
			}
		}	
		return 0;
	}
	
    public static void main(String args[])
    {
    	TuxedoMonitor tuxmonitor = new TuxedoMonitor();
    	
    	tuxmonitor.strServer = "192.168.6.166";
    	tuxmonitor.strPort = "4011";
    }	
}
