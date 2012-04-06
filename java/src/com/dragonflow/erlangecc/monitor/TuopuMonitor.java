package com.dragonflow.erlangecc.monitor;

import java.io.File;
import java.io.FileInputStream;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.dbcp.BasicDataSource;
import org.apache.commons.lang.StringEscapeUtils;
import org.apache.commons.lang.SystemUtils;
import org.apache.commons.lang.math.NumberUtils;

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


public class TuopuMonitor extends BaseMonitor 
{
	public static final String EMPTY_ERLANG_LIST_STRING = (new OtpErlangList()).toString();
	StringBuffer lastError = new StringBuffer("");
	
	String strName = "";
	String strRoot = "";
	String strApp = "";
	
	Map<String, Map<String, Object>> mapMonitors = null;
	Map<String, Map<String, Object>> mapGroups = null;
	Map<String, Map<String, Object>> mapMachines = null;	
	public TuopuMonitor() 
	{
		
	}
    
	@Override
	public void run() 
	{
		this.handleMessage();
	}

	@Override
	public int handleMessage() 
	{
//		System.out.println("TuopuMonitor raw message from erlang: handleMessage");		
		String action = this.getMessage().getAction();
		String error = null;
		Map<String, Object> resp = null;

		if (action != null && action.equals("update")) 
		{
			resp = this.MakeTuopu();
			error = this.lastError.toString();
			if (error != null && error.length() > 0) 
			{
				resp = new HashMap<String, Object>();
				resp.put("error", ServicePlatform.replaceUnicodeChar(error));
				this.sendResponse("error", resp);
			} 
			else 
			{
				resp.put("name", (Object)this.strName);
				this.sendResponse2("ok", resp);
			}
		}
		
		return 0;
	}
	
	public Map<String, Object> MakeTuopu()
	{
		this.initArgs(this.getMessage().getParams());
		new MakeTuopuData(strRoot, strApp, strName, mapGroups, mapMachines, mapMonitors);
		return new HashMap<String, Object>();
	}
	
	public Map<String, Map<String, Object>> AnalysisTuopuData(Map<String, Object> map, String strIn)
	{
		Map<String, Map<String, Object>> tmpMap = new HashMap();
		Object o = map.get(strIn);
		if (o != null && (o instanceof OtpErlangList)) 
		{				
			OtpErlangList counters = (OtpErlangList) o;
			if (counters.arity() > 0) 
			{
				OtpErlangObject[] objs = counters.elements();
				for (OtpErlangObject e : objs)
				{						
					if (e instanceof OtpErlangTuple)
					{
						OtpErlangTuple t = (OtpErlangTuple) e;
						String strId = ((OtpErlangString) t.elementAt(0)).stringValue();
						OtpErlangList Datas = (OtpErlangList)t.elementAt(1);
						Map<String,Object> tmpObj = ErlangUtils.erlangListToMap((OtpErlangList)Datas);
						tmpMap.put(strId, tmpObj);
					}
				}
			}
		}
		else 
		{
			//
		}
		
		return tmpMap;
	}
	
	void initArgs(Map<String, Object> map) 
	{
		String s = null;
		
		try 
		{
			s = (String) map.get("name");
			if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) 
			{
				this.strName = s.trim();
			}
			
			s = (String) map.get("rootpath");
			if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) 
			{
				this.strRoot = s.trim();
			}

			s = (String) map.get("apppath");
			if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) 
			{
				this.strApp = s.trim();
			}
			
			this.mapGroups = AnalysisTuopuData(map, "groupinfo");
			this.mapMachines = AnalysisTuopuData(map, "machineinfo");
			this.mapMonitors = AnalysisTuopuData(map, "monitorinfo");
		}
		catch (Exception e) 
		{
//			this.lastError.append(e.getMessage() + "; <br>");
			e.printStackTrace();
		}
	}

	public static void main(String[] args) 
	{
//		TuopuMonitor dr = new TuopuMonitor();
	}
}
