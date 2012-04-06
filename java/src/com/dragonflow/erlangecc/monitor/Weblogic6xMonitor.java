package com.dragonflow.erlangecc.monitor;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

import com.dragonflow.erlangecc.monitor.NonDeferringClassLoader;
import com.dragonflow.erlangecc.util.SvMessage;
import com.dragonflow.siteview.infra.util.ServicePlatform;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class Weblogic6xMonitor extends BaseMonitor{
	private String lastError;
	
	
	public int getBrowseData(Map<String,Object> map,String host,String user,String passwd,Boolean secure,String webJarDir){
		NonDeferringClassLoader nondeferringclassloader = null;
		String path = "file:///" + System.getProperty("user.dir") + File.separator + "bin" + File.separator;
		URLClassLoader urlclassloader;
		try {
			urlclassloader = new URLClassLoader(new URL[] { new URL(path) });
			nondeferringclassloader = new NonDeferringClassLoader(new URL[] { new URL("file:///" + System.getProperty("user.dir") + File.separator + "plugin" + File.separator+ webJarDir) }, urlclassloader);
			ClassLoader classloader = Thread.currentThread().getContextClassLoader();
            if (classloader != nondeferringclassloader) {
                Thread.currentThread().setContextClassLoader(nondeferringclassloader);
            }
            System.out.println("Detected WebLogic Version: " + nondeferringclassloader.loadClass("weblogic.version").getMethod("getReleaseBuildVersion", null).invoke(null, null));
            Weblogic6xMonitorImpl wlm = new Weblogic6xMonitorImpl(host,user,passwd,secure);
    		StringBuffer sb = new StringBuffer();
    		String ret =wlm.getBrowseData(map);
            Thread.currentThread().setContextClassLoader(classloader);
            if (ret.equals("OK")){
            	return 0;
            }else{
            	lastError = ret;
            	return -2;
            }
		} catch (MalformedURLException e) {
			lastError = e.getMessage();
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			lastError = e.getMessage();
			e.printStackTrace();
		} catch (SecurityException e) {
			lastError = e.getMessage();
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			lastError = e.getMessage();
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			lastError = e.getMessage();
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			lastError = e.getMessage();
			e.printStackTrace();
		} catch (ClassNotFoundException e) {
			lastError = e.getMessage();
			e.printStackTrace();
		}
		return -1;
	}
	
	public int getValues(String[] desc,String[] counters,Object[] values,String host,String user,String passwd,Boolean secure,String webJarDir){
		NonDeferringClassLoader nondeferringclassloader = null;
		String path = "file:///" + System.getProperty("user.dir") + File.separator + "bin" + File.separator;
		URLClassLoader urlclassloader;
		try {
			urlclassloader = new URLClassLoader(new URL[] { new URL(path) });
			nondeferringclassloader = new NonDeferringClassLoader(new URL[] { new URL("file:///" + System.getProperty("user.dir") + File.separator + "plugin" + File.separator + webJarDir) }, urlclassloader);
			ClassLoader classloader = Thread.currentThread().getContextClassLoader();
            if (classloader != nondeferringclassloader) {
                Thread.currentThread().setContextClassLoader(nondeferringclassloader);
            }
            System.out.println("Detected WebLogic Version: " + nondeferringclassloader.loadClass("weblogic.version").getMethod("getReleaseBuildVersion", null).invoke(null, null));
            Weblogic6xMonitorImpl wlm = new Weblogic6xMonitorImpl(host,user,passwd,secure);
    		StringBuffer sb = new StringBuffer();
    		String Ret =wlm.update(desc, counters, values);
            Thread.currentThread().setContextClassLoader(classloader);
            if (Ret.equals("OK")){
            	return 0;
            }else{
            	lastError = Ret;
            	return -2;
            }
		} catch (MalformedURLException e) {
			lastError = e.getMessage();
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			lastError = e.getMessage();
			e.printStackTrace();
		} catch (SecurityException e) {
			lastError = e.getMessage();
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			lastError = e.getMessage();
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			lastError = e.getMessage();
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			lastError = e.getMessage();
			e.printStackTrace();
		} catch (ClassNotFoundException e) {
			lastError = e.getMessage();
			e.printStackTrace();
		}
		return -1;
	}
	
	public String getLastError() {
		return lastError;
	}

	@Override
	public int handleMessage() {
		SvMessage msg = this.getMessage();
		if (msg.getAction().equals("getBrowseData")){
			Map<String,Object> params = msg.getParams();
			Map<String,Object> counters =new LinkedHashMap<String,Object>();
			String host = (String)params.get("host");
			String usr = (String)params.get("usr");
			String pwd = (String)params.get("pwd");
			Boolean scure = new Boolean(false);
			Boolean.parseBoolean((String)params.get("secure"));
			

			String weblogicJar = (String)params.get("weblogicJar");
			if (0==getBrowseData(counters,host, usr, pwd, scure, weblogicJar)){
				this.sendResponse2("ok", counters);
			}else{
				if(this.getLastError().length()>250)
					this.sendResponse(ServicePlatform.replaceUnicodeChar(this.getLastError().substring(0,250)), counters);
				else
					this.sendResponse(ServicePlatform.replaceUnicodeChar(this.getLastError()), counters);
			}
		}else if(msg.getAction().equals("getValues")){
			Map<String,Object> params = msg.getParams();
			String host = (String)params.get("host");
			String usr = (String)params.get("usr");
			String pwd = (String)params.get("pwd");
			Boolean scure = new Boolean(false);
			Boolean.parseBoolean((String)params.get("secure"));
			String weblogicJar = (String)params.get("weblogicJar");
			
			OtpErlangList counters = (OtpErlangList)params.get("counter");
			OtpErlangObject[] objs = counters.elements();
			Map<String,Object> rets =new LinkedHashMap<String,Object>();
			
			Object[] vals = new Object[counters.elements().length];
			String[] ids = new String[counters.elements().length];
			String[] des = new String[counters.elements().length];
			
			for (int i = 0;i < counters.elements().length; i++) {
				OtpErlangObject obj =counters.elementAt(i);
				if (obj instanceof OtpErlangTuple) {
					OtpErlangTuple tup = (OtpErlangTuple) obj;
					ids[i]=((OtpErlangString)tup.elementAt(0)).stringValue();
					des[i]=((OtpErlangString)tup.elementAt(1)).stringValue();
				}else{
					ids[i]="";
					des[i]="";
				}
			}
			
			if (0==getValues(des,ids,vals,host,usr,pwd,scure,weblogicJar)){
				for (int i = 0;i< ids.length; i++){
					rets.put(ids[i], vals[i]);
				}
				this.sendResponse2("ok", rets);
			}else{
				if(this.getLastError().length()>250)
					this.sendResponse(ServicePlatform.replaceUnicodeChar(this.getLastError().substring(0,250)), rets);
				else
					this.sendResponse(ServicePlatform.replaceUnicodeChar(this.getLastError()), rets);
			}
		}
		
		return 0;
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		
		Weblogic6xMonitor wm = new Weblogic6xMonitor();
		Map<String,Object> map = new HashMap<String,Object>();
		int  Ret = wm.getBrowseData(map,"192.168.0.137:7001", "system", "weblogic", false, "weblogic.ja");
		Iterator<Map.Entry<String, Object>> e = map.entrySet().iterator();
		while (e.hasNext()){
			Map.Entry<String, Object> i = e.next();
			System.out.println(i.getKey()+"****"+ i.getValue());
		}
	
//		System.out.println(System.getProperty("user.dir"));

	}

}
