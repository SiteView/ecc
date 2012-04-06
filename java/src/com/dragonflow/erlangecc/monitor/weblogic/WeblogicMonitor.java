package com.dragonflow.erlangecc.monitor.weblogic;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;


import com.dragonflow.erlangecc.monitor.BaseMonitor;
import com.dragonflow.erlangecc.monitor.NonDeferringClassLoader;
import com.dragonflow.erlangecc.monitor.Weblogic6xMonitor;
import com.dragonflow.erlangecc.monitor.Weblogic6xMonitorImpl;
import com.dragonflow.erlangecc.util.Base64Encoder;
import com.dragonflow.erlangecc.util.SvMessage;
import com.dragonflow.siteview.infra.util.ServicePlatform;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class WeblogicMonitor extends BaseMonitor {
	private String lastError;
	
	
	public Map<String,Object> getBrowseData(String host,String user,String passwd,Boolean secure,String webJarDir,String wlCipherJar,String jvm,String classpath,String license){
		lastError = "";
		WebLogicServer remoteServer = getWebLogicServer(host,user,passwd,secure,webJarDir, wlCipherJar,jvm,classpath,license,false);
		if (remoteServer != null){
			String Ret;
			try {
				return remoteServer.getBrowseData();
				
			} catch (RemoteException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				lastError = e.getMessage();
			}
			
		}
		return null;
	}
	
	public Object[] getValues(String[] desc,String[] counters,String host,String user,String passwd,Boolean secure,String webJarDir,String wlCipherJar,String jvm,String classpath,String license){
		lastError = "";
		WebLogicServer remoteServer = getWebLogicServer(host,user,passwd,secure,webJarDir, wlCipherJar,jvm,classpath,license,false);
		if (remoteServer != null){

			try {
				return remoteServer.update(desc, counters);
				
			} catch (RemoteException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				lastError = e.getMessage();
			}
			
		}
		return null;
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
			Boolean scure = Boolean.parseBoolean((String)params.get("secure"));
			String wlCipherJar = (String)params.get("wlCipherJar");
			String jvm = (String)params.get("jvm");
			String classpath = (String)params.get("classpath");
			String license = (String)params.get("license");
			

			String weblogicJar = (String)params.get("weblogicJar");
			if (null!=(counters = getBrowseData(host, usr, pwd, scure, weblogicJar,wlCipherJar,jvm,classpath,license))){
				this.sendResponse2("ok", counters);
			}else{
				this.sendResponse("error", getLastError());
			}
		}else if(msg.getAction().equals("getValues")){
			Map<String,Object> params = msg.getParams();
			String host = (String)params.get("host");
			String usr = (String)params.get("usr");
			String pwd = (String)params.get("pwd");
			Boolean scure = Boolean.parseBoolean((String)params.get("secure"));
			String weblogicJar = (String)params.get("weblogicJar");
			String wlCipherJar = (String)params.get("wlCipherJar");
			String jvm = (String)params.get("jvm");
			String classpath = (String)params.get("classpath");
			String license = (String)params.get("license");
			
			OtpErlangList counters = (OtpErlangList)params.get("counter");
			Map<String,Object> rets =new LinkedHashMap<String,Object>();
			
			Object[] vals = null;
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
			
			if (null!=(vals=getValues(des,ids,host,usr,pwd,scure,weblogicJar,wlCipherJar,jvm,classpath,license))){
				for (int i = 0;i< ids.length; i++){
					rets.put(ids[i], vals[i]);
				}
				this.sendResponse2("ok", rets);
			}else{
				this.sendResponse("error", getLastError());
			}
		}
		
		return 0;
	}

	
    private WebLogicServer getWebLogicServer(String host,String user,String passwd,Boolean secure,String webJarDir, String wlCipherJar,String strJVM,String classpath,String strLicense,boolean isGetBrowseDataRequest)
    {
    	String uniqueID = constructHashIDForConnectionProperties(host,user,passwd,"7001","50000",secure.toString());
    	String[] paths = classpath.split(";");

        WeblogicProcessProperties processProps = new WeblogicProcessProperties(webJarDir, wlCipherJar, strLicense, strJVM, paths, isGetBrowseDataRequest);
        WeblogicConnectionProperties connProps = new WeblogicConnectionProperties(host, user, passwd, 50000,secure, uniqueID);
        int status = RMIProcessLauncher.launch(processProps);
        if(status != 0 && status != 1){
        	lastError = "launch RMI Process error";
            return null;
        }
        if(System.getSecurityManager() == null)
            System.setSecurityManager(new NullSecurityManager());
        WeblogicService svc;
        try
        {
            if((svc = getWebLogicService(10000L, processProps.getURL())) != null)
                return svc.getServer(connProps);
        }
        catch(MalformedURLException e)
        {
        	
        	System.err.println(e.toString());
        	e.printStackTrace();
        	lastError = e.getMessage();
            return null;
        }
        catch(RemoteException e)
        {
        	System.err.println(e.toString());
        	e.printStackTrace();
        	lastError = e.getMessage();
            return null;
        }
        return null;
    }
    
    private WeblogicService getWebLogicService(long timeoutMillis, String url)
    throws MalformedURLException, RemoteException
    {
    	long currentTime = (new Date()).getTime();
    	long endTime = currentTime + timeoutMillis;
    	do
    	{
    		if(endTime <= currentTime)
    			break;
    		try
    		{
    			return (WeblogicService)Naming.lookup(url);
    		}
    		catch(NotBoundException e)
    		{
    			currentTime = (new Date()).getTime();
    			lastError = e.getMessage();
    		}
    		try
    		{
    			Thread.sleep(500L);
    		}
    		catch(InterruptedException e)
    		{
    			System.err.println("Exception: " + e.toString());
    			lastError = e.getMessage();
    		}
    	} while(true);
    	return null;
    }
    
    private static String constructHashIDForConnectionProperties(String pServerName,String pUsername,String pPassword,String Port,String pTimeout,String pSecure)
    {
        StringBuffer hashIDStringBuffer = (new StringBuffer(pServerName)).append(":").append(Port).append(":").append(pTimeout).append(":").append(pSecure).append(":");
        String userAndPassword = pUsername + pPassword;
        String userAndPassHash = "";
        try
        {
            MessageDigest usernamePasswordDigest = MessageDigest.getInstance("SHA-1");
            usernamePasswordDigest.update(userAndPassword.getBytes());
            byte sha1Digest[] = usernamePasswordDigest.digest();
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            ByteArrayInputStream bis = new ByteArrayInputStream(sha1Digest);
            Base64Encoder enc = new Base64Encoder(bis, bos);
            enc.process();
            userAndPassHash = bos.toString();
        }
        catch(NoSuchAlgorithmException e)
        {
            System.err.println("WebLogicMonitor.constructHashIDForConnectionProperties() failed due to an exception: " + e.toString());
        }
        catch(IOException e)
        {
        	System.err.println((new StringBuilder()).append("WebLogicMonitor.constructHashIDForConnectionProperties() failed due to an exception: ").append(e).toString());
        }
        return hashIDStringBuffer.append(userAndPassHash).toString();
    }
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		
		WeblogicMonitor wm = new WeblogicMonitor();
		Map<String,Object> map = null;
		map = wm.getBrowseData("192.168.6.168:7002", "weblogic", "weblogic", true, "E:\\erlang\\svIAD\\Server\\java\\plugin\\weblogic_9x.jar","E:\\erlang\\svIAD\\Server\\java\\plugin\\wlcipher_9x.jar", "", "","E:\\temp\\license.bea");
		
		System.out.println("map2 size:" + map.size());
		Iterator<Map.Entry<String, Object>> e = map.entrySet().iterator();
		while (e.hasNext()){
			Map.Entry<String, Object> i = e.next();
			System.out.println(i.getKey()+"/"+ i.getValue());
		}
	
//		System.out.println(System.getProperty("user.dir"));

	}

}
