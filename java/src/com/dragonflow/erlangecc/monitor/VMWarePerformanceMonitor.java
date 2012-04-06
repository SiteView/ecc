package com.dragonflow.erlangecc.monitor;

import java.util.*;

import org.apache.axis.AxisFault;
import java.security.cert.CertificateException;
import javax.net.ssl.SSLHandshakeException;

import com.dragonflow.erlangecc.common.ErrorCode;
import com.dragonflow.erlangecc.util.SvMessage;
import com.dragonflow.siteview.ErlangUtils;
import com.dragonflow.siteview.infra.vmware.Connector;
import com.ericsson.otp.erlang.OtpErlangList;
import com.dragonflow.siteview.infra.vmware.*;

public class VMWarePerformanceMonitor extends BaseMonitor{
	private String lastError;
	
	String WebServiceURL;
	String Username;
	String Password;
	int Timeout;
	
	public VMWarePerformanceMonitor()
    {
    }

    private Connector makeConnector()
    {
        return Connector.getConnector(WebServiceURL, Username, Password, Timeout);
    }

    private void disposeConnector(Connector connector)
    {
        Connector.disposeConnector(connector);
    }	

	int getValues(Map<String,Object> value, Map<String,Object>counters)
	{
        int erroneousCounters;
        StringBuilder statestring;
        Connector conn;
        
        erroneousCounters = 0;
        statestring = new StringBuilder();
        conn = makeConnector();
        
        boolean result;
        List ids = new ArrayList();
        List countername = new ArrayList();
        
		Iterator<Map.Entry<String, Object>> iter = counters.entrySet().iterator();     

        while(iter.hasNext()) {     
		    Map.Entry<String, Object> entry = (Map.Entry<String, Object>)iter.next();     
		    String propId = entry.getKey(); 
		    int Len = propId.length();
		    if(Len>2)
		    	propId = propId.substring(1, Len-1);
            ids.add(propId);
            countername.add(propId);
        }

        try
        {
        	List values = conn.readMetrics(ids);
        	for(int i = 0; i < values.size(); i++)
        	{
        		String val = (String)values.get(i);
        		String key = (String)countername.get(i);
        		value.put(key, val);
            
        		if("unavailable".equals(val))
        		{
        			erroneousCounters++;
        		}
        	}
        	value.put("countersInError", erroneousCounters+"");
        }
        catch(Exception e)
        {
        	disposeConnector(conn);
        	lastError = "Connection error: " + ExceptionTranslator.translate(e);
        	return 1;
        }
        if(erroneousCounters>0)
        	value.put("countersInError", erroneousCounters+"");
        
        return 0;
	}
	
	int getCounters(Map<String,Object> counters)
	{
        Connector conn = makeConnector();

        try
        {
        	String result = conn.buildXML(counters);
        	System.out.println(result);
    		Iterator<Map.Entry<String, Object>> iter = counters.entrySet().iterator();     

        	disposeConnector(conn);
        }
        catch(AxisFault e)
        {
        	if(e.getCause() != null)
        	{
        		if(isUntrustedCertificateProblem(e.getCause()))
        			lastError = "Untrusted Certificate Chain";
        		else
        			lastError = e.getCause().getMessage();
        	} 
        	else
        	{
        		lastError = e.getFaultString();
        	}
        	disposeConnector(conn);
        	return 1;
        }
        catch(Exception e)
        {
        	lastError = e.getMessage();
        	disposeConnector(conn);
        	return 1;
        }
		return 0;
	}
   
    private boolean isUntrustedCertificateProblem(Throwable cause)
    {
        if(cause instanceof SSLHandshakeException)
        {
            Throwable c = cause.getCause();
            if((c instanceof CertificateException) && c.getMessage().toLowerCase().contains("untrusted"))
                return true;
        }
        return false;
    }
    
	public int handleMessage() {
		SvMessage msg = this.getMessage();
		if (msg.getAction().equals("getCounters")){
			Map<String,Object> params = msg.getParams();
			Map<String,Object> counters = new HashMap<String,Object>();
			WebServiceURL = (String)params.get("url");
			Username = (String)params.get("usr");
			Password = (String)params.get("pwd");
			Timeout = Integer.parseInt((String)params.get("timeout"))*1000; 

			if (0==this.getCounters(counters)){
				this.sendResponse2("ok", counters);
			}else{
				String LastErr = this.getLastError();
				if(LastErr == null)
					LastErr = "";
				if((LastErr != null) && (LastErr.length()>250))
					this.sendResponse(LastErr.substring(0,250), counters);
				else
					this.sendResponse(LastErr, counters);
			}
		}else if (msg.getAction().equals("getValues")){
			Map<String,Object> params = msg.getParams();
			Map<String,Object> values = new HashMap<String,Object>();

			WebServiceURL = (String)params.get("url");
			Username = (String)params.get("usr");
			Password = (String)params.get("pwd");
			Timeout = Integer.parseInt((String)params.get("timeout"))*1000; 
			OtpErlangList counterslist = (OtpErlangList)params.get("counters");
			Map<String,Object> counters = ErlangUtils.erlangListToMap(counterslist);
			
			if (0==this.getValues(values,counters)){
				this.sendResponse2("ok", values);
			}else{
				if(this.getLastError().length()>250)
					this.sendResponse(this.getLastError().substring(0,250), values);
				else
					this.sendResponse(this.getLastError(), values);			}
		}
		return ErrorCode.OK;
	}

	public String getLastError() {
		return lastError;
	}
	
	private void setLastError(String Error) {
		lastError = Error;
	}

	public static void main(String[] args) {
		VMWarePerformanceMonitor vmware = new VMWarePerformanceMonitor();
		Map<String,Object> counters = new HashMap<String,Object>();
		//Map<String,Object> values = new HashMap<String,Object>();
//		counters.put("\"rescpu.runav1.latest(absolute)##20#0invpath:/ha-datacenter/host/localhost.localdomain/localhost.localdomain\"", "HostSystem/localhost.localdomain/Realtime/rescpu/runav1.latest[]");
//		ipmi.getValues(values, "192.168.3.2", "623", "root", "root", counters);
		vmware.WebServiceURL = "http://192.168.3.144/sdk";
		vmware.Username = "root";
		vmware.Password = "888888";
		vmware.Timeout = 120*1000000;
		vmware.getCounters(counters);
		//vmware.getValues(values, counters);
		System.out.println(counters);
	}	
}
