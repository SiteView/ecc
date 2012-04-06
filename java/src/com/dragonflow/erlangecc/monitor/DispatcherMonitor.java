/*
 * 
 * Created on 2005-2-15 12:55:26
 *
 * DispatcherMonitor.java
 *
 * History:
 *
 */
package com.dragonflow.erlangecc.monitor;

/**
 * Comment for <code>DispatcherMonitor</code>
 * 
 * @author
 * @version 0.0
 * 
 * 
 */
import java.util.Enumeration;
import java.util.Map;
import java.text.NumberFormat;
import java.util.ArrayList;
//import java.util.HashMap;
//import java.util.Iterator;
//import java.util.List;
//import java.util.Set;

import com.dragonflow.erlangecc.util.SvMessage;
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
import jgl.HashMap;
//import COM.dragonflow.HTTP.HTTPRequest;
//import COM.dragonflow.Log.LogManager;
//import COM.dragonflow.Properties.NumericProperty;
//import COM.dragonflow.Properties.StringProperty;
//import COM.dragonflow.Utils.Base64Encoder;
//import COM.dragonflow.Utils.TextUtils;

// Referenced classes of package COM.dragonflow.SiteView:
// AtomicMonitor, DispatcherConnection, MasterConfig, TopazInfo

public class DispatcherMonitor extends BaseMonitor 
{
	public static String NAME_PROPERTY;

    public static String ID_PROPERTY;

    public static String TERMINATE_MON = "TERMINATE_MON";

    public static String TERMINATE = "TERMINATE";

    public static String INIT_MON = "INIT_MON";

    public static String EDIT_MON = "UPDATE_CONN";

    public static String INIT_CONN = "INIT_CONN";

    public static String TERMINATE_CONN = "TERMINATE_CONN";

    public static String UPDATE = "UPDATE";

    public static String DISP_VERSION = "1.0";

    public static String GET_BROWSE_DATA = "GET_BROWSE_DATA";
    
	public static final String COUNTERS_IN_ERROR = "counters_in_error";

//    public static StringProperty pServerName;
//
//    public static StringProperty pCountersInError;

    public static String pServerName;
    
    public static String pCountersInError;
    
    public static int nMaxCounters;

    static DispatcherConnection dispConnection = new DispatcherConnection();

    static boolean base64EncodedPasswords = false;

    public DispatcherMonitor() 
    {
    	
    }

    protected boolean update(Map<String, Object> counters) 
    {
        StringBuffer stringbuffer = new StringBuffer("");
        String s = "";
        HashMap hashmap = getUpdateData(stringbuffer);
        if (stringbuffer.length() > 0) {
            if (stringbuffer.toString().equals("-1")) {
                s = "Dispatcher Communication Failure";
//                setProperty(pNoData, "n/a");
            } else {
                s = stringbuffer.toString();
            }
//            setProperty(pCountersInError, nMaxCounters);
            counters.put(COUNTERS_IN_ERROR, 30);
            
        } else {
//            s = saveCounters(hashmap);
            int i = 0;
            HashMap hashResults = (HashMap)hashmap.get("results");
            if (hashResults != null) {
                for (Enumeration enumeration = hashResults.keys(); enumeration
                        .hasMoreElements();) {
                    String s1 = (String) enumeration.nextElement();
                    if(counters.containsKey(s1))
                    {
                    	String s2 = (String) hashResults.get(s1);                
                    	counters.put(s1, s2);
                    }
                }
            }
            
            HashMap hashErrors = (HashMap)hashmap.get("errors");
            if (hashErrors != null) {
                for (Enumeration enumeration = hashErrors.keys(); enumeration
                        .hasMoreElements();) {
                    String s1 = (String) enumeration.nextElement();
                    if(counters.containsKey(s1))
                    {
                    	i++;
                    	String s2 = (String) hashErrors.get(s1);                
                    	counters.put(s1, "Error: " + s2);
                    }
                }
            }
            counters.put(COUNTERS_IN_ERROR, i);        	
        }
//        setProperty(pStateString, s);
//        LogManager.log("RunMonitor", "DispatcherMonitor update w/ errorStr ("
//                + stringbuffer + ") and state (" + s + ").");

        return true;
    }

//    public boolean isUsingCountersCache() 
//    {
//        return false;
//    }

    public String getBrowseData(StringBuffer stringbuffer) 
    {
//        notifyDispatcherMonitor(INIT_CONN, this);
    	
    	notifyDispatcherMonitor(INIT_CONN, null);
    	
        String s = getData(dispConnection, GET_BROWSE_DATA, null);
        if (s == null) {
            stringbuffer.append("Unknown error retrieving counters");
            return "";
        }
        String s1 = s.trim();
        long l = -1L;
        try {
            int i = s1.indexOf(" ");
            s1 = s1.substring(i + 1);
            i = s1.indexOf(" ");
            l = (new Long(s1.substring(0, i))).longValue();
            s1 = s1.substring(i + 1);
        } catch (Exception exception) {
//            LogManager.log("error",
//                    "DispatcherMonitor.getBrowseData Exception, "
//                            + exception.getMessage());
//            LogManager.log("RunMonitor",
//                    "DispatcherMonitor.getBrowseData Exception, "
//                            + exception.getMessage());
            stringbuffer.append("Format error in xml document");
            return "";
        }
        if (l != 0L) {
            stringbuffer.append(s1);
            return "";
        }
        if (s1.indexOf("<browse_data") < 0) {
            stringbuffer.append("Unknown error retrieving counters");
            return "";
        } else {
            return s1;
        }
    }

    public HashMap getUpdateData(StringBuffer stringbuffer) {
        HashMap hashmap = new HashMap();
        int i = 0;
        do {
            if (i >= nMaxCounters) {
                break;
            }
//            String s1 = getProperty(ID_PROPERTY + (i + 1));
//            if (s1.length() <= 0) {
//                break;
//            }
//            hashmap.add(ID_PROPERTY + (i + 1), s1);
            i++;
        } while (true);
        String s = getData(dispConnection, UPDATE, hashmap);
        return parseResultBuffer(s, stringbuffer);
    }

    String getConnectionParamStr(HashMap hashmap) {
        Object obj = null;
        String s = "";
        if (hashmap != null) {
            for (Enumeration enumeration = hashmap.keys(); enumeration
                    .hasMoreElements();) {
                String s1 = (String) enumeration.nextElement();
                String s2 = (String) hashmap.get(s1);
                s = s + s1.length() + " " + s1 + s2.length() + " " + s2;
            }
        }
        s = s.length() + " " + s;
        return s;
    }

    /**
     * CAUTION: Decompiled by hand.
     * 
     * @param hashmap
     * @return
     */
    public String getCounterParamStr(HashMap hashmap) {
        Object obj = null;
        String s = "";
        if (hashmap != null) {
            Enumeration enumeration = hashmap.elements();
            while (enumeration.hasMoreElements()) {
                String s1 = (String) enumeration.nextElement();
                if (s1.length() > 0) {
                    s = s + s1.length() + " " + s1;
                }
            } 
        }
        return s.length() + " " + s;
    }

    /**
     * CAUTION: Decompiled by hand.
     * 
     * @param hashmap
     * @return
     */
    public String getCounterParamStr() 
    {
    	return "";
    }
    
    public static void enableBase64Encoding() {
        base64EncodedPasswords = true;
    }

    public static void disableBase64Encoding() {
        base64EncodedPasswords = false;
    }

    public static boolean isBase64Encoded() {
        return base64EncodedPasswords;
    }

    public String getData(DispatcherConnection dispatcherconnection, String s,
            HashMap hashmap) {
        HashMap hashmap1 = new HashMap();
        
        //ת��Ϊ��
        Array array = getConnectionProperties();
        Array keys = getConnectionKeys();
//        for (int i = 0; i < array.size(); i++) {
//            String s2 = ((StringProperty) array.at(i)).getName();
//            String s3 = getProperty(s2);
//            if (((StringProperty) array.at(i)).isPassword && isBase64Encoded()) {
//                s3 = (new Base64Encoder(s3)).processString();
//            }
//            hashmap1.add(s2.substring(1), s3);
//        }

      for (int i = 0; i < array.size(); i++) {
      String s2 = array.at(i).toString();
      String s3 = keys.at(i).toString();
//      if (((StringProperty) array.at(i)).isPassword && isBase64Encoded()) {
//          s3 = (new Base64Encoder(s3)).processString();
//      }      
      hashmap1.add(s3, s2);
      }
        
//        String s1 = (String) getClassProperty("topazName");
        String s1 = "Tuxedo";
        
        return dispatcherRequest(dispatcherconnection, s, s1, hashmap1,
                hashmap, getFullID());
    }

    public String getFullID()
    {
    	return "0";
    }
    
     String dispatcherRequest(DispatcherConnection dispatcherconnection,
            String s, String s1, HashMap hashmap, HashMap hashmap1, String s2) {
//        LogManager.log("RunMonitor", "DispatcherMonitor Enter getData, opCode="
//                + s + ", monName=" + s1);\
        String s3 = null;
        String s4 = getConnectionParamStr(hashmap);
        String s5 = "";
        if(s.equals(UPDATE))
        {
        	s5 = getCounterParamStr().replace("-", "/");
        }
        else
        {
        	s5 = getCounterParamStr(hashmap1);
        }	
        
        String s6 = DISP_VERSION.length() + " " + DISP_VERSION + s.length()
                + " " + s + s1.length() + " " + s1;
        String s7 = s2.length() + " " + s2 + s4 + s5;
        s6 = s6 + s7.length() + " " + s7;
        s3 = dispatcherconnection.connectToDispatcher(s6.length() + " " + s6);
//        LogManager.log("RunMonitor", "DispatcherMonitor Exit getData, opCode="
//                + s + ", monName=" + s1);
        return s3;
    }

    public static HashMap parseResultBuffer(String s, StringBuffer stringbuffer) {
        HashMap hashmap = new HashMap();
        HashMap hashmap1 = new HashMap();
        try {
            String s1 = s;
            int i = s1.indexOf(" ");
            s1 = s1.substring(i + 1);
            i = s1.indexOf(" ");
            long l = (new Long(s1.substring(0, i))).longValue();
            s1 = s1.substring(i + 1);
            if (l == 0L) {
                int j = s1.indexOf(" ");
                s1 = s1.substring(j + 1);
                j = s1.indexOf(" ");
                long l1 = (new Long(s1.substring(0, j))).longValue();
                s1 = s1.substring(j + 1);
                String s2 = "";
                if (l1 > 0L) {
                    s2 = s1.substring(0, (int) l1);
                    s1 = s1.substring((int) l1);
                }
                j = s1.indexOf(" ");
                if (j < 0) {
                    j = s1.length();
                }
                long l2 = (new Long(s1.substring(0, j))).longValue();
                String s3 = "";
                if (l2 > 0L) {
                    s1 = s1.substring(j + 1);
                    s3 = s1.substring(0, (int) l2);
                }
                getResults(s2, hashmap, true);
                getResults(s3, hashmap1, false);
            } else {
                stringbuffer.append(s1);
                hashmap = new HashMap();
                hashmap1 = new HashMap();
            }
        } catch (Exception exception) {
            hashmap = new HashMap();
            hashmap1 = new HashMap();
            stringbuffer.append("Format error in results buffer");
//            LogManager.log("error",
//                    "DispatcherMonitor.parseResultBuffer Exception, "
//                            + exception.getMessage());
        }
        HashMap hashmap2 = new HashMap();
        hashmap2.put("results", hashmap);
        hashmap2.put("errors", hashmap1);
        return hashmap2;
    }

    static void getResults(String s, HashMap hashmap, boolean flag) {
        if (s.length() > 0) {
            String s1;
            String s2;
            for (; s.length() > 0; hashmap.add(s1, s2)) {
                int i = s.indexOf(" ");
                long l = (new Long(s.substring(0, i))).longValue();
                s1 = s.substring(i + 1, i + (int) l + 1);
                s = s.substring(i + (int) l + 1);
                i = s.indexOf(" ");
                l = (new Long(s.substring(0, i))).longValue();
                s2 = s.substring(i + 1, i + (int) l + 1);
                if (flag) {
                    s2 = s2.replace(',', '.');
                }
                s = s.substring(i + (int) l + 1);
            }

        }
    }

//    String saveCounters(HashMap hashmap) {
//        int i = 0;
//        for (int j = 1; j <= nMaxCounters; j++) {
//            setProperty("value" + j, "n/a");
//        }
//
//        HashMap hashmap1 = (HashMap) hashmap.get("results");
//        String s = "";
//        for (int k = 1; k <= nMaxCounters; k++) {
//            String s1 = getProperty(ID_PROPERTY + k);
//            if (s1.length() <= 0) {
//                break;
//            }
//            String s2 = (String) hashmap1.get(s1);
//            if (s2 == null) {
//                continue;
//            }
//            setProperty("value" + k, s2);
//            if (s.length() > 0) {
//                s = s + ", ";
//            }
//            s = s + getProperty(NAME_PROPERTY + k) + "=" + s2;
//        }
//
//        HashMap hashmap2 = (HashMap) hashmap.get("errors");
//        for (int l = 1; l <= nMaxCounters; l++) {
//            String s3 = getProperty(ID_PROPERTY + l);
//            if (s3.length() <= 0) {
//                break;
//            }
//            String s4 = (String) hashmap2.get(s3);
//            if (s4 == null) {
//                continue;
//            }
//            setProperty("value" + l, "Error: " + s4);
//            i++;
//            if (s.length() > 0) {
//                s = s + ", ";
//            }
//            s = s + getProperty(NAME_PROPERTY + l) + "=" + s4;
//        }
//
//        setProperty(pCountersInError, i);
//        return s;
//    }
//
//    public String verify(StringProperty stringproperty, String s,
//            HTTPRequest httprequest, HashMap hashmap) {
//        if (stringproperty == pServerName) {
//            if (s.trim().length() == 0) {
//                hashmap.put(stringproperty, stringproperty.getLabel()
//                        + " missing");
//            }
//            return s;
//        } else {
//            return super.verify(stringproperty, s, httprequest, hashmap);
//        }
//    }
//
//    public String getHostname() {
//        return getProperty(pServerName);
//    }
//
//    public synchronized HashMap getLabels() {
//        HashMap hashmap = new HashMap();
//        for (int i = 0; i < nMaxCounters; i++) {
//            hashmap.add("Counter " + (i + 1) + " Value",
//                    getProperty(NAME_PROPERTY + (i + 1)));
//        }
//
//        return hashmap;
//    }
//
//    public String getPropertyName(StringProperty stringproperty) {
//        String s = stringproperty.getName();
//        String s1 = TextUtils.getValue(getLabels(), stringproperty.getLabel());
//        if (s1.length() == 0) {
//            s1 = s;
//        }
//        return s1;
//    }
//
//    public String GetPropertyLabel(StringProperty stringproperty, boolean flag) {
//        String s = stringproperty.printString();
//        if (stringproperty == pCountersInError) {
//            return s;
//        }
//        String s1 = TextUtils.getValue(getLabels(), s);
//        if (s1.length() != 0) {
//            return s1;
//        }
//        if (flag) {
//            return "";
//        } else {
//            return s;
//        }
//    }
//
//    public Array getLogProperties() {
//        Array array = super.getLogProperties();
//        array.add(pCountersInError);
//        for (int i = 0; i < nMaxCounters
//                && getProperty(NAME_PROPERTY + (i + 1)).length() > 0; i++) {
//            array.add(getPropertyObject("value" + (i + 1)));
//        }
//
//        return array;
//    }
//
//    public Enumeration getStatePropertyObjects(boolean flag) 
//    {
//        Array array = new Array();
//        array.add(pCountersInError);
//        for (int i = 0; i < nMaxCounters
//                && getProperty(NAME_PROPERTY + (i + 1)).length() > 0; i++) {
//            array.add(getPropertyObject("value" + (i + 1)));
//        }
//
//        return array.elements();
//    }

    public Array getConnectionProperties() {
        Array array = new Array();
//        array.add(pServerName);
        return array;
    }
    
    public Array getConnectionKeys()
    {
        Array array = new Array();        
//        array.add("server");
        return array;
    }

    
    public void notifyDispatcherMonitor(String s, HashMap atomicmonitor) 
    {
//        if (!atomicmonitor.isDispatcher()) {
//            return;
//        }
        HashMap hashmap = new HashMap();
//        int i = 0;
//        do {
//            if (i >= nMaxCounters) {
//                break;
//            }
//            String s1 = atomicmonitor.getProperty(ID_PROPERTY + (i + 1));
//            if (s1.length() <= 0) {
//                break;
//            }
//            hashmap.add(ID_PROPERTY + (i + 1), s1);
//            i++;
//        } while (true);
//        ((DispatcherMonitor) atomicmonitor).getData(dispConnection, s, hashmap);
        this.getData(dispConnection, s, hashmap);
    }

    public void killDispatcher() {
        dispatcherRequest(dispConnection, "TERMINATE", "", null, null, "");
    }

//    public int getMaxCounters() {
//        return nMaxCounters;
//    }
//
//    public void setMaxCounters(int i) {
//        nMaxCounters = i;
//        HashMap hashmap = MasterConfig.getMasterConfig();
//        hashmap.put("_DispatcherMaxCounters", (new Integer(i)).toString());
//        MasterConfig.saveMasterConfig(hashmap);
//    }
//
//    public String getBrowseName() {
//        return NAME_PROPERTY;
//    }
//
//    public String getBrowseID() {
//        return ID_PROPERTY;
//    }
//
//    public String setBrowseName(Array array) {
//        String s = "";
//        if (array.size() <= 0) {
//            return s;
//        }
//        for (int i = array.size() - 1; i >= 0; i--) {
//            if (s.length() > 0) {
//                s = s + "/";
//            }
//            s = s + (String) array.at(i);
//        }
//
//        return s;
//    }
//
//    public String setBrowseID(Array array) 
//    {
//        String s = "";
//        if (array.size() <= 0) {
//            return s;
//        }
//        for (int i = array.size() - 1; i >= 0; i--) {
//            String s1 = (String) array.at(i);
//            s = s + s1.length() + " " + s1;
//        }
//
//        return s;
//    }

//    public int getCostInLicensePoints() {
//        int i = 0;
//        for (int j = 0; j < nMaxCounters
//                && getProperty(NAME_PROPERTY + (j + 1)).length() > 0; j++) {
//            i++;
//        }
//
//        return i * 1;
//    }

    @Override
	public SvMessage getMessage() {
		// TODO Auto-generated method stub
		return super.getMessage();
	}

	@Override
	public void run() 
	{
		this.handleMessage();
	}

	void initArgs(Map<String, Object> map) 
	{

	}
	
	@Override
	public int handleMessage() 
	{
		return 0;
	}
    
    public static void main(String args[]) 
    {
    	
    }

//    static 
//    {
//        NAME_PROPERTY = "_browseName";
//        ID_PROPERTY = "_browseNameid";
//        nMaxCounters = 10;
//        HashMap hashmap = MasterConfig.getMasterConfig();
//        if (TextUtils.getValue(hashmap, "_DispatcherBase64Encoding").length() > 0) {
//            enableBase64Encoding();
//        }
//        nMaxCounters = TextUtils.toInt(TextUtils.getValue(hashmap,
//                "_DispatcherMaxCounters"));
//        if (nMaxCounters <= 0) {
//            nMaxCounters = 30;
//        }
//        Array array = new Array();
//        pServerName = new StringProperty("_server");
//        pServerName.setDisplayText("Server", "the name of the server");
//        pServerName.setParameterOptions(false, 4, false);
//        array.add(pServerName);
//        StringProperty astringproperty[] = new StringProperty[nMaxCounters];
//        StringProperty astringproperty1[] = new StringProperty[nMaxCounters];
//        StringProperty astringproperty2[] = new StringProperty[nMaxCounters];
//        for (int i = 0; i < nMaxCounters; i++) {
//            astringproperty[i] = new StringProperty(NAME_PROPERTY + (i + 1));
////            astringproperty[i].setDisplayText("Counter " + (i + 1) + " Name",
////                    TopazInfo.getTopazName() + " Counter Name");
//            astringproperty[i].setParameterOptions(false, i + 4, false);
//            array.add(astringproperty[i]);
//            astringproperty1[i] = new StringProperty(ID_PROPERTY + (i + 1));
////            astringproperty1[i].setDisplayText("Counter " + (i + 1) + " ID",
////                    TopazInfo.getTopazName() + " Counter ID");
//            astringproperty1[i].setParameterOptions(false,
//                    nMaxCounters + i + 4, false);
//            array.add(astringproperty1[i]);
//            astringproperty2[i] = new NumericProperty("value" + (i + 1));
////            astringproperty2[i].setDisplayText("Counter " + (i + 1) + " Value",
////                    TopazInfo.getTopazName() + " Counter Value");
//            astringproperty2[i].setStateOptions(i + 1);
//            astringproperty2[i].setIsThreshold(true);
//            array.add(astringproperty2[i]);
//        }
//
//        pCountersInError = new NumericProperty("countersInError");
//        pCountersInError.setLabel("counters in error");
//        pCountersInError.setStateOptions(nMaxCounters + 1);
//        pCountersInError.setIsThreshold(true);
//        array.add(pCountersInError);
//        StringProperty astringproperty3[] = new StringProperty[array.size()];
//        for (int j = 0; j < array.size(); j++) {
//            astringproperty3[j] = (StringProperty) array.at(j);
//        }
//
//        addProperties("COM.dragonflow.SiteView.DispatcherMonitor",
//                astringproperty3);
//    }    

}
