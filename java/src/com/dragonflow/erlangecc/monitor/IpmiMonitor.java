package com.dragonflow.erlangecc.monitor;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.net.SocketException;

import com.dragonflow.erlangecc.common.ErrorCode;
import com.dragonflow.erlangecc.util.SvMessage;
import com.dragonflow.siteview.ErlangUtils;
import com.ericsson.otp.erlang.OtpErlangList;

import com.dragonflow.siteview.infra.ipmi.interfaces.lan.LANIfs;
import com.dragonflow.siteview.infra.ipmi.*;
import com.dragonflow.siteview.infra.ipmi.util.*;
import com.dragonflow.siteview.infra.ipmi.commands.*;
import com.dragonflow.siteview.infra.util.ServicePlatform;

public class IpmiMonitor extends BaseMonitor{
    private static class CounterReading
    {

        public boolean isError()
        {
            return error;
        }

        public void setError()
        {
            warning = good = false;
            error = true;
        }

        public boolean isWarning()
        {
            return warning;
        }

        public void setWarning()
        {
            error = good = false;
            warning = true;
        }

        public boolean isGood()
        {
            return good;
        }

        public void setGood()
        {
            warning = error = false;
            good = true;
        }

        public String getValue()
        {
            return value;
        }

        public void setValue(String value)
        {
            this.value = value;
        }

        private String value;
        private boolean error;
        private boolean warning;
        private boolean good;



        private CounterReading()
        {
            value = "";
            error = false;
            warning = false;
            good = true;
        }

    }

    private String lastError;

	private IPMITranslator translator;
	private String Hostname;
	private int Port;
	private String Username;
	private String Password;
	
	public IpmiMonitor() {
		super();
	}
	
	public IpmiMonitor(SvMessage message) {
		super(message);
		translator = TranslatorFactory.getTranslator(Locale.ENGLISH);
	}

	int getValues(Map<String,Object> values, String host, String port, String usr, String pwd, Map<String,Object>counters)
	{
        StringBuilder myErrBuff = new StringBuilder();

		Hostname = host;
		Port = Integer.parseInt(port);
		Username = usr;
		Password  = pwd;
		
        HashMap sensorReadingsMap = null;
        Vector vOfSensors = getUniqueSensorsIdsFromCounters(counters);
        if(vOfSensors.size()>0)
        {
        	sensorReadingsMap = readSensors(vOfSensors, myErrBuff);
        	if(sensorReadingsMap == null)
        	{
        		lastError = "Failed to update monitor status: " + myErrBuff.toString();
        		return 1;
        	}
        }
        
        Vector chassisCounterIds = getChassisIdsFromCounters(counters);
        Chassis.ChassisStatus chassisStatus = null;
        if(chassisCounterIds.size() > 0)
        {
        	try
        	{
        		chassisStatus = readChassisStatus(myErrBuff);
        		if(chassisStatus == null)
        		{
        			lastError = "Failed to read chassis status: " + myErrBuff.toString();
        			return 1;
        		}
        	}
        	catch(IPMIException e)
        	{
    			lastError = "Failed to read chassis status: " + myErrBuff.toString();
    			return 1;  
        	}
        }  
        
		Iterator<Map.Entry<String, Object>> iter = counters.entrySet().iterator();     

		int errorCounterNum = 0;
		
        while(iter.hasNext()) {     
		    Map.Entry<String, Object> entry = (Map.Entry<String, Object>)iter.next();     
		    String counter = entry.getKey(); 
		    int Len = counter.length();
		    if(Len>2)
		    	counter = counter.substring(1, Len-1);
		    
		    CounterReading counterReading = null;
		    String sensorId = getSensorIdForCounter(counter);
            if(sensorId != null)
            {
                SensorReadingRecord sensorReadingRecord = (SensorReadingRecord)sensorReadingsMap.get(sensorId);
                counterReading = updateSensorCounter(counter, sensorReadingRecord);
            }
            else if(counter.startsWith("Chassis") && chassisCounterIds.size() > 0)
            {
            	counterReading = updateChassisCounter(entry.getValue().toString(), chassisStatus);
            }
            if(counterReading!=null)
            {
            	values.put(counter, counterReading.value);
            }
            else
            {
            	values.put(counter, "unavailable");
            	errorCounterNum++;
            }
        }
    	values.put("countersInError", errorCounterNum+"");
    	System.out.println("\tJava Node Return Value To Erlang : OK");
		return 0;
	}
	
	int getCounters(Map<String,Object> counters, String host, String port, String usr, String pwd)
	{
		Hostname = host;
		Port = Integer.parseInt(port);
		Username = usr;
		Password  = pwd;
		
        StringBuilder myErrBuff = new StringBuilder();
   
        HashMap allSensorsMap = getSensorsInfo(myErrBuff);
        if(myErrBuff.length()>0)
        {
            lastError = myErrBuff.toString();
            return 1;   
        }
        if(allSensorsMap == null)
        {
            lastError = "Unable to retrieve browse tree: " + myErrBuff.toString();
            return 1;
        }

        HashMap sensorsGroup = hashSensorsInfoByGroup(allSensorsMap);
        
        for(Iterator iter = sensorsGroup.keySet().iterator(); iter.hasNext(); )
        {
            String groupName = (String)iter.next();
            counters.put(groupName, groupName);
            
            Vector groupValues = (Vector)sensorsGroup.get(groupName);
            for(int i = 0; i < groupValues.size(); i++)
            {
                SensorInfoRecord sensorInfoRecord = (SensorInfoRecord)groupValues.get(i);
                String sensorEntityName = (new StringBuilder()).append(" (0x").append(IPMIUtils.byteToHex(sensorInfoRecord.getSensorNumber())).append(") - ").append(translator.getEntityNameById(sensorInfoRecord.getEntityId())).toString();
                String sensorName = (new StringBuilder()).append(new String(sensorInfoRecord.getIDStringBytes())).append(sensorEntityName).append(" (status)").toString();
                String sensorID = (new StringBuilder()).append("").append(sensorInfoRecord.getSensorUniqueIdAsString()).append("_**Status").toString();
                counters.put(sensorID, groupName + "/" + sensorName);
                if(!sensorInfoRecord.isAnalog())
                    continue;
                String sensorValueName = (new StringBuilder()).append(new String(sensorInfoRecord.getIDStringBytes())).append(sensorEntityName).toString();
                String sensorValueID = (new StringBuilder()).append(sensorInfoRecord.getSensorUniqueIdAsString()).append("_**Value").toString();
                if(sensorInfoRecord instanceof FullSensorInfoRecord)
                    sensorValueName = (new StringBuilder()).append(sensorValueName).append(" (").append(((FullSensorInfoRecord)sensorInfoRecord).getUnitsString(Locale.ENGLISH)).append(" )").toString();
                counters.put(sensorValueID, groupName + "/" + sensorValueName);
            }

        }		
        return 0;
	}

  //  @SuppressWarnings("unchecked")
	private HashMap getSensorsInfo(StringBuilder errorStr)
    {
        LANIfs lanIfs = null;
        try
        {
            lanIfs = new LANIfs(Hostname, Port, Username.getBytes(), Password.getBytes());
            lanIfs.init();
        }
        catch(IPMIException e)
        {
        	errorStr.append("Failed to init IPMI lan session: ").append(e.getMessage());
            return null;
        }
        catch(SocketException e)
        {
        	errorStr.append("Failed to init IPMI lan session: ").append(e.getMessage());
        	return null;
        }
        
        SDR sdr;
        try
        {
            sdr = new SDR(lanIfs);
        }
        catch(IPMIException e)
        {
            return null;
        }
        
        Vector vOfRecordInfo = null;
        try
        {
            vOfRecordInfo = sdr.getFullRecords();
        }
        catch(IPMIException e)
        {
            return null;
        }
        
        HashMap tmpSensorsInfoMap = new HashMap();
        for(int i = 0; i < vOfRecordInfo.size(); i++)
        {
            SensorInfoRecord sensorInfoRecord = (SensorInfoRecord)vOfRecordInfo.get(i);
            tmpSensorsInfoMap.put(sensorInfoRecord.getSensorUniqueIdAsString(), sensorInfoRecord);
        }

        try
        {
            lanIfs.end();
        }
        catch(Exception e)
        {
            return null;
        }
        return tmpSensorsInfoMap;
    }	
   
    private HashMap hashSensorsInfoByGroup(HashMap sensorsMap)
    {
        HashMap sensorsGroupedByCategory = new HashMap();
        SensorInfoRecord sensorInfoRecord;
        Vector groupVec;
        
        for(Iterator iter = sensorsMap.values().iterator(); iter.hasNext(); groupVec.add(sensorInfoRecord))
        {
            sensorInfoRecord = (SensorInfoRecord)iter.next();
            byte sensorType = sensorInfoRecord.getSensorType();
            String groupStr = translator.getSensorType(sensorType);
            groupVec = (Vector)sensorsGroupedByCategory.get(groupStr);
            if(groupVec == null)
            {
                groupVec = new Vector();
                sensorsGroupedByCategory.put(groupStr, groupVec);
            }
        }

        return sensorsGroupedByCategory;
    }
    
    private Vector getChassisIdsFromCounters(Map<String, Object> counters)
    {
		Iterator<Map.Entry<String, Object>> iter = counters.entrySet().iterator();     
        HashMap chassisMap = new HashMap();

        while(iter.hasNext()) {     
		    Map.Entry<String, Object> entry = (Map.Entry<String, Object>)iter.next();     
		    String counter = entry.getKey(); 
		    int Len = counter.length();
		    if(Len>2)
		    	counter = counter.substring(1, Len-1);

		    if(!counter.startsWith("Chassis"))
		    	continue;
	        chassisMap.put(counter, null);
        }

        return new Vector(chassisMap.keySet());			
    }
    
    private Chassis.ChassisStatus readChassisStatus(StringBuilder errorStr) throws IPMIException
    {
        LANIfs lanIfs;
        Chassis.ChassisStatus chassisStatus;
       
        lanIfs = null;
        try
        {
            lanIfs = new LANIfs(Hostname, Port, Username.getBytes(), Password.getBytes());
            lanIfs.init();
        }
        catch(SocketException e)
        {
            errorStr.append("Failed to initialize RMCP datagram socket for IPMI LAN session");
            return null;
        }
        catch(IPMIException e)
        {
            errorStr.append("Failed to initialize IPMI LAN session");
            return null;
        }
        chassisStatus = null;
        chassisStatus = Chassis.getChassisStatus(lanIfs);
      
        try
        {
            lanIfs.end();
        }
        // Misplaced declaration of an exception variable
        catch(IPMIException e)
        {
            errorStr.append("Failed to end lan session");
        }
        catch(Exception e)
        {
        	Chassis.ChassisStatus chassisstatus;
        	errorStr.append("Failed to read chassis status info");
        	chassisstatus = null;
        }
        try
        {
            lanIfs.end();
        }
        catch(IPMIException e)
        {
            return null;
        }
        catch(Exception e)
        {
        	return null;
        }
        
        return chassisStatus;
    }

    private Vector getUniqueSensorsIdsFromCounters(Map<String, Object> counters) 
    {
		Iterator<Map.Entry<String, Object>> iter = counters.entrySet().iterator();     
        HashMap sensorsMap = new HashMap();

        while(iter.hasNext()) {     
		    Map.Entry<String, Object> entry = (Map.Entry<String, Object>)iter.next();     
		    String counter = entry.getKey(); 
		    int Len = counter.length();
		    if(Len>2)
		    	counter = counter.substring(1, Len-1);

		    if(counter.startsWith("Chassis"))
		    	continue;
		    String sensorID = getSensorIdForCounter(counter);
		    if(sensorID != null)
	        	sensorsMap.put(sensorID, null);
        }

        return new Vector(sensorsMap.keySet());			
    	
    }
    
    private String getSensorIdForCounter(String counterID)
    {
        String sensorId = null;
        if(counterID.endsWith("_**Status"))
            sensorId = counterID.substring(0, counterID.indexOf("_**Status"));
        else
        if(counterID.endsWith("_**Value"))
            sensorId = counterID.substring(0, counterID.indexOf("_**Value"));
        return sensorId;
    }
    
    private HashMap readSensors(Vector requestedSensorIds, StringBuilder errorStr)
    {
        HashMap sensorsInfoMap;
        LANIfs lanIfs;
        Sensor sensor;
        HashMap sensorReadingInfo;
        StringBuilder error = new StringBuilder();
        sensorsInfoMap = getSensorsInfo(error);
        if(sensorsInfoMap == null)
        {
            errorStr.append("Failed to readSensors: ").append(error).toString();
            return null;
        }
  
        lanIfs = null;
        try
        {
            lanIfs = new LANIfs(Hostname, Port, Username.getBytes(), Password.getBytes());
            lanIfs.init();
        }
        catch(SocketException e)
        {
            return null;
        }
        catch(IPMIException e)
        {
            return null;
        }
        sensor = new Sensor(lanIfs);
        sensorReadingInfo = new HashMap();
        for(int i = 0; i < requestedSensorIds.size(); i++)
        {
            String id = (String)requestedSensorIds.get(i);
            SensorInfoRecord sensorInfoRecord = (SensorInfoRecord)sensorsInfoMap.get(id);
            if(sensorInfoRecord == null)
            {
                errorStr.append("Failed to get SensorInfoRecord for record id: ").append(id).toString();
                continue;
            }
            SensorReadingRecord sensorReadingRecord = null;
            try
            {
                sensorReadingRecord = sensor.readSensor(sensorInfoRecord);
            }
            catch(Exception e)
            {
                String name = new String(sensorInfoRecord.getIDStringBytes());
                errorStr.append("Failed to read sensor info: ").append(name).toString();
                continue;
            }
            sensorReadingInfo.put(id, sensorReadingRecord);
        }

        try
        {
            lanIfs.end();
        }
        catch(IPMIException e)
        {
            errorStr.append("Failed to end ipmi lan session.");
        }
        catch(Exception e)
        {
        	errorStr.append("Fail to end im");
        }
     
        return sensorReadingInfo;
    }
    
    protected CounterReading updateSensorCounter(String counterID, SensorReadingRecord sensorReadingRecord)
    {
        CounterReading counterReading = null;
        try
        {
            counterReading = new CounterReading();
            int sensorStatus = 0;
            if(sensorReadingRecord != null && sensorReadingRecord.isError())
            {
                counterReading.value = translator.getGenericErrorMessage(sensorReadingRecord.getCcode());
                counterReading.setError();
            } else
            if(sensorReadingRecord == null || !sensorReadingRecord.isValidRead())
            {
                counterReading.value = "failed to read sensor";
                counterReading.setError();
            } else
            if(counterID.endsWith("_**Status"))
            {
                if(sensorReadingRecord instanceof AnalogSensorReadingRecord)
                {
                    sensorStatus = sensorReadingRecord.getStatus();
                    counterReading.value = translator.getSensorStatus(sensorStatus);
                } else
                if(sensorReadingRecord instanceof DiscreteSensorReadingRecord)
                {
                    Set sOfAsserts = ((DiscreteSensorReadingRecord)sensorReadingRecord).getStateAsserted();
                    StringBuilder value = new StringBuilder();
                    for(Iterator i$ = sOfAsserts.iterator(); i$.hasNext(); value.append(" "))
                    {
                        Byte ofAssert = (Byte)i$.next();
                        value.append(translator.getSensorAssert(sensorReadingRecord.getSensorInfoRecord(), ofAssert.byteValue()));
                    }

                    counterReading.value = value.toString();
                    sensorStatus = sensorReadingRecord.getStatus();
                }
            } else
            if(counterID.endsWith("_**Value"))
            {
                counterReading.value = String.valueOf(sensorReadingRecord.getValue());
                sensorStatus = sensorReadingRecord.getStatus();
            } else
            {
            }
            if(sensorStatus == 2)
                counterReading.setError();
            if(sensorStatus == 3)
                counterReading.setWarning();
        }
        catch(Exception e)
        {
        }
        return counterReading;
    }    
    
    protected CounterReading updateChassisCounter(String counterID, Chassis.ChassisStatus chassisStatus)
    {
        CounterReading counterReading = new CounterReading();
        if(counterID.equalsIgnoreCase("ChassisPowerOn"))
        {
            counterReading.value = chassisStatus.isPowerOn() ? "True" : "False";
            if(chassisStatus.isPowerOn())
                counterReading.setGood();
            else
                counterReading.setError();
        } else
        if(counterID.equalsIgnoreCase("ChassisPowerOverload"))
        {
            counterReading.value = chassisStatus.isPowerOverload() ? "True" : "False";
            if(!chassisStatus.isPowerOverload())
                counterReading.setGood();
            else
                counterReading.setError();
        } else
        if(counterID.equalsIgnoreCase("ChassisPowerInterlock"))
        {
            counterReading.value = chassisStatus.isPowerInterlock() ? "True" : "False";
            if(!chassisStatus.isPowerInterlock())
                counterReading.setGood();
            else
                counterReading.setError();
        } else
        if(counterID.equalsIgnoreCase("ChassisPowerFault"))
        {
            counterReading.value = chassisStatus.isPowerFault() ? "True" : "False";
            if(!chassisStatus.isPowerFault())
                counterReading.setGood();
            else
                counterReading.setError();
        } else
        if(counterID.equalsIgnoreCase("ChassisPowerControlFault"))
        {
            counterReading.value = chassisStatus.isPowerControlFault() ? "True" : "False";
            if(!chassisStatus.isPowerControlFault())
                counterReading.setGood();
            else
                counterReading.setError();
        } else
        if(counterID.equalsIgnoreCase("ChassisIntrusion"))
        {
            counterReading.value = chassisStatus.isChassisIntrusion() ? "Active" : "Inactive";
            if(!chassisStatus.isChassisIntrusion())
                counterReading.setGood();
            else
                counterReading.setError();
        } else
        if(counterID.equalsIgnoreCase("ChassisFrontPanelLockout"))
        {
            counterReading.value = chassisStatus.isFrontPanelLockout() ? "Active" : "Inactive";
            if(!chassisStatus.isFrontPanelLockout())
                counterReading.setGood();
            else
                counterReading.setError();
        } else
        if(counterID.equalsIgnoreCase("ChassisDriveFault"))
        {
            counterReading.value = chassisStatus.isDriveFault() ? "True" : "False";
            if(!chassisStatus.isDriveFault())
                counterReading.setGood();
            else
                counterReading.setError();
        } else
        if(counterID.equalsIgnoreCase("ChassisCoolingFanFault"))
        {
            counterReading.value = chassisStatus.isCoolingFanFault() ? "True" : "False";
            if(!chassisStatus.isCoolingFanFault())
                counterReading.setGood();
            else
                counterReading.setError();
        } else
        {
        }
        return counterReading;
    }    
    
	public int handleMessage() {
		SvMessage msg = this.getMessage();
		if (msg.getAction().equals("getCounters")){
			Map<String,Object> params = msg.getParams();
			Map<String,Object> counters = new HashMap<String,Object>();
			String host = (String)params.get("host");
			String port = (String)params.get("port");
			String usr = (String)params.get("usr");
			String pwd = (String)params.get("pwd");
			
			if (0==getCounters(counters, host, port, usr, pwd)){
				this.sendResponse2("ok", counters);
			}else{
				this.sendResponse(this.getLastError(), counters);
			}
		}else if (msg.getAction().equals("getValues")){
			Map<String,Object> params = msg.getParams();
			Map<String,Object> values = new HashMap<String,Object>();
			String host = (String)params.get("host");
			String port = (String)params.get("port");
			String usr = (String)params.get("usr");
			String pwd = (String)params.get("pwd");
			OtpErlangList counterslist = (OtpErlangList)params.get("counters");
			Map<String,Object> counters = ErlangUtils.erlangListToMap(counterslist);
			
			if (0==this.getValues(values,host,port,usr,pwd,counters)){
				this.sendResponse2("ok", values);
			}else{
				if(this.getLastError().length()>250)
					this.sendResponse(this.getLastError().substring(0, 250), values);
				else
					this.sendResponse(this.getLastError(), values);
			}
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
		IpmiMonitor ipmi = new IpmiMonitor();
		Map<String,Object> counters = new HashMap<String,Object>();
		Map<String,Object> values = new HashMap<String,Object>();
		counters.put("\"Chassis/Power On\"", "ChassisPowerOn");
		ipmi.getValues(values, "192.168.0.101", "623", "clubs", "clubs", counters);
		//ipmi.getCounters(values, "192.168.0.101", "623", "clubs", "clubs");
//		ipmi.getCounters(counters, "192.168.3.2", "623", "root", "root");
	}
}
