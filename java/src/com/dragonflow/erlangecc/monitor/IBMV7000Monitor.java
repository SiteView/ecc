package com.dragonflow.erlangecc.monitor;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Vector;

import javax.cim.CIMInstance;
import javax.cim.CIMObjectPath;
import javax.cim.CIMProperty;
import javax.cim.UnsignedInteger16;
import javax.security.auth.Subject;
import javax.wbem.CloseableIterator;
import javax.wbem.WBEMException;
import javax.wbem.client.PasswordCredential;
import javax.wbem.client.UserPrincipal;
import javax.wbem.client.WBEMClient;
import javax.wbem.client.WBEMClientFactory;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.dragonflow.siteview.infra.util.ServicePlatform;


import com.dragonflow.siteview.san.*;
import com.dragonflow.siteview.san.array.MMapFCPorts;
import com.dragonflow.siteview.san.array.MMapVolumes;
import com.dragonflow.siteview.san.array.MapArrayStats;
import com.dragonflow.siteview.san.array.MapDiskDrives;
import com.dragonflow.siteview.san.array.MapFCPortsFujitsuE6000;
import com.dragonflow.siteview.san.array.MapInitiatorPorts;
import com.dragonflow.siteview.san.array.MapPhysicalPackages;
import com.dragonflow.siteview.san.array.MapSoftwareIdentity;
import com.dragonflow.siteview.san.array.MapSymmetrixDiskDrives;
import com.dragonflow.siteview.san.beans.*;
import com.dragonflow.siteview.san.fcswitch.MapSwitchFCPorts;
import com.dragonflow.siteview.san.fcswitch.MapSwitchFabric;
import com.dragonflow.siteview.san.fcswitch.MapSwitchPhysicalPackages;
import com.dragonflow.siteview.san.fcswitch.MapSwitchSoftwareIdentity;
import com.dragonflow.siteview.san.hba.MapHBA;
import com.dragonflow.siteview.san.util.CIM_DataTypes;
import com.dragonflow.siteview.san.util.CIM_Qualifiers;
import com.dragonflow.siteview.san.util.PerformanceMetrics;

public class IBMV7000Monitor extends BaseMonitor  {
	
	
	private CloseableIterator<?> instanceEnum = null;
	private WBEMClient cc = null;
	private String enumerationNameKey = null;
    private Integer computerSystemIDFinal;
	public String status="nodata";
	public String ftag="";
	public String errorq="";
	CIM_ComputerSystem cs;
	CIM_SoftwareIdentity csi;
    boolean fujitsuE6000Model;
	boolean ibm511Model;
	boolean ibm921Model;
	boolean ibm931Model;
	boolean netappManufacturer;
	String model;
	String manufacturer;
	private CIM_DataTypes cim_DT;
	private CIM_Qualifiers cim_Q = new CIM_Qualifiers();
	
	public  MapHBA mapHBA;
    public	MMapFCPorts mapFCP;
	public  MMapVolumes mapV;
	MapSymmetrixDiskDrives mapSymmDD;
	MapDiskDrives mapDD;
	MapInitiatorPorts mapIP;
	MapPhysicalPackages mapPP;
	MapSoftwareIdentity mapSI;
	MapArrayStats mapAS;
	MapFCPortsFujitsuE6000 mapFujitsuFCP;
	MapSwitchFabric mapSwitchF;
	MapSwitchPhysicalPackages mapSwitchPP;
	MapSwitchSoftwareIdentity mapSwitchSI;
	MapSwitchFCPorts mapSwitchFCP;
	
	String ipAddress="192.168.6.199";
	String username="root";
	String password="system@123";
	String namespace="root/cimv2";
	String port="5989";
	String protocol="https";
	OtpErlangList EMPTY_ERLANG_LIST_STRING = new OtpErlangList();
	String lastError = "";
	Map<String, Object> Counters = new HashMap<String, Object>();
	public void initArgs(Map<String, Object> map) {
		String s = null;

		try {
			s = (String) map.get("ipAddress");
			if (s != null && s.length() > 0) {
				this.ipAddress = s.trim();
			}
			s = (String) map.get("username");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.username = s.trim();
			}
			s = (String) map.get("password");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.password = s.trim();
			}
			s = (String) map.get("namespace");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.namespace = s.trim();
			}
			s = (String) map.get("port");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.port = s.trim();
			}
			s = (String) map.get("protocol");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.protocol = s.trim();
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
			lastError = e.getMessage();
			e.printStackTrace();
		}

	}

	public Map<String, Object> getBrowseData() {
		Map<String, Object> res_map = new HashMap<String, Object>();
		this.initArgs(this.getMessage().getParams());
		String status=getserverstatus(this.ipAddress, this.username, this.password, this.namespace, this.port, this.protocol,"v7000");
		if(status.equals("ONLINE"))
		{
	    cimClient(ipAddress, username, password, namespace, port, protocol, "v7000");	
		res_map.put("SAN", "SAN");
		res_map.put("status", "SAN/status");
		res_map.put("totalManagedSpace", "SAN/totalManagedSpace");
		res_map.put("remainingManagedSpace", "SAN/remainingManagedSpace");
		res_map.put("AllocatedCapacityRate", "SAN/AllocatedCapacityRate");
		res_map.put("FibreChannel", "SAN/FibreChannel");
		if(mapFCP!=null)
       	for (CIM_FCPort fcport : mapFCP.FCPortlist) {
       		if(!res_map.containsKey(fcport.getSystemName()))
       		{
       		   res_map.put(fcport.getSystemName(), "SAN/FibreChannel/"+fcport.getSystemName());
       		   res_map.put(fcport.getDeviceId(), "SAN/FibreChannel/"+fcport.getSystemName()+"/"+fcport.getElementName());
       		   res_map.put(fcport.getDeviceId()+"OperationalStatus", "SAN/FibreChannel/"+fcport.getSystemName()+"/"+fcport.getElementName()+"/"+"OperationalStatus");
       		}else
       		{
       			res_map.put(fcport.getDeviceId(), "SAN/FibreChannel/"+fcport.getSystemName()+"/"+fcport.getElementName());
       			res_map.put(fcport.getDeviceId()+"OperationalStatus", "SAN/FibreChannel/"+fcport.getSystemName()+"/"+fcport.getElementName()+"/"+"OperationalStatus");
       		} 		
			
		}	
        res_map.put("Pools", "SAN/Pools");
        if (mapV!=null)
      	for (CIM_StorageVolume StorageVolume : mapV.v7000StorageVolumes) {
       		if(!res_map.containsKey(StorageVolume.getPoolName()))
       		{
       		   res_map.put(StorageVolume.getPoolName(), "SAN/Pools/"+StorageVolume.getPoolName());
       		   res_map.put(StorageVolume.getDeviceId(), "SAN/Pools/"+StorageVolume.getPoolName()+"/"+StorageVolume.getDeviceId());
       		   res_map.put(StorageVolume.getDeviceId()+"OperationalStatus", "SAN/Pools/"+StorageVolume.getPoolName()+"/"+StorageVolume.getDeviceId()+"/OperationalStatus");
       		   res_map.put(StorageVolume.getDeviceId()+"LunSize", "SAN/Pools/"+StorageVolume.getPoolName()+"/"+StorageVolume.getDeviceId()+"/LUN Size");
       		}else
       		{
       			res_map.put(StorageVolume.getDeviceId(), "SAN/Pools/"+StorageVolume.getPoolName()+"/"+StorageVolume.getDeviceId());
       			res_map.put(StorageVolume.getDeviceId()+"OperationalStatus", "SAN/Pools/"+StorageVolume.getPoolName()+"/"+StorageVolume.getDeviceId()+"/OperationalStatus");
       			res_map.put(StorageVolume.getDeviceId()+"LunSize", "SAN/Pools/"+StorageVolume.getPoolName()+"/"+StorageVolume.getDeviceId()+"/LUN Size");
       		
       		} 		
			
		}
		}
		return res_map;
	}

	public Map<String, Object> update() {

		Map<String, Object> result = new HashMap<String, Object>();
		this.initArgs(this.getMessage().getParams());
		cimClient(ipAddress, username, password, namespace, port, protocol, "v7000");	
		HashMap<String, String> cmap = new HashMap<String, String>();
		cmap.put("status",status);
		if (errorq.equals(""))
		{
		
	 try
	 {
		double TotalManagedSpace=0;
		double RemainingManagedSpace=0;
		double assignedrate=0.00;
		if((mapV!=null)&&mapV.v7000StoragePools!=null)
		{
		for (CIM_StoragePool  pool: mapV.v7000StoragePools) {
			TotalManagedSpace=TotalManagedSpace+pool.getTotalManagedSpace();
			RemainingManagedSpace=RemainingManagedSpace+pool.getRemainingManagedSpace();
			
		}
		TotalManagedSpace=TotalManagedSpace/1024/1024/1024;
		BigDecimal bd=new BigDecimal(TotalManagedSpace);
	    bd=bd.setScale(1, BigDecimal.ROUND_HALF_UP);
	    TotalManagedSpace=bd.doubleValue();
		RemainingManagedSpace=RemainingManagedSpace/1024/1024/1024;
		bd=new BigDecimal(RemainingManagedSpace);
	    bd=bd.setScale(1, BigDecimal.ROUND_HALF_UP);
	    RemainingManagedSpace=bd.doubleValue();
	    if(TotalManagedSpace>0)
		assignedrate=(TotalManagedSpace-RemainingManagedSpace)/TotalManagedSpace;
		bd=new BigDecimal(assignedrate);
	    bd=bd.setScale(2, BigDecimal.ROUND_HALF_UP);
	    assignedrate=bd.doubleValue();
		//inf.getUpdate();
	    cmap.put("totalManagedSpace", TotalManagedSpace+"");
	    cmap.put("remainingManagedSpace", RemainingManagedSpace+"");
	    cmap.put("AllocatedCapacityRate", assignedrate+"");
		}
	    if((mapFCP!=null)&&(mapFCP.FCPortlist!=null))
	    for (CIM_FCPort fcport : mapFCP.FCPortlist) {
	    	
	    	cmap.put(fcport.getDeviceId()+"OperationalStatus", fcport.getOperationalStatus());	
			
		}
	    if((mapV!=null)&&(mapV.v7000StorageVolumes!=null))
	    {
	    for (CIM_StorageVolume StorageVolume : mapV.v7000StorageVolumes) {
	    	cmap.put(StorageVolume.getDeviceId()+"OperationalStatus", StorageVolume.getOperationalStatus());	
	    	double lunsize=StorageVolume.getBlockSize()*StorageVolume.getNumberOfBlocks()/1024/1024/1024;
	    	BigDecimal bd=new BigDecimal(lunsize);
		    bd=bd.setScale(1, BigDecimal.ROUND_HALF_UP);
		    lunsize=bd.doubleValue();
	    	cmap.put(StorageVolume.getDeviceId()+"LunSize", lunsize+"");	
	    	
			
		}
	    }
	 }catch (Exception e)
	 {
	 }
		}else
		{
			cmap.put("error", errorq);
		}
		
		int len = this.Counters.size();
		if (cmap.containsKey("error")) {
			this.lastError = cmap.get("error");
			result.put("stateString", "update fail");
			result.put("countersInError", len);
		} else {
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
						 if(v instanceof String)
						 {
							if(v.equals("false"))
							{
								result.put(s1, false);	
								result.put(s, false);	
							}
							else if(v.equals("true"))
							{ result.put(s1, true);
							result.put(s, true);}
							else
							{ result.put(s1, v);result.put(s, v);}
						 }else
						 {
						 result.put(s1, v);
						 result.put(s, v);
						 }
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
	public static void write(String path, String content) {
	      String s = new String();
	      String s1 = new String();
	      try {
	       File f = new File(path);
	       if (f.exists()) {
	        System.out.println("文件存在");
	       } else {
	        System.out.println("文件不存在，正在创建...");
	        if (f.createNewFile()) {
	         System.out.println("文件创建成功！");
	        } else {
	         System.out.println("文件创建失败！");
	        }

	       }
	       BufferedReader input = new BufferedReader(new FileReader(f));

	       while ((s = input.readLine()) != null) {
	        s1 += s + "\n";
	       }
	       System.out.println("文件内容：" + s1);
	       input.close();
	       s1 += content;

	       BufferedWriter output = new BufferedWriter(new FileWriter(f));
	       output.write(s1);
	       output.close();
	      } catch (Exception e) {
	       e.printStackTrace();
	      }
	}

	public void cimClient(String ipAddress, String username,
			String password, String namespace, String port, String protocol,String ftag) {
		this.ftag=ftag;
		String unsecureClientNameSpace = protocol + "://" + ipAddress + ":"
				+ port + "/" + namespace;

		try {
			CIMObjectPath cns = new CIMObjectPath(unsecureClientNameSpace);
			UserPrincipal up = new UserPrincipal(username);
			PasswordCredential pc = new PasswordCredential(password);

			Subject s = new Subject();
			s.getPrincipals().add(up);
			s.getPrivateCredentials().add(pc);

			this.cc = WBEMClientFactory.getClient("CIM-XML");
			Locale[] l = { Locale.ENGLISH };
			this.cc.initialize(cns, s, l);
			try {
				this.instanceEnum = this.cc.enumerateInstances(
						new CIMObjectPath("CIM_System", namespace), true,
						false, true, null);
			} catch (WBEMException ce) {
				this.instanceEnum = null;
			}
			if (this.instanceEnum == null) {
				this.status="OFFLINE";
				// this.ucs.updateCimom(Cimom.STATUS_OFFLINE, cimomId);
			} else if(instanceEnum != null){
				this.status="ONLINE";
				getComputerSystem(cc,"CIM_ComputerSystem",namespace);
				// this.ucs.updateCimom(Cimom.STATUS_ONLINE, cimomId);
			}

		} catch (WBEMException ce) {
			// ucs.updateCimom(Cimom.STATUS_OFFLINE, cimomId);
			this.errorq=ce.getMessage();
		}
		//return this.cc;
	}
	

	public String getserverstatus(String ipAddress, String username,
			String password, String namespace, String port, String protocol,String ftag)
	{

        this.status="OFFLINE";
        this.ftag=ftag;
		String unsecureClientNameSpace = protocol + "://" + ipAddress + ":"
				+ port + "/" + namespace;
		
		
		try {
			CIMObjectPath cns = new CIMObjectPath(unsecureClientNameSpace);
			UserPrincipal up = new UserPrincipal(username);
			PasswordCredential pc = new PasswordCredential(password);

			Subject s = new Subject();
			s.getPrincipals().add(up);
			s.getPrivateCredentials().add(pc);
			this.cc = WBEMClientFactory.getClient("CIM-XML");
			Locale[] l = { Locale.ENGLISH,Locale.CHINESE };
			this.cc.initialize(cns, s, l);
			try {
				this.instanceEnum = this.cc.enumerateInstances(
						new CIMObjectPath("CIM_System", namespace), true,
						false, true, null);
			} catch (WBEMException ce) {
				
				this.instanceEnum = null;
			}
			if (this.instanceEnum == null) {
				this.status="OFFLINE";
				// this.ucs.updateCimom(Cimom.STATUS_OFFLINE, cimomId);
			} else if(instanceEnum != null){
				this.status="ONLINE";
				// this.ucs.updateCimom(Cimom.STATUS_ONLINE, cimomId);
			}

		} catch (WBEMException ce) {
			// ucs.updateCimom(Cimom.STATUS_OFFLINE, cimomId);
		}
		//return this.cc;
		return this.status;
		
	}

	/**
	 * Adds the computer system to DB and determines what path to go based 
	 * on the CIM_ComputerSystem.dedicated
	 * @param cc
	 * @param cim_class
	 */
	@SuppressWarnings("unchecked")
	public void getComputerSystem( WBEMClient cc, String cim_class,String namespace) {
		if (cc != null) {
			try {
				
				this.cim_DT = new CIM_DataTypes();
				CIMObjectPath cop = new CIMObjectPath(cim_class, namespace);
			    PerformanceMetrics pm = new PerformanceMetrics();
			    long statInstanceMean = pm.enumerationTime(cc, cop);
			    CloseableIterator computerSystemEnum = cc.enumerateInstances(cop, true, false, false, null);
				while (computerSystemEnum.hasNext()) {      
				this.cs = new CIM_ComputerSystem();
		        Calendar statCalBefore = Calendar.getInstance();
		        long msBeforeforTotalDisc = statCalBefore.getTimeInMillis();
		        this.cs.setInstanceTimeMean(Long.valueOf(statInstanceMean));
		        CIMInstance ci = (CIMInstance)computerSystemEnum.next();
		        int instancePropertySize = ci.getPropertyCount();
		        this.cs.setInstancePropertySize(instancePropertySize);
		        CIMObjectPath instanceCOP = ci.getObjectPath();
		        CIMProperty cp = instanceCOP.getKey("Name");
		        this.enumerationNameKey = cp.getValue().toString();

		       // this.logger.info("Name = " + this.enumerationNameKey);
		        //this.logger.debug(this.CN + " computerSystemName " + this.enumerationNameKey);

		        String enumerationNameKey = null;
		        String enumerationCreationClassNameKey = null;
		        String elementName = "Not Available";
		        String operationalStatus = null;
		        String statusDescriptions = null;
		        String description = null;
		        int enabledState = 0;

		        enumerationNameKey = cp.getValue().toString();
		        cp = instanceCOP.getKey("CreationClassName");
		        enumerationCreationClassNameKey = cp.getValue().toString();
		        //this.logger.info("CreationClassName = " + enumerationCreationClassNameKey);

		        String caption = "";
		        try {
		          caption = this.cim_DT.getCIMInstancePropertyValueString(ci, "Caption");
		        } catch (Exception e) {
		          caption = "Not Available";
		        }
		        //this.logger.info("Caption = " + caption);

		        String description1 = "";
		        try {
		          description1 = this.cim_DT.getCIMInstancePropertyValueString(ci, "Description");
		        } catch (Exception e) {
		          description1 = "Not Available";
		        }
		        //this.logger.info("Description = " + description1);
		        try
		        {
		          elementName = this.cim_DT.getCIMInstancePropertyValueString(ci, "ElementName");
		        } catch (Exception e) {
		          elementName = "Not Available";
		        }
		       // this.logger.info("ElementName = " + elementName);

		        int requestedState = 0;
		        try {
		          requestedState = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(ci, "RequestedState").intValue();
		        }
		        catch (Exception localException1) {
		        }
		        int enabledDefault = 0;
		        try {
		          enabledDefault = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(ci, "EnabledDefault").intValue();
		        }
		        catch (Exception localException2) {
		        }
		        String nameFormat = null;
		        try {
		          nameFormat = this.cim_DT.getCIMInstancePropertyValueString(ci, "NameFormat");
		        } catch (Exception e) {
		         // this.logger.error(this.CN, e);
		        }

		        try
		        {
		          UnsignedInteger16[] operationalStatusArray = this.cim_DT.getUint16ArrayPropertyValue(ci, "OperationalStatus");
		          int operationalStatusSize = 0;
		          if (operationalStatusArray != null) {
		            operationalStatusSize = operationalStatusArray.length;
		          }
		          //this.logger.debug("operationalStatusSize = " + operationalStatusSize);
		          Vector operationalStatusString = new Vector();
		          for (int x = 0; x < operationalStatusSize; ++x) {
		            UnsignedInteger16 opstsint = operationalStatusArray[x];

		            int operationalStatusInt = Integer.parseInt(opstsint.toString());

		            String operationalStatusValue = this.cim_Q.operationalStatus(operationalStatusInt);

		            operationalStatusString.add(operationalStatusValue);
		          }
		          String operationalStatusFinal = this.cim_Q.buildStringFromVector(operationalStatusString, ",");
		          this.cs.setOperationalStatus(operationalStatusFinal);
		        } catch (Exception e) {
		          //this.logger.error("OperationalStatus", e);
		          this.cs.setOperationalStatus("Unknown");
		        }

		        String statusDescriptionsFinal = null;
		        try
		        {
		          String[] statusDescriptionsArray = this.cim_DT.getStringArrayPropertyValue(ci, "StatusDescriptions");
		          int statusDescriptionsSize = 0;
		          if (statusDescriptionsArray != null) {
		            statusDescriptionsSize = statusDescriptionsArray.length;
		          }
		          //this.logger.debug("statusDescriptionsSize = " + statusDescriptionsSize);
		          Vector statusDescriptionsString = new Vector();
		          for (int y = 0; y < statusDescriptionsSize; ++y)
		          {
		            String statusDescriptionsValue = statusDescriptionsArray[y].toString();

		            statusDescriptionsString.add(statusDescriptionsValue);
		          }
		          statusDescriptionsFinal = this.cim_Q.buildStringFromVector(statusDescriptionsString, ",");
		        } catch (Exception e) {
		         // this.logger.error("StatusDescriptions", e);
		        }

		        String otherIdentifyingInfoFinal = null;
		        try
		        {
		          String[] otherIdentifyingInfoArray = this.cim_DT.getStringArrayPropertyValue(ci, "OtherIdentifyingInfo");
		          int otherIdentifyingInfoSize = 0;
		          if (otherIdentifyingInfoArray != null) {
		            otherIdentifyingInfoSize = otherIdentifyingInfoArray.length;
		          }
		         // this.logger.debug("otherIdentifyingInfoSize = " + otherIdentifyingInfoSize);
		          Vector otherIdentifyingInfoString = new Vector();
		          for (int y = 0; y < otherIdentifyingInfoSize; ++y)
		          {
		            String otherIdentifyingInfoValue = otherIdentifyingInfoArray[y].toString();

		            otherIdentifyingInfoString.add(otherIdentifyingInfoValue);
		          }
		          otherIdentifyingInfoFinal = this.cim_Q.buildStringFromVector(otherIdentifyingInfoString, ",");
		          //this.logger.info("OtherIdentifyingInfo = " + otherIdentifyingInfoFinal);
		        } catch (Exception e) {
		          //this.logger.error("OtherIdentifyingInfo", e);
		        }

		        String identifyingDescriptionsFinal = null;
		        try
		        {
		          String[] identifyingDescriptionsArray = this.cim_DT.getStringArrayPropertyValue(ci, "IdentifyingDescriptions");
		          int identifyingDescriptionsSize = 0;
		          if (identifyingDescriptionsArray != null) {
		            identifyingDescriptionsSize = identifyingDescriptionsArray.length;
		          }
		          //this.logger.debug("identifyingDescriptinsSize = " + identifyingDescriptionsSize);
		          Vector identifyingDescriptionsString = new Vector();
		          for (int y = 0; y < identifyingDescriptionsSize; ++y)
		          {
		            String identfyingDescriptionsValue = identifyingDescriptionsArray[y].toString();

		            identifyingDescriptionsString.add(identfyingDescriptionsValue);
		          }
		          identifyingDescriptionsFinal = this.cim_Q.buildStringFromVector(identifyingDescriptionsString, ",");
		          //this.logger.info("IdentifyingDescriptions = " + identifyingDescriptionsFinal);
		        } catch (Exception e) {
		         // this.logger.error("IdentifyingDescriptions", e);
		        }
		        try
		        {
		          enabledState = Integer.parseInt(this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(ci, "enabledState").toString());
		        } catch (Exception e) {
		          //this.logger.warn("enabledState does not exist for CIMOM = " + enumerationNameKey);
		        }

//		        this.logger.debug("description = " + description + 
//		          " operationalStatus = " + operationalStatus + " statusDescriptions = " + statusDescriptions + 
//		          " enabledState = " + enabledState);

		        this.cs.setName(enumerationNameKey);
		        this.cs.setCreationClassName(enumerationCreationClassNameKey);
		        this.cs.setCaption(caption);
		        this.cs.setDescription(description1);
		        this.cs.setRequestedState(requestedState);
		        this.cs.setElementName(elementName);
		        this.cs.setStatusDescriptions(statusDescriptionsFinal);
		        this.cs.setEnabledDefault(enabledDefault);
		        this.cs.setNameFormat(nameFormat);
		        this.cs.setEnabledState(enabledState);

		        Calendar cal = Calendar.getInstance();
		        Date now = new Date();

		        this.cs.setTimeOfCreation(cal);
		        this.cs.setTimeOfCreationString(DateFormat.getDateTimeInstance(3, 3).format(now).toString());
//		        Integer cimom_Id_Integer = new Integer(cimom_Id);
//		        this.cs.setCimomID(cimom_Id_Integer);

		        int dedicatedSize = 0;
		        UnsignedInteger16 dedicatedFill = new UnsignedInteger16("555");
		        UnsignedInteger16[] dedicated = { dedicatedFill };
		        try
		        {
		          dedicated = this.cim_DT.getUint16ArrayPropertyValue(ci, "Dedicated");

		          if (dedicated != null)
		            dedicatedSize = dedicated.length;
		        }
		        catch (Exception e) {
		         // this.logger.error(this.CN, e);
		        }
		        //this.logger.info("dedicatedSize = " + dedicatedSize);

		        int intDedicated1 = 0;

		        int intDedicated2 = 0;

		        int intDedicated3 = 0;

		        int intDedicated4 = 0;

		        int intDedicated5 = 0;
		        this.cs.setDedicated("555");
		        String dedicated1;
		        if (dedicatedSize == 1) {
		          dedicated1 = dedicated[0].toString();
		          intDedicated1 = Integer.parseInt(dedicated1);
		          this.cs.setDedicated(dedicated1);
		         // this.logger.info("DedicateFilter1 = " + intDedicated1);
		        }
		        else
		        {
		          String dedicated2;
		          if (dedicatedSize == 2) {
		            dedicated1 = dedicated[0].toString();
		            intDedicated1 = Integer.parseInt(dedicated1);
		            dedicated2 = dedicated[1].toString();
		            intDedicated2 = Integer.parseInt(dedicated2);
		            this.cs.setDedicated(dedicated1 + "," + dedicated2);
		            //this.logger.info("DedicateFilter1 = " + intDedicated1 + " DedicatedFilter2 = " + intDedicated2);
		          }
		          else
		          {
		            String dedicated3;
		            if (dedicatedSize == 3) {
		              dedicated1 = dedicated[0].toString();
		              intDedicated1 = Integer.parseInt(dedicated1);
		              dedicated2 = dedicated[1].toString();
		              intDedicated2 = Integer.parseInt(dedicated2);
		              dedicated3 = dedicated[2].toString();
		              intDedicated3 = Integer.parseInt(dedicated3);
		              this.cs.setDedicated(dedicated1 + "," + dedicated2 + "," + dedicated3);
		              //this.logger.info("DedicateFilter1 = " + intDedicated1 + " DedicatedFilter2 = " + intDedicated2 + " DedicatedFilter3 = " + intDedicated3);
		            }
		            else
		            {
		              String dedicated4;
		              if (dedicatedSize == 4) {
		                dedicated1 = dedicated[0].toString();
		                intDedicated1 = Integer.parseInt(dedicated1);
		                dedicated2 = dedicated[1].toString();
		                intDedicated2 = Integer.parseInt(dedicated2);
		                dedicated3 = dedicated[2].toString();
		                intDedicated3 = Integer.parseInt(dedicated3);
		                dedicated4 = dedicated[3].toString();
		                intDedicated4 = Integer.parseInt(dedicated4);
		                this.cs.setDedicated(dedicated1 + "," + dedicated2 + "," + dedicated3 + "," + dedicated4);
		                //this.logger.info("DedicateFilter1 = " + intDedicated1 + " DedicatedFilter2 = " + intDedicated2 + " DedicateFilter3 = " + intDedicated3 + " DedicatedFilter4 = " + intDedicated4);
		              }
		              else if (dedicatedSize == 5) {
		                dedicated1 = dedicated[0].toString();
		                intDedicated1 = Integer.parseInt(dedicated1);
		                dedicated2 = dedicated[1].toString();
		                intDedicated2 = Integer.parseInt(dedicated2);
		                dedicated3 = dedicated[2].toString();
		                intDedicated3 = Integer.parseInt(dedicated3);
		                dedicated4 = dedicated[3].toString();
		                intDedicated4 = Integer.parseInt(dedicated4);
		                String dedicated5 = dedicated[4].toString();
		                intDedicated5 = Integer.parseInt(dedicated5);
		                this.cs.setDedicated(dedicated1 + "," + dedicated2 + "," + dedicated3 + "," + dedicated4 + "," + dedicated5);
		                //this.logger.info("DedicateFilter1 = " + intDedicated1 + " DedicatedFilter2 = " + intDedicated2 + " DedicateFilter3 = " + intDedicated3 + " DedicatedFilter4 = " + intDedicated4 + " DedicatedFilter5 = " + intDedicated5); 
		                } } }
		        }
		        //this.logger.info("Dedicated = " + this.cs.getDedicated());
		        //this.logger.info("------------------------------------------COMPUTER SYSTEM END------------------------------------------");
		        try
		        {
		          CloseableIterator cim_PhysicalPackageEnum = cc.associators(instanceCOP, 
		            "CIM_SystemPackaging", "CIM_PhysicalPackage", 
		            "Dependent", "Antecedent", false, false, 
		            null);
		          this.fujitsuE6000Model = false;
		          this.ibm511Model = false;
		          this.netappManufacturer = false;
		          while (cim_PhysicalPackageEnum.hasNext()) {
		            //this.logger.debug(this.CN + " Enumerated PhysicalPackage and has more elements");
		            CIMInstance cim_PhysicalPackageCI = (CIMInstance)cim_PhysicalPackageEnum.next();

		            this.fujitsuE6000Model = false;
		            this.ibm511Model = false;
		            if (cim_PhysicalPackageEnum == null) continue;
		            try {
		              this.model = this.cim_DT.getCIMInstancePropertyValueString(cim_PhysicalPackageCI, "Model");
		              this.fujitsuE6000Model = this.model.startsWith("E6000");
		              this.ibm511Model = this.model.startsWith("511");
		              this.ibm921Model = this.model.startsWith("921");
		              this.ibm931Model = this.model.startsWith("931");
		            } catch (NullPointerException npe) {
		              this.model = null;
		            }
		            try {
		              this.manufacturer = this.cim_DT.getCIMInstancePropertyValueString(cim_PhysicalPackageCI, "Manufacturer");
		              this.netappManufacturer = this.manufacturer.startsWith("Network Appliance");
		            } catch (NullPointerException npe) {
		              this.manufacturer = null;
		            }
		          }
		        }
		        catch (Exception e) {
		          //this.logger.warn("No Physical Package for " + this.cs.getCreationClassName());
		        }

		        if ((!(this.cs.getDedicated().equals("555"))) && 
		          (!(this.cs.getCreationClassName().equals("LSISSI_StorageProcessorSystem"))) && 
		          (!(this.cs.getCreationClassName().equals("HPEVA_StorageProcessorSystem"))) && 
		          (!(this.cs.getCreationClassName().equals("HITACHI_StorageProcessorSystem"))) && 
		          (!(this.cs.getCreationClassName().equals("Brocade_PhysicalComputerSystem"))) && 
		          (!(this.cs.getCreationClassName().equals("CISCO_LogicalComputerSystem"))) && 
		          (!(this.cs.getCreationClassName().equals("SunStorEdge_DSPStorageProcessorSystem"))) && 
		          (!(this.cs.getCreationClassName().equals("OpenWBEM_UnitaryComputerSystem"))) && 
		          (!(this.cs.getCreationClassName().equals("IBMTSSVC_IOGroup"))) && 
		          (!(this.cs.getCreationClassName().equals("HPMSA_ArrayController"))) && ((
		          (this.cs.getDedicated().equals("3,15")) || 
		          (this.cs.getDedicated().equals("15,3")) || 
		          (this.cs.getDedicated().equals("3")) || 
		          (this.cs.getDedicated().equals("0")) || 
		          (this.cs.getDedicated().equals("3,15,16,25")) || 
		          (this.cs.getDedicated().equals("3,15,16,21,25")) || 
		          (this.cs.getDedicated().equals("3,15,25")) || 
		          (this.cs.getDedicated().equals("15")) || 
		          (this.cs.getDedicated().equals("5")) || 
		          (this.cs.getDedicated().equals("3,22")) || 
		          (this.cs.getDedicated().equals("3,15,21")) || 
		          (this.cs.getDedicated().equals("15,21")))))
		        {
		         // session.save(this.cs);

		         /* String computerSystemID = session.getIdentifier(this.cs).toString();
		          Integer computerSystemIDp = Integer.valueOf(computerSystemID);
		          setComputerSystemIDFinal(computerSystemIDp);
		          */
		          //this.logger.debug("computerSystemIDp = " + computerSystemIDp);

		        	
		        	
		        	/////&& (computerSystemIDp != null)
		          if ((enumerationNameKey != null)  && (dedicated != null)) {
		           // this.logger.info("Started Discovery For " + this.cs.getCreationClassName());

		            if (((intDedicated1 == 3) && (intDedicated2 == 15) && (intDedicated3 != 25)) || ((intDedicated1 == 15) && (intDedicated2 == 3) && (intDedicated3 != 25))) {
		              if (this.cs.getCreationClassName().equals("Symm_StorageSystem"))
		              {
		               this.mapSymmDD = new MapSymmetrixDiskDrives(cc, instanceCOP, this.cs);
		              }
		              else {
		                this.mapDD = new MapDiskDrives(cc, instanceCOP, this.cs);
		              }
		              this.mapIP = new MapInitiatorPorts(cc, instanceCOP, this.cs);
		              if ((this.fujitsuE6000Model) || (this.ibm511Model) || (this.ibm921Model) || (this.ibm931Model) || (this.netappManufacturer))
		              {
		                this.mapFujitsuFCP = new MapFCPortsFujitsuE6000(cc, instanceCOP, cs);
		              }
		              else {
		                this.mapFCP = new MMapFCPorts( cc, instanceCOP, this.cs,"");
		              }
		              this.mapV = new MMapVolumes(cc, instanceCOP, this.cs,"");
		              this.mapPP = new MapPhysicalPackages( cc, instanceCOP,this.cs);
		              this.mapSI = new MapSoftwareIdentity( cc, instanceCOP, this.cs);
		              this.mapAS = new MapArrayStats( cc, instanceCOP, this.cs);
		            }
		            else if ((intDedicated1 == 15) && (intDedicated2 == 21))
		            {
		              this.mapFCP = new MMapFCPorts(cc,instanceCOP,this.cs,this.ftag);
		              this.mapV = new MMapVolumes(cc, instanceCOP,this.cs,this.ftag);
		            }
		            else if (intDedicated1 == 0) {
		              if (this.cs.getCreationClassName().equals("Linux_ComputerSystem")) {
		                //this.logger.debug("Host For Linux");
//		                this.mapBase = new MapBase(session, cc, instanceCOP, computerSystemIDp);
//		                this.mapFSVol = new MapFSVol(session, cc, instanceCOP, computerSystemIDp);
//		                this.mapNetwork = new MapNetwork(session, cc, instanceCOP, computerSystemIDp);
		              } else {
		                //this.logger.debug("Host For FCHBA");
		            	  //session,  , computerSystemIDp
		                this.mapHBA = new MapHBA( cc, instanceCOP,this.cs);
		              }
		            }
		            else if ((intDedicated1 == 5) && (!(this.cs.getCreationClassName().equals("CISCO_LogicalComputerSystem"))))
		            {
		              if ((this.cs.getElementName() != null) || (!(this.cs.getElementName().equals(""))))
		              {
		               // this.logger.debug("Switch");
		                this.mapSwitchPP = new MapSwitchPhysicalPackages( cc, instanceCOP, cs);
		                this.mapSwitchSI = new MapSwitchSoftwareIdentity(cc, instanceCOP, cs);
		                this.mapSwitchFCP = new MapSwitchFCPorts(cc, instanceCOP, cs);
		                this.mapSwitchF = new MapSwitchFabric(cc, instanceCOP, cs);
		              }
		            } else if ((intDedicated1 == 3) && (intDedicated2 == 22)) {
			              //this.logger.debug("Tape");
//			              this.mapPhysicalPackagesTape = new MapPhysicalPackagesTape(session, cc, instanceCOP, computerSystemIDp);
//			              this.mapSoftwareIdentityTape = new MapSoftwareIdentityTape(session, cc, instanceCOP, computerSystemIDp);
//			              this.mapMediaAccessDevicesTape = new MapMediaAccessDevicesTape(session, cc, instanceCOP, computerSystemIDp);
//			              this.mapChangerDeviceTape = new MapChangerDeviceTape(session, cc, instanceCOP, computerSystemIDp);
//			              this.mapAccessPointsTape = new MapAccessPointsTape(session, cc, instanceCOP, computerSystemIDp);
//			              this.mapFCPortsTape = new MapFCPortsTape(session, cc, instanceCOP, computerSystemIDp);
//			              this.mapLimitedAccessPortTape = new MapLimitedAccessPort(session, cc, instanceCOP, computerSystemIDp);
			            }else if ((intDedicated1 == 3) && (intDedicated2 == 15) && (intDedicated3 == 25)) {
//			              new MapNASDiskDrives(session, cc, instanceCOP, computerSystemIDp);
//			              new MapNASInitiatorPorts(session, cc, instanceCOP, computerSystemIDp);
//			              new MapNASFCPorts(session, cc, instanceCOP, computerSystemIDp);
//			              new MapNASVolumes(session, cc, instanceCOP, computerSystemIDp);
//			              new MapNASPhysicalPackages(session, cc, instanceCOP, computerSystemIDp);
//			              new MapNASSoftwareIdentity(session, cc, instanceCOP, computerSystemIDp);
			            }  else if ((intDedicated1 == 15) && (this.cs.getCreationClassName().equals("VMWARE_ESXComputerSystem"))) {
		              //this.logger.debug("VMWare");
		              //this.mapVM = new MapVirtualMachines(session, cc, instanceCOP, computerSystemIDp);
		             // this.mapESXStorageExtents = new MapESXStorageExtents(session, cc, instanceCOP, computerSystemIDp);
		             // this.mapESXStoragePools = new MapESXStoragePools(session, cc, instanceCOP, computerSystemIDp);
		              //this.mapESXStorageVolumes = new MapESXStorageVolumes(session, cc, instanceCOP, computerSystemIDp);
		            } else {
		              //this.logger.info("NOTHING");
		            }
		           // this.logger.info("Finished Discovery For " + this.cs.getCreationClassName());
		          }

		        }

		        try
		        {
		        	/* Calendar statCalAfter = Calendar.getInstance();
		          long msAfterforTotalDisc = statCalAfter.getTimeInMillis();
		          long totalDiscoveryTimeFinal = (msAfterforTotalDisc - msBeforeforTotalDisc) / 1000L;
		          //this.logger.info("MSAFTERTIME = " + msAfterforTotalDisc + "MSBEFORE = " + msBeforeforTotalDisc);
		          //this.logger.info("Discovery Time = " + totalDiscoveryTimeFinal);
		          //this.logger.info("COMUTERSYSTEMID = " + getComputerSystemIDFinal());
		          CIM_ComputerSystem ccs1 = (CIM_ComputerSystem)session.get(CIM_ComputerSystem.class, getComputerSystemIDFinal());
		          ccs1.setTotalDiscoveryTime(Long.valueOf(totalDiscoveryTimeFinal));
		          //session.save(ccs1);
		          setComputerSystemIDFinal(null);
		          */
		          
		        }
		        catch (Exception localException3)
		        {
		        }
		        }
				
				//session.getTransaction().commit();
				//session.close();
				//session.disconnect();
				//session.flush();
			
				// close session.
				 if (cc != null) 
				       cc.close();
				
				// Call the garbage collection to run
				//System.gc();
				
			} catch (WBEMException ce) {
				ce.printStackTrace();
				//logger.warn("Unable to login at this time");
			} catch (Exception e) {
				e.printStackTrace();
				e.getCause();
			} finally {
				//session.close();
			}
        } else {
        	//logger.info("Unable to login at this time");
        }
	}
	 public Integer getComputerSystemIDFinal()
	  {
	    return this.computerSystemIDFinal;
	  }

	  public void setComputerSystemIDFinal(Integer computerSystemIDFinal)
	  {
	    this.computerSystemIDFinal = computerSystemIDFinal;
	  }
	public static void main(String[] args){
		IBMV7000Monitor m=new IBMV7000Monitor();
		try
		{
			System.out.println(m.ipAddress);
			Map<String, Object> ss= m.getBrowseData();
			for (String ddd : ss.keySet()) {
				System.out.println(ddd);
			}
		
		}catch(Exception e)
		{
			System.out.println(e.getLocalizedMessage());
		}
	}

}
