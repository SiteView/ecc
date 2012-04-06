package com.dragonflow.erlangecc.monitor;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.dragonflow.siteview.exchange.*;
public class Exchange2007Monitor extends BaseMonitor {
	
	   String pServer;
	   String pUserDomain;
	   String pMailboxAlias;
	   String pPscFilePath="C:" + File.separator + "Program Files" + File.separator + "Microsoft" + File.separator + "Exchange Server" + File.separator + "Bin" + File.separator + "ExShell.psc1";
	   int pTimeout=60;
	   String pClientAccess;
	   String pEdge;
	   String pHabTransport;
	   String pMailbox;
	   String pUnifiedMessaging;
	   private static List<ObjectInfo> objectsList;
	   private static void initBrowseXMLObjects()
	   {
	     List mapiRoles = new LinkedList();
	     mapiRoles.add("IsMailboxServer");
	     ObjectInfo mapiConnectivity = new ObjectInfo("MAPI Connectivity", "Testing MAPI Connectivity using Test-MAPIConnectivity cmdlet", mapiRoles);
	     mapiConnectivity.addCounter("Result", "MAPI Connectivity test result");
	     mapiConnectivity.addCounter("Latency", "MAPI Connectivity test latency");
	     objectsList.add(mapiConnectivity);
	 
	     List mailFlowRoles = new LinkedList();
	     mailFlowRoles.add("IsHubTransportServer");
	     mailFlowRoles.add("IsMailboxServer");
	     ObjectInfo mailFlow = new ObjectInfo("Mail Flow", "Testing Mail flow using Test-Mailflow cmdlet", mailFlowRoles);
	     mailFlow.addCounter("TestMailflowResult", "Mail flow test result");
	     mailFlow.addCounter("MessageLatencyTime", "Mail flow test latency");
	     objectsList.add(mailFlow);
	 
	     List searchRoles = new LinkedList();
	     searchRoles.add("IsMailboxServer");
	     ObjectInfo exchangeSearch = new ObjectInfo("Exchange Search", "Testing Exchange Search using Test-ExchangeSearch cmdlet", searchRoles);
	     exchangeSearch.addCounter("ResultFound", "Exchange Search test result");
	     exchangeSearch.addCounter("SearchTime", "Exchange Search test search time");
	     objectsList.add(exchangeSearch);
	 
	     List owaRoles = new LinkedList();
	     owaRoles.add("IsClientAccessServer");
	     ObjectInfo owaConnectivity = new ObjectInfo("OWA Connectivity", "Testing Microsoft Office Outlook Web Access Connectivity using Test-OWAConnectivity cmdlet", owaRoles);
	     owaConnectivity.addCounter("Result", "Microsoft OWA test result");
	     owaConnectivity.addCounter("Latency", "Microsoft OWA test latency");
	     objectsList.add(owaConnectivity);
	 
	     List wsRoles = new LinkedList();
	     wsRoles.add("IsClientAccessServer");
	     ObjectInfo wsConnectivity = new ObjectInfo("Web Services Connectivity", "Testing Exchange Web Services using Test-WebServicesConnectivity cmdlet", wsRoles);
	 
	     ObjectInfo wsGetFolderScenario = wsConnectivity.addObject("GetFolder", "Exchange Web Services Test: GetFolder Scenario");
	     wsGetFolderScenario.addCounter("Result", "Exchange Web Services Test: GetFolder Scenario result");
	     wsGetFolderScenario.addCounter("Latency", "Exchange Web Services Test: GetFolder Scenario latency");
	 
	     ObjectInfo wsSyncFolderItemsScenario = wsConnectivity.addObject("SyncFolderItems", "Exchange Web Services Test: SyncFolderItems Scenario");
	     wsSyncFolderItemsScenario.addCounter("Result", "Exchange Web Services Test: SyncFolderItems Scenario result");
	     wsSyncFolderItemsScenario.addCounter("Latency", "Exchange Web Services Test: SyncFolderItems Scenario latency");
	 
	     ObjectInfo wsCreateItemScenario = wsConnectivity.addObject("CreateItem", "Exchange Web Services Test: CreateItem Scenario");
	     wsCreateItemScenario.addCounter("Result", "Exchange Web Services Test: CreateItem Scenario result");
	     wsCreateItemScenario.addCounter("Latency", "Exchange Web Services Test: CreateItem Scenario latency");
	 
	     ObjectInfo wsDeleteItemScenario = wsConnectivity.addObject("DeleteItem", "Exchange Web Services Test: DeleteItem Scenario");
	     wsDeleteItemScenario.addCounter("Result", "Exchange Web Services Test: DeleteItem Scenario result");
	     wsDeleteItemScenario.addCounter("Latency", "Exchange Web Services Test: DeleteItem Scenario latency");
	 
	     objectsList.add(wsConnectivity);
	   }
	 
//	   public boolean isLoadable()
//	  {
//	     return PlatformNew.isWindows();
//	   }
//	 
//	   public String getHostname() {
//	     return getProperty(pServer);
//	   }
	 
	   public List getConnectionProperties() {
	     List connProps = new ArrayList(4);
	     connProps.add(pServer);
	     connProps.add(pUserDomain);
	     connProps.add(pMailboxAlias);
	     connProps.add(pPscFilePath);
	     return connProps;
	   }
	 
	   private Map<String, String> getPropertiesMapper()
	   {
	    Map propsMapper = new HashMap();
	     propsMapper.put("$PS_CONSOLE_FILE$", this.pPscFilePath);
	     propsMapper.put("$SERVER$", this.pServer);
	     propsMapper.put("$ACCOUNT$", this.pMailboxAlias);
	     propsMapper.put("$DOMAIN_CONTROLLER$", this.pUserDomain);
	 
	     return propsMapper;
	   }
	 
//	   private Map<String, String> getPropertiesAndRolesMapper(MultiValueHashMap<String, String> countersNamesMapper)
//	   {
//	     Map propsMapper = getPropertiesMapper();
//	     getSelectedCountersLabels(propsMapper, countersNamesMapper);
//	     getServerRolesFromPersistency(propsMapper);
//	 
//	     return propsMapper;
//	   }
	 
	   public String getBrowseData(StringBuffer errorStr) {
	     
	 
	     Map propsMapper = getPropertiesMapper();
	     int timeout = this.pTimeout;
	     List dataObjsList;
	     try
	     {
	       dataObjsList = Exch2007Util.getCmdletDataObjectResults(timeout, new GetExchangeServerCmdlet(), propsMapper);
	     } catch (Exception e) {
	       errorStr.append(e.getMessage());
	       return "";
	    }
	 
	     StringBuilder browseData = new StringBuilder();
	     if ((dataObjsList != null) && (!(dataObjsList.isEmpty())) && (dataObjsList.get(0) != null)) {
	       ExchSrvRolesData serverRoles = (ExchSrvRolesData)dataObjsList.get(0);
	       buildBrowseData(browseData, serverRoles);
	       setServerRoles(serverRoles.getCountersValuesMapper()); } else {
	     }
	    
	 
	     return browseData.toString();
	   }
	 
	   private void buildBrowseData(StringBuilder browseData, ExchSrvRolesData serverRoles) {
	     browseData.append("<browse_data>");
	     for (ObjectInfo object : objectsList) {
	       if (serverRoles.isRunnableCmd(object.getRoles())) {
	         browseData.append(object.toString());
	       }
	      
	     }
	 
	     browseData.append("</browse_data>");
	   }
	 
	   private void setServerRoles(Map<String, String> serverRolesValuesMapper)
	  {
	     ServerRolesMapper.addServerRolesMapper(this.pServer, serverRolesValuesMapper);
	   }
	 
	   private Map<String, String> getServerRolesMapper() {
	     return ServerRolesMapper.getServerRolesMapper(this.pServer);
	   }
	 
//	   public void onMonitorConfigChange()
//	   {
//	     super.onMonitorConfigChange();
//	     Map serverRolesMapper = getServerRolesMapper();
//	 
//	     if ((serverRolesMapper == null) || (serverRolesMapper.isEmpty())) {
//	       
//	       getBrowseData(new StringBuffer());
//	       serverRolesMapper = getServerRolesMapper();
//	    }
//	 
//	     if ((serverRolesMapper != null) && (!(serverRolesMapper.isEmpty()))) {
//	       setServerRolesInMG(serverRolesMapper);
//	      setServerRolesInPersistency(serverRolesMapper); } else {
////	       logger.error("ServerRolesMapper doesn't contain mapping for " + getProperty(pServer) + ": serverRolesMapper is null or empty. No counter values will be retrieved from Exchange Server."); }
//	   }
	 
//	   private void setServerRolesInMG(Map<String, String> serverRolesValuesMapper) {
//	     ConfigManagerSession configSession = new ConfigManagerSession(super.getClass().getName());
//	     ITreeObjectConfig theMonitor = (ITreeObjectConfig)configSession.query(getUniqueId());
//	    if (theMonitor != null) {
//	       SiteScopeObject siteScopeObject = theMonitor.toSiteScopeObject();
//	       String tmp;
//	       siteScopeObject.setProperty(pClientAccess, ((tmp = (String)serverRolesValuesMapper.get("IsClientAccessServer")) != null) ? tmp : "False");
//	       siteScopeObject.setProperty(pEdge, ((tmp = (String)serverRolesValuesMapper.get("IsEdgeServer")) != null) ? tmp : "False");
//	       siteScopeObject.setProperty(pHabTransport, ((tmp = (String)serverRolesValuesMapper.get("IsHubTransportServer")) != null) ? tmp : "False");
//	       siteScopeObject.setProperty(pMailbox, ((tmp = (String)serverRolesValuesMapper.get("IsMailboxServer")) != null) ? tmp : "False");
//	       siteScopeObject.setProperty(pUnifiedMessaging, ((tmp = (String)serverRolesValuesMapper.get("IsUnifiedMessagingServer")) != null) ? tmp : "False");
//	     }
//	     try {
//	       configSession.commit();
//	     } catch (Exception e) {
//	     }
//	   }
//	 
//	   private void setServerRolesInPersistency(Map<String, String> serverRolesValuesMapper)
//	   {
//	     String tmp;
//	     setProperty(pClientAccess, ((tmp = (String)serverRolesValuesMapper.get("IsClientAccessServer")) != null) ? tmp : "False");
//	     setProperty(pEdge, ((tmp = (String)serverRolesValuesMapper.get("IsEdgeServer")) != null) ? tmp : "False");
//	     setProperty(pHabTransport, ((tmp = (String)serverRolesValuesMapper.get("IsHubTransportServer")) != null) ? tmp : "False");
//	     setProperty(pMailbox, ((tmp = (String)serverRolesValuesMapper.get("IsMailboxServer")) != null) ? tmp : "False");
//	     setProperty(pUnifiedMessaging, ((tmp = (String)serverRolesValuesMapper.get("IsUnifiedMessagingServer")) != null) ? tmp : "False");
//	  }
//	 
//	   private void getServerRolesFromPersistency(Map<String, String> propsMapper) {
//	     propsMapper.put("IsClientAccessServer", getProperty(pClientAccess));
//	     propsMapper.put("IsEdgeServer", getProperty(pEdge));
//	     propsMapper.put("IsHubTransportServer", getProperty(pHabTransport));
//	     propsMapper.put("IsMailboxServer", getProperty(pMailbox));
//	     propsMapper.put("IsUnifiedMessagingServer", getProperty(pUnifiedMessaging));
//	   }
	 
	   private void getSelectedCountersLabels(Map<String, String> propsMapper, MultiValueHashMap<String, String> countersNamesMapper)
	   {
	     propsMapper.put("MAPI Connectivity", (countersNamesMapper.get("MAPI Connectivity") != null) ? "True" : "False");
	     propsMapper.put("Mail Flow", (countersNamesMapper.get("Mail Flow") != null) ? "True" : "False");
	     propsMapper.put("Exchange Search", (countersNamesMapper.get("Exchange Search") != null) ? "True" : "False");
	     propsMapper.put("OWA Connectivity", (countersNamesMapper.get("OWA Connectivity") != null) ? "True" : "False");
	     propsMapper.put("Web Services Connectivity", (countersNamesMapper.get("Web Services Connectivity") != null) ? "True" : "False");
	   }
	 
	   
	 
	 
	   protected boolean update()
	   {
	    
	 
//	     int maxCounters = getMaxCounters();
//	     if (maxCounters == 0) {
////	       if (logger.isDebugEnabled())
////	         logger.debug("Leaving Exchange2007Monitor.update() because there were no counters to retrieve.");
//	       return true;
//	     }
//	 
//	     resetCountersValues(maxCounters);
//	 
	     StringBuilder errorStr = new StringBuilder();
//	 
    //   MultiValueHashMap countersNamesMap = getCountersNamesMapper(20);
//	 
//	     if (countersNamesMap.isEmpty()) {
//	      // logger.error("Mismatch between counters types (cmdlets) and labels (name).");
//	     }
//	 
	     Map results = new HashMap();
	    Map errResults = new HashMap();
	 
	     boolean fatalError = executeCmdlets(null, results, errResults, errorStr);
	 
//	     setCountersValues(fatalError, maxCounters, results, errResults, errorStr);
	 
//	     if (logger.isDebugEnabled()) {
//	       logger.debug("Leaving Exchange2007Monitor.update()");
//	     }
	 
	    // return (!(fatalError));
	    return false;
	   }
	 
//	   private void resetCountersValues(int maxCounters) {
//	     if (stillActive())
//	       synchronized (this) {
//	         for (int i = 0; i < maxCounters; ++i) {
//	           setProperty("browsableValue" + (i + 1), "n/a");
//	         }
//	         setProperty(getPropertyObject("countersInError"), 0);
//	       }
//	   }
//	 
//	   private void setCountersValues(boolean fatalError, int maxCounters, Map<String, Map<String, String>> results, Map<String, String> errResults, StringBuilder errorStr)
//	   {
//	     if (stillActive())
//	       synchronized (this)
//	       {
//	         StringBuffer statestring = new StringBuffer();
//	 
//	         if (!(fatalError))
//	         {
//	           int errorCount = 0;
//	 
//	           for (int i = 0; i < maxCounters; ++i) {
//	            Boolean failed = Boolean.valueOf(false);
//	             String cType = null;
//	             String cName = null;
//	             String label = getProperty("_browseName" + (i + 1));
//	             statestring.append(label).append(" = ");
//	 
//	             String[] counter = Exch2007Util.getCounterTypeAndName(label);
//	             if (counter != null) {
//	               cType = counter[0];
//	               cName = Exch2007Util.getCounterName(counter);
//	             }
//	             if ((counter != null) && (cType != null) && (cName != null)) {
//	               Map countersValuesMappers = (Map)results.get(cType);
//	               if (countersValuesMappers != null)
//	               {
//	                 String cValue = (String)countersValuesMappers.get(cName);
//	                 if (cValue != null) {
//	                   setProperty("browsableValue" + (i + 1), cValue);
//	                   statestring.append(cValue);
//	                 } else {
//	                   failed = Boolean.valueOf(true);
//	                 }
//	               } else {
//	                 failed = Boolean.valueOf(true);
//	               }
//	             }
//	             else {
//	               failed = Boolean.valueOf(true);
//	             }
//	 
//	             if (failed.booleanValue()) {
//	               ++errorCount;
//	               setProperty("browsableValue" + (i + 1), "n/a");
//	               String error = (String)errResults.get(cType);
//	               if ((error != null) && (error.length() > 0))
//	                 statestring.append(error);
//	               else {
//	                 statestring.append("n/a");
//	               }
//	             }
//	 
//	             if (i != maxCounters - 1) {
//	               statestring.append(", ");
//	             }
//	           }
//	           setProperty(getPropertyObject("countersInError"), errorCount);
//	         }
//	         else {
//	           setProperty(pNoData, "n/a");
//	           statestring.append(errorStr.toString());
//	           setProperty(getPropertyObject("countersInError"), getMaxCounters());
//	         }
//	 
//	         setProperty(pStateString, statestring);
//	      }
//	   }
//	 
//	   private MultiValueHashMap<String, String> getCountersNamesMapper(int maxCounters)
//	   {
//	     MultiValueHashMap countersNamesMap = new MultiValueHashMap();
//	 
//	     for (int i = 0; i < maxCounters; ++i) {
//	       String id = getProperty("_browseNameid" + (i + 1));
//	       assert (id.length() > 0);
//	      String[] counter = Exch2007Util.getCounterTypeAndNameByID(id);
//	       if (counter == null) {
//	         continue;
//	       }
//	      String cType = counter[0];
//	      String cName = Exch2007Util.getCounterName(counter);
//	 
//	       countersNamesMap.putValue(cType, cName);
//	     }
//	     return countersNamesMap;
//	   }
	 
	   public boolean executeCmdlets(MultiValueHashMap<String, String> countersNamesMapper, Map<String, Map<String, String>> results, Map<String, String> errResults, StringBuilder errorStr)
	   {
	    boolean fatalError = false;
	 
	     Map propsMapper = new HashMap();
//	     propsMapper.put("IsClientAccessServer", getProperty(pClientAccess));
//	     propsMapper.put("IsEdgeServer", getProperty(pEdge));
//	     propsMapper.put("IsHubTransportServer", getProperty(pHabTransport));
//	     propsMapper.put("IsMailboxServer", getProperty(pMailbox));
//	     propsMapper.put("IsUnifiedMessagingServer", getProperty(pUnifiedMessaging));
	     propsMapper.put("$PS_CONSOLE_FILE$", this.pPscFilePath);
	     propsMapper.put("$SERVER$", this.pServer);
	     propsMapper.put("$ACCOUNT$", this.pMailboxAlias);
	     propsMapper.put("$DOMAIN_CONTROLLER$", this.pUserDomain);
	     propsMapper.put("IsClientAccessServer","True");
	     propsMapper.put("IsEdgeServer","False");
	     propsMapper.put("IsHubTransportServer", "True");
	     propsMapper.put("IsMailboxServer", "True");
	     propsMapper.put("IsUnifiedMessagingServer", "True");
	     MultiValueHashMap<String, String>  countersNamesMapper1=new MultiValueHashMap<String, String>();
	     countersNamesMapper1.putValue("test-mailflow", "IsClientAccessServer");
	     try {
	       Exch2007Util.getCmdletDataObjectResults(this.pTimeout, new ExchMngtShellCmdlet(), countersNamesMapper1, propsMapper, results, errResults);
	     } catch (Exception e) {
	      String errMsg = e.getMessage();
	       
	       fatalError = true;
	       errorStr.append(errMsg);
	     }
	 
	     return fatalError;
	   }
	 
	   static
	  {
	     //logger = LogFactory.getEasyLog(MailMonitorUtils.class.getPackage().getName() + '.' + Exchange2007Monitor.class.getSimpleName());
	 
//	     int propIndex = BrowsableBase.COUNTER_PROPERTY_INDEX;
//	     List propertyArray = new ArrayList();
//	     pServer = new StringProperty("_server", "");
//	     pServer.setDisplayText("Exchange Server", "MS Exchange Server");
//	     pServer.setParameterOptions(false, true, propIndex++, false);
//	     propertyArray.add(pServer);
//	 
//	     pUserDomain = new StringProperty("_userDomain", "");
//	     pUserDomain.setDisplayText("Exchange Domain", "The domain to which both the mailbox owner and the MS Exchange Server belong");
//	     pUserDomain.setParameterOptions(false, true, propIndex++, false);
//	     propertyArray.add(pUserDomain);
//	 
//	     pMailboxAlias = new StringProperty("_mailbox", "");
//	     pMailboxAlias.setDisplayText("Mailbox ", "The alias of the mailbox");
//	     pMailboxAlias.setParameterOptions(false, true, propIndex++, false);
//	     propertyArray.add(pMailboxAlias);
//	 
//	     pPscFilePath = new StringProperty("_pscFilePath", "C:" + File.separator + "Program Files" + File.separator + "Microsoft" + File.separator + "Exchange Server" + File.separator + "Bin" + File.separator + "ExShell.psc1");
//	     pPscFilePath.setDisplayText("Exchange PS Console File Path", "The full path to the Exchange Management Shell PowerShell console file");
//	     pPscFilePath.setParameterOptions(false, true, propIndex++, false);
//	     propertyArray.add(pPscFilePath);
//	 
//	     pTimeout = new NumericProperty("_timeout", "120", "seconds");
//	     pTimeout.setDisplayText("Timeout", "The timeout, in seconds, to wait for the response");
//	     pTimeout.setParameterOptions(false, true, propIndex++, true);
//	     propertyArray.add(pTimeout);
//	 
//	     pClientAccess = new StringProperty("_clientAccessRole", "");
//	     pClientAccess.setParameterOptions(false, true, propIndex++, true);
//	     propertyArray.add(pClientAccess);
//	 
//	     pEdge = new StringProperty("_edgeRole", "");
//	     pEdge.setParameterOptions(false, true, propIndex++, true);
//	     propertyArray.add(pEdge);
//	 
//	     pHabTransport = new StringProperty("_habTransportRole", "");
//	     pHabTransport.setParameterOptions(false, true, propIndex++, true);
//	     propertyArray.add(pHabTransport);
//	 
//	     pMailbox = new StringProperty("_mailboxRole", "");
//	     pMailbox.setParameterOptions(false, true, propIndex++, true);
//	     propertyArray.add(pMailbox);
//	 
//	     pUnifiedMessaging = new StringProperty("_unifiedMessagingRole", "");
//	     pUnifiedMessaging.setParameterOptions(false, true, propIndex++, true);
//	     propertyArray.add(pUnifiedMessaging);
//	 
//	     StringProperty[] myProperties = new StringProperty[propertyArray.size()];
//	     for (int i = 0; i < propertyArray.size(); ++i) {
//	      myProperties[i] = ((StringProperty)propertyArray.get(i));
//	     }
//	 
//	     String fullClassName = Exchange2007Monitor.class.getName();
//	 
//	     addProperties(fullClassName, myProperties);
//	     addClassElement(fullClassName, Rule.stringToClassifier("countersInError > 0\terror", true));
//	     addClassElement(fullClassName, Rule.stringToClassifier("always\tgood"));
//
//	     setClassProperty(fullClassName, "description", "Monitors MS Exchange Server operational statistics using the Exchange Management Shell");
//	     setClassProperty(fullClassName, "help", "exchange2007Mon.htm");
//	     setClassProperty(fullClassName, "title", "Microsoft Exchange 2007");
//	     setClassProperty(fullClassName, "class", "Exchange2007Monitor");
//	     setClassProperty(fullClassName, "target", "_server");
//	     setClassProperty(fullClassName, "topazName", "Microsoft Exchange 2007");
//	     setClassProperty(fullClassName, "classType", "application");
//	     setClassProperty(fullClassName, "topazType", "Application Server");
//	 
//	     objectsList = new ArrayList();
//	 
//	     initBrowseXMLObjects();
	  }
	 

	@Override
	public int handleMessage() {
		// TODO Auto-generated method stub
		return 0;
	}
	public static void main(String[] args) {
		Exchange2007Monitor mo = new Exchange2007Monitor();
		mo.pServer = "exchange2007.exchan.local";
		//mo.pPscFilePath="";
		mo.pUserDomain = "exchan.local";
		mo.pMailboxAlias="Administrator";
		mo.pTimeout=60;
		StringBuffer errorStr=new StringBuffer();
		//mo.getBrowseData(errorStr);
		mo.update();
	}

}
