package com.dragonflow.erlangecc.monitor;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import COM.datachannel.xml.om.Document; //import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import javax.xml.parsers.*;
import javax.xml.transform.*;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.*;
import javax.xml.xpath.*;

import com.dragonflow.erlangecc.monitor.BaseMonitor;
import com.dragonflow.siteview.ErlangUtils;
import com.dragonflow.siteview.infra.util.ServicePlatform;
import com.dragonflow.siteview.websphere.util.*;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class WebSphereMonitor extends BaseMonitor {
	// Classpath,ClientProps, getProperty(pTrustStorePassword),
	// getProperty(pKeyStore), getProperty(pKeyStorePassword),
	// constructHashIDForConnectionProperties());
	String WebSphereDir = "F:/WAS_6_1";
	String Version = "6.1x";
	String UseExternalJVM = "0";
	String ServerName = "127.0.0.1";
	String Credentials = "";
	String Username = "siteview";
	String Password;
	int Port = 8880;
	String TrustStore = "F:/WAS_6_1/DummyClientTrustFile.jks";
	String TrustStorePassword = "WebAS";
	String KeyStore = "F:/WAS_6_1/DummyClientKeyFile.jks";
	String KeyStorePassword = "WebAS";
	String Classpath = "";
	String ClientProps = "soap.client.props";
	Boolean bWindows =System.getProperties().getProperty("os.name").startsWith("Windows");
	int Timeout = 60;
	private WebSphereServer remoteServer;
	OtpErlangList EMPTY_ERLANG_LIST_STRING=new OtpErlangList();
//	List<String> Counters = new ArrayList<String>();
	Map<String, Object> Counters = new HashMap<String, Object>();
	String lastError="";
	public String buildBrowseData(StringBuffer errors) {
		File f = new File(WebSphereDir);
		if (!(f.isDirectory())) {
			errors.append("Please supply a valid Websphere Folder");
			return "";
		}

		String browseData = "";

		if (Version.equals("3.5x")) {
			StringBuffer res = new StringBuffer();

			// boolean bOK = startWebSphereEPMProcess(null, res);
			//	 
			// if (!(bOK))
			// errors.append(res);
			// else
			// browseData = res.toString();
		} else {
			// StopWatch sw = null;
			if (UseExternalJVM.equals("0")) {
				//
				// sw = new StopWatch();
				// sw.start();

				WebSphereConnectionProperties connectionProperties = getWebSphereConnectionProperties();
				WebSphereConnectorProperties connectorProperties = getConnectorProperties(errors);
				connectorProperties = (WebSphereConnectorProperties) ObjectChecks
						.nonNull(connectorProperties, "connectorProperties");

				ClassLoader websphereClassLoader = getClassLoader(errors,
						connectorProperties);
				websphereClassLoader = (ClassLoader) ObjectChecks.nonNull(
						websphereClassLoader, "websphereClassLoader");

				IWebSphereServerCommand command = WebSphereServerCommandFactory
						.getInstance().getCounterListCommand();
				WebSphereBaseUpdater updater = new WebSphereBaseUpdater(
						connectionProperties, websphereClassLoader, command);

				StringBuilder updaterErrors = new StringBuilder();
				try {
					IWebSphereServerCommandResult commanderResult = executeUpdater(
							updaterErrors, updater, Timeout);
					if (commanderResult != null) {
						String counters = (String) commanderResult.getResult();
						if ((counters != null) && (counters.length() != 0))
							browseData = counters;
					}
				} catch (Throwable throwable) {
					errors.append(updaterErrors);
					// logger.error(updaterErrors, throwable);
				}

				// if (logger.isErrorEnabled()) {
				// sw.stop();
				// logger.error("Internal JVM - Time to get data:" + sw);
				// }
			} else {
				try {
					// if (logger.isErrorEnabled())
					// {
					// sw = new StopWatch();
					// sw.start();
					// }

					this.remoteServer = getWebSphereServer(errors);
					if (this.remoteServer != null) {
						browseData = this.remoteServer.getBrowseData();
					}

					// if (logger.isErrorEnabled()) {
					// sw.stop();
					// logger.error("External JVM - Time to get data:" + sw);
					// }
				} catch (RemoteException e) {
					// logger.error("WebSphereMonitor received RemoteException in getBrowseData(): "
					// + e.getMessage(), e);
					errors
							.append("An error occurred while attempting to retrieve list of WebSphere counters. Please check the connection properties, verify that the application server is running, and try again. (See the error log for details.)");
				}
			}
		}
		 this.lastError= errors.toString();
		return browseData;
	}

	private ClassLoader getClassLoader(StringBuffer errors,
			WebSphereConnectorProperties connectorProperties) {
		try {
			ClassLoader websphereClassLoader = WebSphereClassLoaderManager
					.getInstance().getClassLoader(connectorProperties);
			return websphereClassLoader;
		} catch (Exception e) {

			errors
					.append("siteview was unable to initialize WebSphereMonitor ClassLoader due to an exception (See the logs for details)");
		}

		return null;
	}

	private WebSphereConnectionProperties getWebSphereConnectionProperties() {
		// new WebSphereConnectionProperties(getProperty(pServerName),
		// getUsername(pCredentials, pUsername), getPassword(pCredentials,
		// pPassword), getPropertyAsInteger(pPort), getProperty(pVersion),
		// getProperty(pTrustStore), getProperty(pTrustStorePassword),
		// getProperty(pKeyStore), getProperty(pKeyStorePassword),
		// constructHashIDForConnectionProperties());
		WebSphereConnectionProperties connectionProperties = new WebSphereConnectionProperties(
				ServerName, Username, Password, Port, Version, TrustStore,
				TrustStorePassword, KeyStore, KeyStorePassword,
				constructHashIDForConnectionProperties());
		return connectionProperties;
	}

	private WebSphereConnectorProperties getConnectorProperties(
			StringBuffer errors) {
		try {
			WebSphereConnectorProperties connectorProperties = new WebSphereConnectorProperties(
					Classpath, ClientProps, WebSphereDir, Version, bWindows,
					ServerName);
			return connectorProperties;
		} catch (Exception e) {

			errors
					.append("SiteView was unable to initialize WebSphere Connector properties due to an exception (See the logs for details)");
		}
		return null;
	}

	private String constructHashIDForConnectionProperties() {
		StringBuffer hashIDStringBuffer = new StringBuffer(ServerName).append(
				":").append(Port).append(":").append(Version).append(":");

		// String userAndPassword = getUsername(pCredentials, pUsername) +
		// getPassword(pCredentials, pPassword);
		String userAndPassword = Username + Password;
		String userAndPassHash = "";
		try {
			MessageDigest usernamePasswordDigest = MessageDigest
					.getInstance("SHA-1");
			usernamePasswordDigest.update(userAndPassword.getBytes());
			byte[] sha1Digest = usernamePasswordDigest.digest();

			ByteArrayOutputStream bos = new ByteArrayOutputStream();
			ByteArrayInputStream bis = new ByteArrayInputStream(sha1Digest);
			Base64Encoder enc = new Base64Encoder(bis, bos);
			enc.process();
			userAndPassHash = bos.toString();
		} catch (NoSuchAlgorithmException e) {
			// logger.error("WebSphereMonitor.constructHashIDForConnectionProperties() failed due to an exception: "
			// + e);
		} catch (IOException e) {
			// logger.error("WebSphereMonitor.constructHashIDForConnectionProperties() failed due to an exception: "
			// + e);
		}

		return userAndPassHash;
	}

	private <T> IWebSphereServerCommandResult<T> executeUpdater(
			StringBuilder updaterErrors, WebSphereBaseUpdater<T> updater,
			int timeout) throws Throwable {
		updater = (WebSphereBaseUpdater) ObjectChecks.nonNull(updater,
				"WebSphere base updater");
		try {
			Thread updaterThread = new Thread(updater,
					WebSphereBaseUpdater.class.getSimpleName());
			updaterThread.start();
			updaterThread.join(timeout * 10000000);
			if (updaterThread.isAlive()) {
				updaterErrors
						.append("Timeout to get data from remote WebSphere Application server (See the logs for details)");
				return null;
			}
			if (updater.isUpdateSuccessful()) {
				return updater.getCommandResult();
			}
			Throwable throwable = updater.getThrowable();
			updaterErrors.append(updater.getErrors());
			// if (logger.isDebugEnabled())
			// {
			updaterErrors.append("\n");
			updaterErrors.append("The root cause exception: ");
			// }

			throw throwable;
		} catch (InterruptedException e) {
			updaterErrors
					.append("WebSphere Updater thread is waiting, sleeping, or otherwise occupied, and the thread is interrupted, either before or during the activity (See the logs for details)");
			throw new RuntimeException(e);
		}
	}

	private WebSphereService getWebSphereService(long timeoutMillis, String url)
			throws MalformedURLException, RemoteException {
		long currentTime = new Date().getTime();
		long endTime = currentTime + timeoutMillis;

		while (endTime > currentTime) {
			try {
				return ((WebSphereService) Naming.lookup(url));
			} catch (NotBoundException e) {
				currentTime = new Date().getTime();
			} catch (RemoteException re) {
				currentTime = new Date().getTime();
				if (currentTime + 500L > endTime)
					throw re;
			}
			try {
				Thread.sleep(500L);
			} catch (InterruptedException e) {
				// if (logger.isErrorEnabled()) {
				// logger.error(e.getMessage());
				// }
			}
		}
		return null;
	}

	private WebSphereServer getWebSphereServer(StringBuffer errorMsgs) {
		WebSphereProcessProperties processProps = new WebSphereProcessProperties(
				Classpath, ClientProps, WebSphereDir, Version, bWindows,
				ServerName);

		WebSphereConnectionProperties connProps = getWebSphereConnectionProperties();

		int status = RMIProcessLauncher.launch(processProps);
		if ((status == 0) || (status == 1)) {
			if (System.getSecurityManager() == null) {
				System.setSecurityManager(new NullSecurityManager());
			}

			long timemout = Timeout * 1000;
			if (timemout <= 0L)
				timemout = 10000L;
			try {
				WebSphereService svc;
				if ((svc = getWebSphereService(timemout, processProps.getURL())) != null) {
					return svc.getServer(connProps);
				}
				String errorMsg = "WebSphereService was never bound to the local registry in WebSphereMonitor.getWebSphereServer(). URL was:  "
						+ processProps.getURL();
				// logger.error(errorMsg);
				errorMsgs.append(errorMsg);
				return null;
			} catch (MalformedURLException e) {
				// logger.error("Exception while looking up WebSphereService at URL: "
				// + processProps.getURL() + ". Exception was: " +
				// e.getMessage(), e);

				errorMsgs.append(e);
				return null;
			} catch (RemoteException e) {
				// logger.error("WebSphereMonitor received RemoteException in getWebSphereService(): "
				// + e.getMessage(), e);
				errorMsgs.append(e);
				return null;
			}
		}
		// logger.error("WebSphereMonitor failed to launch WebSphereService.");
		errorMsgs.append("WebSphereMonitor failed to launch WebSphereService.");
		return null;
	}

	public void initArgs(Map<String, Object> map) {
		String s = null;

		 try {
		 s = (String) map.get("server");
		 if (s != null && s.length() > 0) {
		 this.ServerName = s.trim();
		 }
		 s = (String) map.get("port");
		 if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.Port = Integer.parseInt(s.trim()) ;
		 }
		 s = (String) map.get("username");
		 if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.Username = s.trim();
		 }
		 s = (String) map.get("password");
		 if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.Password = s.trim();
		 }
		 s = (String) map.get("version");
		 if (s != null && s.length() > 0
		 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.Version = s.trim();
		 }
		 s = (String) map.get("webSphereDir");
		 if (s != null && s.length() > 0
		 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.WebSphereDir = s.trim();
		 }
		 s = (String) map.get("clientProps");
		 if (s != null && s.length() > 0
		 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.ClientProps = s.trim();
		 }
		 s = (String) map.get("timeout");
		 if (s != null && (s.length() > 0)
		 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.Timeout =Integer.parseInt( s.trim());
		 }
		 s = (String) map.get("password");
		 if (s != null && s.length() > 0
		 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.Password = s.trim();
		 }
		 s = (String) map.get("trustStore");
		 if (s != null && s.length() > 0
		 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.TrustStore = s.trim();
		 }
		 s = (String) map.get("trustStorePassword");
		 if (s != null && s.length() > 0
		 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.TrustStorePassword = s.trim();
		 }
		 s = (String) map.get("keyStore");
		 if (s != null && s.length() > 0
		 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.KeyStore = s.trim();
		 }
		 s = (String) map.get("keyStorePassword");
		 if (s != null && s.length() > 0
		 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.KeyStorePassword = s.trim();
		 }			
		
		 } catch (Exception e) {
		 lastError = e.getMessage();
		 e.printStackTrace();
		 }

	}
	public void initArgs1(Map<String, Object> map) {
		String s = null;

		 try {
		 s = (String) map.get("server");
		 if (s != null && s.length() > 0) {
		 this.ServerName = s.trim();
		 }
		 s = (String) map.get("port");
		 if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.Port = Integer.parseInt(s.trim()) ;
		 }
		 s = (String) map.get("username");
		 if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.Username = s.trim();
		 }
		 s = (String) map.get("password");
		 if (s != null && s.length() > 0 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.Password = s.trim();
		 }
		 s = (String) map.get("version");
		 if (s != null && s.length() > 0
		 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.Version = s.trim();
		 }
		 s = (String) map.get("webSphereDir");
		 if (s != null && s.length() > 0
		 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.WebSphereDir = s.trim();
		 }
		 s = (String) map.get("clientProps");
		 if (s != null && s.length() > 0
		 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.ClientProps = s.trim();
		 }
		 s = (String) map.get("timeout");
		 if (s != null && (s.length() > 0)
		 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.Timeout =Integer.parseInt( s.trim());
		 }
		 s = (String) map.get("password");
		 if (s != null && s.length() > 0
		 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.Password = s.trim();
		 }
		 s = (String) map.get("trustStore");
		 if (s != null && s.length() > 0
		 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.TrustStore = s.trim();
		 }
		 s = (String) map.get("trustStorePassword");
		 if (s != null && s.length() > 0
		 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.TrustStorePassword = s.trim();
		 }
		 s = (String) map.get("keyStore");
		 if (s != null && s.length() > 0
		 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.KeyStore = s.trim();
		 }
		 s = (String) map.get("keyStorePassword");
		 if (s != null && s.length() > 0
		 && !s.equals(EMPTY_ERLANG_LIST_STRING)) {
		 this.KeyStorePassword = s.trim();
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
							this.Counters.put(id, value);
						}
					}
				}
			} else {
				this.Counters.put("MALFORMED COUNTER ID", null);
			}
//	this.Counters = ErlangUtils.erlangList2arrayList(counters);
		
		 } catch (Exception e) {
		 lastError = e.getMessage();
		 e.printStackTrace();
		 }

	}

	/**
	 * doc2XmlFile 将Document对象保存为一个xml文件到本地
	 * 
	 * @return true:保存成功 flase:失败
	 * @param filename
	 *            保存的文件名
	 * @param document
	 *            需要保存的document对象
	 */
	public boolean doc2XmlFile(org.w3c.dom.Document document, String filename) {
		boolean flag = true;
		try {
			/** 将document中的内容写入文件中 */
			TransformerFactory tFactory = TransformerFactory.newInstance();
			Transformer transformer = tFactory.newTransformer();
			/** 编码 */
			// transformer.setOutputProperty(OutputKeys.ENCODING, "GB2312");
			// transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
			DOMSource source = new DOMSource(document);
			StreamResult result = new StreamResult(new File(filename));
			transformer.transform(source, result);
		} catch (Exception ex) {
			flag = false;
			ex.printStackTrace();
		}
		return flag;
	}

	public org.w3c.dom.Document stringToDoc(String xmlStr) {
		// 字符串转XML
		org.w3c.dom.Document doc = null;
		try {
			// xmlStr = new String(xmlStr.getBytes(),"UTF-8");
			StringReader sr = new StringReader(xmlStr);
			InputSource is = new InputSource(sr);
			DocumentBuilderFactory factory = DocumentBuilderFactory
					.newInstance();
			DocumentBuilder builder;
			builder = factory.newDocumentBuilder();
			doc = builder.parse(is);

		} catch (ParserConfigurationException e) {
			System.err.println(xmlStr);
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SAXException e) {
			System.err.println(xmlStr);
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			System.err.println(xmlStr);
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return doc;
	}
	public Map<String, Object> update()
	   {
//	     if (logger.isDebugEnabled())
//	       logger.debug("Entering WebSphereMonitor.update().");
//	     int len = getMaxCounters();
//	 
//	     if (len == 0) {
//	       if (logger.isDebugEnabled())
//	         logger.debug("Leaving WebSphereMonitor.update() because there were no counters to retrieve.");
//	       return true;
//	     }
		Map<String, Object> result = new HashMap<String, Object>();

		this.initArgs1(this.getMessage().getParams());
		int len=this.Counters.size();
		WebSphereCounter[] counters = new WebSphereCounter[len];
		int i=0;
		for(String s :this.Counters.keySet())
		{
		  counters[i] = new WebSphereCounter(s);
		  String s1=this.Counters.get(s).toString();//.replace("-", "/");
		  result.put(s1, "n/a");
		  i=i+1;
		}
	     
	     boolean updateSuccessful = false;
	     StringBuffer errors = new StringBuffer();
	     if (this.Version.equals("3.5x")) {
//	       updateSuccessful = getCountersEPM(counters, errors);
	     } else {
//	       StopWatch sw = null;
	       if (UseExternalJVM.equals("0"))  {
//	         if (logger.isErrorEnabled())
//	         {
//	           sw = new StopWatch();
//	           sw.start();
//	         }
	 
	         WebSphereConnectionProperties connectionProperties = getWebSphereConnectionProperties();
	 
	         WebSphereConnectorProperties connectorProperties = getConnectorProperties(errors);
	         connectorProperties = (WebSphereConnectorProperties)ObjectChecks.nonNull(connectorProperties, "connectorProperties");
	 
	         ClassLoader websphereClassLoader = getClassLoader(errors, connectorProperties);
	         websphereClassLoader = (ClassLoader)ObjectChecks.nonNull(websphereClassLoader, "websphereClassLoader");
	 
	         IWebSphereServerCommand command = WebSphereServerCommandFactory.getInstance().getCounterValuesCommand(counters);
	         WebSphereBaseUpdater updater = new WebSphereBaseUpdater(connectionProperties, websphereClassLoader, command);
	 
	         StringBuilder updaterErrors = new StringBuilder();
	         try
	         {
	           IWebSphereServerCommandResult commanderResult = executeUpdater(updaterErrors, updater, Timeout);
	           if (commanderResult != null) {
	             WebSphereCounter[] counterValues = (WebSphereCounter[])commanderResult.getResult();
	             if (counterValues != null) {
	               counters = counterValues;
	               updateSuccessful = true;
	             } else {
	               updateSuccessful = false;
	             }
	           }
	         } catch (Throwable throwable) {
	           errors.append(updaterErrors);
	 
//	           logger.error(updaterErrors, throwable);
	         }
	 
//	         if (logger.isErrorEnabled()) {
//	           sw.stop();
//	           logger.error("Internal JVM - Time to get data:" + sw);
//	         }
	       } else {
//	         if (logger.isErrorEnabled())
//	         {
//	           sw = new StopWatch();
//	           sw.start();
//	         }
//	         try
//	         {
//          WebSphereJmxUpdater updater = new WebSphereJmxUpdater(counters, errors);
//	           Thread updaterThread = new Thread(updater, WebSphereJmxUpdater.class.getSimpleName());
//	           updaterThread.start();
//	           updaterThread.join(getPropertyAsInteger(pTimeout) * 1000);
//	           if (updaterThread.isAlive())
//	           {
//	             errors.append("Timeout");
//	           } else {
//	             updateSuccessful = updater.wasUpdateSuccessful();
//	             counters = updater.getCounters();
//	           }
//	         } catch (InterruptedException e) {
//	           throw new RuntimeException(e);
//	         }
//	 
//	         if (logger.isErrorEnabled()) {
//	           sw.stop();
//	           logger.error("External JVM - Time to get data:" + sw);
//	         }
	       }
	     }
	 
	     if (updateSuccessful) {
	           int errorCount = 0;
	           StringBuffer statestring = new StringBuffer();
	 
	           for (int j = 0; j < len; ++j) {
	             String value = counters[j].getValue();
	 
	             if (value == null) {
	               value = "n/a";
	             }
	 
	            if ((counters[j].isError()) || (value.indexOf("n/a") >= 0) || (value.trim().equals("NaN"))) {
	               ++errorCount;
	             }
	             Object[] vvalues=this.Counters.values().toArray();
	             String label1 =vvalues[j].toString();
	             String label =counters[j].getName();
	             String error = counters[j].getErrorMessage();
	             if ((error != null) && (error.length() > 0)) {
	               statestring.append(label1).append(" = ").append(error);
	             }
	             else if (value.equals(""))
	               statestring.append(label1).append(" = \"\"");
	             else {
	               statestring.append(label1).append(" = ").append(value);
	             }
	 
	             if (j != len - 1)
	               statestring.append(", ");
	             try
					{
					int V=Integer.parseInt(value) ;
					result.put(label, V);
					}catch(Exception ex)
					{
						 result.put(label, value);
					}
	            
	           }
	           result.put("stateString",statestring.toString());
	           result.put("countersInError", errorCount); 
	      
	     }
	     else {
	    	 result.put("stateString", "update fail");
	         result.put("countersInError", len);
	         errors.append("update fail");
	 
	     }
	     this.lastError= errors.toString();
	 
//	     if (logger.isDebugEnabled())
//	       logger.debug("Leaving WebSphereMonitor.update() and returning true.");
	     return result;
	   }
	@Override
	public int handleMessage() {
		// TODO Auto-generated method stub
		String action = this.getMessage().getAction();
		String error = null;

		Map<String, Object> resp = null;

		if (action != null && action.equals("update")) 
		{
			resp = this.update();
			error = this.lastError.toString();
			ArrayList<String> R = new ArrayList<String>();
			if (lastError.length() > 0) {
				R.add(lastError);
				this.sendResponse3("error", R);
			} else {
				System.out.println("update result: " + resp);
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
				resp.put("error", ServicePlatform.replaceUnicodeChar(error));
				this.sendResponse("error", resp);
			}
			else 
			{
				System.out.println("getBrowseData data result: \n" + resp);
				this.sendResponse2("ok", resp);
			}
		}	
		return 0;
	}

  //parentId 是为处理Additional Metrics 
	public void buildTreemap(Map<String, Object> res_map, Element node,
			String parentName,String parentId,Boolean isAdditional) {
		if (node == null || !node.hasChildNodes()) {
			return;
		}
		String strTemp = "";
		NodeList tnodelist = node.getChildNodes();
		int i = tnodelist.getLength();
		try {
			for (int j = 0; j < tnodelist.getLength(); j++) {
				Element tempNode = (Element) tnodelist.item(j);
				String type=tempNode.getTagName().toLowerCase();
				String id = "";
				id = tempNode.getAttribute("id");
				String solutionsID = tempNode.getAttribute("solutionsID");
				if (id.isEmpty())
					id = solutionsID;
				
				//特殊
				if (isAdditional)
				{
				  id=parentId+id;	
				}
				String name=tempNode.getAttribute("name").replace("/", "-");
				String name1=tempNode.getAttribute("name");
				if (parentName == "") {
					if (!id.isEmpty()) {
						if (!res_map.containsKey(id))
						{
						if (type.equals("counter"))
							res_map.put(id, name);
//							res_map.put(name1, name);
						else
							res_map.put(id, name);
						}
							
					}
				} else {
					
					if (!id.isEmpty()) {
						if (!res_map.containsKey(id))
						{
						if (type.equals("counter"))
//						res_map.put(parentName1+name1,parentName+name);
							res_map.put(id,parentName+name);
						else
						res_map.put(id,parentName+name);
						}
					}
				}
				//特殊处理
	
				if(name.equals("Additional Metrics"))
				{
					buildTreemap(res_map,tempNode,parentName+name+"/","",true);
					continue;
				}
				if(isAdditional)
				{
				 buildTreemap(res_map,tempNode,parentName+name+"/",id+"/",isAdditional);
				}
				else
				{
				 buildTreemap(res_map,tempNode,parentName+name+"/","",isAdditional);
				}
			}
		} catch (Exception r) {
			System.out.println(r);
		}

	}

	public Map<String, Object> getBrowseData() {
		Map<String, Object> res_map = new HashMap<String, Object>();

		this.initArgs(this.getMessage().getParams());
		StringBuffer stringbuffer = new StringBuffer();
		String filename = ServicePlatform.getRoot()
				+ "/templates.applications/WebSphere_" + this.ServerName + "_"
				+ String.valueOf(this.Port) + "_" + this.Version;
		if (!this.Username.isEmpty()) {
			filename = filename + "_" + this.Username.trim();
		}
		if (!this.TrustStorePassword.isEmpty()) {
			filename = filename + "_" + this.TrustStorePassword.trim();
		}
		if (!this.ClientProps.isEmpty()) {
			filename = filename + "_soap.client.props_.xml";
		}

		File f = new File(filename);
		Document document = new Document();
		if (f.exists()) {
			// try {
			// DocumentBuilderFactory domfac =
			// DocumentBuilderFactory.newInstance();
			// DocumentBuilder dombuilder = domfac.newDocumentBuilder();
			// try{
			// document= dombuilder.parse(filename);
			// }catch(Exception e)
			// {
			// System.err.println(e);
			// System.exit(1);
			// }
			//
			// } catch (Exception e) {
			//
			// }
			//			

		} else {
			String s1 = buildBrowseData(stringbuffer);
			document.loadXML(s1);

			// document = stringToDoc(s1);
			// doc2XmlFile(document, filename);

		}

		Element root = (Element) document.getFirstChild();
		buildTreemap(res_map, root, "","",false);
		return res_map;
	}
	
	public static void main(String[] args) {
		StringBuffer eror = new StringBuffer();

		WebSphereMonitor testmonitor = new WebSphereMonitor();
		testmonitor.getBrowseData();

		String a = "";
	}
}
