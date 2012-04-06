package com.dragonflow.erlangecc.monitor.weblogic;

import java.io.File;
import java.util.ArrayList;

import com.dragonflow.siteview.node.JavaNode;



public class WeblogicProcessProperties extends RMIProcessProperties
{

	public WeblogicProcessProperties(String weblogicJar, String wlCipherJar, String license, String jvm, String additionalClasspaths[], boolean isGetBrowseData)
	{
		this.jvm = null;
		isWindows = System.getProperties().getProperty("os.name").startsWith("Windows");
		this.isGetBrowseData = false;
		this.isGetBrowseData = isGetBrowseData;
		this.additionalClasspaths = additionalClasspaths;
		if(jvm == null || jvm.equals(""))
			setJVM();
		else
			this.jvm = jvm;
		this.weblogicJar = weblogicJar;
		this.wlCipherJar = wlCipherJar;
		setBEAHome(license);
		StringBuffer hashKeySB = (new StringBuffer(this.weblogicJar)).append(this.wlCipherJar).append(beaHome).append(this.jvm).append(this.isGetBrowseData);
		if(additionalClasspaths != null)
		{
			for(int i = 0; i < additionalClasspaths.length; i++)
				hashKeySB.append(additionalClasspaths[i]);

		}
		hashKey = hashKeySB.toString().hashCode();
	}

	private void setJVM()
	{
		String strHome = System.getProperty("java.home");
		if (strHome.endsWith(";"))
			strHome.replaceAll(";", "");
		jvm = strHome + File.separator + "bin" +  File.separator  + "java.exe";
	}

	private void setBEAHome(String license)
	{
		if(license != null && license.length() > 0)
		{
			File licenseFile = new File(license);
			beaHome = licenseFile.getAbsoluteFile().getParent();
		}
	}

	public String[] getCommandLineArray()
	{
		String packagePrefix = getClass().getPackage().getName();
		ArrayList argumentList = new ArrayList();
		argumentList.add(jvm);
		if(extraCommandLineArgs != null && extraCommandLineArgs.length > 0)
		{
			for(int i = 0; i < extraCommandLineArgs.length; i++)
				argumentList.add(extraCommandLineArgs[i]);

		}
		argumentList.add("-classpath");
		argumentList.add(getClasspathString());
		argumentList.add((new StringBuilder()).append("-Djava.ext.dirs=\"").append(JavaNode.getLibPath()).append("\"").toString());
		argumentList.add((new StringBuilder()).append("-Djava.rmi.server.codebase=").append(CODEBASE).toString());
		argumentList.add("-Dsun.lang.ClassLoader.allowArraySyntax=true");
		argumentList.add("-Djava.rmi.server.hostname=localhost");
		argumentList.add((new StringBuilder()).append("-D").append(packagePrefix).append(".").append("registryURL").append("=").append(getURL()).toString());
		argumentList.add((new StringBuilder()).append("-D").append(packagePrefix).append(".").append("heartBeatFrequency").append("=").append(heartBeatFrequency).toString());
		argumentList.add((new StringBuilder()).append("-D").append(packagePrefix).append(".").append("serviceTimeout").append("=").append(wlServiceObjectTimeout).toString());
		argumentList.add((new StringBuilder()).append("-D").append(packagePrefix).append(".").append("token").append("=").append(siteViewToken).toString());
		if(weblogicJar != null && weblogicJar.length() > 0)
			argumentList.add((new StringBuilder()).append("-D").append(packagePrefix).append(".").append("weblogicJarURL").append("=file:///").append(getJarFileURL(weblogicJar)).toString());
		if(wlCipherJar != null && wlCipherJar.length() > 0)
			argumentList.add((new StringBuilder()).append("-D").append(packagePrefix).append(".").append("wlcipherJarURL").append("=file:///").append(getJarFileURL(wlCipherJar)).toString());
		if(beaHome != null && beaHome.length() > 0)
			argumentList.add((new StringBuilder()).append("-Dbea.home=").append(beaHome).toString());
		String launchWithRemoteDebug = System.getProperty("weblogic.monitor.remote.debugger");
		if(launchWithRemoteDebug != null && launchWithRemoteDebug.equalsIgnoreCase("true"))
		{
			argumentList.add("-Xdebug");
			argumentList.add("-Xnoagent");
			argumentList.add("-Djava.compiler=NONE");
			argumentList.add("-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=5005");
		}

		argumentList.add(WEBLOGIC_SERVICE_CLASSNAME);
		String args[] = new String[argumentList.size()];
		args = (String[])(String[])argumentList.toArray(args);
		return args;
	}

	private String getJarFileURL(String jarFile)
	{
		String jarPathToUse = null;
		if(jarFile.charAt(0) == '/')
			jarPathToUse = jarFile.substring(1);
		else
			jarPathToUse = jarFile;
		return jarPathToUse;
	}

	public int getHashKey()
	{
		return hashKey;
	}

	public String getURL()
	{
		StringBuffer u = (new StringBuffer("rmi://")).append("127.0.0.1").append(":").append(RMIProcessLauncher.getRegistryPort());
		u.append("/WebLogicService").append(hashKey);
		return u.toString();
	}

	private String getClasspathString()
	{
		StringBuffer classpathString = new StringBuffer();
		String strCur = JavaNode.getCurrentDir();
		classpathString.append(".");
		//classpathString.append(File.pathSeparator).append("\"").append(strCur).append("\"").append(File.pathSeparator);
		//classpathString.append(".").append(File.pathSeparator).append(CODEBASE).append(File.pathSeparator);
		//classpathString.append(strCur + File.separator + "lib" + File.separator + "jsafe.jar").append(File.pathSeparator);
		if(additionalClasspaths != null)
		{
			for(int i = 0; i < additionalClasspaths.length; i++)
				classpathString.append(additionalClasspaths[i]).append(File.pathSeparator);

		}
		return classpathString.toString();
	}

	public String getLogPrefix()
	{
		return (new StringBuilder()).append("WebLogicService[").append(hashKey).append("]: ").toString();
	}

	public void registerNewProcess(Process p)
	{
		super.registerNewProcess(p);
		(new WebLogicProcessWatchdogThread(this)).start();
	}

	public static long getWatchdogThreadFrequency()
	{
		return watchdogThreadFrequency;
	}

	public synchronized void kill()
	{
		if(isRunning)
		{
			proc.destroy();
			isRunning = false;
		}
	}

	String weblogicJar;
	String wlCipherJar;
	String beaHome;
	String additionalClasspaths[];
	String jvm;
	private int hashKey;
	boolean isWindows;
	private static long heartBeatFrequency;
	private static long watchdogThreadFrequency;
	private static long wlServiceObjectTimeout;
	private static long siteViewToken = System.currentTimeMillis();
	private static String extraCommandLineArgs[] = null;
	private static final String WEBLOGIC_SERVICE_CLASSNAME ="com.dragonflow.erlangecc.monitor.weblogic.WeblogicServiceImpl";
	boolean isGetBrowseData;

	static 
	{
		
		heartBeatFrequency = 3000L;
		watchdogThreadFrequency = 10000L;
		wlServiceObjectTimeout = 60000L;
	}
}

