package com.siteview.agent;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Properties;

public class ConfigLoader {
	public static Properties Load(Class<?> theClass, String configFile) throws FileNotFoundException, IOException
	{
		String pathAndJar = theClass.getProtectionDomain().getCodeSource().
		   getLocation().toString().replaceFirst("^(?:file:/)", "");
		String path = pathAndJar.substring(0,pathAndJar.lastIndexOf('/'));
		Properties prop = new Properties();
		FileInputStream in =new FileInputStream(path+configFile);		
		prop.load(in);
		in.close();
		return prop;		
	}
}
