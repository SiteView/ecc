package com.siteview.agent;

import java.io.FileNotFoundException;
import java.io.IOException;

import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;

public class Log {

	private Logger loger;
	private static Log log;

	private Log() {
		loger = Logger.getLogger(this.getClass());
		// loger所需的配置文件路径
		try {
			PropertyConfigurator.configure(ConfigLoader.Load(this.getClass(),
					"/conf/log4j.properties"));
		} catch (FileNotFoundException e) {
		} catch (IOException e) {
		}
	}

	public static Log getInstance(Class<?> fromClass) {
		if (log == null)
			log = new Log();
		return log;
	}

	public void info(String info, String msg) {
		this.loger.info(info + ":" + msg);
		// System.out.println(info + ":" + msg);
	}

	public void error(String info,Throwable t) {
		this.loger.error(info, t);
	}
}
