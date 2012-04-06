package com.dragonflow.erlangecc.monitor;

import java.util.HashMap;
import java.util.Map;

import com.dragonflow.siteview.dbutils.DBMonitor;
import com.dragonflow.siteview.dbutils.DBMonitorFactory;
import com.dragonflow.siteview.infra.util.ServicePlatform;

public class MySqlDBMonitor extends BaseMonitor {
	public static final String OK = "ok";
	public static final String ERROR = "error";

	private Map<String,String> getParams() throws Exception{
		return null;
	}
	@Override
	public int handleMessage() {
		Map<String, Object> resp = new HashMap<String, Object>();
		if ("update".equals(this.getMessage().getAction())){
			try {
				resp.put("FreeSharedMemory", DBMonitorFactory.getDBMonitorValue(DBMonitor.DBTYPE_MYSQL,this.getParams(),"FreeSharedMemory"));
				this.sendResponse(OK, resp);
			} catch (Exception e) {
				e.printStackTrace();
				resp.put(ERROR, ServicePlatform.replaceUnicodeChar(e.getMessage()));
				this.sendResponse(ERROR, resp);
			}
		}else if ("getBrowseData".equals(this.getMessage().getAction())){
			try {
				this.sendResponse2(OK, resp);
			} catch (Exception e) {
				e.printStackTrace();
				resp.put(ERROR, ServicePlatform.replaceUnicodeChar(e.getMessage()));
				this.sendResponse(ERROR, resp);
			}
		}
		return 0;
	}

}
