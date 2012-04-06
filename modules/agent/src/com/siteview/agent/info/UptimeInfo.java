package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class UptimeInfo extends Info {
	double uptime = 0.0D;

	public double getUptime() {
		return uptime;
	}

	public void setUptime(double uptime) {
		this.uptime = uptime;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("Uptime", uptime);
		return map;
	}
}
