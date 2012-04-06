package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class WhoInfo extends Info {
	String user = null;

	String device = null;

	String host = null;

	long time = 0L;

	public String getUser() {
		return user;
	}

	public void setUser(String user) {
		this.user = user;
	}

	public String getDevice() {
		return device;
	}

	public void setDevice(String device) {
		this.device = device;
	}

	public String getHost() {
		return host;
	}

	public void setHost(String host) {
		this.host = host;
	}

	public long getTime() {
		return time;
	}

	public void setTime(long time) {
		this.time = time;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("User", user);
		map.put("Device", device);
		map.put("Host", host);
		map.put("Time", time);
		return map;
	}
}
