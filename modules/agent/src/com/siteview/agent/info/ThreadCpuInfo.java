package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class ThreadCpuInfo extends Info {
	long user = 0L;

	long sys = 0L;

	long total = 0L;

	public long getUser() {
		return user;
	}

	public void setUser(long user) {
		this.user = user;
	}

	public long getSys() {
		return sys;
	}

	public void setSys(long sys) {
		this.sys = sys;
	}

	public long getTotal() {
		return total;
	}

	public void setTotal(long total) {
		this.total = total;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("User", user);
		map.put("Sys", sys);
		map.put("Total", total);
		return map;
	}
}
