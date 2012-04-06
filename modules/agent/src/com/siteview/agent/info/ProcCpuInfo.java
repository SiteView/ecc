package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class ProcCpuInfo extends Info {
	double percent = 0.0D;

	long lastTime = 0L;

	long startTime = 0L;

	long user = 0L;

	long sys = 0L;

	long total = 0L;

	public double getPercent() {
		return percent;
	}

	public void setPercent(double percent) {
		this.percent = percent;
	}

	public long getLastTime() {
		return lastTime;
	}

	public void setLastTime(long lastTime) {
		this.lastTime = lastTime;
	}

	public long getStartTime() {
		return startTime;
	}

	public void setStartTime(long startTime) {
		this.startTime = startTime;
	}

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
		map.put("Percent", percent);
		map.put("LastTime", lastTime);
		map.put("StartTime", startTime);
		map.put("User", user);
		map.put("Sys", sys);
		map.put("Total", total);
		return map;
	}
}
