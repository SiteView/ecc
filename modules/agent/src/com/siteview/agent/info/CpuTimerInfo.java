package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class CpuTimerInfo extends Info {
	private long totalTime;
	private long cpuTotal;
	private long cpuUser;
	private long cpuSys;
	private long cpuSampleFirst;
	private long cpuSampleLast;
	private long cpuSampleTime;
	private long startTime;
	private long stopTime;

	public long getTotalTime() {
		return totalTime;
	}

	public void setTotalTime(long totalTime) {
		this.totalTime = totalTime;
	};

	public long getCpuTotal() {
		return cpuTotal;
	}

	public void setCpuTotal(long cpuTotal) {
		this.cpuTotal = cpuTotal;
	};

	public long getCpuUser() {
		return cpuUser;
	}

	public void setCpuUser(long cpuUser) {
		this.cpuUser = cpuUser;
	};

	public long getCpuSys() {
		return cpuSys;
	}

	public void setCpuSys(long cpuSys) {
		this.cpuSys = cpuSys;
	};

	public long getCpuSampleFirst() {
		return cpuSampleFirst;
	}

	public void setCpuSampleFirst(long cpuSampleFirst) {
		this.cpuSampleFirst = cpuSampleFirst;
	};

	public long getCpuSampleLast() {
		return cpuSampleLast;
	}

	public void setCpuSampleLast(long cpuSampleLast) {
		this.cpuSampleLast = cpuSampleLast;
	};

	public long getCpuSampleTime() {
		return cpuSampleTime;
	}

	public void setCpuSampleTime(long cpuSampleTime) {
		this.cpuSampleTime = cpuSampleTime;
	};

	public long getStartTime() {
		return startTime;
	}

	public void setStartTime(long startTime) {
		this.startTime = startTime;
	};

	public long getStopTime() {
		return stopTime;
	}

	public void setStopTime(long stopTime) {
		this.stopTime = stopTime;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("TotalTime", totalTime);
		map.put("CpuTotal", cpuTotal);
		map.put("CpuUser", cpuUser);
		map.put("CpuSys", cpuSys);
		map.put("CpuSampleFirst", cpuSampleFirst);
		map.put("CpuSampleLast", cpuSampleLast);
		map.put("CpuSampleTime", cpuSampleTime);
		map.put("StartTime", startTime);
		map.put("StopTime", stopTime);
		return map;
	}
}
