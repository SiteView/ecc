package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class MemInfo extends Info {
	long total = 0L;

	long ram = 0L;

	long used = 0L;

	long free = 0L;

	long actualUsed = 0L;

	long actualFree = 0L;

	double usedPercent = 0.0D;

	double freePercent = 0.0D;

	public long getTotal() {
		return total;
	}

	public void setTotal(long total) {
		this.total = total;
	}

	public long getRam() {
		return ram;
	}

	public void setRam(long ram) {
		this.ram = ram;
	}

	public long getUsed() {
		return used;
	}

	public void setUsed(long used) {
		this.used = used;
	}

	public long getFree() {
		return free;
	}

	public void setFree(long free) {
		this.free = free;
	}

	public long getActualUsed() {
		return actualUsed;
	}

	public void setActualUsed(long actualUsed) {
		this.actualUsed = actualUsed;
	}

	public long getActualFree() {
		return actualFree;
	}

	public void setActualFree(long actualFree) {
		this.actualFree = actualFree;
	}

	public double getUsedPercent() {
		return usedPercent;
	}

	public void setUsedPercent(double usedPercent) {
		this.usedPercent = usedPercent;
	}

	public double getFreePercent() {
		return freePercent;
	}

	public void setFreePercent(double freePercent) {
		this.freePercent = freePercent;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("Total", total);
		map.put("Ram", ram);
		map.put("Used", used);
		map.put("Free", free);
		map.put("ActualUsed", actualUsed);
		map.put("ActualFree", actualFree);
		map.put("UsedPercent", usedPercent);
		map.put("FreePercent", freePercent);
		return map;
	}
}
