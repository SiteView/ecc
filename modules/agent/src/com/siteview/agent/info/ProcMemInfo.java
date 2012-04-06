package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class ProcMemInfo extends Info {
	long size = 0L;

	long resident = 0L;

	long share = 0L;

	long minorFaults = 0L;

	long majorFaults = 0L;

	long pageFaults = 0L;

	public long getSize() {
		return size;
	}

	public void setSize(long size) {
		this.size = size;
	}

	public long getResident() {
		return resident;
	}

	public void setResident(long resident) {
		this.resident = resident;
	}

	public long getShare() {
		return share;
	}

	public void setShare(long share) {
		this.share = share;
	}

	public long getMinorFaults() {
		return minorFaults;
	}

	public void setMinorFaults(long minorFaults) {
		this.minorFaults = minorFaults;
	}

	public long getMajorFaults() {
		return majorFaults;
	}

	public void setMajorFaults(long majorFaults) {
		this.majorFaults = majorFaults;
	}

	public long getPageFaults() {
		return pageFaults;
	}

	public void setPageFaults(long pageFaults) {
		this.pageFaults = pageFaults;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("Size", size);
		map.put("Resident", resident);
		map.put("Share", share);
		map.put("MinorFaults", minorFaults);
		map.put("MajorFaults", majorFaults);
		map.put("PageFaults", pageFaults);
		return map;
	}
}
