package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class CpuInfo extends Info {
	String vendor = null;

	String model = null;

	int mhz = 0;

	long cacheSize = 0L;

	int totalCores = 0;

	int totalSockets = 0;

	int coresPerSocket = 0;

	public String getVendor() {
		return vendor;
	}

	public void setVendor(String vendor) {
		this.vendor = vendor;
	}

	public String getModel() {
		return model;
	}

	public void setModel(String model) {
		this.model = model;
	}

	public int getMhz() {
		return mhz;
	}

	public void setMhz(int mhz) {
		this.mhz = mhz;
	}

	public long getCacheSize() {
		return cacheSize;
	}

	public void setCacheSize(long cacheSize) {
		this.cacheSize = cacheSize;
	}

	public int getTotalCores() {
		return totalCores;
	}

	public void setTotalCores(int totalCores) {
		this.totalCores = totalCores;
	}

	public int getTotalSockets() {
		return totalSockets;
	}

	public void setTotalSockets(int totalSockets) {
		this.totalSockets = totalSockets;
	}

	public int getCoresPerSocket() {
		return coresPerSocket;
	}

	public void setCoresPerSocket(int coresPerSocket) {
		this.coresPerSocket = coresPerSocket;
	}

	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		String strvendor = String.valueOf(this.vendor);

		if (!"-1".equals(strvendor))
			map.put("Vendor", strvendor);
		String strmodel = String.valueOf(this.model);

		if (!"-1".equals(strmodel))
			map.put("Model", strmodel);
		String strmhz = String.valueOf(this.mhz);

		if (!"-1".equals(strmhz))
			map.put("Mhz", strmhz);
		String strcacheSize = String.valueOf(this.cacheSize);

		if (!"-1".equals(strcacheSize))
			map.put("CacheSize", strcacheSize);
		String strtotalCores = String.valueOf(this.totalCores);

		if (!"-1".equals(strtotalCores))
			map.put("TotalCores", strtotalCores);
		String strtotalSockets = String.valueOf(this.totalSockets);

		if (!"-1".equals(strtotalSockets))
			map.put("TotalSockets", strtotalSockets);
		String strcoresPerSocket = String.valueOf(this.coresPerSocket);

		if (!"-1".equals(strcoresPerSocket))
			map.put("CoresPerSocket", strcoresPerSocket);
		return map;
	}
}
