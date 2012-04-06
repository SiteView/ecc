package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class DiskUsageInfo extends Info {
	long reads = 0L;

	long writes = 0L;

	long readBytes = 0L;

	long writeBytes = 0L;

	double queue = 0.0D;

	double serviceTime = 0.0D;

	public long getReads() {
		return reads;
	}

	public void setReads(long reads) {
		this.reads = reads;
	}

	public long getWrites() {
		return writes;
	}

	public void setWrites(long writes) {
		this.writes = writes;
	}

	public long getReadBytes() {
		return readBytes;
	}

	public void setReadBytes(long readBytes) {
		this.readBytes = readBytes;
	}

	public long getWriteBytes() {
		return writeBytes;
	}

	public void setWriteBytes(long writeBytes) {
		this.writeBytes = writeBytes;
	}

	public double getQueue() {
		return queue;
	}

	public void setQueue(double queue) {
		this.queue = queue;
	}

	public double getServiceTime() {
		return serviceTime;
	}

	public void setServiceTime(double serviceTime) {
		this.serviceTime = serviceTime;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("Reads", reads);
		map.put("Writes", writes);
		map.put("ReadBytes", readBytes);
		map.put("WriteBytes", writeBytes);
		map.put("Queue", queue);
		map.put("ServiceTime", serviceTime);
		return map;
	}
}
