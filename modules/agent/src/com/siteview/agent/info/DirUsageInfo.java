package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class DirUsageInfo extends Info {
	long total = 0L;

	long files = 0L;

	long subdirs = 0L;

	long symlinks = 0L;

	long chrdevs = 0L;

	long blkdevs = 0L;

	long sockets = 0L;

	long diskUsage = 0L;

	public long getTotal() {
		return total;
	}

	public void setTotal(long total) {
		this.total = total;
	}

	public long getFiles() {
		return files;
	}

	public void setFiles(long files) {
		this.files = files;
	}

	public long getSubdirs() {
		return subdirs;
	}

	public void setSubdirs(long subdirs) {
		this.subdirs = subdirs;
	}

	public long getSymlinks() {
		return symlinks;
	}

	public void setSymlinks(long symlinks) {
		this.symlinks = symlinks;
	}

	public long getChrdevs() {
		return chrdevs;
	}

	public void setChrdevs(long chrdevs) {
		this.chrdevs = chrdevs;
	}

	public long getBlkdevs() {
		return blkdevs;
	}

	public void setBlkdevs(long blkdevs) {
		this.blkdevs = blkdevs;
	}

	public long getSockets() {
		return sockets;
	}

	public void setSockets(long sockets) {
		this.sockets = sockets;
	}

	public long getDiskUsage() {
		return diskUsage;
	}

	public void setDiskUsage(long diskUsage) {
		this.diskUsage = diskUsage;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("Total", total);
		map.put("Files", files);
		map.put("Subdirs", subdirs);
		map.put("Symlinks", symlinks);
		map.put("Chrdevs", chrdevs);
		map.put("Blkdevs", blkdevs);
		map.put("Sockets", sockets);
		map.put("DiskUsage", diskUsage);
		return map;
	}
}
