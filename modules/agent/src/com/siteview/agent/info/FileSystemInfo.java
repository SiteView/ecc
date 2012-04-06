package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class FileSystemInfo extends Info {
	String dirName = null;

	String devName = null;

	String typeName = null;

	String sysTypeName = null;

	String options = null;

	int type = 0;

	long flags = 0L;

	public String getDirName() {
		return dirName;
	}

	public void setDirName(String dirName) {
		this.dirName = dirName;
	}

	public String getDevName() {
		return devName;
	}

	public void setDevName(String devName) {
		this.devName = devName;
	}

	public String getTypeName() {
		return typeName;
	}

	public void setTypeName(String typeName) {
		this.typeName = typeName;
	}

	public String getSysTypeName() {
		return sysTypeName;
	}

	public void setSysTypeName(String sysTypeName) {
		this.sysTypeName = sysTypeName;
	}

	public String getOptions() {
		return options;
	}

	public void setOptions(String options) {
		this.options = options;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public long getFlags() {
		return flags;
	}

	public void setFlags(long flags) {
		this.flags = flags;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("DirName", dirName);
		map.put("DevName", devName);
		map.put("TypeName", typeName);
		map.put("SysTypeName", sysTypeName);
		map.put("Options", options);
		map.put("Type", type);
		map.put("Flags", flags);
		return map;
	}
}
