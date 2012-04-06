package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class ProcExeInfo extends Info {
	String name = null;

	String cwd = null;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getCwd() {
		return cwd;
	}

	public void setCwd(String cwd) {
		this.cwd = cwd;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("Name", name);
		map.put("Cwd", cwd);
		return map;
	}
}
