package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class ProcCredNameInfo extends Info {
	String user = null;

	String group = null;

	public String getUser() {
		return user;
	}

	public void setUser(String user) {
		this.user = user;
	}

	public String getGroup() {
		return group;
	}

	public void setGroup(String group) {
		this.group = group;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("User", user);
		map.put("Group", group);
		return map;
	}
}
