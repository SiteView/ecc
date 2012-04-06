package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class LiteralInfo extends Info {
	private String key;
	private Object value;

	public LiteralInfo(String key, Object value) {
		this.key = key;
		this.value = value;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put(key, value);
		return map;
	}

}
