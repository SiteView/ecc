package com.siteview.agent;



import java.util.ArrayList;

import org.json.simple.JSONAware;
import org.json.simple.JSONObject;

public class Counter implements JSONAware  {
	private String cmd;
	private String text;
	private ArrayList<Counter> childs;

	public Counter(String cmd, String text, ArrayList<Counter> child) {
		this.cmd = cmd;
		this.text = text;
		this.childs = child;
	}

	@Override
	public String toJSONString() {
		StringBuffer sb = new StringBuffer();

		sb.append("[");
		sb.append("\"" + JSONObject.escape(cmd) + "\"");

		sb.append(",");

		sb.append("\"" + JSONObject.escape(text) + "\"");

		sb.append(",");

		sb.append("[");
		if (childs != null) {
			boolean comma = false;
			for (Counter counter : childs) {
				if(comma)
					sb.append(',');
				else
					comma = true;
				sb.append(counter.toJSONString());
			}
		}
		sb.append("]");
		sb.append("]");

		return sb.toString();
	}
}
