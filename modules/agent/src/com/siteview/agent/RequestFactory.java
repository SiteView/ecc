package com.siteview.agent;

import java.util.HashMap;
import java.util.Map;

import org.json.simple.JSONArray;
import org.json.simple.JSONValue;

public class RequestFactory {

	private RequestFactory() {
	}

	public static RequestBase instance(String requestStr) {
		RequestBase request = null;
		try {
			Object obj = JSONValue.parse(requestStr);
			JSONArray cmd = (JSONArray) obj;
			int commandParamCount = cmd.size();
			request = commandParamCount == 2 ? new Request(requestStr)
					: invalidRequest(requestStr);
		} catch (Exception e) {
			request = invalidRequest(requestStr);
		}
		return request;
	}

	private static RequestBase invalidRequest(final String requestStr) {
		return new RequestBase() {
			@Override
			public String getCommand() {
				return "";
			}

			@Override
			public Map<String, String> getCommandParam() {
				return new HashMap<String, String>();
			}

			@Override
			public String getOriginRequest() {
				return requestStr;
			}

			@Override
			public String toString() {
				return "Invalid request:" + requestStr;
			}
		};
	}

}
