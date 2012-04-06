package com.siteview.agent;

import java.util.Map;

public abstract class RequestBase {

	public abstract String getCommand();

	public abstract Map<String, String> getCommandParam() ;

	public abstract String getOriginRequest();

	public boolean valid() {
		return false;
	}
}
