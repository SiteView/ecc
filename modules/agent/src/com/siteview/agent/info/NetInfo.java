package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class NetInfo extends Info {
	String defaultGateway = null;

	String hostName = null;

	String domainName = null;

	String primaryDns = null;

	String secondaryDns = null;

	public String getDefaultGateway() {
		return defaultGateway;
	}

	public void setDefaultGateway(String defaultGateway) {
		this.defaultGateway = defaultGateway;
	}

	public String getHostName() {
		return hostName;
	}

	public void setHostName(String hostName) {
		this.hostName = hostName;
	}

	public String getDomainName() {
		return domainName;
	}

	public void setDomainName(String domainName) {
		this.domainName = domainName;
	}

	public String getPrimaryDns() {
		return primaryDns;
	}

	public void setPrimaryDns(String primaryDns) {
		this.primaryDns = primaryDns;
	}

	public String getSecondaryDns() {
		return secondaryDns;
	}

	public void setSecondaryDns(String secondaryDns) {
		this.secondaryDns = secondaryDns;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("DefaultGateway", defaultGateway);
		map.put("HostName", hostName);
		map.put("DomainName", domainName);
		map.put("PrimaryDns", primaryDns);
		map.put("SecondaryDns", secondaryDns);
		return map;
	}
}
