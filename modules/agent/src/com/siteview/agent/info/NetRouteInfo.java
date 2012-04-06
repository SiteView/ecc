package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class NetRouteInfo extends Info {
	String destination = null;

	String gateway = null;

	long flags = 0L;

	long refcnt = 0L;

	long use = 0L;

	long metric = 0L;

	String mask = null;

	long mtu = 0L;

	long window = 0L;

	long irtt = 0L;

	String ifname = null;

	public String getDestination() {
		return destination;
	}

	public void setDestination(String destination) {
		this.destination = destination;
	}

	public String getGateway() {
		return gateway;
	}

	public void setGateway(String gateway) {
		this.gateway = gateway;
	}

	public long getFlags() {
		return flags;
	}

	public void setFlags(long flags) {
		this.flags = flags;
	}

	public long getRefcnt() {
		return refcnt;
	}

	public void setRefcnt(long refcnt) {
		this.refcnt = refcnt;
	}

	public long getUse() {
		return use;
	}

	public void setUse(long use) {
		this.use = use;
	}

	public long getMetric() {
		return metric;
	}

	public void setMetric(long metric) {
		this.metric = metric;
	}

	public String getMask() {
		return mask;
	}

	public void setMask(String mask) {
		this.mask = mask;
	}

	public long getMtu() {
		return mtu;
	}

	public void setMtu(long mtu) {
		this.mtu = mtu;
	}

	public long getWindow() {
		return window;
	}

	public void setWindow(long window) {
		this.window = window;
	}

	public long getIrtt() {
		return irtt;
	}

	public void setIrtt(long irtt) {
		this.irtt = irtt;
	}

	public String getIfname() {
		return ifname;
	}

	public void setIfname(String ifname) {
		this.ifname = ifname;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("Destination", destination);
		map.put("Gateway", gateway);
		map.put("Flags", flags);
		map.put("Refcnt", refcnt);
		map.put("Use", use);
		map.put("Metric", metric);
		map.put("Mask", mask);
		map.put("Mtu", mtu);
		map.put("Window", window);
		map.put("Irtt", irtt);
		map.put("Ifname", ifname);
		return map;
	}
}
