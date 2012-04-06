package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class NetInterfaceConfigInfo extends Info {
	String name = null;

	String hwaddr = null;

	String type = null;

	String description = null;

	String address = null;

	String destination = null;

	String broadcast = null;

	String netmask = null;

	long flags = 0L;

	long mtu = 0L;

	long metric = 0L;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getHwaddr() {
		return hwaddr;
	}

	public void setHwaddr(String hwaddr) {
		this.hwaddr = hwaddr;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getAddress() {
		return address;
	}

	public void setAddress(String address) {
		this.address = address;
	}

	public String getDestination() {
		return destination;
	}

	public void setDestination(String destination) {
		this.destination = destination;
	}

	public String getBroadcast() {
		return broadcast;
	}

	public void setBroadcast(String broadcast) {
		this.broadcast = broadcast;
	}

	public String getNetmask() {
		return netmask;
	}

	public void setNetmask(String netmask) {
		this.netmask = netmask;
	}

	public long getFlags() {
		return flags;
	}

	public void setFlags(long flags) {
		this.flags = flags;
	}

	public long getMtu() {
		return mtu;
	}

	public void setMtu(long mtu) {
		this.mtu = mtu;
	}

	public long getMetric() {
		return metric;
	}

	public void setMetric(long metric) {
		this.metric = metric;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("Name", name);
		map.put("Hwaddr", hwaddr);
		map.put("Type", type);
		map.put("Description", description);
		map.put("Address", address);
		map.put("Destination", destination);
		map.put("Broadcast", broadcast);
		map.put("Netmask", netmask);
		map.put("Flags", flags);
		map.put("Mtu", mtu);
		map.put("Metric", metric);
		return map;
	}
}
