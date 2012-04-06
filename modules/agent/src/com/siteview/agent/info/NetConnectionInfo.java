package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class NetConnectionInfo extends Info {
	long localPort = 0L;

	String localAddress = null;

	long remotePort = 0L;

	String remoteAddress = null;

	int type = 0;

	int state = 0;

	long sendQueue = 0L;

	long receiveQueue = 0L;

	public long getLocalPort() {
		return localPort;
	}

	public void setLocalPort(long localPort) {
		this.localPort = localPort;
	}

	public String getLocalAddress() {
		return localAddress;
	}

	public void setLocalAddress(String localAddress) {
		this.localAddress = localAddress;
	}

	public long getRemotePort() {
		return remotePort;
	}

	public void setRemotePort(long remotePort) {
		this.remotePort = remotePort;
	}

	public String getRemoteAddress() {
		return remoteAddress;
	}

	public void setRemoteAddress(String remoteAddress) {
		this.remoteAddress = remoteAddress;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public int getState() {
		return state;
	}

	public void setState(int state) {
		this.state = state;
	}

	public long getSendQueue() {
		return sendQueue;
	}

	public void setSendQueue(long sendQueue) {
		this.sendQueue = sendQueue;
	}

	public long getReceiveQueue() {
		return receiveQueue;
	}

	public void setReceiveQueue(long receiveQueue) {
		this.receiveQueue = receiveQueue;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("LocalPort", localPort);
		map.put("LocalAddress", localAddress);
		map.put("RemotePort", remotePort);
		map.put("RemoteAddress", remoteAddress);
		map.put("Type", type);
		map.put("State", state);
		map.put("SendQueue", sendQueue);
		map.put("ReceiveQueue", receiveQueue);
		return map;
	}
}
