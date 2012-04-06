package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class NetInterfaceStatInfo extends Info {
	long rxBytes = 0L;

	long rxPackets = 0L;

	long rxErrors = 0L;

	long rxDropped = 0L;

	long rxOverruns = 0L;

	long rxFrame = 0L;

	long txBytes = 0L;

	long txPackets = 0L;

	long txErrors = 0L;

	long txDropped = 0L;

	long txOverruns = 0L;

	long txCollisions = 0L;

	long txCarrier = 0L;

	long speed = 0L;

	public long getRxBytes() {
		return rxBytes;
	}

	public void setRxBytes(long rxBytes) {
		this.rxBytes = rxBytes;
	}

	public long getRxPackets() {
		return rxPackets;
	}

	public void setRxPackets(long rxPackets) {
		this.rxPackets = rxPackets;
	}

	public long getRxErrors() {
		return rxErrors;
	}

	public void setRxErrors(long rxErrors) {
		this.rxErrors = rxErrors;
	}

	public long getRxDropped() {
		return rxDropped;
	}

	public void setRxDropped(long rxDropped) {
		this.rxDropped = rxDropped;
	}

	public long getRxOverruns() {
		return rxOverruns;
	}

	public void setRxOverruns(long rxOverruns) {
		this.rxOverruns = rxOverruns;
	}

	public long getRxFrame() {
		return rxFrame;
	}

	public void setRxFrame(long rxFrame) {
		this.rxFrame = rxFrame;
	}

	public long getTxBytes() {
		return txBytes;
	}

	public void setTxBytes(long txBytes) {
		this.txBytes = txBytes;
	}

	public long getTxPackets() {
		return txPackets;
	}

	public void setTxPackets(long txPackets) {
		this.txPackets = txPackets;
	}

	public long getTxErrors() {
		return txErrors;
	}

	public void setTxErrors(long txErrors) {
		this.txErrors = txErrors;
	}

	public long getTxDropped() {
		return txDropped;
	}

	public void setTxDropped(long txDropped) {
		this.txDropped = txDropped;
	}

	public long getTxOverruns() {
		return txOverruns;
	}

	public void setTxOverruns(long txOverruns) {
		this.txOverruns = txOverruns;
	}

	public long getTxCollisions() {
		return txCollisions;
	}

	public void setTxCollisions(long txCollisions) {
		this.txCollisions = txCollisions;
	}

	public long getTxCarrier() {
		return txCarrier;
	}

	public void setTxCarrier(long txCarrier) {
		this.txCarrier = txCarrier;
	}

	public long getSpeed() {
		return speed;
	}

	public void setSpeed(long speed) {
		this.speed = speed;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("RxBytes", rxBytes);
		map.put("RxPackets", rxPackets);
		map.put("RxErrors", rxErrors);
		map.put("RxDropped", rxDropped);
		map.put("RxOverruns", rxOverruns);
		map.put("RxFrame", rxFrame);
		map.put("TxBytes", txBytes);
		map.put("TxPackets", txPackets);
		map.put("TxErrors", txErrors);
		map.put("TxDropped", txDropped);
		map.put("TxOverruns", txOverruns);
		map.put("TxCollisions", txCollisions);
		map.put("TxCarrier", txCarrier);
		map.put("Speed", speed);
		return map;
	}
}
