package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class ProcStatInfo extends Info {
	private long total = 0L;

	private long idle = 0L;

	private long running = 0L;

	private long sleeping = 0L;

	private long stopped = 0L;

	private long zombie = 0L;

	private long threads = 0L;

	public long getTotal() {
		return total;
	}

	public void setTotal(long total) {
		this.total = total;
	}

	public long getIdle() {
		return idle;
	}

	public void setIdle(long idle) {
		this.idle = idle;
	}

	public long getRunning() {
		return running;
	}

	public void setRunning(long running) {
		this.running = running;
	}

	public long getSleeping() {
		return sleeping;
	}

	public void setSleeping(long sleeping) {
		this.sleeping = sleeping;
	}

	public long getStopped() {
		return stopped;
	}

	public void setStopped(long stopped) {
		this.stopped = stopped;
	}

	public long getZombie() {
		return zombie;
	}

	public void setZombie(long zombie) {
		this.zombie = zombie;
	}

	public long getThreads() {
		return threads;
	}

	public void setThreads(long threads) {
		this.threads = threads;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("Total", total);
		map.put("Idle", idle);
		map.put("Running", running);
		map.put("Sleeping", sleeping);
		map.put("Stopped", stopped);
		map.put("Zombie", zombie);
		map.put("Threads", threads);
		return map;
	}
}
