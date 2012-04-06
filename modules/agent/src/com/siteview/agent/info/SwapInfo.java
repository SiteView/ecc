package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class SwapInfo extends Info {
	long total = 0L;

	long used = 0L;

	long free = 0L;

	long pageIn = 0L;

	long pageOut = 0L;

	public long getTotal() {
		return total;
	}

	public void setTotal(long total) {
		this.total = total;
	}

	public long getUsed() {
		return used;
	}

	public void setUsed(long used) {
		this.used = used;
	}

	public long getFree() {
		return free;
	}

	public void setFree(long free) {
		this.free = free;
	}

	public long getPageIn() {
		return pageIn;
	}

	public void setPageIn(long pageIn) {
		this.pageIn = pageIn;
	}

	public long getPageOut() {
		return pageOut;
	}

	public void setPageOut(long pageOut) {
		this.pageOut = pageOut;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("Total", total);
		map.put("Used", used);
		map.put("Free", free);
		map.put("PageIn", pageIn);
		map.put("PageOut", pageOut);
		return map;
	}
}
