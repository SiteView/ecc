package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class TcpInfo extends Info {
	long activeOpens = 0L;

	long passiveOpens = 0L;

	long attemptFails = 0L;

	long estabResets = 0L;

	long currEstab = 0L;

	long inSegs = 0L;

	long outSegs = 0L;

	long retransSegs = 0L;

	long inErrs = 0L;

	long outRsts = 0L;

	public long getActiveOpens() {
		return activeOpens;
	}

	public void setActiveOpens(long activeOpens) {
		this.activeOpens = activeOpens;
	}

	public long getPassiveOpens() {
		return passiveOpens;
	}

	public void setPassiveOpens(long passiveOpens) {
		this.passiveOpens = passiveOpens;
	}

	public long getAttemptFails() {
		return attemptFails;
	}

	public void setAttemptFails(long attemptFails) {
		this.attemptFails = attemptFails;
	}

	public long getEstabResets() {
		return estabResets;
	}

	public void setEstabResets(long estabResets) {
		this.estabResets = estabResets;
	}

	public long getCurrEstab() {
		return currEstab;
	}

	public void setCurrEstab(long currEstab) {
		this.currEstab = currEstab;
	}

	public long getInSegs() {
		return inSegs;
	}

	public void setInSegs(long inSegs) {
		this.inSegs = inSegs;
	}

	public long getOutSegs() {
		return outSegs;
	}

	public void setOutSegs(long outSegs) {
		this.outSegs = outSegs;
	}

	public long getRetransSegs() {
		return retransSegs;
	}

	public void setRetransSegs(long retransSegs) {
		this.retransSegs = retransSegs;
	}

	public long getInErrs() {
		return inErrs;
	}

	public void setInErrs(long inErrs) {
		this.inErrs = inErrs;
	}

	public long getOutRsts() {
		return outRsts;
	}

	public void setOutRsts(long outRsts) {
		this.outRsts = outRsts;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("ActiveOpens", activeOpens);
		map.put("PassiveOpens", passiveOpens);
		map.put("AttemptFails", attemptFails);
		map.put("EstabResets", estabResets);
		map.put("CurrEstab", currEstab);
		map.put("InSegs", inSegs);
		map.put("OutSegs", outSegs);
		map.put("RetransSegs", retransSegs);
		map.put("InErrs", inErrs);
		map.put("OutRsts", outRsts);
		return map;
	}
}
