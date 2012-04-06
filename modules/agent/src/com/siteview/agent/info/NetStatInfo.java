package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class NetStatInfo extends Info {
	int[] tcpStates;
	int tcpInboundTotal;
	int tcpOutboundTotal;
	int allInboundTotal;
	int allOutboundTotal;

	public int[] getTcpStates() {
		return tcpStates;
	}

	public void setTcpStates(int[] tcpStates) {
		this.tcpStates = tcpStates;
	};

	public int getTcpInboundTotal() {
		return tcpInboundTotal;
	}

	public void setTcpInboundTotal(int tcpInboundTotal) {
		this.tcpInboundTotal = tcpInboundTotal;
	};

	public int getTcpOutboundTotal() {
		return tcpOutboundTotal;
	}

	public void setTcpOutboundTotal(int tcpOutboundTotal) {
		this.tcpOutboundTotal = tcpOutboundTotal;
	};

	public int getAllInboundTotal() {
		return allInboundTotal;
	}

	public void setAllInboundTotal(int allInboundTotal) {
		this.allInboundTotal = allInboundTotal;
	};

	public int getAllOutboundTotal() {
		return allOutboundTotal;
	}

	public void setAllOutboundTotal(int allOutboundTotal) {
		this.allOutboundTotal = allOutboundTotal;
	}
	
	public int getTcpEstablished()
	  {
	    return this.tcpStates[1];
	  }

	  public int getTcpSynSent() {
	    return this.tcpStates[2];
	  }

	  public int getTcpSynRecv() {
	    return this.tcpStates[3];
	  }

	  public int getTcpFinWait1() {
	    return this.tcpStates[4];
	  }

	  public int getTcpFinWait2() {
	    return this.tcpStates[5];
	  }

	  public int getTcpTimeWait() {
	    return this.tcpStates[6];
	  }

	  public int getTcpClose() {
	    return this.tcpStates[7];
	  }

	  public int getTcpCloseWait() {
	    return this.tcpStates[8];
	  }

	  public int getTcpLastAck() {
	    return this.tcpStates[9];
	  }

	  public int getTcpListen() {
	    return this.tcpStates[10];
	  }

	  public int getTcpClosing() {
	    return this.tcpStates[11];
	  }

	  public int getTcpIdle() {
	    return this.tcpStates[12];
	  }

	  public int getTcpBound() {
	    return this.tcpStates[13];
	  }

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("TcpInboundTotal", tcpInboundTotal);
		map.put("TcpOutboundTotal", tcpOutboundTotal);
		map.put("AllInboundTotal", allInboundTotal);
		map.put("AllOutboundTotal", allOutboundTotal);
		return map;
	}
}
