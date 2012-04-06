package com.siteview.agent;

import com.siteview.agent.AgentServer;

public class AgentServerUdpImpl extends AgentServer {

	public AgentServerUdpImpl(String address, String port) {
		int portNum = (port!=null && port.length()>0) ? Integer.parseInt(port) : 3344;
		this.connection = new ConnectionUdpImpl(address, portNum);
		this.dispatcher = new Dispatcher(new SigarCollector());
	}
}
