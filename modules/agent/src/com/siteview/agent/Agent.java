package com.siteview.agent;

import java.util.Properties;

import com.siteview.agent.AgentServer;

public class Agent {
	private AgentServer agentServer;
	private Properties props;
	public Agent(Properties props)
	{
		this.props = props;
	}
	
	public boolean Run(String cmd) {
		if("start".equalsIgnoreCase(cmd))
		{
			agentServer = new AgentServerUdpImpl(props.getProperty("Address"),props.getProperty("Port"));
			agentServer.start();
			return true;
		}
		if("stop".equalsIgnoreCase(cmd))
		{
			if(null != agentServer)
				agentServer.Stop();
		}
		return false;
	}
}
