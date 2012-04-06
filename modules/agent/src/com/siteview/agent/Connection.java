package com.siteview.agent;

import com.siteview.agent.Response;

public abstract class Connection {

	public abstract void Open();

	public abstract void Close();

	public abstract RequestBase Receive();
	
	public abstract void SendBack(Response response);
}
